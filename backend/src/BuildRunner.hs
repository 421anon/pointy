{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BuildRunner (
    BuildState (..),
    StepRequirements (..),
    BuildKey (..),
    JobId (..),
    SlurmJob (..),
    JobComment (..),
    buildKeyForOutPath,
    submitAndWait,
    submitJob,
    queryJobIds,
    queryState,
    querySlurmJobs,
    parseSlurmJobLine,
    encodeJobComment,
    decodeJobComment,
    waitForCompletion,
    cancel,
    isRunningState,
    shellCommand,
) where

import Config (Config (..), SlurmConfig (..), loadConfig, resolveConfigPath)
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode, object, withObject, (.:), (.=))
import Data.Bits (xor)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char (isAlphaNum)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Word (Word64)
import Effectful (Eff, IOE, (:>))
import Effects (Slurm, SlurmQuery (..), SubmitRequest (..), cancelJob, querySlurm)
import qualified Effects as Effects
import Numeric (showHex)
import System.Exit (ExitCode (..))

data StepRequirements = StepRequirements
    { ram :: T.Text
    , cpu :: Int
    , ior :: T.Text
    , iow :: T.Text
    }
    deriving (Eq, Show)

instance FromJSON StepRequirements where
    parseJSON = withObject "StepRequirements" $ \obj ->
        StepRequirements
            <$> obj .: "ram"
            <*> obj .: "cpu"
            <*> obj .: "ior"
            <*> obj .: "iow"

data BuildState = BRunning | BSucceeded | BFailed | BAbsent deriving (Eq, Show)

newtype BuildKey = BuildKey {unBuildKey :: String} deriving (Eq, Show)

newtype JobId = JobId {unJobId :: String} deriving (Eq, Show)

data SlurmJob = SlurmJob
    { slurmJobId :: JobId
    , slurmJobName :: String
    , slurmJobComment :: Maybe String
    , slurmJobState :: String
    }
    deriving (Eq, Show)

data JobComment = JobComment
    { jobCommentKind :: String
    , jobCommentStep :: Int
    , jobCommentCommit :: String
    , jobCommentOutPath :: String
    }
    deriving (Eq, Show)

instance FromJSON JobComment where
    parseJSON = withObject "JobComment" $ \obj ->
        JobComment
            <$> obj .: "kind"
            <*> obj .: "step"
            <*> obj .: "commit"
            <*> obj .: "outPath"

instance ToJSON JobComment where
    toJSON (JobComment kind step commit outPath) =
        object
            [ "kind" .= kind
            , "step" .= step
            , "commit" .= commit
            , "outPath" .= outPath
            ]

encodeJobComment :: JobComment -> String
encodeJobComment = LBS.unpack . encode

decodeJobComment :: String -> Maybe JobComment
decodeJobComment = decode . LBS.pack

buildKeyForOutPath :: FilePath -> BuildKey
buildKeyForOutPath outPath =
    BuildKey $ jobNamePrefix ++ take stemLength sanitizedPath ++ hashSuffix
  where
    sanitizedPath = map sanitizeJobNameChar (dropWhile (== '/') outPath)
    hashSuffix = "-" ++ padLeft 16 '0' (showHex (fnv1a outPath) "")
    stemLength = max 0 (maxJobNameLength - length jobNamePrefix - length hashSuffix)

maxJobNameLength :: Int
maxJobNameLength = 128

fnv1a :: String -> Word64
fnv1a = foldl' step 14695981039346656037
  where
    step hash char = (hash `xor` fromIntegral (fromEnum char)) * 1099511628211

padLeft :: Int -> Char -> String -> String
padLeft width fill value = replicate (max 0 (width - length value)) fill ++ value

jobNamePrefix :: String
jobNamePrefix = "pointy-nix-build-"

sanitizeJobNameChar :: Char -> Char
sanitizeJobNameChar '/' = '-'
sanitizeJobNameChar c
    | isAlphaNum c = c
    | c == '-' = c
    | c == '_' = c
    | c == '.' = c
    | otherwise = '-'

submitAndWait :: (Slurm :> es, IOE :> es) => StepRequirements -> BuildKey -> String -> [String] -> Eff es ExitCode
submitAndWait requirements key comment command = do
    state <- queryState key
    case state of
        BRunning -> ExitSuccess <$ waitForCompletion key
        BSucceeded -> pure ExitSuccess
        BFailed -> pure $ ExitFailure 1
        BAbsent -> submitNewJob requirements key comment command

queryState :: (Slurm :> es) => BuildKey -> Eff es BuildState
queryState (BuildKey key) = do
    result <- querySlurm (JobStatesByName key)
    pure $ case result of
        Right stdout
            | any isRunningState (lines stdout) -> BRunning
            | null (lines stdout) -> BAbsent
            | otherwise -> BRunning
        Left _ -> BAbsent

queryJobIds :: (Slurm :> es) => BuildKey -> Eff es [JobId]
queryJobIds (BuildKey key) = do
    result <- querySlurm (JobIdsByName key)
    pure $ case result of
        Right stdout -> map JobId (filter (not . null) (lines stdout))
        Left _ -> []

querySlurmJobs :: (Slurm :> es) => Eff es [SlurmJob]
querySlurmJobs = do
    result <- querySlurm AllJobs
    pure $ case result of
        Right stdout -> mapMaybe parseSlurmJobLine (lines stdout)
        Left _ -> []

parseSlurmJobLine :: String -> Maybe SlurmJob
parseSlurmJobLine line = case splitOn '|' line of
    [jobId, name, comment, state] | not (null jobId) && not (null name) ->
        Just $ SlurmJob (JobId jobId) name (parseComment comment) state
    _ -> Nothing
  where
    parseComment c
        | null c || c == "(null)" = Nothing
        | otherwise = Just c

splitOn :: Char -> String -> [String]
splitOn sep = go []
  where
    go acc [] = [reverse acc]
    go acc (c : rest)
        | c == sep = reverse acc : go [] rest
        | otherwise = go (c : acc) rest

cancel :: (Slurm :> es) => BuildKey -> Eff es ()
cancel (BuildKey key) = cancelJob key

submitNewJob :: (Slurm :> es, IOE :> es) => StepRequirements -> BuildKey -> String -> [String] -> Eff es ExitCode
submitNewJob requirements (BuildKey key) comment command = do
    slurm <- liftIO $ configSlurm <$> (resolveConfigPath >>= loadConfig)
    result <-
        Effects.submitJob
            SubmitRequest
                { submitJobName = key
                , submitComment = comment
                , submitOptions = requirementSlurmArgs slurm requirements ++ slurmArgs slurm
                , submitDependencies = []
                , submitCommand = command
                , submitWait = True
                }
    pure $ case result of
        Right _ -> ExitSuccess
        Left _ -> ExitFailure 1

submitJob :: (Slurm :> es, IOE :> es) => StepRequirements -> BuildKey -> [JobId] -> String -> [String] -> Eff es (Either String JobId)
submitJob requirements (BuildKey key) depJobIds comment command = do
    slurm <- liftIO $ configSlurm <$> (resolveConfigPath >>= loadConfig)
    result <-
        Effects.submitJob
            SubmitRequest
                { submitJobName = key
                , submitComment = comment
                , submitOptions = requirementSlurmArgs slurm requirements ++ slurmArgs slurm
                , submitDependencies = map unJobId depJobIds
                , submitCommand = command
                , submitWait = False
                }
    pure $ case result of
        Right stdout ->
            case parseJobId stdout of
                Just jobId -> Right jobId
                Nothing -> Left ("sbatch produced no job id: " ++ show stdout)
        Left err -> Left err

parseJobId :: String -> Maybe JobId
parseJobId out =
    case lines out of
        (first : _) ->
            let jobId = takeWhile (/= ';') first
             in if null jobId then Nothing else Just (JobId jobId)
        [] -> Nothing

waitForCompletion :: (Slurm :> es, IOE :> es) => BuildKey -> Eff es ()
waitForCompletion key = do
    state <- queryState key
    case state of
        BRunning -> do
            liftIO $ threadDelay pollDelayMicros
            waitForCompletion key
        _ -> pure ()

pollDelayMicros :: Int
pollDelayMicros = 1000000

requirementSlurmArgs :: SlurmConfig -> StepRequirements -> [String]
requirementSlurmArgs slurm requirements =
    enforcedResourceArgs slurm requirements
        ++ [ "--export=ALL,POINTY_REQ_CPU="
                ++ show (cpu requirements)
                ++ ",POINTY_REQ_RAM="
                ++ T.unpack (ram requirements)
                ++ ",POINTY_REQ_IOR="
                ++ T.unpack (ior requirements)
                ++ ",POINTY_REQ_IOW="
                ++ T.unpack (iow requirements)
           ]

enforcedResourceArgs :: SlurmConfig -> StepRequirements -> [String]
enforcedResourceArgs slurm requirements
    | slurmEnforcement slurm == "metadata-only" = []
    | otherwise = ["--cpus-per-task=" ++ show (cpu requirements)] ++ memArg
  where
    memArg = ["--mem=" ++ T.unpack (ram requirements) | not (T.null (ram requirements))]

slurmArgs :: SlurmConfig -> [String]
slurmArgs slurm =
    partitionArg ++ accountArg ++ timeLimitArg ++ map T.unpack (slurmExtra slurm)
  where
    partition = T.unpack $ slurmPartition slurm
    partitionArg = ["--partition=" ++ partition | not (null partition)]
    accountArg = maybeTextArg "--account=" (slurmAccount slurm)
    timeLimitArg = maybeTextArg "--time=" (slurmTimeLimit slurm)

maybeTextArg :: String -> Maybe T.Text -> [String]
maybeTextArg prefix value = [prefix ++ unpacked | Just text <- [value], let unpacked = T.unpack text, not (null unpacked)]

shellCommand :: [String] -> String
shellCommand = unwords . map shellQuote

shellQuote :: String -> String
shellQuote s = "'" ++ concatMap quoteChar s ++ "'"
  where
    quoteChar '\'' = "'\\''"
    quoteChar c = [c]

isRunningState :: String -> Bool
isRunningState state =
    state
        `elem` [ "PENDING"
               , "CONFIGURING"
               , "RUNNING"
               , "COMPLETING"
               , "SUSPENDED"
               , "RESIZING"
               , "STAGE_OUT"
               ]
