{-# LANGUAGE OverloadedStrings #-}

module BuildRunner (
    BuildState (..),
    StepRequirements (..),
    BuildKey (..),
    JobId (..),
    buildKeyForOutPath,
    submitAndWait,
    submitJob,
    queryJobIds,
    queryState,
    waitForCompletion,
    cancel,
) where

import Config (Config (..), SlurmConfig (..), loadConfig, resolveConfigPath)
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Bits (xor)
import Data.Char (isAlphaNum)
import Data.List (foldl', intercalate)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (showHex)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)

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

-- | A slurm job id, as printed by @sbatch --parsable@ and @squeue -o %i@.
newtype JobId = JobId {unJobId :: String} deriving (Eq, Show)

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

submitAndWait :: StepRequirements -> BuildKey -> [String] -> IO ExitCode
submitAndWait requirements key command = do
    state <- queryState key
    case state of
        BRunning -> ExitSuccess <$ waitForCompletion key
        BSucceeded -> pure ExitSuccess
        BFailed -> pure $ ExitFailure 1
        BAbsent -> submitNewJob requirements key command

queryState :: BuildKey -> IO BuildState
queryState (BuildKey key) = do
    (exitCode, stdout, _) <- readProcessWithExitCode "squeue" ["-h", "-n", key, "-o", "%T"] ""
    pure $ case exitCode of
        ExitSuccess
            | any isRunningState (lines stdout) -> BRunning
            | null (lines stdout) -> BAbsent
            | otherwise -> BRunning
        ExitFailure _ -> BAbsent

-- | Job ids of every queued or running job with the given name.
queryJobIds :: BuildKey -> IO [JobId]
queryJobIds (BuildKey key) = do
    (exitCode, stdout, _) <- readProcessWithExitCode "squeue" ["-h", "-n", key, "-o", "%i"] ""
    pure $ case exitCode of
        ExitSuccess -> map JobId (filter (not . null) (lines stdout))
        ExitFailure _ -> []

cancel :: BuildKey -> IO ()
cancel (BuildKey key) = do
    _ <- readProcessWithExitCode "scancel" ["--name=" ++ key] ""
    pure ()

submitNewJob :: StepRequirements -> BuildKey -> [String] -> IO ExitCode
submitNewJob requirements (BuildKey key) command = do
    slurm <- configSlurm <$> (resolveConfigPath >>= loadConfig)
    (exitCode, _, _) <-
        readProcessWithExitCode
            "sbatch"
            ( [ "--wait"
              , "--parsable"
              , "--job-name=" ++ key
              , "--output=/dev/null"
              , "--error=/dev/null"
              ]
                ++ requirementSlurmArgs slurm requirements
                ++ slurmArgs slurm
                ++ ["--wrap=" ++ shellCommand command]
            )
            ""
    pure exitCode

{- | Submit a job without waiting for completion. Non-empty @depJobIds@
become @afterok@ dependency edges; the job is killed if any of them fails.
-}
submitJob :: StepRequirements -> BuildKey -> [JobId] -> [String] -> IO (Either String JobId)
submitJob requirements (BuildKey key) depJobIds command = do
    slurm <- configSlurm <$> (resolveConfigPath >>= loadConfig)
    (exitCode, stdout, stderr) <-
        readProcessWithExitCode
            "sbatch"
            ( [ "--parsable"
              , "--job-name=" ++ key
              , "--output=/dev/null"
              , "--error=/dev/null"
              ]
                ++ dependencyArgs depJobIds
                ++ requirementSlurmArgs slurm requirements
                ++ slurmArgs slurm
                ++ ["--wrap=" ++ shellCommand command]
            )
            ""
    pure $ case exitCode of
        ExitSuccess ->
            case parseJobId stdout of
                Just jobId -> Right jobId
                Nothing -> Left ("sbatch produced no job id: " ++ show stdout)
        ExitFailure code -> Left ("sbatch failed (exit " ++ show code ++ "): " ++ stderr)

dependencyArgs :: [JobId] -> [String]
dependencyArgs [] = []
dependencyArgs jobIds =
    [ "--dependency=afterok:" ++ intercalate ":" (map unJobId jobIds)
    , "--kill-on-invalid-dep=yes"
    ]

-- | @--parsable@ prints @jobid@ or @jobid;cluster@ on the first line.
parseJobId :: String -> Maybe JobId
parseJobId out =
    case lines out of
        (first : _) ->
            let jobId = takeWhile (/= ';') first
             in if null jobId then Nothing else Just (JobId jobId)
        [] -> Nothing

{- | Poll until no queued or running job with this name remains. Completion
does not imply success; callers must check the expected store path.
-}
waitForCompletion :: BuildKey -> IO ()
waitForCompletion key = do
    state <- queryState key
    case state of
        BRunning -> do
            threadDelay pollDelayMicros
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
           , "--comment=" ++ requirementComment requirements
           ]

enforcedResourceArgs :: SlurmConfig -> StepRequirements -> [String]
enforcedResourceArgs slurm requirements
    | slurmEnforcement slurm == "metadata-only" = []
    | otherwise = ["--cpus-per-task=" ++ show (cpu requirements)] ++ memArg
  where
    memArg = ["--mem=" ++ T.unpack (ram requirements) | not (T.null (ram requirements))]

requirementComment :: StepRequirements -> String
requirementComment requirements =
    unwords
        [ "pointy-requirements"
        , "cpu=" ++ show (cpu requirements)
        , "ram=" ++ T.unpack (ram requirements)
        , "ior=" ++ T.unpack (ior requirements)
        , "iow=" ++ T.unpack (iow requirements)
        ]

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
