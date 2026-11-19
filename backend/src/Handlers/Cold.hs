{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.Cold (
    ColdStatus (..),
    ColdFile (..),
    PrefetchResponse (..),
    coldFilesFor,
    offloadedEnclosing,
    offloadedFilesNamedIn,
    offloadedFilesNamed,
    stepIdsNamedIn,
    stepLabel,
    unreadableLines,
    offloadedReadNote,
    refuseColdFile,
    refuseOffloadedIn,
    coldStatusHandler,
    prefetchHandler,
    parseColdStatus,
    parseColdFiles,
    parsePrefetchResponse,
) where

import Control.Exception (IOException, catch)
import Control.Monad (forM, forM_, guard)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict, eitherDecodeStrict, object, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isDigit)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.List (find, isPrefixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (Eff, IOE, UnliftStrategy (SeqUnlift), withEffToIO, (:>))
import Effects (AppM, ColdStorage, coldStorageConfigured, coldStoragePrefetch, coldStorageStatus)
import GHC.Generics (Generic)
import Handlers.StepOutput (resolveStepOutPath)
import NixStore (rootedPath)
import Servant (ServerError (..), err409, err502, err503, throwError)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import System.Directory (getSymbolicLinkTarget, pathIsSymbolicLink)
import System.FilePath (joinPath, normalise, splitDirectories)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

data ColdStatus = ColdStatus
    { cold :: Bool
    , onloading :: Bool
    , files :: [ColdFile]
    }
    deriving (Generic, Show, Eq, ToJSON)

data ColdFile = ColdFile
    { filePath :: Text
    , fileDone :: Integer
    , fileTotal :: Integer
    }
    deriving (Generic, Show, Eq)

instance ToJSON ColdFile where
    toJSON file =
        object
            [ "path" .= filePath file
            , "done" .= fileDone file
            , "total" .= fileTotal file
            ]

data PrefetchResponse = PrefetchResponse
    { ok :: Bool
    , started :: [Text]
    , skipped :: [Text]
    , unknown :: [Text]
    }
    deriving (Generic, Show, Eq, ToJSON)

data StatusLine = StatusLine
    { statusLineOk :: Bool
    , statusLineStatus :: Maybe StoreStatus
    }

data StoreStatus = StoreStatus
    { storeStatusDatasets :: [Dataset]
    }

data Dataset = Dataset
    { datasetPhase :: Text
    , datasetFiles :: [DatasetFile]
    }

data Onload = Onload
    { onloadDone :: Integer
    , onloadTotal :: Integer
    }

data DatasetFile = DatasetFile
    { datasetFilePath :: Text
    , datasetFileLocal :: Bool
    , datasetFileOnload :: Maybe Onload
    }

data PrefetchLine = PrefetchLine
    { prefetchLineOk :: Bool
    , prefetchLineError :: Maybe Text
    , prefetchLineStarted :: [Text]
    , prefetchLineSkipped :: [Text]
    , prefetchLineUnknown :: [Text]
    }

instance FromJSON StatusLine where
    parseJSON = withObject "cold storage status" $ \o ->
        StatusLine <$> o .: "ok" <*> o .:? "status"

instance FromJSON StoreStatus where
    parseJSON = withObject "cold storage status body" $ \o ->
        StoreStatus . fromMaybe [] <$> o .:? "datasets"

instance FromJSON Dataset where
    parseJSON = withObject "cold storage dataset" $ \o ->
        Dataset
            <$> o .: "phase"
            <*> (fromMaybe [] <$> o .:? "files")

instance FromJSON Onload where
    parseJSON = withObject "cold storage onload" $ \o ->
        Onload
            <$> (fromMaybe 0 <$> o .:? "bytes_done")
            <*> (fromMaybe 0 <$> o .:? "bytes_total")

instance FromJSON DatasetFile where
    parseJSON = withObject "cold storage dataset file" $ \o ->
        DatasetFile <$> o .: "path" <*> o .: "local" <*> o .:? "onload"

instance FromJSON PrefetchLine where
    parseJSON = withObject "cold storage prefetch" $ \o ->
        PrefetchLine
            <$> o .: "ok"
            <*> o .:? "error"
            <*> (fromMaybe [] <$> o .:? "started")
            <*> (fromMaybe [] <$> o .:? "skipped")
            <*> (fromMaybe [] <$> o .:? "unknown")

parseColdStatus :: BS.ByteString -> Maybe ColdStatus
parseColdStatus = fmap datasetColdStatus . firstDataset

parseColdFiles :: BS.ByteString -> Maybe (Set Text)
parseColdFiles bytes = do
    line <- decodeStrict bytes
    guard (statusLineOk line)
    let dataset = listToMaybe (maybe [] storeStatusDatasets (statusLineStatus line))
    pure $ maybe Set.empty (Set.fromList . map (T.pack . normalise . T.unpack) . datasetColdPaths) dataset

parsePrefetchResponse :: BS.ByteString -> Either Text PrefetchResponse
parsePrefetchResponse bytes = case eitherDecodeStrict bytes of
    Left err -> Left ("Failed to parse cold storage prefetch output: " <> T.pack err)
    Right line
        | prefetchLineOk line ->
            Right
                PrefetchResponse
                    { ok = True
                    , started = prefetchLineStarted line
                    , skipped = prefetchLineSkipped line
                    , unknown = prefetchLineUnknown line
                    }
        | otherwise -> Left (fromMaybe "Cold storage prefetch failed" (prefetchLineError line))

firstDataset :: BS.ByteString -> Maybe Dataset
firstDataset bytes = do
    line <- decodeStrict bytes
    guard (statusLineOk line)
    listToMaybe (maybe [] storeStatusDatasets (statusLineStatus line))

datasetColdStatus :: Dataset -> ColdStatus
datasetColdStatus dataset =
    ColdStatus
        { cold = not (null coldFiles)
        , onloading = datasetPhase dataset == "onloading" || any (isJust . datasetFileOnload) coldFiles
        , files = map coldFile coldFiles
        }
  where
    coldFiles = filter (not . datasetFileLocal) (datasetFiles dataset)

coldFile :: DatasetFile -> ColdFile
coldFile file =
    ColdFile
        { filePath = datasetFilePath file
        , fileDone = maybe 0 onloadDone (datasetFileOnload file)
        , fileTotal = maybe 0 onloadTotal (datasetFileOnload file)
        }

datasetColdPaths :: Dataset -> [Text]
datasetColdPaths dataset =
    [datasetFilePath file | file <- datasetFiles dataset, not (datasetFileLocal file)]

emptyColdStatus :: ColdStatus
emptyColdStatus = ColdStatus False False []

{-# NOINLINE coldOutPaths #-}
coldOutPaths :: SingleFlightCache (Int, Text) Text
coldOutPaths = unsafePerformIO (newSingleFlightCache 256)

coldOutPath :: Int -> Maybe Text -> AppM Text
coldOutPath stepId mCommit = case mCommit of
    Nothing -> resolveStepOutPath stepId Nothing
    Just commit ->
        ExceptT $
            withEffToIO SeqUnlift $ \unlift ->
                cachedSuccess coldOutPaths (stepId, commit) (unlift (runExceptT (resolveStepOutPath stepId (Just commit))))

coldFilesFor :: (ColdStorage :> es, IOE :> es) => Text -> Eff es (Maybe (Set Text))
coldFilesFor storePath = do
    configured <- coldStorageConfigured
    if not configured
        then pure (Just Set.empty)
        else do
            datasetPath <- liftIO (coldDatasetPath storePath)
            either (const Nothing) parseColdFiles <$> coldStorageStatus datasetPath

offloadedEnclosing :: Set Text -> FilePath -> Maybe Text
offloadedEnclosing coldFiles rel = find (`Set.member` coldFiles) enclosing
  where
    components = dropWhile (== "/") (splitDirectories (normalise rel))
    enclosing = [T.pack (joinPath (take n components)) | n <- [1 .. length components]]

refuseColdFile :: Text -> FilePath -> AppM ()
refuseColdFile storePath rel = do
    coldFiles <- lift (coldFilesFor storePath)
    refuseOffloadedIn (fromMaybe Set.empty coldFiles) rel

refuseOffloadedIn :: Set Text -> FilePath -> AppM ()
refuseOffloadedIn coldFiles rel =
    forM_ (offloadedEnclosing coldFiles rel) $ \_ ->
        throwError err409{errBody = "This file is offloaded to cold storage. Copy it back from its snowflake in the file browser, then try again."}

offloadedFilesNamedIn :: (ColdStorage :> es, IOE :> es) => [(Text, Text)] -> Text -> Eff es [(Text, Text)]
offloadedFilesNamedIn labelledOutputs output = do
    let unreadable = unreadableLines output
        named = labelledOutputs ++ [(storePath, storePath) | storePath <- concatMap storePathsIn unreadable]
    coldByLabel <- forM named $ \(label, storePath) -> (,) label . fromMaybe Set.empty <$> coldFilesFor storePath
    pure (offloadedFilesNamed unreadable coldByLabel)

unreadableLines :: Text -> [Text]
unreadableLines = filter reportsUnreadable . T.lines
  where
    reportsUnreadable line = any (`T.isInfixOf` line) ["Input/output error", "I/O error", "offloaded to cold storage"]

storePathsIn :: Text -> [Text]
storePathsIn line =
    [ "/nix/store/" <> base
    | (_, match) <- T.breakOnAll "/nix/store/" line
    , let base = T.takeWhile (\c -> isAlphaNum c || c `elem` ("+-._?=" :: String)) (T.drop (T.length "/nix/store/") match)
    , not (T.null base)
    ]

stepIdsNamedIn :: [Text] -> [Int]
stepIdsNamedIn unreadable =
    Set.toList $
        Set.fromList
            [ stepId
            | line <- unreadable
            , token <- T.split (`elem` (" '\"`:(" :: String)) line
            , let (digits, rest) = T.span isDigit (fromMaybe token (T.stripPrefix "./" token))
            , not (T.null digits) && "/" `T.isPrefixOf` rest
            , Just stepId <- [readMaybe (T.unpack digits)]
            ]

stepLabel :: Int -> Text
stepLabel stepId = "step " <> T.pack (show stepId)

offloadedFilesNamed :: [Text] -> [(Text, Set Text)] -> [(Text, Text)]
offloadedFilesNamed unreadable coldByLabel =
    [ (label, file)
    | (label, coldFiles) <- coldByLabel
    , let namedByOwnedLines = Set.fromList (concat [namedIn coldFiles line | line <- unreadable, ownedBy label line])
    , file <- Set.toList namedByOwnedLines
    ]
  where
    namedIn coldFiles line =
        let named = filter (names line) (Set.toList coldFiles)
         in filter (\file -> not (any (\longer -> longer /= file && file `T.isSuffixOf` longer) named)) named
    names line file = file `T.isPrefixOf` line || any (\boundary -> (boundary <> file) `T.isInfixOf` line) ["/", " ", "'", "\""]
    ownedBy label line = case map stepLabel (stepIdsNamedIn [line]) ++ storePathsIn line of
        [] -> True
        owners -> label `elem` owners

offloadedReadNote :: (Text, Text) -> Text
offloadedReadNote (label, file) =
    file <> " of " <> label <> " is offloaded to cold storage, so it could not be read. Copy it back from its snowflake in the file browser, then try again."

coldStatusHandler :: Int -> Maybe Text -> AppM ColdStatus
coldStatusHandler stepId mCommit = do
    configured <- lift coldStorageConfigured
    if not configured
        then pure emptyColdStatus
        else do
            outPath <- coldOutPath stepId mCommit
            datasetPath <- liftIO (coldDatasetPath outPath)
            result <- lift (coldStorageStatus datasetPath)
            pure $ either (const emptyColdStatus) (fromMaybe emptyColdStatus . parseColdStatus) result

prefetchHandler :: Int -> Maybe Text -> FilePath -> AppM PrefetchResponse
prefetchHandler stepId mCommit rel = do
    configured <- lift coldStorageConfigured
    if not configured
        then throwError err503{errBody = "cold storage is not configured"}
        else do
            outPath <- coldOutPath stepId mCommit
            datasetPath <- liftIO (coldDatasetPath outPath)
            result <- lift (coldStoragePrefetch datasetPath rel)
            case result >>= parsePrefetchResponse of
                Left message -> throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 message)}
                Right response -> pure response

coldDatasetPath :: Text -> IO Text
coldDatasetPath outPath = do
    let real = rootedPath (T.unpack outPath)
    symlink <- pathIsSymbolicLink real `catch` \(_ :: IOException) -> pure False
    if not symlink
        then pure outPath
        else do
            target <- getSymbolicLinkTarget real
            pure $
                if "/nix/store/" `isPrefixOf` target
                    then T.pack (normalise target)
                    else outPath
