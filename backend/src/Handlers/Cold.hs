{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Cold (
    ColdStatus (..),
    PrefetchResponse (..),
    coldFilesFor,
    coldStatusHandler,
    prefetchHandler,
    parseColdStatus,
    parseColdFiles,
    parsePrefetchResponse,
) where

import Control.Exception (IOException, try)
import Control.Monad (guard)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict, eitherDecodeStrict, withObject, (.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (UnliftStrategy (SeqUnlift), withEffToIO)
import Effects (AppM)
import GHC.Generics (Generic)
import Handlers.StepOutput (resolveStepOutPath)
import Servant (ServerError (..), err502, err503, throwError)
import SingleFlightCache (SingleFlightCache, cachedSuccess, newSingleFlightCache)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (normalise)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import System.Timeout (timeout)

data ColdStatus = ColdStatus
    { cold :: Bool
    , onloading :: Bool
    , done :: Integer
    , total :: Integer
    , files :: [Text]
    }
    deriving (Generic, Show, Eq, ToJSON)

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
    , datasetLocal :: Bool
    , datasetOnload :: Maybe Onload
    , datasetFiles :: [DatasetFile]
    }

data Onload = Onload
    { onloadDone :: Integer
    , onloadTotal :: Integer
    }

data DatasetFile = DatasetFile
    { datasetFilePath :: Text
    , datasetFileLocal :: Bool
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
            <*> o .: "local"
            <*> o .:? "onload"
            <*> (fromMaybe [] <$> o .:? "files")

instance FromJSON Onload where
    parseJSON = withObject "cold storage onload" $ \o ->
        Onload
            <$> (fromMaybe 0 <$> o .:? "bytes_done")
            <*> (fromMaybe 0 <$> o .:? "bytes_total")

instance FromJSON DatasetFile where
    parseJSON = withObject "cold storage dataset file" $ \o ->
        DatasetFile <$> o .: "path" <*> o .: "local"

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

parseColdFiles :: BS.ByteString -> Set Text
parseColdFiles bytes =
    maybe Set.empty (Set.fromList . map (T.pack . normalise . T.unpack) . datasetColdPaths) (firstDataset bytes)

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
        { cold = not (datasetLocal dataset)
        , onloading = datasetPhase dataset == "onloading"
        , done = maybe 0 onloadDone (datasetOnload dataset)
        , total = maybe 0 onloadTotal (datasetOnload dataset)
        , files = datasetColdPaths dataset
        }

datasetColdPaths :: Dataset -> [Text]
datasetColdPaths dataset =
    [datasetFilePath file | file <- datasetFiles dataset, not (datasetFileLocal file)]

emptyColdStatus :: ColdStatus
emptyColdStatus = ColdStatus False False 0 0 []

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

coldFilesFor :: Text -> AppM (Set Text)
coldFilesFor storePath = do
    mSocket <- liftIO coldStorageSocket
    case mSocket of
        Nothing -> pure Set.empty
        Just socket -> do
            result <- liftIO $ runStatus socket storePath
            pure $ either (const Set.empty) parseColdFiles result

coldStatusHandler :: Int -> Maybe Text -> AppM ColdStatus
coldStatusHandler stepId mCommit = do
    outPath <- coldOutPath stepId mCommit
    mSocket <- liftIO coldStorageSocket
    case mSocket of
        Nothing -> pure emptyColdStatus
        Just socket -> do
            result <- liftIO $ runStatus socket outPath
            pure $ either (const emptyColdStatus) (fromMaybe emptyColdStatus . parseColdStatus) result

prefetchHandler :: Int -> Maybe Text -> FilePath -> AppM PrefetchResponse
prefetchHandler stepId mCommit rel = do
    outPath <- coldOutPath stepId mCommit
    mSocket <- liftIO coldStorageSocket
    case mSocket of
        Nothing -> throwError err503{errBody = "cold storage is not configured"}
        Just socket -> do
            result <- liftIO $ runColdStorage (prefetchCommand socket outPath rel)
            case result >>= parsePrefetchResponse of
                Left message -> throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 message)}
                Right response -> pure response

coldStorageSocket :: IO (Maybe String)
coldStorageSocket = do
    value <- lookupEnv "POINTY_COLD_STORE_SOCKET"
    pure $ case value of
        Just socket | not (null socket) -> Just socket
        _ -> Nothing

coldStorageProgram :: FilePath
coldStorageProgram = "pointy-scratch-store"

coldStatusTimeoutMicros :: Int
coldStatusTimeoutMicros = 2000000

runStatus :: String -> Text -> IO (Either Text BS.ByteString)
runStatus socket storePath =
    fromMaybe (Left "cold storage status command timed out")
        <$> timeout coldStatusTimeoutMicros (runColdStorage (statusCommand socket storePath))

statusCommand :: String -> Text -> [String]
statusCommand socket storePath =
    ["status", "--ingest-socket", socket, "--store-path", T.unpack storePath]

prefetchCommand :: String -> Text -> FilePath -> [String]
prefetchCommand socket storePath rel =
    ["prefetch", "--ingest-socket", socket, "--store-path", T.unpack storePath, "--file", rel]

runColdStorage :: [String] -> IO (Either Text BS.ByteString)
runColdStorage args = do
    result <-
        try (readProcessWithExitCode coldStorageProgram args "") ::
            IO (Either IOException (ExitCode, String, String))
    pure $ case result of
        Left err -> Left (T.pack (show err))
        Right (ExitSuccess, out, _) -> Right (TE.encodeUtf8 (T.pack out))
        Right (ExitFailure code, out, err) -> Left (commandFailure code (TE.encodeUtf8 (T.pack out)) err)

commandFailure :: Int -> BS.ByteString -> String -> Text
commandFailure code out err =
    case eitherDecodeStrict out of
        Right line | Just reported <- prefetchLineError line, not (T.null reported) -> reported
        _ -> fallback
  where
    stderrText = T.strip (T.pack err)
    fallback
        | T.null stderrText = "cold storage command exited with code " <> T.pack (show code)
        | otherwise = stderrText
