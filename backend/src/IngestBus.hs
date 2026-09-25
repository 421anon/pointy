{-# LANGUAGE OverloadedStrings #-}

module IngestBus (
    IngestJob (..),
    IngestJobState (..),
    reserveJob,
    recordProgress,
    recordSuccess,
    recordFailure,
    jobsSnapshot,
    snapshotAndSubscribe,
) where

import Control.Concurrent.STM (TChan, TVar, atomically, dupTChan, newBroadcastTChanIO, newTVarIO, readTVar, readTVarIO, writeTChan, writeTVar)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

data IngestJobState = JobRunning | JobSucceeded | JobFailed deriving (Eq, Show)

data IngestJob = IngestJob
    { jobId :: Int
    , jobStepId :: Int
    , jobSource :: Text
    , jobPath :: Maybe Text
    , jobState :: IngestJobState
    , jobDone :: Maybe Integer
    , jobTotal :: Maybe Integer
    , jobHash :: Maybe Text
    , jobError :: Maybe Text
    , jobStamp :: UTCTime
    }

instance ToJSON IngestJob where
    toJSON job =
        object
            [ "id" .= jobId job
            , "stepId" .= jobStepId job
            , "source" .= jobSource job
            , "path" .= jobPath job
            , "state" .= stateName (jobState job)
            , "done" .= jobDone job
            , "total" .= jobTotal job
            , "hash" .= jobHash job
            , "error" .= jobError job
            ]

{-# NOINLINE jobsVar #-}
jobsVar :: TVar (Map Int IngestJob)
jobsVar = unsafePerformIO (newTVarIO Map.empty)

{-# NOINLINE nextJobIdVar #-}
nextJobIdVar :: TVar Int
nextJobIdVar = unsafePerformIO (newTVarIO 1)

{-# NOINLINE ingestChanVar #-}
ingestChanVar :: TChan (Map Int IngestJob)
ingestChanVar = unsafePerformIO newBroadcastTChanIO

progressInterval :: NominalDiffTime
progressInterval = 1

reserveJob :: Int -> Text -> Maybe Text -> IO (Maybe Int)
reserveJob stepId source path = do
    now <- getCurrentTime
    atomically $ do
        jobs <- readTVar jobsVar
        case Map.lookup stepId jobs of
            Just job | jobState job == JobRunning -> pure Nothing
            _ -> do
                ident <- readTVar nextJobIdVar
                writeTVar nextJobIdVar (ident + 1)
                let job = IngestJob ident stepId source path JobRunning Nothing Nothing Nothing Nothing now
                    updated = Map.insert stepId job jobs
                writeTVar jobsVar updated
                writeTChan ingestChanVar updated
                pure (Just ident)

recordProgress :: Int -> Integer -> Integer -> IO ()
recordProgress ident done total = do
    now <- getCurrentTime
    atomically $ do
        jobs <- readTVar jobsVar
        case locate ident jobs of
            Just (stepId, job) | jobState job == JobRunning && diffUTCTime now (jobStamp job) >= progressInterval -> do
                let updated = Map.insert stepId job{jobDone = Just done, jobTotal = Just total, jobStamp = now} jobs
                writeTVar jobsVar updated
                writeTChan ingestChanVar updated
            _ -> pure ()

recordSuccess :: Int -> Text -> IO ()
recordSuccess ident hash = finish ident JobSucceeded (Just hash) Nothing

recordFailure :: Int -> Text -> IO ()
recordFailure ident message = finish ident JobFailed Nothing (Just message)

jobsSnapshot :: IO (Map Int IngestJob)
jobsSnapshot = readTVarIO jobsVar

snapshotAndSubscribe :: IO (Map Int IngestJob, TChan (Map Int IngestJob))
snapshotAndSubscribe = atomically $ do
    jobs <- readTVar jobsVar
    chan <- dupTChan ingestChanVar
    pure (jobs, chan)

finish :: Int -> IngestJobState -> Maybe Text -> Maybe Text -> IO ()
finish ident state mHash mError = atomically $ do
    jobs <- readTVar jobsVar
    case locate ident jobs of
        Nothing -> pure ()
        Just (stepId, job) -> do
            let updated = Map.insert stepId job{jobState = state, jobHash = mHash, jobError = mError} jobs
            writeTVar jobsVar updated
            writeTChan ingestChanVar updated

locate :: Int -> Map Int IngestJob -> Maybe (Int, IngestJob)
locate ident jobs = case filter ((== ident) . jobId . snd) (Map.toList jobs) of
    (found : _) -> Just found
    [] -> Nothing

stateName :: IngestJobState -> Text
stateName JobRunning = "running"
stateName JobSucceeded = "succeeded"
stateName JobFailed = "failed"
