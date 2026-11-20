{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module SlurmWatch (
    TerminalJob (..),
    WatchEvent (..),
    applyWatchEvent,
    parseWatchEvent,
    snapshotJobs,
    startSlurmWatch,
    watchHeartbeatTimeoutMicros,
) where

import BuildRunner (JobId (..), SlurmJob (..), isRunningState, parseSlurmJobLine)
import ClusterBus (ClusterStatus (..), applySnapshot, setClusterStatus, updateJobProgress, wholeSeconds)
import ClusterStatusReport (clusterStatusFromAvailability, deriveJobProgress)
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, SomeException, displayException, try)
import Control.Monad (unless, void, when)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.:), (.:?), (.!=))
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import EffectRunner (runAppEffects)
import Effects (SlurmQuery (..), querySlurm)
import JobOutcome (TerminalState (..), recordTerminalStates)
import JobWatch (markJobsEnded, markTerminalJobs, watchedJobs)
import System.Environment (lookupEnv)
import System.IO (Handle)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), proc, terminateProcess, waitForProcess, withCreateProcess)
import System.Timeout (timeout)

data TerminalJob = TerminalJob
    { terminalJobId :: Text
    , terminalJobName :: Text
    , terminalJobState :: Maybe Text
    }
    deriving (Eq, Show)

instance FromJSON TerminalJob where
    parseJSON = withObject "terminal Slurm job" $ \object ->
        TerminalJob <$> object .: "id" <*> object .: "name" <*> object .:? "state"

data WatchEvent
    = WatchSnapshot
        { snapshotJobsText :: Text
        , snapshotClusterText :: Maybe Text
        , snapshotTerminalJobs :: [TerminalJob]
        }
    | WatchHeartbeat
    deriving (Eq, Show)

instance FromJSON WatchEvent where
    parseJSON = withObject "Slurm watch event" $ \object -> do
        kind <- object .: "type" :: Parser Text
        case kind of
            "snapshot" ->
                WatchSnapshot
                    <$> object .: "jobs"
                    <*> object .:? "cluster"
                    <*> (object .:? "terminalJobs" .!= [])
            "heartbeat" -> pure WatchHeartbeat
            other -> fail ("unknown Slurm watch event type: " ++ T.unpack other)

parseWatchEvent :: BS.ByteString -> Either String WatchEvent
parseWatchEvent = eitherDecodeStrict'

snapshotJobs :: Text -> [SlurmJob]
snapshotJobs = mapMaybe parseSlurmJobLine . lines . T.unpack

startSlurmWatch :: IO ()
startSlurmWatch = do
    command <- watchCommand
    setClusterUnavailable "waiting for a Slurm watch snapshot"
    void $ forkIO $ supervise command initialBackoffMicros
  where
    watchCommand = do
        configured <- lookupEnv "POINTY_SLURM_WATCH_COMMAND"
        pure $ case configured of
            Just path | not (null path) -> path
            _ -> "pointy-slurm-watch"

supervise :: FilePath -> Int -> IO ()
supervise command delay = do
    (received, failure) <- watchOnce command `catchUnexpected` \err -> do
        logLine ("slurm watch failed: " ++ err)
        pure (False, Just (T.pack err))
    case failure of
        Just detail -> setClusterUnavailable detail
        Nothing -> pure ()
    let nextDelay = if received then initialBackoffMicros else min maxBackoffMicros (delay * 2)
    threadDelay (if received then initialBackoffMicros else delay)
    supervise command nextDelay
  where
    catchUnexpected :: IO a -> (String -> IO a) -> IO a
    catchUnexpected action handler = do
        outcome <- try @SomeException action
        either (handler . displayException) pure outcome

watchOnce :: FilePath -> IO (Bool, Maybe Text)
watchOnce command = do
    outcome <- try @SomeException $
        withCreateProcess (proc command []){std_in = NoStream, std_out = CreatePipe, std_err = Inherit} $ \_ mOut _ processHandle ->
            case mOut of
                Nothing -> pure (False, Just "slurm watch produced no stdout pipe")
                Just output -> readFrames output processHandle False
    case outcome of
        Left err -> pure (False, Just (T.pack (displayException err)))
        Right result -> pure result

readFrames :: Handle -> ProcessHandle -> Bool -> IO (Bool, Maybe Text)
readFrames output processHandle received = do
    next <- timeout watchHeartbeatTimeoutMicros (try (BS.hGetLine output) :: IO (Either IOException BS.ByteString))
    case next of
        Nothing -> do
            logLine "slurm watch heartbeat timed out"
            terminateProcess processHandle
            void $ waitForProcess processHandle
            pure (received, Just "no Slurm watch heartbeat")
        Just (Left err) -> do
            logLine ("slurm watch stream ended: " ++ displayException (err :: IOException))
            pure (received, Just (T.pack (displayException err)))
        Just (Right line) -> case parseWatchEvent line of
            Left err -> do
                logLine ("slurm watch ignored a malformed frame: " ++ err)
                readFrames output processHandle True
            Right event -> do
                applyWatchEvent event
                readFrames output processHandle True

applyWatchEvent :: WatchEvent -> IO ()
applyWatchEvent WatchHeartbeat = pure ()
applyWatchEvent (WatchSnapshot jobsText clusterText terminalJobs) = do
    now <- wholeSeconds <$> getCurrentTime
    let jobs = snapshotJobs jobsText
        hints = [(T.unpack (terminalJobId job), T.unpack (terminalJobName job)) | job <- terminalJobs]
        states =
            [ TerminalState (T.unpack (terminalJobId job)) (T.unpack (terminalJobName job)) (T.unpack state)
            | job <- terminalJobs
            , Just state <- [terminalJobState job]
            ]
    case clusterText of
        Nothing -> applySnapshot Nothing now jobs
        Just transcript ->
            let (status, detail) = clusterStatusFromAvailability (Right (T.unpack transcript))
             in applySnapshot (Just (status, detail)) now jobs
    recordTerminalStates states
    markTerminalJobs hints
    reconcileCompletion jobs

reconcileCompletion :: [SlurmJob] -> IO ()
reconcileCompletion snapshotJobs' = do
    watched <- watchedJobs
    let activeIds = jobIdsByName snapshotJobs'
        candidates =
            [ (watchId, name, jobIds)
            | (watchId, name, jobIds) <- watched
            , needsRecheck name jobIds activeIds
            ]
        names = Set.toList (Set.fromList [name | (_, name, _) <- candidates])
    unless (null candidates) $ do
        result <- timeout recheckTimeoutMicros (runAppEffects (querySlurm (JobsByNames names)))
        case result of
            Nothing -> logLine "slurm watch completion recheck timed out"
            Just (Left err) -> logLine ("slurm watch completion recheck failed: " ++ err)
            Just (Right stdout) -> do
                now <- wholeSeconds <$> getCurrentTime
                let jobs = mapMaybe parseSlurmJobLine (lines stdout)
                    recheckedIds = jobIdsByName jobs
                    ended = [watchId | (watchId, name, jobIds) <- candidates, jobIsAbsent name jobIds recheckedIds]
                when (not (null jobs)) $ updateJobProgress (deriveJobProgress now jobs)
                markJobsEnded ended
  where
    needsRecheck name jobIds activeIds
        | Set.null jobIds = Set.notMember name (Map.keysSet activeIds)
        | otherwise = Set.null (Set.intersection jobIds (Map.findWithDefault Set.empty name activeIds))
    jobIsAbsent name jobIds recheckedIds
        | Set.null jobIds = Set.notMember name (Map.keysSet recheckedIds)
        | otherwise = Set.null (Set.intersection jobIds (Map.findWithDefault Set.empty name recheckedIds))

jobIdsByName :: [SlurmJob] -> Map.Map String (Set.Set String)
jobIdsByName jobs =
    Map.fromListWith Set.union
        [ (slurmJobName job, Set.singleton (unJobId (slurmJobId job)))
        | job <- jobs
        , isRunningState (slurmJobState job)
        ]

setClusterUnavailable :: Text -> IO ()
setClusterUnavailable detail = setClusterStatus Unavailable (Just (T.take 200 detail))

logLine :: String -> IO ()
logLine = putStrLn . ("slurm watch: " ++)

initialBackoffMicros :: Int
initialBackoffMicros = 1000000

maxBackoffMicros :: Int
maxBackoffMicros = 30000000

watchHeartbeatTimeoutMicros :: Int
watchHeartbeatTimeoutMicros = 45000000

recheckTimeoutMicros :: Int
recheckTimeoutMicros = 10000000
