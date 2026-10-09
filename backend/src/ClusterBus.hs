module ClusterBus
    ( ClusterStatus (..)
    , ClusterSnapshot (..)
    , StepPhase (..)
    , StepActivity (..)
    , StepOutcome (..)
    , FinishedStep (..)
    , TrackedBuild (..)
    , JobProgress (..)
    , setClusterStatus
    , beginBuild
    , endBuild
    , buildingSteps
    , buildingStepsAt
    , trackedBuilds
    , updateJobProgress
    , recordFinished
    , wholeSeconds
    , requestStop
    , stopRequested
    , takeStopRequest
    , snapshotAndSubscribe
    ) where
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime (..), getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data ClusterStatus = Available | Degraded | Unavailable deriving (Eq, Show)

data StepPhase = Preparing | Queued | Running deriving (Eq, Ord, Show)

data StepActivity = StepActivity
    { activityPhase :: StepPhase
    , activitySince :: UTCTime
    , activityReason :: Maybe Text
    , activityCommits :: [Text]
    } deriving (Eq, Show)

data StepOutcome = Succeeded | Failed | Stopped deriving (Eq, Show)

data FinishedStep = FinishedStep
    { finishedStepId :: Int
    , finishedOutcome :: StepOutcome
    , finishedAt :: UTCTime
    , finishedDetail :: Maybe Text
    } deriving (Eq, Show)

data ClusterSnapshot = ClusterSnapshot
    { clusterStatus :: ClusterStatus
    , clusterDetail :: Maybe Text
    , activeSteps :: Map Int StepActivity
    , recentSteps :: [FinishedStep]
    } deriving (Eq, Show)

data TrackedBuild = TrackedBuild
    { trackedSince :: UTCTime
    , trackedCommits :: Map Text Int
    } deriving (Eq, Show)

data JobProgress = JobProgress
    { progressJobId :: Text
    , progressPhase :: StepPhase
    , progressSince :: UTCTime
    , progressReason :: Maybe Text
    } deriving (Eq, Show)

{-# NOINLINE snapshotVar #-}
snapshotVar :: TVar ClusterSnapshot
snapshotVar = unsafePerformIO $ newTVarIO (ClusterSnapshot Available Nothing Map.empty [])

{-# NOINLINE broadcastChan #-}
broadcastChan :: TChan ClusterSnapshot
broadcastChan = unsafePerformIO newBroadcastTChanIO

{-# NOINLINE runningBuilds #-}
runningBuilds :: TVar (Map Int TrackedBuild)
runningBuilds = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE jobProgress #-}
jobProgress :: TVar (Map Int JobProgress)
jobProgress = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE stopRequests #-}
stopRequests :: TVar (Set Int)
stopRequests = unsafePerformIO $ newTVarIO Set.empty

recentLimit :: Int
recentLimit = 50

requestStop :: Int -> IO ()
requestStop stepId = atomically $ modifyTVar' stopRequests (Set.insert stepId)

stopRequested :: Int -> IO Bool
stopRequested stepId = Set.member stepId <$> readTVarIO stopRequests

takeStopRequest :: Int -> IO Bool
takeStopRequest stepId = atomically $ stateTVar stopRequests (\requests -> (Set.member stepId requests, Set.delete stepId requests))

wholeSeconds :: UTCTime -> UTCTime
wholeSeconds time = time {utctDayTime = fromInteger (floor (utctDayTime time))}

publish :: (ClusterSnapshot -> ClusterSnapshot) -> STM ()
publish change = do
    snap <- readTVar snapshotVar
    builds <- readTVar runningBuilds
    progress <- readTVar jobProgress
    let newSnap = (change snap) {activeSteps = Map.mapWithKey (stepActivity progress) builds}
    when (newSnap /= snap) $ do
        writeTVar snapshotVar newSnap
        writeTChan broadcastChan newSnap

stepActivity :: Map Int JobProgress -> Int -> TrackedBuild -> StepActivity
stepActivity progress stepId build = case Map.lookup stepId progress of
    Just job -> StepActivity (progressPhase job) (progressSince job) (progressReason job) commits
    Nothing -> StepActivity Preparing (trackedSince build) Nothing commits
  where
    commits = Map.keys (trackedCommits build)

setClusterStatus :: ClusterStatus -> Maybe Text -> IO ()
setClusterStatus newStatus newDetail =
    atomically $ publish (\snap -> snap {clusterStatus = newStatus, clusterDetail = newDetail})

beginBuild :: Int -> Text -> IO ()
beginBuild stepId commit = do
    now <- wholeSeconds <$> getCurrentTime
    updateRunningBuilds (Map.insertWith track stepId (TrackedBuild now (Map.singleton commit 1)))
  where
    track _ build = build {trackedCommits = Map.insertWith (+) commit 1 (trackedCommits build)}

endBuild :: Int -> Text -> IO ()
endBuild stepId commit =
    updateRunningBuilds (Map.update release stepId)
  where
    release build =
        let commits = Map.update (\count -> if count > 1 then Just (count - 1) else Nothing) commit (trackedCommits build)
         in if Map.null commits then Nothing else Just build {trackedCommits = commits}

buildingStepsAt :: Text -> STM (Set Int)
buildingStepsAt commit = Map.keysSet . Map.filter (Map.member commit . trackedCommits) <$> readTVar runningBuilds

buildingSteps :: STM (Set Int)
buildingSteps = Map.keysSet <$> readTVar runningBuilds

trackedBuilds :: STM (Map Int TrackedBuild)
trackedBuilds = readTVar runningBuilds

updateRunningBuilds :: (Map Int TrackedBuild -> Map Int TrackedBuild) -> IO ()
updateRunningBuilds change = atomically $ do
    modifyTVar' runningBuilds change
    builds <- readTVar runningBuilds
    modifyTVar' jobProgress (`Map.restrictKeys` Map.keysSet builds)
    publish id

updateJobProgress :: (Map Int TrackedBuild -> Map Int JobProgress -> Map Int JobProgress) -> IO ()
updateJobProgress derive = atomically $ do
    builds <- readTVar runningBuilds
    modifyTVar' jobProgress (\progress -> derive builds progress `Map.restrictKeys` Map.keysSet builds)
    publish id

recordFinished :: Int -> StepOutcome -> Maybe Text -> IO ()
recordFinished stepId outcome detail = do
    now <- wholeSeconds <$> getCurrentTime
    atomically $
        publish $ \snap ->
            snap
                { recentSteps =
                    take recentLimit $
                        FinishedStep stepId outcome now detail
                            : filter ((/= stepId) . finishedStepId) (recentSteps snap)
                }

snapshotAndSubscribe :: IO (ClusterSnapshot, TChan ClusterSnapshot)
snapshotAndSubscribe = atomically $ do
    snap <- readTVar snapshotVar
    chan <- dupTChan broadcastChan
    return (snap, chan)
