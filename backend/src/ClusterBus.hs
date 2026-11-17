module ClusterBus
    ( ClusterStatus (..)
    , ClusterSnapshot (..)
    , StepPhase (..)
    , StepActivity (..)
    , TrackedBuild (..)
    , JobProgress (..)
    , setClusterStatus
    , requestBuild
    , clearBuildRequest
    , beginBuild
    , endBuild
    , markQueued
    , buildingSteps
    , buildingStepsAt
    , deriveJobProgress
    , updateJobProgress
    , applySnapshot
    , wholeSeconds
    , requestStop
    , stopRequested
    , takeStopRequest
    , snapshotAndSubscribe
    ) where
import BuildRunner (JobComment (..), JobId (..), SlurmJob (..), decodeJobComment, isRunningState)
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime (..), addUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data ClusterStatus = Available | Degraded | Unavailable deriving (Eq, Show)

data StepPhase = Preparing | Queued | Running | Publishing deriving (Eq, Ord, Show)

data StepActivity = StepActivity
    { activityPhase :: StepPhase
    , activitySince :: UTCTime
    , activityReason :: Maybe Text
    , activityCommits :: [Text]
    } deriving (Eq, Show)

data ClusterSnapshot = ClusterSnapshot
    { clusterStatus :: ClusterStatus
    , clusterDetail :: Maybe Text
    , activeSteps :: Map Int StepActivity
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
snapshotVar = unsafePerformIO $ newTVarIO (ClusterSnapshot Available Nothing Map.empty)

{-# NOINLINE broadcastChan #-}
broadcastChan :: TChan ClusterSnapshot
broadcastChan = unsafePerformIO newBroadcastTChanIO

{-# NOINLINE runningBuilds #-}
runningBuilds :: TVar (Map Int TrackedBuild)
runningBuilds = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE requestedBuilds #-}
requestedBuilds :: TVar (Map Int UTCTime)
requestedBuilds = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE jobProgress #-}
jobProgress :: TVar (Map Int JobProgress)
jobProgress = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE latestJobs #-}
latestJobs :: TVar [SlurmJob]
latestJobs = unsafePerformIO $ newTVarIO []

{-# NOINLINE stopRequests #-}
stopRequests :: TVar (Set Int)
stopRequests = unsafePerformIO $ newTVarIO Set.empty

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
    requested <- readTVar requestedBuilds
    progress <- readTVar jobProgress
    let tracked = Map.mapWithKey (stepActivity progress) builds
        preparing = Map.map (\since -> StepActivity Preparing since Nothing []) requested
    let newSnap = (change snap) {activeSteps = Map.union tracked preparing}
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

requestBuild :: Int -> IO ()
requestBuild stepId = do
    now <- wholeSeconds <$> getCurrentTime
    atomically $ do
        builds <- readTVar runningBuilds
        when (not (Map.member stepId builds)) $
            modifyTVar' requestedBuilds (Map.insertWith (\_ old -> old) stepId now)
        publish id

clearBuildRequest :: Int -> IO ()
clearBuildRequest stepId =
    atomically $ do
        modifyTVar' requestedBuilds (Map.delete stepId)
        publish id

beginBuild :: Int -> Text -> IO ()
beginBuild stepId commit = do
    now <- wholeSeconds <$> getCurrentTime
    atomically $ do
        modifyTVar' requestedBuilds (Map.delete stepId)
        modifyTVar' runningBuilds (Map.insertWith track stepId (TrackedBuild now (Map.singleton commit 1)))
        jobs <- readTVar latestJobs
        builds <- readTVar runningBuilds
        modifyTVar' jobProgress (\progress -> deriveJobProgress now jobs builds progress `Map.restrictKeys` Map.keysSet builds)
        publish id
  where
    track _ build = build {trackedCommits = Map.insertWith (+) commit 1 (trackedCommits build)}

endBuild :: Int -> Text -> IO ()
endBuild stepId commit =
    atomically $ do
        modifyTVar' runningBuilds (Map.update release stepId)
        restrictProgress
        publish id
  where
    release build =
        let commits = Map.update (\count -> if count > 1 then Just (count - 1) else Nothing) commit (trackedCommits build)
         in if Map.null commits then Nothing else Just build {trackedCommits = commits}

restrictProgress :: STM ()
restrictProgress = do
    builds <- readTVar runningBuilds
    modifyTVar' jobProgress (`Map.restrictKeys` Map.keysSet builds)

markQueued :: Int -> String -> IO ()
markQueued stepId identifier = do
    let jobId = T.pack identifier
    now <- wholeSeconds <$> getCurrentTime
    atomically $ do
        builds <- readTVar runningBuilds
        progress <- readTVar jobProgress
        let current = Map.lookup stepId progress
            alreadyAdvanced = case current of
                Just known -> progressJobId known == jobId && progressPhase known >= Queued
                Nothing -> False
        when (Map.member stepId builds && not alreadyAdvanced) $ do
            let since = maybe now trackedSince (Map.lookup stepId builds)
            modifyTVar' jobProgress (Map.insert stepId (JobProgress jobId Queued since Nothing))
            publish id

buildingStepsAt :: Text -> STM (Set Int)
buildingStepsAt commit = Map.keysSet . Map.filter (Map.member commit . trackedCommits) <$> readTVar runningBuilds

buildingSteps :: STM (Set Int)
buildingSteps = Map.keysSet <$> readTVar runningBuilds

updateJobProgress :: (Map Int TrackedBuild -> Map Int JobProgress -> Map Int JobProgress) -> IO ()
updateJobProgress derive = atomically $ do
    builds <- readTVar runningBuilds
    modifyTVar' jobProgress (\progress -> derive builds progress `Map.restrictKeys` Map.keysSet builds)
    publish id

applySnapshot :: Maybe (ClusterStatus, Maybe Text) -> UTCTime -> [SlurmJob] -> IO ()
applySnapshot statusChange now jobs = atomically $ do
    writeTVar latestJobs jobs
    builds <- readTVar runningBuilds
    modifyTVar' jobProgress (\progress -> deriveJobProgress now jobs builds progress `Map.restrictKeys` Map.keysSet builds)
    let change = case statusChange of
            Just (status, detail) -> \snap -> snap {clusterStatus = status, clusterDetail = detail}
            Nothing -> id
    publish change

deriveJobProgress :: UTCTime -> [SlurmJob] -> Map Int TrackedBuild -> Map Int JobProgress -> Map Int JobProgress
deriveJobProgress now jobs builds previous =
    Map.union (Map.mapWithKey progressFor (Map.fromListWith furthest (mapMaybe trackedJob jobs))) previous
  where
    trackedJob job = do
        comment <- decodeJobComment =<< slurmJobComment job
        build <- Map.lookup (jobCommentStep comment) builds
        if jobCommentKind comment == "step"
            && isRunningState (slurmJobState job)
            && Map.member (T.pack (jobCommentCommit comment)) (trackedCommits build)
            then Just (jobCommentStep comment, job)
            else Nothing
    furthest a b = if jobPhase a >= jobPhase b then a else b
    progressFor stepId job =
        let phase = jobPhase job
            jobId = T.pack (unJobId (slurmJobId job))
            reason = if phase == Queued then T.pack <$> slurmJobReason job else Nothing
            since = case Map.lookup stepId previous of
                Just known | progressJobId known == jobId && progressPhase known == phase -> progressSince known
                _ -> phaseStart stepId phase job
         in JobProgress jobId phase since reason
    phaseStart _ Running job = maybe now (\seconds -> addUTCTime (negate (fromIntegral seconds)) now) (slurmJobElapsed job)
    phaseStart _ Publishing _ = now
    phaseStart stepId _ _ = maybe now trackedSince (Map.lookup stepId builds)

jobPhase :: SlurmJob -> StepPhase
jobPhase job = case slurmJobState job of
    "PENDING" -> Queued
    "PUBLISHING" -> Publishing
    _ -> Running

snapshotAndSubscribe :: IO (ClusterSnapshot, TChan ClusterSnapshot)
snapshotAndSubscribe = atomically $ do
    snap <- readTVar snapshotVar
    chan <- dupTChan broadcastChan
    return (snap, chan)
