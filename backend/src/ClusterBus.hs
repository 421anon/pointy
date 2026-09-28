module ClusterBus
    ( ClusterStatus (..)
    , ClusterSnapshot (..)
    , setClusterStatus
    , beginBuild
    , endBuild
    , buildingSteps
    , buildingStepsAt
    , requestStop
    , takeStopRequest
    , snapshotAndSubscribe
    ) where
import Control.Concurrent.STM
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

data ClusterStatus = Available | Degraded | Unavailable deriving (Eq, Show)

data ClusterSnapshot = ClusterSnapshot
    { clusterStatus :: ClusterStatus
    , clusterDetail :: Maybe Text
    , runningStepIds :: Set Int
    } deriving (Eq, Show)

{-# NOINLINE snapshotVar #-}
snapshotVar :: TVar ClusterSnapshot
snapshotVar = unsafePerformIO $ newTVarIO (ClusterSnapshot Available Nothing Set.empty)

{-# NOINLINE broadcastChan #-}
broadcastChan :: TChan ClusterSnapshot
broadcastChan = unsafePerformIO newBroadcastTChanIO

{-# NOINLINE runningBuilds #-}
runningBuilds :: TVar (Map Int (Map Text Int))
runningBuilds = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE stopRequests #-}
stopRequests :: TVar (Set Int)
stopRequests = unsafePerformIO $ newTVarIO Set.empty

requestStop :: Int -> IO ()
requestStop stepId = atomically $ modifyTVar' stopRequests (Set.insert stepId)

takeStopRequest :: Int -> IO Bool
takeStopRequest stepId = atomically $ stateTVar stopRequests (\requests -> (Set.member stepId requests, Set.delete stepId requests))

setClusterStatus :: ClusterStatus -> Maybe Text -> IO ()
setClusterStatus newStatus newDetail = atomically $ do
    snap <- readTVar snapshotVar
    let newSnap = snap {clusterStatus = newStatus, clusterDetail = newDetail}
    when (newSnap /= snap) $ do
        writeTVar snapshotVar newSnap
        writeTChan broadcastChan newSnap

beginBuild :: Int -> Text -> IO ()
beginBuild stepId commit =
    updateRunningBuilds (Map.insertWith (Map.unionWith (+)) stepId (Map.singleton commit 1))

endBuild :: Int -> Text -> IO ()
endBuild stepId commit =
    updateRunningBuilds (Map.update (nonEmpty . Map.update release commit) stepId)
  where
    release count = if count > 1 then Just (count - 1) else Nothing
    nonEmpty counts = if Map.null counts then Nothing else Just counts

buildingStepsAt :: Text -> STM (Set Int)
buildingStepsAt commit = Map.keysSet . Map.filter (Map.member commit) <$> readTVar runningBuilds

buildingSteps :: STM (Set Int)
buildingSteps = Map.keysSet <$> readTVar runningBuilds

updateRunningBuilds :: (Map Int (Map Text Int) -> Map Int (Map Text Int)) -> IO ()
updateRunningBuilds change = atomically $ do
    modifyTVar' runningBuilds change
    builds <- readTVar runningBuilds
    snap <- readTVar snapshotVar
    let newSnap = snap {runningStepIds = Map.keysSet builds}
    when (newSnap /= snap) $ do
        writeTVar snapshotVar newSnap
        writeTChan broadcastChan newSnap

snapshotAndSubscribe :: IO (ClusterSnapshot, TChan ClusterSnapshot)
snapshotAndSubscribe = atomically $ do
    snap <- readTVar snapshotVar
    chan <- dupTChan broadcastChan
    return (snap, chan)
