module ClusterBus
    ( ClusterStatus (..)
    , ClusterSnapshot (..)
    , setClusterStatus
    , updateRunningSteps
    , snapshotAndSubscribe
    , restoreRunningStepIds
    ) where
import Control.Concurrent.STM
import Control.Exception (mask, onException)
import Control.Monad (when)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

data ClusterStatus = Available | Degraded | Unavailable deriving (Eq, Show)

data ClusterSnapshot = ClusterSnapshot
    { clusterStatus :: ClusterStatus
    , runningStepIds :: Set Int
    } deriving (Eq, Show)

{-# NOINLINE snapshotVar #-}
snapshotVar :: TVar ClusterSnapshot
snapshotVar = unsafePerformIO $ newTVarIO (ClusterSnapshot Available Set.empty)

{-# NOINLINE broadcastChan #-}
broadcastChan :: TChan ClusterSnapshot
broadcastChan = unsafePerformIO newBroadcastTChanIO

{-# NOINLINE restoreTracker #-}
restoreTracker :: TVar (Maybe (Set Int))
restoreTracker = unsafePerformIO $ newTVarIO Nothing

setClusterStatus :: ClusterStatus -> IO ()
setClusterStatus newStatus = atomically $ do
    snap <- readTVar snapshotVar
    let newSnap = snap {clusterStatus = newStatus}
    when (newSnap /= snap) $ do
        writeTVar snapshotVar newSnap
        writeTChan broadcastChan newSnap

updateRunningSteps :: Map Int (Text, Maybe Text) -> STM ()
updateRunningSteps steps = do
    snap <- readTVar snapshotVar
    let newIds = Map.foldlWithKey'
            (\acc k (statusText, _) ->
                if statusText == T.pack "running"
                    then Set.insert k acc
                    else Set.delete k acc)
            (runningStepIds snap)
            steps
        newSnap = snap {runningStepIds = newIds}
    mTracker <- readTVar restoreTracker
    case mTracker of
        Just touched ->
            writeTVar restoreTracker (Just (Set.union touched (Map.keysSet steps)))
        Nothing -> return ()
    when (newSnap /= snap) $ do
        writeTVar snapshotVar newSnap
        writeTChan broadcastChan newSnap

snapshotAndSubscribe :: IO (ClusterSnapshot, TChan ClusterSnapshot)
snapshotAndSubscribe = atomically $ do
    snap <- readTVar snapshotVar
    chan <- dupTChan broadcastChan
    return (snap, chan)

-- | While the supplied scan runs, every 'updateRunningSteps' call records
-- its touched step IDs.  After the scan completes, only recovered IDs that
-- were /not/ touched by live updates are unioned into the running set.
-- The tracker is always cleared on exception so no stale state leaks.
restoreRunningStepIds :: IO (Set Int) -> IO ()
restoreRunningStepIds scan = mask $ \restore -> do
    atomically $ writeTVar restoreTracker (Just Set.empty)
    recovered <- restore scan
        `onException` atomically (writeTVar restoreTracker Nothing)
    atomically $ do
        mTouched <- readTVar restoreTracker
        let touched = fromMaybe Set.empty mTouched
            toRestore = Set.difference recovered touched
        when (not (Set.null toRestore)) $ do
            snap <- readTVar snapshotVar
            let newSnap = snap {runningStepIds = Set.union (runningStepIds snap) toRestore}
            writeTVar snapshotVar newSnap
            writeTChan broadcastChan newSnap
        writeTVar restoreTracker Nothing
