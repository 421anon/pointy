module Bus (ProjectSnapshot (..), broadcastSnapshot, subscribe) where

import ClusterBus (updateRunningSteps)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data ProjectSnapshot = ProjectSnapshot
    { projectId :: Int
    , commit :: Text
    , statuses :: Map Int (Text, Maybe Text)
    }
    deriving (Show)

{-# NOINLINE statusBus #-}
statusBus :: TChan ProjectSnapshot
statusBus = unsafePerformIO newBroadcastTChanIO

{- | Bounded history of the most recent snapshots.  @subscribe@ replays it
onto each new channel so a client that connects (or the global status
stream reconnecting) just after a broadcast does not lose it: broadcast
channels hold no history of their own.
-}
{-# NOINLINE recentSnapshots #-}
recentSnapshots :: TVar [ProjectSnapshot]
recentSnapshots = unsafePerformIO $ newTVarIO []

replayLimit :: Int
replayLimit = 256

broadcastSnapshot :: Int -> Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastSnapshot pid c stats = atomically $ do
    writeTChan statusBus (ProjectSnapshot pid c stats)
    modifyTVar' recentSnapshots (take replayLimit . (ProjectSnapshot pid c stats :))
    updateRunningSteps stats

subscribe :: IO (TChan ProjectSnapshot)
subscribe = atomically $ do
    chan <- dupTChan statusBus
    recent <- readTVar recentSnapshots
    mapM_ (writeTChan chan) (reverse recent)
    pure chan
