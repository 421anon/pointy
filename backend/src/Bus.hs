module Bus (ProjectSnapshot (..), broadcastSnapshot, subscribe) where

import ClusterBus (buildingStepsAt)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
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

{-# NOINLINE recentSnapshots #-}
recentSnapshots :: TVar [ProjectSnapshot]
recentSnapshots = unsafePerformIO $ newTVarIO []

replayLimit :: Int
replayLimit = 256

broadcastSnapshot :: Int -> Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastSnapshot pid c reported = atomically $ do
    building <- buildingStepsAt c
    let stats = Map.filterWithKey (\sid (st, _) -> Set.notMember sid building || st `elem` map pack ["running", "success"]) reported
    writeTChan statusBus (ProjectSnapshot pid c stats)
    modifyTVar' recentSnapshots (take replayLimit . (ProjectSnapshot pid c stats :))

subscribe :: IO (TChan ProjectSnapshot)
subscribe = atomically $ do
    chan <- dupTChan statusBus
    recent <- readTVar recentSnapshots
    mapM_ (unGetTChan chan) recent
    pure chan
