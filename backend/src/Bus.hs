module Bus (ProjectSnapshot (..), StatusEvent (..), broadcastLastBuilt, broadcastSnapshot, subscribe) where

import BuiltSteps (LastBuiltVersion)
import ClusterBus (buildingStepsAt)
import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text, pack)
import RollupCache (invalidateRollupCache)
import System.IO.Unsafe (unsafePerformIO)

data ProjectSnapshot = ProjectSnapshot
    { projectId :: Int
    , commit :: Text
    , statuses :: Map Int (Text, Maybe Text)
    , certificates :: Map Int Text
    }
    deriving (Show)

data StatusEvent
    = SnapshotEvent ProjectSnapshot
    | LastBuiltEvent Text (Map Int LastBuiltVersion)
    deriving (Show)

{-# NOINLINE statusBus #-}
statusBus :: TChan StatusEvent
statusBus = unsafePerformIO newBroadcastTChanIO

{-# NOINLINE recentEvents #-}
recentEvents :: TVar [StatusEvent]
recentEvents = unsafePerformIO $ newTVarIO []

replayLimit :: Int
replayLimit = 256

publish :: StatusEvent -> STM ()
publish event = do
    writeTChan statusBus event
    modifyTVar' recentEvents (take replayLimit . (event :))

broadcastSnapshot :: Int -> Text -> Map Int Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastSnapshot pid c certified reported = do
    atomically $ do
        building <- buildingStepsAt c
        let stats = Map.filterWithKey (\sid (st, _) -> Set.notMember sid building || st `elem` map pack ["running", "success"]) reported
        publish (SnapshotEvent (ProjectSnapshot pid c stats (Map.intersection certified stats)))
    invalidateRollupCache

broadcastLastBuilt :: Text -> Map Int LastBuiltVersion -> IO ()
broadcastLastBuilt c = atomically . publish . LastBuiltEvent c

subscribe :: IO (TChan StatusEvent)
subscribe = atomically $ do
    chan <- dupTChan statusBus
    recent <- readTVar recentEvents
    mapM_ (unGetTChan chan) recent
    pure chan
