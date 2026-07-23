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

broadcastSnapshot :: Int -> Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastSnapshot pid c stats = atomically $ do
    writeTChan statusBus (ProjectSnapshot pid c stats)
    updateRunningSteps stats

subscribe :: IO (TChan ProjectSnapshot)
subscribe = atomically $ dupTChan statusBus
