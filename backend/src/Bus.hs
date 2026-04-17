module Bus (ProjectSnapshot (..), broadcastSnapshot, subscribe) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data ProjectSnapshot = ProjectSnapshot
    { projectId :: Int
    , statuses :: Map Int (Text, Maybe Text)
    , outPaths :: Map Int Text
    }
    deriving (Show)

{-# NOINLINE statusBus #-}
statusBus :: TChan ProjectSnapshot
statusBus = unsafePerformIO newBroadcastTChanIO

broadcastSnapshot :: Int -> Map Int (Text, Maybe Text) -> Map Int Text -> IO ()
broadcastSnapshot pid stats paths = atomically $ writeTChan statusBus (ProjectSnapshot pid stats paths)

subscribe :: IO (TChan ProjectSnapshot)
subscribe = atomically $ dupTChan statusBus
