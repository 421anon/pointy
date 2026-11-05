{-# LANGUAGE OverloadedStrings #-}

module StepEventBus (
    StepEvent (..),
    StepEventKind (..),
    publishStepEvent,
    subscribeStepEvents,
) where

import Control.Concurrent.STM (TChan, TVar, atomically, dupTChan, modifyTVar', newBroadcastTChan, newTVarIO, readTVar, writeTChan)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data StepEventKind = BuildStarted | BuildFinished

data StepEvent = StepEvent
    { stepEventKind :: StepEventKind
    , stepEventStepId :: Int
    , stepEventRevision :: Text
    , stepEventAppInstanceId :: Maybe Text
    }

instance ToJSON StepEvent where
    toJSON event =
        object $
            ["stepId" .= stepEventStepId event, "revision" .= stepEventRevision event]
                ++ maybe [] (pure . ("appInstanceId" .=)) (stepEventAppInstanceId event)

{-# NOINLINE stepChannels #-}
stepChannels :: TVar (Map Int (TChan StepEvent))
stepChannels = unsafePerformIO (newTVarIO Map.empty)

publishStepEvent :: StepEvent -> IO ()
publishStepEvent event =
    atomically $ readTVar stepChannels >>= traverse_ (`writeTChan` event) . Map.lookup (stepEventStepId event)

subscribeStepEvents :: Int -> IO (TChan StepEvent)
subscribeStepEvents stepId = atomically $ do
    existing <- Map.lookup stepId <$> readTVar stepChannels
    channel <- maybe openChannel pure existing
    dupTChan channel
  where
    openChannel = do
        channel <- newBroadcastTChan
        modifyTVar' stepChannels (Map.insert stepId channel)
        pure channel
