{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepEventStream (stepEventStreamHandler) where

import Control.Concurrent.STM (TChan, atomically, check, orElse, readTChan, readTVar, registerDelay, tryReadTChan)
import Control.Monad.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import EffectRunner (runAppEffects)
import Effects (AppM, pathValid)
import Handlers.Cold (coldOutPath)
import Servant (Header, Headers, addHeader)
import qualified Servant.Types.SourceT as S
import qualified Sse
import StepEventBus (StepEvent (..), StepEventKind (..), subscribeStepEvents)
import UserRepo (ReadRepoContext (..), withReadRepoTransactionIO)

recheckDelayMicros :: Int
recheckDelayMicros = 5 * 1000000

stepEventStreamHandler :: Int -> Text -> AppM (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString))
stepEventStreamHandler stepId commit = do
    channel <- liftIO (subscribeStepEvents stepId)
    pure $ addHeader "no-transform" $ addHeader "no" $ S.fromStepT $ S.Yield (Sse.sseComment "connected") (S.Effect (streamLoop channel))
  where
    streamLoop channel = do
        answer <- newerRevision stepId commit
        pure $ S.Yield (renderNewerRevision answer) $ S.Effect $ do
            events <- eventsWithin recheckDelayMicros channel
            pure $ foldr (S.Yield . renderStepEvent) (S.Effect (streamLoop channel)) events

    renderNewerRevision answer =
        Sse.sseEvent "newer-revision" $
            encode (object ["stepId" .= stepId, "commit" .= commit, "revision" .= fmap fst answer, "built" .= maybe False snd answer])

newerRevision :: Int -> Text -> IO (Maybe (Text, Bool))
newerRevision stepId commit = either (const Nothing) id <$> runExceptT comparison
  where
    comparison = do
        headCommit <- ExceptT (withReadRepoTransactionIO (pure . T.pack . readCommitHash))
        viewedOutput <- outputAt commit
        headOutput <- outputAt headCommit
        if headOutput == viewedOutput
            then pure Nothing
            else Just . (,) headCommit <$> liftIO (runAppEffects (pathValid (T.unpack headOutput)))

    outputAt revision = withExceptT show (ExceptT (runAppEffects (runExceptT (coldOutPath stepId (Just revision)))))

eventsWithin :: Int -> TChan a -> IO [a]
eventsWithin delayMicros channel = do
    due <- registerDelay delayMicros
    atomically $ ((:) <$> readTChan channel <*> drain) `orElse` ([] <$ (readTVar due >>= check))
  where
    drain = tryReadTChan channel >>= maybe (pure []) (\event -> (event :) <$> drain)

renderStepEvent :: StepEvent -> BS.ByteString
renderStepEvent event = Sse.sseEvent (eventName (stepEventKind event)) (encode event)

eventName :: StepEventKind -> Text
eventName BuildStarted = "build-started"
eventName BuildFinished = "build-finished"
