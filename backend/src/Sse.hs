{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Sse (EventStream, SseResponse, broadcastLoop, heartbeatDelayMicros, sseComment, sseEvent, sseEventWithId, sseResponse) where

import Control.Concurrent.STM (STM, atomically, orElse, readTVar, registerDelay, retry)
import Data.Aeson (encode, object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Media ((//))
import Servant (Header, Headers, addHeader)
import Servant.API.ContentTypes (Accept (..), MimeRender (..))
import qualified Servant.Types.SourceT as S

data EventStream

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance MimeRender EventStream BS.ByteString where
    mimeRender _ = LBS.fromStrict

type SseResponse = Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString)

sseResponse :: S.StepT IO BS.ByteString -> SseResponse
sseResponse events =
    addHeader "no-transform" $
        addHeader "no" $
            S.fromStepT (S.Yield (sseComment "connected") (S.Yield padding events))
  where
    padding = sseComment ("padding " <> T.replicate 4096 " ")

heartbeatDelayMicros :: Int
heartbeatDelayMicros = 30 * 1000000

sseEvent :: Text -> LBS.ByteString -> BS.ByteString
sseEvent eventName payload =
    TE.encodeUtf8 ("event: " <> eventName <> "\n")
        <> "data: "
        <> LBS.toStrict payload
        <> "\n\n"

sseEventWithId :: Text -> Text -> LBS.ByteString -> BS.ByteString
sseEventWithId eventId eventName payload =
    TE.encodeUtf8 ("id: " <> eventId <> "\n") <> sseEvent eventName payload

sseComment :: Text -> BS.ByteString
sseComment text_ =
    TE.encodeUtf8 (": " <> text_ <> "\n\n")

broadcastLoop :: STM (Text, LBS.ByteString) -> IO (S.StepT IO BS.ByteString)
broadcastLoop nextEvent = do
    heartbeatDue <- registerDelay heartbeatDelayMicros
    batch <- atomically $ (Just <$> ((:) <$> nextEvent <*> drain)) `orElse` (readTVar heartbeatDue >>= \due -> if due then pure Nothing else retry)
    pure $ case batch of
        Nothing -> S.Yield (sseEvent "heartbeat" (encode (object []))) loop
        Just events -> foldr (S.Yield . uncurry sseEvent) loop events
  where
    loop = S.Effect (broadcastLoop nextEvent)
    drain = ((:) <$> nextEvent <*> drain) `orElse` pure []
