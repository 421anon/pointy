{-# LANGUAGE OverloadedStrings #-}

module Sse (broadcastLoop, heartbeatDelayMicros, sseComment, sseEvent) where

import Control.Concurrent.STM (STM, TChan, atomically, orElse, readTChan, readTVar, registerDelay, retry, tryReadTChan)
import Data.Aeson (encode, object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Servant.Types.SourceT as S

heartbeatDelayMicros :: Int
heartbeatDelayMicros = 30 * 1000000

sseEvent :: Text -> LBS.ByteString -> BS.ByteString
sseEvent eventName payload =
    TE.encodeUtf8 ("event: " <> eventName <> "\n")
        <> "data: "
        <> LBS.toStrict payload
        <> "\n\n"

sseComment :: Text -> BS.ByteString
sseComment text_ =
    TE.encodeUtf8 (": " <> text_ <> "\n\n")

broadcastLoop :: (a -> (Text, LBS.ByteString)) -> TChan a -> IO (S.StepT IO BS.ByteString)
broadcastLoop render chan = do
    heartbeatDue <- registerDelay heartbeatDelayMicros
    waitForEvent heartbeatDue
  where
    loop = S.Effect (broadcastLoop render chan)

    waitForEvent heartbeatDue = do
        batch <-
            atomically $
                ( do
                    first <- readTChan chan
                    rest <- drain chan
                    pure $ Just (first : rest)
                )
                    `orElse` ( do
                                due <- readTVar heartbeatDue
                                if due then pure Nothing else retry
                             )
        case batch of
            Nothing ->
                return $ S.Yield (sseEvent "heartbeat" (encode (object []))) loop
            Just values ->
                return $ foldr S.Yield loop (map (uncurry sseEvent . render) values)

    drain chan = do
        mItem <- tryReadTChan chan
        case mItem of
            Nothing -> return []
            Just item -> (item :) <$> drain chan
