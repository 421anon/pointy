{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.StatusStream (EventStream, stepStatusStreamHandler, projectStatusHandler, streamLoop) where

import Bus (ProjectSnapshot, subscribe)
import qualified Bus
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (STM, TChan, atomically, orElse, readTChan, readTVar, registerDelay, retry, tryReadTChan)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Statuses (broadcastProjectStatus)
import Network.HTTP.Media ((//))
import Servant (Handler, Header, Headers, NoContent (..), addHeader, throwError)
import Servant.API.ContentTypes (Accept (..), MimeRender (..))
import Servant.Server (err500, errBody)
import qualified Servant.Types.SourceT as S
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

data EventStream

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance MimeRender EventStream BS.ByteString where
    mimeRender _ = LBS.fromStrict

{- | A single app-global SSE stream carrying snapshot and heartbeat events
for every project's steps, unfiltered.  Clients open this stream once;
completion toasts then fire regardless of which page is open.
-}
stepStatusStreamHandler :: Handler (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString))
stepStatusStreamHandler = do
    busChan <- liftIO subscribe
    let padding = sseComment $ "padding " <> pack (replicate 4096 ' ')
    let source =
            S.fromStepT
                ( S.Yield
                    (sseComment "connected")
                    ( S.Yield
                        padding
                        (S.Effect (streamLoop busChan))
                    )
                )
    pure $ addHeader "no-transform" $ addHeader "no" source

{- | Re-evaluate a project's step statuses and broadcast them on the global
step status stream.  The target commit defaults to the current repo head
when omitted; the evaluation runs in a forked thread so the request
returns immediately.
-}
projectStatusHandler :: Int -> Maybe Text -> Handler NoContent
projectStatusHandler projectId commit = do
    targetCommit <-
        case commit of
            Just c -> pure c
            Nothing -> do
                result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
                case result of
                    Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
                    Right c -> pure c
    liftIO $ void $ forkIO $ broadcastProjectStatus projectId targetCommit Nothing
    pure NoContent

heartbeatDelayMicros :: Int
heartbeatDelayMicros = 30 * 1000000

{- | Block on the snapshot broadcast channel, racing against a 30-second
heartbeat.  A snapshot fires a @snapshot@ SSE event (burst-coalesced with
anything already queued); a timeout fires an empty @heartbeat@.  No
project or commit filtering: this is a global stream.
-}
streamLoop :: TChan ProjectSnapshot -> IO (S.StepT IO BS.ByteString)
streamLoop busChan = do
    heartbeatDue <- registerDelay heartbeatDelayMicros
    waitForEvent heartbeatDue
  where
    waitForEvent heartbeatDue = do
        snapshots <-
            atomically $
                ( do
                    first <- readTChan busChan
                    rest <- drainTChan busChan
                    pure $ Just (first : rest)
                )
                    `orElse` ( do
                                due <- readTVar heartbeatDue
                                if due then pure Nothing else retry
                             )
        case snapshots of
            Nothing ->
                return $
                    S.Yield
                        (sseEvent "heartbeat" (encode (object [])))
                        (S.Effect (streamLoop busChan))
            Just batch -> do
                let events = map snapshotEvent batch
                return $ yieldAll events (S.Effect (streamLoop busChan))
    snapshotEvent snapshot =
        sseEvent "snapshot" (encodeSnapshot (Bus.projectId snapshot) (Bus.commit snapshot) (Bus.statuses snapshot))

drainTChan :: TChan a -> STM [a]
drainTChan chan = do
    mItem <- tryReadTChan chan
    case mItem of
        Nothing -> return []
        Just item -> (item :) <$> drainTChan chan

encodeSnapshot :: Int -> Text -> Map Int (Text, Maybe Text) -> LBS.ByteString
encodeSnapshot projectId targetCommit statuses =
    encode
        ( object
            [ "projectId" .= projectId
            , "commit" .= targetCommit
            , "steps"
                .= map
                    ( \(sid, (st, mErr)) ->
                        object
                            ( ["stepId" .= sid, "status" .= st]
                                ++ maybe [] (\e -> ["error" .= e]) mErr
                            )
                    )
                    (Map.toList statuses)
            ]
        )

sseEvent :: Text -> LBS.ByteString -> BS.ByteString
sseEvent eventName payload =
    TE.encodeUtf8 ("event: " <> eventName <> "\n")
        <> "data: "
        <> LBS.toStrict payload
        <> "\n\n"

sseComment :: Text -> BS.ByteString
sseComment text_ =
    TE.encodeUtf8 (": " <> text_ <> "\n\n")

yieldAll :: [BS.ByteString] -> S.StepT IO BS.ByteString -> S.StepT IO BS.ByteString
yieldAll chunks rest = foldr S.Yield rest chunks
