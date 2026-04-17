{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.StatusStream (EventStream, stepStatusStreamHandler) where

import Bus (ProjectSnapshot, subscribe)
import qualified Bus
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, TChan, atomically, tryReadTChan)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Statuses (getStatuses)
import Network.HTTP.Media ((//))
import OutPaths (getProjectOutPaths)
import Servant (Handler, Header, Headers, addHeader, throwError)
import Servant.API.ContentTypes (Accept (..), MimeRender (..))
import Servant.Server (err500, errBody)
import qualified Servant.Types.SourceT as S
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

data EventStream

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance MimeRender EventStream BS.ByteString where
    mimeRender _ = LBS.fromStrict

stepStatusStreamHandler :: Int -> Maybe Text -> Handler (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString))
stepStatusStreamHandler projectId commit = do
    eitherCommit <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return $ fromMaybe (pack hash) commit
    targetCommit <-
        case eitherCommit of
            Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
            Right c -> pure c

    busChan <- liftIO subscribe

    let padding = sseComment $ "padding " <> pack (replicate 4096 ' ')
    let source =
            S.fromStepT
                ( S.Yield
                    (sseComment "connected")
                    ( S.Yield
                        padding
                        (S.Effect (prepareInitialSnapshot projectId targetCommit busChan))
                    )
                )
    pure $ addHeader "no-transform" $ addHeader "no" source

prepareInitialSnapshot :: Int -> Text -> TChan ProjectSnapshot -> IO (S.StepT IO BS.ByteString)
prepareInitialSnapshot projectId targetCommit busChan = do
    outPaths <- getProjectOutPaths projectId targetCommit
    initialStatuses <- getStatuses targetCommit outPaths
    let snapshotPayload = encodeSnapshot projectId targetCommit initialStatuses outPaths
    pure
        ( S.Yield
            (sseEvent "snapshot" snapshotPayload)
            (S.Effect (streamLoop projectId targetCommit initialStatuses 0 busChan))
        )
streamLoop :: Int -> Text -> Map Int (Text, Maybe Text) -> Int -> TChan ProjectSnapshot -> IO (S.StepT IO BS.ByteString)
streamLoop projectId targetCommit previousStatuses heartbeatTick busChan = do
    return $ S.Effect $ do
        threadDelay 500000
        busUpdates <- atomically $ drainTChan busChan

        let mLatestSnapshot = find (\snapshot -> Bus.projectId snapshot == projectId) (reverse busUpdates)

        case mLatestSnapshot of
            Just snapshot -> do
                let currentStatuses = Bus.statuses snapshot
                let currentOutPaths = Bus.outPaths snapshot
                let snapshotPayload = encodeSnapshot projectId targetCommit currentStatuses currentOutPaths
                return $
                    S.Yield
                        (sseEvent "snapshot" snapshotPayload)
                        (S.Effect (streamLoop projectId targetCommit currentStatuses 0 busChan))
            Nothing -> do
                let nextHeartbeatTick = heartbeatTick + 1
                let (heartbeatEvents, finalTick) =
                        if nextHeartbeatTick >= 10
                            then ([sseEvent "heartbeat" (encode (object ["projectId" .= projectId]))], 0)
                            else ([], nextHeartbeatTick)

                let tickComment = sseComment $ "tick-" <> pack (show nextHeartbeatTick) <> " " <> pack (replicate 64 ' ')
                let events =
                        if null heartbeatEvents
                            then [tickComment]
                            else heartbeatEvents

                return $ yieldAll events (S.Effect (streamLoop projectId targetCommit previousStatuses finalTick busChan))

drainTChan :: TChan a -> STM [a]
drainTChan chan = do
    mItem <- tryReadTChan chan
    case mItem of
        Nothing -> return []
        Just item -> (item :) <$> drainTChan chan

encodeSnapshot :: Int -> Text -> Map Int (Text, Maybe Text) -> Map Int Text -> LBS.ByteString
encodeSnapshot projectId targetCommit statuses outPaths =
    let combined = Map.intersectionWith (,) statuses outPaths
     in encode
            ( object
                [ "projectId" .= projectId
                , "commit" .= targetCommit
                , "steps"
                    .= map
                        ( \(sid, ((st, mErr), op)) ->
                            object
                                ( ["stepId" .= sid, "status" .= st, "outPath" .= op]
                                    ++ maybe [] (\e -> ["error" .= e]) mErr
                                )
                        )
                        (Map.toList combined)
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
