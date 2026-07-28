{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.StatusStream (EventStream, stepStatusStreamHandler) where

import Bus (ProjectSnapshot, subscribe)
import qualified Bus
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (STM, TChan, atomically, newTChanIO, orElse, readTChan, readTVar, registerDelay, retry, tryReadTChan, writeTChan)
import Control.Monad (void, when)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Statuses (getRawStatusesWithPaths, partitionImmediateStatuses, resolveStepStatus)
import Network.HTTP.Media ((//))

import Servant (Handler, Header, Headers, addHeader, throwError)
import Servant.API.ContentTypes (Accept (..), MimeRender (..))
import Servant.Server (err500, errBody)
import qualified Servant.Types.SourceT as S
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

data EventStream

data LocalStatusUpdate
    = LocalSnapshot ProjectSnapshot
    | LocalError Text

instance Accept EventStream where
    contentType _ = "text" // "event-stream"

instance MimeRender EventStream BS.ByteString where
    mimeRender _ = LBS.fromStrict

stepStatusStreamHandler :: Int -> Maybe Text -> Handler (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString))
stepStatusStreamHandler projectId commit = do
    eitherCtx <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext repoPath hash) ->
        let targetCommit = fromMaybe (pack hash) commit
         in return (ReadRepoContext repoPath (unpack targetCommit))
    ctx <-
        case eitherCtx of
            Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
            Right c -> pure c
    let targetCommit = pack (readCommitHash ctx)

    localChan <- liftIO newTChanIO
    busChan <- liftIO subscribe

    -- Fork async status fetch and resolution; the initial SSE response
    -- goes out immediately with an empty snapshot so the client is never
    -- blocked on Nix evaluation.
    liftIO $
        void $
            forkIO $ do
                result <- getRawStatusesWithPaths projectId targetCommit
                case result of
                    Left err -> do
                        putStrLn $ "Project status stream failed for project " ++ show projectId ++ ": " ++ err
                        atomically $ writeTChan localChan (LocalError (pack err))
                    Right (rawStatuses, outPaths) -> do
                        let (initialStatuses, pendingStatuses) = partitionImmediateStatuses rawStatuses
                        when (not (Map.null initialStatuses)) $ do
                            stillCurrent <- statusStreamCommitStillCurrent commit targetCommit
                            when stillCurrent $
                                atomically $
                                    writeTChan localChan (LocalSnapshot (Bus.ProjectSnapshot projectId targetCommit initialStatuses))
                        when (not (Map.null pendingStatuses)) $
                            publishResolvedStatuses localChan commit projectId ctx outPaths pendingStatuses

    let padding = sseComment $ "padding " <> pack (replicate 4096 ' ')
    let emptySnapshot = encodeSnapshot projectId targetCommit Map.empty
    let source =
            S.fromStepT
                ( S.Yield
                    (sseComment "connected")
                    ( S.Yield
                        padding
                        ( S.Yield
                            (sseEvent "snapshot" emptySnapshot)
                            (S.Effect (streamLoop projectId commit localChan busChan))
                        )
                    )
                )
    pure $ addHeader "no-transform" $ addHeader "no" source

publishResolvedStatuses :: TChan LocalStatusUpdate -> Maybe Text -> Int -> ReadRepoContext -> Map Int Text -> Map Int (Text, Maybe Text) -> IO ()
publishResolvedStatuses updatesChan pinnedCommit projectId ctx outPaths statuses =
    mapConcurrently_ publishOne (Map.toList statuses)
  where
    targetCommit = pack (readCommitHash ctx)

    publishOne (sid, entry) = do
        (sid', status_) <- resolveStepStatus ctx (fmap unpack $ Map.lookup sid outPaths) (sid, entry)
        shouldPublish <- statusStreamCommitStillCurrent pinnedCommit targetCommit
        when shouldPublish $
            atomically $
                writeTChan updatesChan (LocalSnapshot (Bus.ProjectSnapshot projectId targetCommit (Map.singleton sid' status_)))

statusStreamCommitStillCurrent :: Maybe Text -> Text -> IO Bool
statusStreamCommitStillCurrent pinnedCommit targetCommit =
    case pinnedCommit of
        Just _ -> return True
        Nothing -> do
            latest <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
            return $ latest == Right targetCommit

heartbeatDelayMicros :: Int
heartbeatDelayMicros = 30 * 1000000

streamLoop :: Int -> Maybe Text -> TChan LocalStatusUpdate -> TChan ProjectSnapshot -> IO (S.StepT IO BS.ByteString)
streamLoop projectId pinnedCommit localChan busChan = do
    heartbeatDue <- registerDelay heartbeatDelayMicros
    waitForEvent heartbeatDue
  where
    waitForEvent heartbeatDue = do
        activity <-
            atomically $
                ( do
                    firstLocal <- readTChan localChan
                    localUpdates <- (firstLocal :) <$> drainTChan localChan
                    busUpdates <- drainTChan busChan
                    pure $ Just (localUpdates, busUpdates)
                )
                    `orElse` ( do
                                firstBus <- readTChan busChan
                                localUpdates <- drainTChan localChan
                                busUpdates <- (firstBus :) <$> drainTChan busChan
                                pure $ Just (localUpdates, busUpdates)
                             )
                    `orElse` ( do
                                due <- readTVar heartbeatDue
                                if due then pure Nothing else retry
                             )

        case activity of
            Nothing ->
                return $
                    S.Yield
                        (sseEvent "heartbeat" (encode (object ["projectId" .= projectId])))
                        (S.Effect (streamLoop projectId pinnedCommit localChan busChan))
            Just (localUpdates, busUpdates) -> do
                let localErrors = [err | LocalError err <- localUpdates]
                let localSnapshots = [snapshot | LocalSnapshot snapshot <- localUpdates]
                let matchingSnapshots = filter matchesSnapshot (localSnapshots ++ busUpdates)

                case localErrors of
                    (err : _) ->
                        return $ S.Yield (sseEvent "status-error" (encode err)) S.Stop
                    [] ->
                        case matchingSnapshots of
                            [] -> waitForEvent heartbeatDue
                            snapshots -> do
                                let events = map snapshotEvent snapshots
                                return $ yieldAll events (S.Effect (streamLoop projectId pinnedCommit localChan busChan))
    matchesSnapshot snapshot =
        Bus.projectId snapshot == projectId && maybe True (== Bus.commit snapshot) pinnedCommit

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
