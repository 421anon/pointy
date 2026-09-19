{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Runner (
    startAgentTurn,
    stopAgentTurn,
    steerAgentTurn,
    turnLogStreamHandler,
    streamLoop,
) where

import Agent.Git (AgentSessionView, commitAgentTurnOutputs, finalizeApplyResolution, loadAgentSessionView, nameUnnamedAgentSession, refreshSessionBase, sessionHasActiveRunner)
import Agent.Sandbox (bindPath, bindPathReadOnly, expandSandboxArg, nixDaemonBindArgs, piAgentConfigDir, runnerConfigArgs, runnerEnvironment, sandboxHome, sessionPaths)
import Agent.Session (
    AgentSession (..),
    AgentTurn (..),
    PreparedApply (..),
    applyConflictsPending,
    findTurn,
    listTurns,
    loadSessionById,
    newTurnId,
    normalizeSessionName,
    saveSession,
    saveTurn,
    touchSession,
    turnIsUnfinished,
    turnLogFilePath,
    turnLogHasFinalizationFailure,
 )
import Agent.Title (generateSessionTitle)
import Agent.TurnSignal (registerTurnSignal, signalTurnLog, unregisterTurnSignal)
import Agent.WarmSession (WarmSessionMeta (..), getOrBuildWarmSession)
import Config (AgentConfig (..), Config (..), loadConfig, resolveConfigPath)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, withMVar)
import Control.Concurrent.STM (TChan, TMVar, TVar, atomically, modifyTVar', newEmptyTMVarIO, newTVarIO, orElse, readTChan, readTVar, registerDelay, retry, takeTMVar, tryPutTMVar, writeTVar)
import Control.Exception (IOException, SomeException, finally, try)
import Control.Lens (failing, filtered, (^.), (^..), (^?))
import Control.Monad (filterM, forM_, guard, mfilter, unless, void, when)
import Control.Monad.Except (ExceptT (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key, values, _Bool, _Integer, _String)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List.NonEmpty (nonEmpty)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Servant (Handler, Header, Headers, addHeader, err404, errBody, throwError)
import qualified Servant.Types.SourceT as S
import Sse (sseComment, sseEvent)
import System.Directory (copyFile, createDirectoryIfMissing, createDirectoryLink, doesDirectoryExist, doesFileExist, getFileSize, getSymbolicLinkTarget, listDirectory, pathIsSymbolicLink, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hIsEOF, hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import UserRepo (userRepoPath, withUserRepoExclusive)

{-# NOINLINE activeRunners #-}
-- Nothing reserves an accepted turn before its process exists.
activeRunners :: TVar (Map.Map Text (Text, Maybe ProcessHandle))
activeRunners = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE stopRequestedTurns #-}
stopRequestedTurns :: TVar (Set.Set Text)
stopRequestedTurns = unsafePerformIO $ newTVarIO Set.empty

data RunnerInput = RunnerInput
    { inputHandle :: Handle
    , inputPending :: Maybe (Text, TMVar Bool)
    , inputPromptSeen :: Bool
    , inputRetrying :: Bool
    , inputQuestion :: Maybe PendingQuestion
    }

{-# NOINLINE runnerInputs #-}
runnerInputs :: TVar (Map.Map Text (MVar (Maybe RunnerInput)))
runnerInputs = unsafePerformIO $ newTVarIO Map.empty

steerAckTimeoutMicros :: Int
steerAckTimeoutMicros = 10 * 1000000

data SteerOutcome = SteerFailed | SteerAnsweredQuestion Text | SteerSentToAgent

planSteer :: Text -> Text -> TMVar Bool -> RunnerInput -> Maybe (Aeson.Value, RunnerInput, SteerOutcome)
planSteer prompt requestId reply control = case inputQuestion control of
    Just question ->
        let (response, carriedQuestion, answerText) = answerQuestion question prompt
         in Just (response, control{inputQuestion = carriedQuestion}, SteerAnsweredQuestion answerText)
    Nothing ->
        (steerCommand requestId prompt, control{inputPending = Just (requestId, reply)}, SteerSentToAgent)
            <$ guard (isNothing (inputPending control))

steerCommand :: Text -> Text -> Aeson.Value
steerCommand requestId prompt =
    Aeson.object ["id" Aeson..= requestId, "type" Aeson..= ("prompt" :: Text), "message" Aeson..= prompt, "streamingBehavior" Aeson..= ("steer" :: Text)]

jsonLine :: (Aeson.ToJSON a) => a -> Text
jsonLine = TE.decodeUtf8 . LBS.toStrict . Aeson.encode

steerAgentTurn :: Text -> Text -> ExceptT String IO ()
steerAgentTurn sid rawPrompt = do
    let prompt = T.strip rawPrompt
    when (T.null prompt) $ Except.throwError "empty_prompt"
    (tid, _) <- lookupForSession "runner_not_active" activeRunners
    stopped <- liftIO $ turnStopRequested tid
    when stopped $ Except.throwError "runner_stopping"
    input <- lookupForSession "runner_not_ready" runnerInputs
    (requestId, reply) <- liftIO $ (,) <$> newTurnId <*> newEmptyTMVarIO
    outcome <- liftIO $ modifyMVar input (steerStep prompt requestId reply)
    case outcome of
        SteerFailed -> Except.throwError "steering_failed"
        SteerAnsweredQuestion answer -> logAnsweredSteer tid answer
        SteerSentToAgent -> awaitSteerAck reply
  where
    lookupForSession err var =
        liftIO (atomically (Map.lookup sid <$> readTVar var))
            >>= maybe (Except.throwError err) return
    steerStep prompt requestId reply current =
        case current >>= planSteer prompt requestId reply of
            Just plan | Just control <- current -> sendSteerPlan control plan
            _ -> return (current, SteerFailed)
    -- A message that never went out must not be recorded as an accepted steer
    -- or a consumed answer: nothing will ever acknowledge it.
    sendSteerPlan :: RunnerInput -> (Aeson.Value, RunnerInput, SteerOutcome) -> IO (Maybe RunnerInput, SteerOutcome)
    sendSteerPlan control (message, updatedControl, outcome) =
        (try (writeToRunner control message) :: IO (Either IOException ()))
            >>= return . either (const (Just control, SteerFailed)) (const (Just updatedControl, outcome))
    -- The chat renders "steering" log lines as user messages, so an answer names the row it picked.
    logAnsweredSteer tid answer = liftIO $ do
        cfg <- configAgent <$> (resolveConfigPath >>= loadConfig)
        logPath <- turnLogFilePath sid tid
        appendLogLine cfg logPath "steering" (jsonLine answer)
    awaitSteerAck reply = do
        accepted <- liftIO $ timeout steerAckTimeoutMicros (atomically $ takeTMVar reply)
        unless (fromMaybe False accepted) $ Except.throwError "steering_failed"

writeToRunner :: RunnerInput -> Aeson.Value -> IO ()
writeToRunner = writeRpc . inputHandle

writeRpc :: Handle -> Aeson.Value -> IO ()
writeRpc handle message = do
    LBS.hPut handle (Aeson.encode message <> "\n")
    hFlush handle

closeRunnerInput :: MVar (Maybe RunnerInput) -> IO ()
closeRunnerInput input = modifyMVar_ input $ \current -> do
    forM_ current $ \control -> do
        forM_ (inputPending control) $ \(_, reply) ->
            atomically $ void $ tryPutTMVar reply False
        void (try (hClose (inputHandle control)) :: IO (Either IOException ()))
    return Nothing

startAgentTurn :: Text -> Text -> ExceptT String IO AgentTurn
startAgentTurn sid prompt = do
    session_ <- ExceptT $ loadSessionById sid
    when (status session_ == "applied") $ Except.throwError "session_applied"
    when (status session_ == "discarded") $ Except.throwError "session_discarded"
    when (status session_ == "archived") $ Except.throwError "session_archived"
    hasRunner <- sessionHasActiveRunner session_
    when hasRunner $ Except.throwError "runner_active"
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    (freshSession, syncNotes) <- refreshSessionBase session_
    tid <- liftIO newTurnId
    logPath <- liftIO $ turnLogFilePath sid tid
    now <- liftIO getCurrentTime
    let turn =
            AgentTurn
                { turnId = tid
                , turnSessionId = sid
                , turnPrompt = prompt
                , turnStatus = "running"
                , turnExitCode = Nothing
                , turnStartedAt = now
                , turnFinishedAt = Nothing
                , turnLogPath = logPath
                , turnLog = ""
                }
    liftIO $ do
        createDirectoryIfMissing True (takeDirectory logPath)
        TIO.writeFile logPath ""
        mapM_ (appendLogLine (configAgent cfg) logPath "system") syncNotes
        existingTurns <- listTurns sid
        let isFirstTurn = null existingTurns
        saveTurn turn
        -- A conflict-pending apply must survive the turn boundary: the turn is
        -- how the agent resolves the conflict markers in the apply worktree.
        -- Keep the pending apply (and its conflict summary) and stay in
        -- "prepare_conflict" so the UI keeps showing the review state.
        let pendingApply =
                case preparedApply freshSession of
                    Just p | applyConflictsPending p -> Just p
                    _ -> Nothing
            nextStatus =
                if pendingApply /= Nothing
                    then "prepare_conflict"
                    else "open"
            nextError =
                if pendingApply /= Nothing
                    then lastError freshSession
                    else Nothing
        touched <- touchSession freshSession{status = nextStatus, activeTurnId = Nothing, preparedApply = pendingApply, lastError = nextError}
        startSaveResult <- try (saveSession touched) :: IO (Either SomeException ())
        case startSaveResult of
            Left ex -> appendLogLine (configAgent cfg) logPath "system" ("Session start metadata warning: " <> T.pack (show ex))
            Right _ -> return ()
        case pendingApply of
            Just pending ->
                appendLogLine (configAgent cfg) logPath "system" $
                    "An apply merge is waiting for conflict resolution. Resolve the conflict markers in "
                        <> T.pack (candidateWorktree pending)
                        <> " (the apply worktree is bound into your sandbox); the backend stages and commits your resolution automatically when this turn ends."
            Nothing -> return ()
        atomically $ modifyTVar' activeRunners $ Map.insert sid (tid, Nothing)
        let unnamed = isNothing (sessionName freshSession >>= normalizeSessionName)
            titling = not (T.null (T.strip (agentTitlePrompt (configAgent cfg))))
        when (unnamed && titling) $
            void $
                forkIO $
                    nameChat (configAgent cfg) freshSession logPath prompt
        void $ forkIO $ runTurnProcess (configAgent cfg) touched turn prompt isFirstTurn
    return turn

stopAgentTurn :: Text -> ExceptT String IO AgentSessionView
stopAgentTurn sid = do
    mRunner <- liftIO $ atomically $ do
        runners <- readTVar activeRunners
        case Map.lookup sid runners of
            Nothing -> return Nothing
            runner@(Just (tid, _)) -> do
                modifyTVar' stopRequestedTurns (Set.insert tid)
                return runner
    case mRunner of
        Nothing -> return ()
        Just (tid, mProcess) -> liftIO $ do
            forM_ mProcess $ \ph ->
                void (try (terminateProcess ph) :: IO (Either SomeException ()))
            terminated <- awaitRunnerCleanup sid 4
            unless terminated $ do
                latestRunner <- atomically $ Map.lookup sid <$> readTVar activeRunners
                let latestProcess = do
                        (latestTid, process) <- latestRunner
                        if latestTid == tid then process else Nothing
                case latestProcess of
                    Nothing -> return ()
                    Just ph -> do
                        void (try (killRunner ph) :: IO (Either SomeException ()))
                        void $ awaitRunnerCleanup sid 6
    loadAgentSessionView sid

awaitRunnerCleanup :: Text -> Int -> IO Bool
awaitRunnerCleanup sid timeoutSeconds =
    isJust <$> timeout (timeoutSeconds * 1000000) (atomically waitUntilGone)
  where
    waitUntilGone = do
        live <- Map.member sid <$> readTVar activeRunners
        when live retry

turnStopRequested :: Text -> IO Bool
turnStopRequested tid = atomically $ Set.member tid <$> readTVar stopRequestedTurns

attachRunnerProcess :: Text -> Text -> ProcessHandle -> IO Bool
attachRunnerProcess sid tid ph = do
    stopped <- atomically $ do
        modifyTVar' activeRunners $ Map.insert sid (tid, Just ph)
        Set.member tid <$> readTVar stopRequestedTurns
    when stopped $ terminateProcess ph
    return stopped

killRunner :: ProcessHandle -> IO ()
killRunner ph = do
    mPid <- getPid ph
    mapM_ (signalProcess sigKILL) mPid

turnLogStreamHandler :: Text -> Handler (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString))
turnLogStreamHandler tid = do
    mTurn <- liftIO $ findTurn tid
    turn <- case mTurn of
        Nothing -> throwError err404{errBody = "turn not found"}
        Just t -> return t
    signal <- liftIO $ registerTurnSignal (turnLogPath turn)
    let padding = sseComment $ "padding " <> T.pack (replicate 4096 ' ')
        source =
            S.fromStepT
                ( S.Yield
                    (sseComment "connected")
                    (S.Yield padding (S.Effect (streamLoop turn 0 signal)))
                )
    pure $ addHeader "no-transform" $ addHeader "no" source

{- | Let the runner name a chat that has none. This runs beside the turn, not
inside it: a throwaway completion on the opening request, so the name is there
by the time the turn ends and a turn the user stops still gets one. Until it
lands the UI shows the opening request, which is a prompt, not a title.
-}
nameChat :: AgentConfig -> AgentSession -> FilePath -> Text -> IO ()
nameChat cfg session_ logPath prompt =
    generateSessionTitle cfg session_ prompt
        >>= either (note "Could not name this chat: ") store
  where
    store title =
        Except.runExceptT (nameUnnamedAgentSession (sessionId session_) title)
            >>= either (note "Could not store this chat's name: ") return
    note prefix = appendLogLine cfg logPath "system" . (prefix <>) . T.pack

runTurnProcess :: AgentConfig -> AgentSession -> AgentTurn -> Text -> Bool -> IO ()
runTurnProcess cfg session_ turn prompt isFirstTurn =
    continueUnlessStopped run
        `finally` atomically (modifyTVar' activeRunners (Map.delete sid))
  where
    sid = sessionId session_
    tid = turnId turn
    continueUnlessStopped action = do
        stopped <- turnStopRequested tid
        if stopped
            then finishTurn cfg session_ turn (ExitFailure (-15))
            else action
    run = do
        appendLogLine cfg (turnLogPath turn) "system" ("Starting agent turn " <> tid)
        mWarmResult <-
            if isFirstTurn
                then do
                    appendLogLine cfg (turnLogPath turn) "stdout" "*Loading the project context*"
                    getOrBuildWarmSession cfg (baseCommit session_) (void . attachRunnerProcess sid tid)
                else return Nothing
        case mWarmResult of
            Just (Left err) ->
                appendLogLine cfg (turnLogPath turn) "system" ("Warm session unavailable, starting cold: " <> T.pack err)
            _ -> return ()
        continueUnlessStopped $ do
            let mWarmFile = case mWarmResult of
                    Just (Right meta) -> Just (warmSessionFile meta)
                    _ -> Nothing
            result <- try (runConfiguredProcess cfg session_ turn prompt isFirstTurn mWarmFile) :: IO (Either IOException ExitCode)
            exitCode <- case result of
                Left err -> do
                    appendLogLine cfg (turnLogPath turn) "system" ("Runner failed to start: " <> T.pack (show err))
                    return $ ExitFailure 127
                Right code -> return code
            finishTurn cfg session_ turn exitCode

runConfiguredProcess :: AgentConfig -> AgentSession -> AgentTurn -> Text -> Bool -> Maybe FilePath -> IO ExitCode
runConfiguredProcess cfg session_ turn promptText isFirstTurn mWarmFile = do
    repoPath <- userRepoPath
    nixBind <- nixDaemonBindArgs
    let paths = sessionPaths session_
        runnerHome = sandboxHome paths
        expand = expandSandboxArg paths
        outputMarker =
            "__POINTY_AGENT_OUTPUT_BEGIN__" <> T.unpack (turnId turn) <> "__"
        sessionFlag = case (isFirstTurn, mWarmFile) of
            (True, Just warmFile) -> ["--fork", warmFile]
            (True, Nothing) -> []
            (False, _) -> ["-c"]
        runnerArgs =
            agentRunnerCommand cfg : ["--mode", "rpc"] ++ sessionFlag ++ runnerConfigArgs expand (agentRunnerArgs cfg)
        wrapperScript =
            "set -e; printf '%s\\n' \"$POINTY_AGENT_OUTPUT_MARKER\"; printf '%s\\n' \"$POINTY_AGENT_OUTPUT_MARKER\" >&2; exec \"$@\""
        -- When forking a warm session, bind its file read-only into the sandbox.
        -- The warm template path is outside the draft home so sbox won't include it otherwise.
        warmBindArgs = maybe [] bindPathReadOnly mWarmFile
        -- When an apply is waiting for conflict resolution, expose the apply
        -- worktree read-write so the agent can edit the conflict markers there.
        applyBindArgs =
            case preparedApply session_ of
                Just pending | applyConflictsPending pending -> bindPath (candidateWorktree pending)
                _ -> []
        args =
            map expand (agentSboxArgs cfg)
                ++ warmBindArgs
                ++ bindPathReadOnly repoPath
                ++ applyBindArgs
                ++ nixBind
                ++ ["--", "bash", "-lc", wrapperScript, "pointy-agent-runner"]
                ++ runnerArgs
    runnerEnv <-
        runnerEnvironment
            [ ("HOME", runnerHome)
            , ("POINTY_AGENT_WORKTREE", worktreePath session_)
            , ("POINTY_AGENT_SESSION_ID", T.unpack (sessionId session_))
            , ("POINTY_AGENT_OUTPUT_MARKER", outputMarker)
            ]
    let process =
            (proc (agentSboxCommand cfg) args)
                { cwd = Just (worktreePath session_)
                , env = Just runnerEnv
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    createDirectoryIfMissing True runnerHome
    -- Seed the per-session pi config; without it pi falls back to its built-in
    -- registry, whose deepseek default is deepseek-v4-pro. Note that models.json
    -- alone cannot change that default (built-in models are always present);
    -- settings.json carries the default model, and the runner args pass an
    -- explicit --model that also overrides models recorded in session files.
    seedPiConfig runnerHome
    appendLogLine cfg (turnLogPath turn) "system" ("Running: " <> T.pack (agentSboxCommand cfg) <> " " <> T.pack (unwords args))
    (Just hin, Just hout, mErr, ph) <- createProcess process
    input <- newMVar (Just (RunnerInput hin Nothing False False Nothing))
    let cleanup = do
            atomically $ modifyTVar' runnerInputs (Map.delete (sessionId session_))
            closeRunnerInput input
        run = do
            stopped <- attachRunnerProcess (sessionId session_) (turnId turn) ph
            unless stopped $ do
                writeRpc hin $ Aeson.object ["id" Aeson..= ("initial" :: Text), "type" Aeson..= ("prompt" :: Text), "message" Aeson..= promptText]
                writeRpc hin $ Aeson.object ["type" Aeson..= ("get_session_stats" :: Text)]
                atomically $ modifyTVar' runnerInputs (Map.insert (sessionId session_) input)
            outReader <- async $ streamHandle cfg (turnLogPath turn) (T.pack outputMarker) "stdout" (handleRpcEvent cfg (turnLogPath turn) input) hout
            errReader <- maybe (async (return False)) (async . streamHandle cfg (turnLogPath turn) (T.pack outputMarker) "stderr" (const $ return ())) mErr
            mExit <- timeout (agentTimeoutSeconds cfg * 1000000) (waitForProcess ph)
            exitCode <- case mExit of
                Just code -> return code
                Nothing -> do
                    appendLogLine cfg (turnLogPath turn) "system" "Runner timed out; terminating process"
                    terminateProcess ph
                    waitForProcess ph
            modelFailed <- wait outReader
            _ <- wait errReader
            return $ if exitCode == ExitSuccess && modelFailed then ExitFailure 1 else exitCode
    run `finally` cleanup

-- | Copy the operator-provided pi agent config into a session's sandbox HOME.
seedPiConfig :: FilePath -> IO ()
seedPiConfig runnerHome = do
    srcDir <- piAgentConfigDir
    let dstDir = runnerHome </> ".pi" </> "agent"
    present <- filterM (doesFileExist . (srcDir </>)) ["models.json", "settings.json"]
    forM_ present $ \name -> do
        createDirectoryIfMissing True dstDir
        copyFile (srcDir </> name) (dstDir </> name)
    seedExtensions (srcDir </> "extensions") (dstDir </> "extensions")

{- | Only symlinks are re-created: the module links a store path the sandbox
already sees read-only.
-}
seedExtensions :: FilePath -> FilePath -> IO ()
seedExtensions src dst = do
    hasExtensions <- doesDirectoryExist src
    when hasExtensions $ do
        removePathForcibly dst
        createDirectoryIfMissing True dst
        linked <- filterM (pathIsSymbolicLink . (src </>)) =<< listDirectory src
        forM_ linked $ \name ->
            getSymbolicLinkTarget (src </> name) >>= flip createDirectoryLink (dst </> name)

data PendingQuestion = PendingQuestion
    { questionDialogId :: Text
    , questionRows :: [Text]
    , questionStashedReply :: Maybe Text
    }

dialogMethods :: [Text]
dialogMethods = ["select", "input"]

dialogResponse :: Text -> Text -> Aeson.Value
dialogResponse dialogId value =
    Aeson.object ["type" Aeson..= ("extension_ui_response" :: Text), "id" Aeson..= dialogId, "value" Aeson..= value]

dialogDecline :: Text -> Aeson.Value
dialogDecline dialogId =
    Aeson.object ["type" Aeson..= ("extension_ui_response" :: Text), "id" Aeson..= dialogId, "cancelled" Aeson..= True]

chosenIndex :: Int -> Text -> Maybe Int
chosenIndex optionCount = mfilter (`elem` [1 .. optionCount]) . readMaybe . T.unpack . T.strip

{- | Rows are numbered from 1, and the package appends its own "type something"
row last and reads the reply back with parseInt: a reply that names none of the
offered rows is sent as that last row's number, with the text kept for the
follow-up dialog, because anything else cancels the whole questionnaire.
-}
answerQuestion :: PendingQuestion -> Text -> (Aeson.Value, Maybe PendingQuestion, Text)
answerQuestion question reply = (dialogResponse (questionDialogId question) dialogValue, carried, answerText)
  where
    freeTextRow = questionFreeTextRow question
    pick = pickedRow question reply
    dialogValue = maybe reply (T.pack . show) (fst <$> pick <|> freeTextRow)
    answerText = maybe reply snd pick
    carried = question{questionStashedReply = Just reply} <$ guard (isJust freeTextRow && isNothing pick)

questionFreeTextRow :: PendingQuestion -> Maybe Int
questionFreeTextRow = fmap length . nonEmpty . questionRows

numberedQuestionRows :: PendingQuestion -> [(Int, Text)]
numberedQuestionRows = zip [1 ..] . questionRows

pickedRow :: PendingQuestion -> Text -> Maybe (Int, Text)
pickedRow question reply = do
    row <- questionFreeTextRow question
    number <- chosenIndex (row - 1) reply
    fmap ((,) number) (lookup number (numberedQuestionRows question))

{- | A second dialog arriving while one is open is declined: pi runs tool calls
concurrently, and the chat shows one question at a time.
-}
handleDialog :: AgentConfig -> FilePath -> MVar (Maybe RunnerInput) -> Aeson.Value -> IO ()
handleDialog cfg logPath input event =
    case (event ^? key "id" . _String, event ^? key "method" . _String) of
        (Just dialogId, Just method)
            | method `elem` dialogMethods ->
                modifyMVar input (answerDialog dialogId) >>= mapM_ (uncurry (appendLogLine cfg logPath))
            | otherwise -> withMVar input $ mapM_ (\control -> writeToRunner control (dialogDecline dialogId))
        _ -> return ()
  where
    rows = event ^.. key "options" . values . _String
    title = event ^. key "title" . _String
    -- The last row is the package's own "type something" prompt, typed in the composer.
    offeredRows = init rows
    dialogLines =
        [("stdout", line) | line <- "" : T.splitOn "\n" title ++ rows ++ [""]]
            ++ [("question", jsonLine offeredRows) | not (null offeredRows)]
    answerDialog :: Text -> Maybe RunnerInput -> IO (Maybe RunnerInput, [(Text, Text)])
    answerDialog _ Nothing = return (Nothing, [])
    answerDialog dialogId (Just control) = case inputQuestion control of
        Nothing -> return (Just control{inputQuestion = Just (PendingQuestion dialogId rows Nothing)}, dialogLines)
        Just question -> do
            let stashed = questionStashedReply question
            writeToRunner control (maybe (dialogDecline dialogId) (dialogResponse dialogId) stashed)
            return (Just $ maybe control (const control{inputQuestion = Nothing}) stashed, [])

-- Pi 0.75 emits agent_end before automatic retry/compaction events; check its state before EOF.
handleRpcEvent :: AgentConfig -> FilePath -> MVar (Maybe RunnerInput) -> Aeson.Value -> IO ()
handleRpcEvent cfg logPath input event =
    case event ^? key "type" . _String of
        Just "message_end" -> send "get_session_stats"
        Just "message_start"
            | event ^. key "message" . key "role" . _String == "user"
            , Just content <- event ^? key "message" . key "content" -> do
                let prompt = content ^. (_String `failing` (values . key "text" . _String))
                -- The initial prompt is already stored in turn metadata.
                modifyMVar_ input $ traverse $ \control -> do
                    when (inputPromptSeen control) $
                        appendLogLine cfg logPath "steering" (TE.decodeUtf8 $ LBS.toStrict $ Aeson.encode prompt)
                    return control{inputPromptSeen = True}
        Just "extension_ui_request" -> handleDialog cfg logPath input event
        Just "agent_end" -> send "get_state"
        Just "auto_retry_start" -> setRetrying True
        Just "auto_retry_end" -> do
            setRetrying False
            send "get_state"
        Just "compaction_end"
            | event ^? key "willRetry" . _Bool /= Just True -> do
                send "get_session_stats"
                send "get_state"
        Just "response" -> do
            modifyMVar_ input $ traverse $ \control -> case inputPending control of
                Just (requestId, reply)
                    | event ^? key "id" . _String == Just requestId -> do
                        atomically $ void $ tryPutTMVar reply (event ^? key "success" . _Bool == Just True)
                        return control{inputPending = Nothing}
                _ -> return control
            case event ^? key "command" . _String of
                Just "prompt"
                    | event ^? key "success" . _Bool == Just False
                    , event ^? key "id" . _String == Just "initial" ->
                        closeRunnerInput input
                    | otherwise -> send "get_state"
                Just "get_state"
                    | stateFlag "isStreaming" == Just False
                    , stateFlag "isCompacting" /= Just True
                    , event ^? key "data" . key "pendingMessageCount" . _Integer == Just 0 ->
                        modifyMVar_ input $ \current -> case current of
                            Just control
                                | isNothing (inputPending control)
                                , not (inputRetrying control) ->
                                    Nothing <$ hClose (inputHandle control)
                            _ -> return current
                _ -> return ()
        _ -> return ()
  where
    send command = withMVar input $ mapM_ (\control -> writeToRunner control (Aeson.object ["type" Aeson..= (command :: Text)]))
    setRetrying value = modifyMVar_ input $ return . fmap (\control -> control{inputRetrying = value})
    stateFlag name = event ^? key "data" . key name . _Bool

streamHandle :: AgentConfig -> FilePath -> Text -> Text -> (Aeson.Value -> IO ()) -> Handle -> IO Bool
streamHandle cfg logPath outputMarker visibleLabel onEvent handle = do
    hSetBuffering handle LineBuffering
    let loop outputReady failed warned = do
            eof <- hIsEOF handle
            if eof
                then return failed
                else do
                    lineResult <- try (TIO.hGetLine handle) :: IO (Either IOException Text)
                    case lineResult of
                        Left _ -> return failed
                        Right textLine
                            | textLine == outputMarker -> loop True failed warned
                            | otherwise -> do
                                let event = if outputReady && visibleLabel == "stdout" then Aeson.decodeStrict (TE.encodeUtf8 textLine) else Nothing
                                    (lines_, failure) = maybe (Just [textLine], Nothing) piEventLines event
                                    usage = contextUsage =<< event
                                    high = maybe warned contextHalfFull usage
                                    label = if outputReady && isJust lines_ then visibleLabel else "runner"
                                -- One warning per turn; the next poll reports the same pressure.
                                unless (isJust usage && high && warned) $
                                    mapM_ (appendLogLine cfg logPath label) (fromMaybe [textLine] lines_)
                                mapM_ onEvent event
                                loop outputReady (fromMaybe failed failure) high
    loop False False False

contextUsage :: Aeson.Value -> Maybe (Integer, Integer)
contextUsage event = do
    tokens <- usage "tokens"
    capacity <- usage "contextWindow"
    guard (capacity > 0)
    return (tokens, capacity)
  where
    usage name = event ^? key "data" . key "contextUsage" . key name . _Integer

contextHalfFull :: (Integer, Integer) -> Bool
contextHalfFull (tokens, capacity) = tokens * 2 >= capacity

piEventLines :: Aeson.Value -> (Maybe [Text], Maybe Bool)
piEventLines event = maybe (Nothing, Nothing) eventLines (event ^? key "type" . _String)
  where
    visible ls = (Just ls, Nothing)
    text name = event ^. key name . _String
    messageText name = event ^. key "message" . key name . _String
    flag name = event ^? key name . _Bool
    number name = maybe "?" (T.pack . show) (event ^? key name . _Integer)
    assistant = messageText "role" == "assistant"
    eventLines kind = case kind of
        "message_update" ->
            visible ["*Thinking*" | assistant, event ^. key "assistantMessageEvent" . key "type" . _String == "thinking_start"]
        "message_end"
            | assistant ->
                let failed = messageText "stopReason" `elem` ["error", "aborted"]
                    prose = event ^. key "message" . key "content" . values . filtered isText . key "text" . _String
                    lines_ = if T.null prose then [] else "" : T.splitOn "\n" prose ++ [""]
                 in (Just (lines_ ++ ["**Response error:**" <> code (messageText "errorMessage") | failed]), Just failed)
            | otherwise -> visible []
        "compaction_start" -> visible ["*Summarising the conversation so far*"]
        "compaction_end"
            | not (T.null (text "errorMessage")) ->
                ( Just ["**Context summary failed:**" <> code (text "errorMessage")]
                , if flag "willRetry" == Just True then Nothing else Just True
                )
            | otherwise -> visible []
        "auto_retry_start" -> visible ["*Retrying (" <> number "attempt" <> "/" <> number "maxAttempts" <> ")*"]
        "auto_retry_end"
            | flag "success" == Just False ->
                (Just ["**Retry stopped:**" <> code (text "finalError")], Just True)
            | otherwise -> visible []
        "response"
            | text "command" == "get_session_stats"
            , Just usage@(tokens, capacity) <- contextUsage event
            , contextHalfFull usage ->
                visible ["**Context warning:** This chat is using " <> T.pack (show (tokens * 100 `div` capacity)) <> "% of the model's context (" <> T.pack (show tokens) <> " / " <> T.pack (show capacity) <> " tokens)."]
            | text "command" == "prompt"
            , flag "success" == Just False
            , text "id" == "initial" ->
                (Just ["**Response error:**" <> code (text "error")], Just True)
            | otherwise -> visible []
        _
            | kind `elem` silent -> visible []
            | otherwise -> (Nothing, Nothing)

    isText part = part ^. key "type" . _String == "text"
    silent =
        [ "session"
        , "message_start"
        , "agent_start"
        , "agent_end"
        , "turn_start"
        , "turn_end"
        , "tool_execution_start"
        , "tool_execution_update"
        , "tool_execution_end"
        , "queue_update"
        , "session_info_changed"
        , "thinking_level_changed"
        ]
    code value =
        let cleaned = T.unwords (T.words (T.filter (/= '`') value))
            clipped = if T.length cleaned > 100 then T.take 100 cleaned <> "..." else cleaned
         in if T.null cleaned then "" else " `" <> clipped <> "`"

finishTurn :: AgentConfig -> AgentSession -> AgentTurn -> ExitCode -> IO ()
finishTurn cfg _session turn exitCode = do
    stopped <- atomically $ do
        pending <- readTVar stopRequestedTurns
        let wasStopped = Set.member (turnId turn) pending
        when wasStopped $ writeTVar stopRequestedTurns (Set.delete (turnId turn) pending)
        return wasStopped
    let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure code -> code
        finalStatus
            | stopped = "stopped"
            | exitCode == ExitSuccess = "succeeded"
            | otherwise = "failed"
    when stopped $ appendLogLine cfg (turnLogPath turn) "system" "Stopped by user"
    appendLogLine cfg (turnLogPath turn) "system" ("Agent turn finished with exit code " <> T.pack (show exitCodeInt))
    finishResult <-
        ( try
                ( withUserRepoExclusive $ do
                    loaded <- ExceptT $ loadSessionById (turnSessionId turn)
                    autoCommitResult <- liftIO $ Except.runExceptT $ commitAgentTurnOutputs loaded turn
                    autoCommitError <- case autoCommitResult of
                        Left err -> do
                            liftIO $ appendLogLine cfg (turnLogPath turn) "system" ("Agent output auto-commit failed: " <> T.pack err)
                            return $ Just ("auto_commit_failed: " <> T.pack err)
                        Right (mCommit, skippedPaths) -> do
                            liftIO $ case mCommit of
                                Just commitSha -> appendLogLine cfg (turnLogPath turn) "system" ("Committed agent outputs " <> T.take 12 commitSha)
                                Nothing -> appendLogLine cfg (turnLogPath turn) "system" "No agent output changes to commit"
                            let skippedError =
                                    if null skippedPaths
                                        then Nothing
                                        else Just ("ignored non-output changes: " <> summarizePaths skippedPaths)
                            case skippedError of
                                Just msg -> liftIO $ appendLogLine cfg (turnLogPath turn) "system" msg
                                Nothing -> return ()
                            return skippedError
                    let nextStatus = if status loaded == "running" then "open" else status loaded
                        runnerError = if stopped || exitCode == ExitSuccess then Nothing else Just "runner_failed"
                        nextError = combineErrorMessages [runnerError, autoCommitError]
                        updated = loaded{activeTurnId = Nothing, status = nextStatus, lastError = nextError}
                    -- Pick up an agent-side conflict resolution in the apply
                    -- worktree FIRST: the git commit is the important part, and
                    -- it must survive even if the metadata writes below fail
                    -- (observed: intermittent EBUSY on session.json writes). A
                    -- later finalize pass converges on the committed resolution.
                    applyResolution <- liftIO $ Except.runExceptT $ finalizeApplyResolution updated
                    case applyResolution of
                        Left err ->
                            liftIO $
                                appendLogLine cfg (turnLogPath turn) "system" ("Apply resolution finalize failed: " <> T.pack err)
                        Right mResolved ->
                            forM_ mResolved $ \candidateHead_ ->
                                liftIO $
                                    appendLogLine cfg (turnLogPath turn) "system" ("Committed apply conflict resolution " <> T.take 12 candidateHead_)
                    touched <- liftIO $ touchSession updated
                    liftIO $ saveSession touched
                ) ::
                IO (Either SomeException (Either String ()))
            )
    case finishResult of
        Left ex -> appendLogLine cfg (turnLogPath turn) "system" ("Session finalization error: " <> T.pack (show ex))
        Right (Left err) -> appendLogLine cfg (turnLogPath turn) "system" ("Failed to finalize session: " <> T.pack err)
        Right (Right _) -> return ()
    now <- getCurrentTime
    let finalTurn = turn{turnStatus = finalStatus, turnExitCode = Just exitCodeInt, turnFinishedAt = Just now}
    saveResult <- try (saveTurn finalTurn) :: IO (Either SomeException ())
    case saveResult of
        Left ex -> appendLogLine cfg (turnLogPath turn) "system" ("Turn finalization error: " <> T.pack (show ex))
        Right _ -> return ()
    -- Drop the registry entry so abandoned streams do not leak it.
    unregisterTurnSignal (turnLogPath turn)

combineErrorMessages :: [Maybe Text] -> Maybe Text
combineErrorMessages messages =
    case [msg | Just msg <- messages] of
        [] -> Nothing
        present -> Just (T.intercalate "; " present)

summarizePaths :: [Text] -> Text
summarizePaths paths =
    let shown = take 10 paths
        remaining = length paths - length shown
        suffix =
            if remaining > 0
                then " (+" <> T.pack (show remaining) <> " more)"
                else ""
     in T.intercalate ", " shown <> suffix

appendLogLine :: AgentConfig -> FilePath -> Text -> Text -> IO ()
appendLogLine cfg path label line = do
    size <- safeFileSize path
    when (size < fromIntegral (agentOutputLimitBytes cfg)) $ do
        let rendered = "[" <> label <> "] " <> line <> "\n"
        TIO.appendFile path rendered
        signalTurnLog path

safeFileSize :: FilePath -> IO Integer
safeFileSize path = do
    result <- try (getFileSize path) :: IO (Either IOException Integer)
    case result of
        Left _ -> return 0
        Right size -> return size

{- | Block on the turn log wakeup channel, racing against a 5-second
heartbeat.  A log append (or a turn state save) fires a wakeup; the
heartbeat keeps the connection alive during idle stretches.  The log
file is re-read from the current offset on every wake.
-}
heartbeatDelayMicros :: Int
heartbeatDelayMicros = 5 * 1000000

streamLoop :: AgentTurn -> Int -> TChan () -> IO (S.StepT IO BS.ByteString)
streamLoop turn offset signal = return $ S.Effect $ do
    heartbeatDue <- registerDelay heartbeatDelayMicros
    _ <-
        atomically $
            (Just <$> readTChan signal)
                `orElse` (readTVar heartbeatDue >>= \b -> if b then pure Nothing else retry)
    exists <- doesFileExist (turnLogPath turn)
    content <- if exists then TIO.readFile (turnLogPath turn) else return ""
    let contentLength = T.length content
        chunk = T.drop offset content
        newOffset = contentLength
    if not (T.null chunk)
        then
            return $
                S.Yield
                    (sseEvent "chunk" (Aeson.encode (Aeson.object ["turnId" Aeson..= turnId turn, "chunk" Aeson..= chunk])))
                    (S.Effect (streamLoop turn newOffset signal))
        else do
            mTurn <- findTurn (turnId turn)
            let finalizationFailed = turnLogHasFinalizationFailure content
                done =
                    maybe
                        True
                        (\savedTurn -> not (turnIsUnfinished savedTurn) || finalizationFailed)
                        mTurn
            if done
                then do
                    unregisterTurnSignal (turnLogPath turn)
                    return $
                        S.Yield
                            (sseEvent "done" (Aeson.encode (Aeson.object ["turnId" Aeson..= turnId turn])))
                            S.Stop
                else
                    return $
                        S.Yield
                            (sseEvent "heartbeat" (Aeson.encode (Aeson.object ["turnId" Aeson..= turnId turn])))
                            (S.Effect (streamLoop turn newOffset signal))
