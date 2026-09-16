{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Runner (
    startAgentTurn,
    stopAgentTurn,
    turnLogStreamHandler,
    streamLoop,
    piEventLines,
) where

import Agent.Git (AgentSessionView, commitAgentTurnOutputs, finalizeApplyResolution, loadAgentSessionView, refreshSessionBase, sessionHasActiveRunner)
import Agent.Sandbox (nixDaemonBindArgs)
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
import Agent.TurnSignal (registerTurnSignal, signalTurnLog, unregisterTurnSignal)
import Agent.WarmSession (WarmSessionMeta (..), getOrBuildWarmSession)
import Config (AgentConfig (..), Config (..), loadConfig, resolveConfigPath)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, orElse, readTChan, readTVar, registerDelay, retry, writeTVar)
import Control.Exception (IOException, SomeException, finally, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Servant (Handler, Header, Headers, addHeader, err404, errBody, throwError)
import qualified Servant.Types.SourceT as S
import Sse (sseComment, sseEvent)
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getFileSize, getHomeDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hIsEOF, hPutStr, hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import UserRepo (userRepoPath, withUserRepoExclusive)

{-# NOINLINE activeRunners #-}
-- Nothing reserves an accepted turn before its process exists.
activeRunners :: TVar (Map.Map Text (Text, Maybe ProcessHandle))
activeRunners = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE stopRequestedTurns #-}
stopRequestedTurns :: TVar (Set.Set Text)
stopRequestedTurns = unsafePerformIO $ newTVarIO Set.empty

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
            shouldAutoName = isFirstTurn && maybe True (T.null . T.strip) (sessionName freshSession)
            namedSession =
                if shouldAutoName
                    then case normalizeSessionName prompt of
                        Just name -> freshSession{sessionName = Just name}
                        Nothing -> freshSession
                    else
                        freshSession
        saveTurn turn
        -- A conflict-pending apply must survive the turn boundary: the turn is
        -- how the agent resolves the conflict markers in the apply worktree.
        -- Keep the pending apply (and its conflict summary) and stay in
        -- "prepare_conflict" so the UI keeps showing the review state.
        let pendingApply =
                case preparedApply namedSession of
                    Just p | applyConflictsPending p -> Just p
                    _ -> Nothing
            turnStatus =
                if pendingApply /= Nothing
                    then "prepare_conflict"
                    else "open"
            turnError =
                if pendingApply /= Nothing
                    then lastError namedSession
                    else Nothing
        touched <- touchSession namedSession{status = turnStatus, activeTurnId = Nothing, preparedApply = pendingApply, lastError = turnError}
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
    baseEnv <- getEnvironment
    repoPath <- userRepoPath
    nixBind <- nixDaemonBindArgs
    let pathValue = fromMaybe "/run/current-system/sw/bin:/usr/bin:/bin" (lookup "PATH" baseEnv)
        sessionRoot = takeDirectory (worktreePath session_)
        runnerHome = sessionRoot </> "home"
        -- sbox-inner runs `set -euo pipefail` and references USER/SHELL/etc.; we keep
        -- the host's identity envs and a curated set of provider API keys, but strip
        -- everything else so the runner never inherits backend Git/SSH credentials.
        passthroughKeys =
            [ "USER"
            , "LOGNAME"
            , "SHELL"
            , "TERM"
            , "LANG"
            , "LC_ALL"
            , "TZ"
            , "XDG_RUNTIME_DIR"
            , "XDG_DATA_DIRS"
            , "DEEPSEEK_API_KEY"
            , "ANTHROPIC_API_KEY"
            , "OPENAI_API_KEY"
            , "GROQ_API_KEY"
            , "CEREBRAS_API_KEY"
            , "XAI_API_KEY"
            , "OPENROUTER_API_KEY"
            , "MISTRAL_API_KEY"
            , "GOOGLE_API_KEY"
            , "GEMINI_API_KEY"
            ]
        passthrough =
            [(k, v) | (k, v) <- baseEnv, k `elem` passthroughKeys]
        outputMarker =
            "__POINTY_AGENT_OUTPUT_BEGIN__" <> T.unpack (turnId turn) <> "__"
        runnerEnv =
            [ ("PATH", pathValue)
            , ("HOME", runnerHome)
            , ("POINTY_AGENT_WORKTREE", worktreePath session_)
            , ("POINTY_AGENT_SESSION_ID", T.unpack (sessionId session_))
            , ("POINTY_AGENT_OUTPUT_MARKER", outputMarker)
            ]
                ++ passthrough
        promptInArgs = any (T.isInfixOf "{prompt}") (agentRunnerArgs cfg)
        -- Strip any session-management flags the config may contain; we manage them here.
        sessionManagedFlags = ["-c", "--continue", "--fork", "--session", "--no-session"]
        expandedRunnerArgs = map (expandArg session_ promptText) (agentRunnerArgs cfg)
        strippedRunnerArgs = filter (`notElem` sessionManagedFlags) expandedRunnerArgs
        -- Inject the right session flag for this turn
        sessionFlag = case (isFirstTurn, mWarmFile) of
            (True, Just warmFile) -> ["--fork", warmFile]
            (True, Nothing) -> []
            (False, _) -> ["-c"]
        runnerArgs =
            agentRunnerCommand cfg : sessionFlag ++ strippedRunnerArgs
        wrapperScript =
            "set -e; printf '%s\\n' \"$POINTY_AGENT_OUTPUT_MARKER\"; printf '%s\\n' \"$POINTY_AGENT_OUTPUT_MARKER\" >&2; exec \"$@\""
        -- When forking a warm session, bind its file read-only into the sandbox.
        -- The warm template path is outside the draft home so sbox won't include it otherwise.
        warmBindArgs = case mWarmFile of
            Just warmFile -> ["--ro-bind", warmFile, warmFile]
            Nothing -> []
        gitDirBind = ["--ro-bind", repoPath, repoPath]
        -- When an apply is waiting for conflict resolution, expose the apply
        -- worktree read-write so the agent can edit the conflict markers there.
        applyBindArgs =
            case preparedApply session_ of
                Just pending
                    | applyConflictsPending pending ->
                        ["--bind", candidateWorktree pending, candidateWorktree pending]
                _ -> []
        args =
            map (expandArg session_ promptText) (agentSboxArgs cfg)
                ++ warmBindArgs
                ++ gitDirBind
                ++ applyBindArgs
                ++ nixBind
                ++ ["--", "bash", "-lc", wrapperScript, "pointy-agent-runner"]
                ++ runnerArgs
        process =
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
    (mIn, mOut, mErr, ph) <- createProcess process
    stopRequested <- attachRunnerProcess (sessionId session_) (turnId turn) ph
    case mIn of
        Nothing -> return ()
        Just hin -> do
            unless (stopRequested || promptInArgs) $ do
                hPutStr hin (T.unpack promptText)
                hFlush hin
            hClose hin
    outReader <- maybe (async (return False)) (async . streamHandle cfg (turnLogPath turn) (T.pack outputMarker) "stdout") mOut
    errReader <- maybe (async (return False)) (async . streamHandle cfg (turnLogPath turn) (T.pack outputMarker) "stderr") mErr
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

-- | Copy the operator-provided pi agent config into a session's sandbox HOME.
seedPiConfig :: FilePath -> IO ()
seedPiConfig runnerHome = do
    realHome <- getHomeDirectory
    let srcDir = realHome </> ".pi" </> "agent"
        dstDir = runnerHome </> ".pi" </> "agent"
    forM_ ["models.json", "settings.json"] $ \name -> do
        let src = srcDir </> name
            dst = dstDir </> name
        exists <- doesFileExist src
        when exists $ do
            createDirectoryIfMissing True dstDir
            copyFile src dst

streamHandle :: AgentConfig -> FilePath -> Text -> Text -> Handle -> IO Bool
streamHandle cfg logPath outputMarker visibleLabel handle = do
    hSetBuffering handle LineBuffering
    let loop outputReady failed = do
            eof <- hIsEOF handle
            if eof
                then return failed
                else do
                    lineResult <- try (TIO.hGetLine handle) :: IO (Either IOException Text)
                    case lineResult of
                        Left _ -> return failed
                        Right textLine
                            | textLine == outputMarker -> loop True failed
                            | otherwise -> do
                                let (lines_, failure) =
                                        if outputReady && visibleLabel == "stdout"
                                            then piEventLines textLine
                                            else (Just [textLine], Nothing)
                                    label = if outputReady && isJust lines_ then visibleLabel else "runner"
                                mapM_ (appendLogLine cfg logPath label) (fromMaybe [textLine] lines_)
                                loop outputReady (fromMaybe failed failure)
    loop False False

piEventLines :: Text -> (Maybe [Text], Maybe Bool)
piEventLines line =
    case Aeson.decodeStrict (TE.encodeUtf8 line) :: Maybe Aeson.Value of
        Just (Aeson.Object event)
            | Just kind <- field "type" event >>= str -> eventLines kind event
        _ -> visible (T.splitOn "\n" line)
  where
    visible ls = (Just ls, Nothing)
    eventLines kind event =
        let message = object (field "message" event)
            assistant = field "role" message == Just (Aeson.String "assistant")
         in case kind of
                "message_update" ->
                    visible ["*Thinking*" | assistant, text "type" (object (field "assistantMessageEvent" event)) == "thinking_start"]
                "message_end"
                    | assistant ->
                        let failed = text "stopReason" message `elem` ["error", "aborted"]
                            prose = contentText message
                            lines_ = if T.null prose then [] else "" : T.splitOn "\n" prose ++ [""]
                         in (Just (lines_ ++ ["**Response error:**" <> code (text "errorMessage" message) | failed]), Just failed)
                    | otherwise -> visible []
                "tool_execution_start" -> visible []
                "tool_execution_end" -> visible []
                "compaction_start" -> visible ["*Summarising the conversation so far*"]
                "compaction_end"
                    | not (T.null (text "errorMessage" event)) ->
                        let retrying = field "willRetry" event == Just (Aeson.Bool True)
                         in ( Just ["**Context summary failed:**" <> code (text "errorMessage" event)]
                            , if retrying then Nothing else Just True
                            )
                    | otherwise -> visible []
                "auto_retry_start" -> visible ["*Retrying (" <> number "attempt" event <> "/" <> number "maxAttempts" event <> ")*"]
                "auto_retry_end"
                    | field "success" event == Just (Aeson.Bool False) ->
                        (Just ["**Retry stopped:**" <> code (text "finalError" event)], Just True)
                    | otherwise -> visible []
                "agent_end" -> visible []
                _
                    | kind
                        `elem` [ "session"
                               , "message_start"
                               , "agent_start"
                               , "turn_start"
                               , "turn_end"
                               , "tool_execution_update"
                               , "queue_update"
                               , "session_info_changed"
                               , "thinking_level_changed"
                               ] ->
                        visible []
                    | otherwise -> (Nothing, Nothing)

    contentText value = case field "content" value of
        Just (Aeson.Array parts) -> T.concat (foldMap textPart parts)
        _ -> ""
    textPart (Aeson.Object part)
        | text "type" part == "text" = [text "text" part]
    textPart _ = []
    code value =
        let cleaned = T.unwords (T.words (T.filter (/= '`') value))
            clipped = if T.length cleaned > 100 then T.take 100 cleaned <> "..." else cleaned
         in if T.null cleaned then "" else " `" <> clipped <> "`"
    number key value = case field key value of
        Just (Aeson.Number n) -> T.pack (show (floor n :: Integer))
        _ -> "?"
    text key value = fromMaybe "" (field key value >>= str)
    object (Just (Aeson.Object value)) = value
    object _ = KeyMap.empty
    field key = KeyMap.lookup (Key.fromText key)
    str (Aeson.String value) = Just value
    str _ = Nothing

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

expandArg :: AgentSession -> Text -> Text -> String
expandArg session_ promptText arg =
    let sessionRoot = T.pack (takeDirectory (worktreePath session_))
        runnerHome = sessionRoot <> "/home"
     in T.unpack $
            T.replace "{prompt}" promptText $
                T.replace "{worktree}" (T.pack (worktreePath session_)) $
                    T.replace "{home}" runnerHome $
                        T.replace "{sessionRoot}" sessionRoot $
                            T.replace "{sessionId}" (sessionId session_) arg

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
