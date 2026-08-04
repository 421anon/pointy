{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Runner (
    startAgentTurn,
    stopAgentTurn,
    turnLogStreamHandler,
    streamLoop,
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
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (TChan, TVar, atomically, modifyTVar', newTVarIO, orElse, readTChan, readTVar, registerDelay, retry, writeTVar)
import Control.Exception (IOException, SomeException, finally, try)
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Except (ExceptT (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Servant (Handler, Header, Headers, addHeader, err404, errBody, throwError)
import qualified Servant.Types.SourceT as S
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getFileSize, getHomeDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hGetLine, hIsEOF, hPutStr, hSetBuffering)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, getPid, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import UserRepo (userRepoPath, withUserRepoExclusive)

{-# NOINLINE activeRunners #-}
activeRunners :: TVar (Map.Map Text (Text, ProcessHandle))
activeRunners = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE stoppedTurns #-}
stoppedTurns :: TVar (Set.Set Text)
stoppedTurns = unsafePerformIO $ newTVarIO Set.empty

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
        void $ forkIO $ runTurnProcess (configAgent cfg) touched turn prompt isFirstTurn
    return turn

stopAgentTurn :: Text -> ExceptT String IO AgentSessionView
stopAgentTurn sid = do
    mRunner <- liftIO $ atomically $ Map.lookup sid <$> readTVar activeRunners
    case mRunner of
        Nothing -> return ()
        Just (tid, ph) -> liftIO $ do
            atomically $ modifyTVar' stoppedTurns (Set.insert tid)
            cfg <- resolveConfigPath >>= loadConfig
            logPath <- turnLogFilePath sid tid
            appendLogLine (configAgent cfg) logPath "system" "Stopped by user"
            void (try (terminateProcess ph) :: IO (Either SomeException ()))
            terminated <- awaitRunnerExit sid 40
            unless terminated $ do
                appendLogLine (configAgent cfg) logPath "system" "Runner ignored termination; killing it"
                void (try (killRunner ph) :: IO (Either SomeException ()))
                void $ awaitRunnerExit sid 60
    loadAgentSessionView sid

awaitRunnerExit :: Text -> Int -> IO Bool
awaitRunnerExit sid ticks = do
    live <- atomically $ Map.member sid <$> readTVar activeRunners
    if not live
        then return True
        else
            if ticks <= 0
                then return False
                else threadDelay 100000 >> awaitRunnerExit sid (ticks - 1)

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
runTurnProcess cfg session_ turn prompt isFirstTurn = do
    appendLogLine cfg (turnLogPath turn) "system" ("Starting agent turn " <> turnId turn)
    mWarmResult <-
        if isFirstTurn
            then do
                appendLogLine cfg (turnLogPath turn) "system" "Warming up agent context..."
                getOrBuildWarmSession cfg (baseCommit session_)
            else return Nothing
    case mWarmResult of
        Just (Left err) ->
            appendLogLine cfg (turnLogPath turn) "system" ("Warm session unavailable, starting cold: " <> T.pack err)
        _ -> return ()
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
        `finally` atomically (modifyTVar' activeRunners (Map.delete (sessionId session_)))

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
    atomically $ modifyTVar' activeRunners (Map.insert (sessionId session_) (turnId turn, ph))
    case mIn of
        Nothing -> return ()
        Just hin -> do
            hPutStr hin (T.unpack promptText)
            hFlush hin
            hClose hin
    outReader <- maybe (async (return ())) (async . streamHandle cfg (turnLogPath turn) (T.pack outputMarker) "stdout") mOut
    errReader <- maybe (async (return ())) (async . streamHandle cfg (turnLogPath turn) (T.pack outputMarker) "stderr") mErr
    mExit <- timeout (agentTimeoutSeconds cfg * 1000000) (waitForProcess ph)
    exitCode <- case mExit of
        Just code -> return code
        Nothing -> do
            appendLogLine cfg (turnLogPath turn) "system" "Runner timed out; terminating process"
            terminateProcess ph
            waitForProcess ph
    _ <- wait outReader
    _ <- wait errReader
    return exitCode

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

streamHandle :: AgentConfig -> FilePath -> Text -> Text -> Handle -> IO ()
streamHandle cfg logPath outputMarker visibleLabel handle = do
    hSetBuffering handle LineBuffering
    let loop outputReady = do
            eof <- hIsEOF handle
            unless eof $ do
                lineResult <- try (hGetLine handle) :: IO (Either IOException String)
                case lineResult of
                    Left _ -> return ()
                    Right line -> do
                        let textLine = T.pack line
                        if textLine == outputMarker
                            then
                                loop True
                            else do
                                let label = if outputReady then visibleLabel else "runner"
                                appendLogLine cfg logPath label textLine
                                loop outputReady
    loop False

finishTurn :: AgentConfig -> AgentSession -> AgentTurn -> ExitCode -> IO ()
finishTurn cfg _session turn exitCode = do
    stopped <- atomically $ do
        pending <- readTVar stoppedTurns
        let wasStopped = Set.member (turnId turn) pending
        when wasStopped $ writeTVar stoppedTurns (Set.delete (turnId turn) pending)
        return wasStopped
    let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure code -> code
        finalStatus
            | stopped = "stopped"
            | exitCode == ExitSuccess = "succeeded"
            | otherwise = "failed"
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
                    touched <- liftIO $ touchSession updated
                    liftIO $ saveSession touched
                    -- Pick up an agent-side conflict resolution in the apply
                    -- worktree: commit the squash merge once the markers are
                    -- gone, so the apply becomes confirmable.
                    applyResolution <- liftIO $ Except.runExceptT $ finalizeApplyResolution updated
                    case applyResolution of
                        Left err ->
                            liftIO $
                                appendLogLine cfg (turnLogPath turn) "system" ("Apply resolution finalize failed: " <> T.pack err)
                        Right mResolved ->
                            forM_ mResolved $ \candidateHead_ ->
                                liftIO $
                                    appendLogLine cfg (turnLogPath turn) "system" ("Committed apply conflict resolution " <> T.take 12 candidateHead_)
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

sseEvent :: Text -> LBS.ByteString -> BS.ByteString
sseEvent eventName payload =
    TE.encodeUtf8 ("event: " <> eventName <> "\n")
        <> "data: "
        <> LBS.toStrict payload
        <> "\n\n"

sseComment :: Text -> BS.ByteString
sseComment text_ =
    TE.encodeUtf8 (": " <> text_ <> "\n\n")
