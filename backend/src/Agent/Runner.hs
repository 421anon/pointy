{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Runner (
    startAgentTurn,
    turnLogStreamHandler,
) where

import Agent.Git (commitAgentTurnOutputs)
import Agent.Sandbox (nixDaemonBindArgs)
import Agent.Session (
    AgentSession (..),
    AgentTurn (..),
    findTurn,
    listTurns,
    loadSessionById,
    normalizeSessionName,
    newTurnId,
    saveSession,
    saveTurn,
    touchSession,
    turnLogFilePath,
 )
import Agent.WarmSession (WarmSessionMeta (..), getOrBuildWarmSession)
import Config (AgentConfig (..), Config (..), loadConfig, resolveConfigPath)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, SomeException, try)
import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT (..))
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Servant (Handler, Header, Headers, addHeader, err404, errBody, throwError)
import qualified Servant.Types.SourceT as S
import System.Directory (createDirectoryIfMissing, doesFileExist, getFileSize)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (..), Handle, hClose, hFlush, hGetLine, hIsEOF, hPutStr, hSetBuffering)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import UserRepo (userRepoPath, withUserRepoExclusive)

startAgentTurn :: Text -> Text -> ExceptT String IO AgentTurn
startAgentTurn sid prompt = do
    session_ <- ExceptT $ loadSessionById sid
    when (status session_ == "applied") $ Except.throwError "session_applied"
    when (status session_ == "discarded") $ Except.throwError "session_discarded"
    when (status session_ == "archived") $ Except.throwError "session_archived"
    when (status session_ == "running" || activeTurnId session_ /= Nothing) $ Except.throwError "runner_active"
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
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
        existingTurns <- listTurns sid
        let isFirstTurn = null existingTurns
            shouldAutoName = isFirstTurn && maybe True (T.null . T.strip) (sessionName session_)
            namedSession =
                if shouldAutoName then
                    case normalizeSessionName prompt of
                        Just name -> session_{sessionName = Just name}
                        Nothing -> session_

                else
                    session_
        saveTurn turn
        touched <- touchSession namedSession{status = "running", activeTurnId = Just tid, preparedApply = Nothing, lastError = Nothing}
        saveSession touched
        void $ forkIO $ runTurnProcess (configAgent cfg) touched turn prompt isFirstTurn
    return turn

turnLogStreamHandler :: Text -> Handler (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (S.SourceT IO BS.ByteString))
turnLogStreamHandler tid = do
    mTurn <- liftIO $ findTurn tid
    turn <- case mTurn of
        Nothing -> throwError err404{errBody = "turn not found"}
        Just t -> return t
    let padding = sseComment $ "padding " <> T.pack (replicate 4096 ' ')
        source =
            S.fromStepT
                ( S.Yield
                    (sseComment "connected")
                    (S.Yield padding (S.Effect (streamLoop turn 0 0)))
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
        args =
            map (expandArg session_ promptText) (agentSboxArgs cfg)
                ++ warmBindArgs
                ++ gitDirBind
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
    appendLogLine cfg (turnLogPath turn) "system" ("Running: " <> T.pack (agentSboxCommand cfg) <> " " <> T.pack (unwords args))
    (mIn, mOut, mErr, ph) <- createProcess process
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
    let exitCodeInt = case exitCode of
            ExitSuccess -> 0
            ExitFailure code -> code
        finalStatus = if exitCode == ExitSuccess then "succeeded" else "failed"
    appendLogLine cfg (turnLogPath turn) "system" ("Agent turn finished with exit code " <> T.pack (show exitCodeInt))
    finishResult <-
        ( try
                ( withUserRepoExclusive $ do
                    loaded <- ExceptT $ loadSessionById (turnSessionId turn)
                    let clearActive = activeTurnId loaded == Just (turnId turn)
                    if not clearActive
                        then
                            liftIO $
                                appendLogLine cfg (turnLogPath turn) "system" "Skipping session finalization; turn is no longer active"
                        else do
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
                                runnerError = if exitCode == ExitSuccess then Nothing else Just "runner_failed"
                                nextError = combineErrorMessages [runnerError, autoCommitError]
                                updated = loaded{activeTurnId = Nothing, status = nextStatus, lastError = nextError}
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


streamLoop :: AgentTurn -> Int -> Int -> IO (S.StepT IO BS.ByteString)
streamLoop turn offset heartbeatTick = do
    threadDelay 500000
    exists <- doesFileExist (turnLogPath turn)
    content <- if exists then TIO.readFile (turnLogPath turn) else return ""
    let contentLength = T.length content
        chunk = T.drop offset content
        newOffset = contentLength
    mTurn <- findTurn (turnId turn)
    mSession <- loadSessionById (turnSessionId turn)
    let turnDone = maybe False ((/= "running") . turnStatus) mTurn
        noLongerActive = case mSession of
            Left _ -> True
            Right session_ -> activeTurnId session_ /= Just (turnId turn)
        done = turnDone || noLongerActive
    if not (T.null chunk)
        then
            return $
                S.Yield
                    (sseEvent "chunk" (Aeson.encode (Aeson.object ["turnId" Aeson..= turnId turn, "chunk" Aeson..= chunk])))
                    (S.Effect (streamLoop turn newOffset 0))
        else
            if done
                then
                    return $
                        S.Yield
                            (sseEvent "done" (Aeson.encode (Aeson.object ["turnId" Aeson..= turnId turn])))
                            S.Stop
                else do
                    let nextTick = heartbeatTick + 1
                    if nextTick >= 10
                        then
                            return $
                                S.Yield
                                    (sseEvent "heartbeat" (Aeson.encode (Aeson.object ["turnId" Aeson..= turnId turn])))
                                    (S.Effect (streamLoop turn newOffset 0))
                        else return $ S.Yield (sseComment ("tick-" <> T.pack (show nextTick))) (S.Effect (streamLoop turn newOffset nextTick))

sseEvent :: Text -> LBS.ByteString -> BS.ByteString
sseEvent eventName payload =
    TE.encodeUtf8 ("event: " <> eventName <> "\n")
        <> "data: "
        <> LBS.toStrict payload
        <> "\n\n"

sseComment :: Text -> BS.ByteString
sseComment text_ =
    TE.encodeUtf8 (": " <> text_ <> "\n\n")
