{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.WarmSession (
    WarmSessionMeta (..),
    getOrBuildWarmSession,
) where

import Agent.Policy (renderEmbeddedBootstrapPrompt)
import Agent.Sandbox (nixDaemonBindArgs)
import Agent.Session (agentSessionsRoot)
import Config (AgentConfig (..))
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getFileSize,
    getHomeDirectory,
    getModificationTime,
    listDirectory,
    removePathForcibly,
 )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, hClose)
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
 )
import UserRepo (runGitIn, userRepoPath)

data WarmSessionMeta = WarmSessionMeta
    { warmBaseCommit :: Text
    , warmBootstrapPrompt :: Text
    , warmSessionFile :: FilePath
    , warmCreatedAt :: UTCTime
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

warmTemplateDir :: IO FilePath
warmTemplateDir = do
    root <- agentSessionsRoot
    return $ root </> "warm-template"

warmMetaPath :: IO FilePath
warmMetaPath = do
    dir <- warmTemplateDir
    return $ dir </> "meta.json"

loadWarmMeta :: IO (Maybe WarmSessionMeta)
loadWarmMeta = do
    path <- warmMetaPath
    exists <- doesFileExist path
    if not exists
        then return Nothing
        else do
            result <- eitherDecode <$> LBS.readFile path
            return $ case result of
                Left _ -> Nothing
                Right meta -> Just meta

saveWarmMeta :: WarmSessionMeta -> IO ()
saveWarmMeta meta = do
    path <- warmMetaPath
    createDirectoryIfMissing True (takeDirectory path)
    LBS.writeFile path (encode meta)

{- | Returns Nothing when bootstrap is disabled (empty prompt).
Returns Just (Right meta) when a valid warm session is ready.
Returns Just (Left err) when the build failed.
-}
getOrBuildWarmSession :: AgentConfig -> Text -> (ProcessHandle -> IO ()) -> IO (Maybe (Either String WarmSessionMeta))
getOrBuildWarmSession cfg baseCommit onProcessStarted = do
    let configuredPrompt = agentBootstrapPrompt cfg
    if T.null configuredPrompt
        then return Nothing
        else do
            let bootstrapPrompt = renderEmbeddedBootstrapPrompt configuredPrompt
            existing <- loadWarmMeta
            case existing of
                Just meta | warmBaseCommit meta == baseCommit && warmBootstrapPrompt meta == bootstrapPrompt -> do
                    valid <- isValidSessionFile (warmSessionFile meta)
                    if valid
                        then return $ Just $ Right meta
                        else Just <$> buildWarmSession cfg baseCommit bootstrapPrompt onProcessStarted
                _ -> Just <$> buildWarmSession cfg baseCommit bootstrapPrompt onProcessStarted

isValidSessionFile :: FilePath -> IO Bool
isValidSessionFile path = do
    exists <- doesFileExist path
    if not exists
        then return False
        else (> 0) <$> getFileSize path

buildWarmSession :: AgentConfig -> Text -> Text -> (ProcessHandle -> IO ()) -> IO (Either String WarmSessionMeta)
buildWarmSession cfg baseCommit bootstrapPrompt onProcessStarted = do
    repoPath <- userRepoPath
    templateDir <- warmTemplateDir
    let worktreeDir = templateDir </> "worktree"
        home = templateDir </> "home"
        piSessionDir = home </> "pi-sessions"
    -- Tear down any stale template
    result <- try (removePathForcibly templateDir) :: IO (Either IOException ())
    case result of
        Left err ->
            return $ Left $ "Failed to clean warm template dir: " ++ show err
        Right () -> do
            -- Prune stale worktree registrations after removing the directory;
            -- otherwise git worktree add sees a phantom registration and fails.
            _ <- runGitIn repoPath ["worktree", "prune"]
            createDirectoryIfMissing True piSessionDir
            -- Create detached worktree at baseCommit
            worktreeResult <- createBootstrapWorktree repoPath worktreeDir baseCommit
            case worktreeResult of
                Left err -> return $ Left err
                Right () -> do
                    exitCode <- runBootstrapProcess cfg bootstrapPrompt worktreeDir home piSessionDir onProcessStarted
                    case exitCode of
                        ExitFailure code ->
                            return $ Left $ "Bootstrap runner exited with code " ++ show code
                        ExitSuccess -> do
                            mSessionFile <- findSessionFile piSessionDir
                            case mSessionFile of
                                Nothing ->
                                    return $ Left "Bootstrap succeeded but no Pi session file was created"
                                Just sessionFile -> do
                                    now <- getCurrentTime
                                    let meta =
                                            WarmSessionMeta
                                                { warmBaseCommit = baseCommit
                                                , warmBootstrapPrompt = bootstrapPrompt
                                                , warmSessionFile = sessionFile
                                                , warmCreatedAt = now
                                                }
                                    saveWarmMeta meta
                                    return $ Right meta

createBootstrapWorktree :: FilePath -> FilePath -> Text -> IO (Either String ())
createBootstrapWorktree repoPath worktreeDir baseCommit = do
    createDirectoryIfMissing True (takeDirectory worktreeDir)
    (exitCode, _, stderr) <-
        runGitIn repoPath ["worktree", "add", "--detach", worktreeDir, T.unpack baseCommit]
    return $ case exitCode of
        ExitSuccess -> Right ()
        ExitFailure code ->
            Left $ "git worktree add failed (" ++ show code ++ "): " ++ stderr

runBootstrapProcess :: AgentConfig -> Text -> FilePath -> FilePath -> FilePath -> (ProcessHandle -> IO ()) -> IO ExitCode
runBootstrapProcess cfg bootstrapPrompt worktreeDir home piSessionDir onProcessStarted = do
    baseEnv <- getEnvironment
    realHome <- getHomeDirectory
    repoPath <- userRepoPath
    nixBind <- nixDaemonBindArgs
    let realPiAgentDir = realHome </> ".pi" </> "agent"
    let pathValue = fromMaybe "/run/current-system/sw/bin:/usr/bin:/bin" (lookup "PATH" baseEnv)
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
        passthrough = [(k, v) | (k, v) <- baseEnv, k `elem` passthroughKeys]
        runnerEnv =
            [ ("PATH", pathValue)
            , ("HOME", home)
            , ("PI_CODING_AGENT_DIR", realPiAgentDir)
            , ("PI_CODING_AGENT_SESSION_DIR", piSessionDir)
            ]
                ++ passthrough
        bootstrapPromptStr = T.unpack bootstrapPrompt
        -- Bootstrap: read-only tools, non-interactive, no output marker wrapper needed
        runnerArgs = [agentRunnerCommand cfg, "--tools", "read,grep,find,ls", "-p", bootstrapPromptStr]
        -- Expand sbox args using bootstrap worktree/home paths
        sboxArgExpanded =
            map
                (expandBootstrapArg worktreeDir home)
                (agentSboxArgs cfg)
        piConfigBind = ["--ro-bind", realPiAgentDir, realPiAgentDir]
        -- Bind the main git repo so the worktree's .git file resolves inside sbox.
        gitDirBind = ["--ro-bind", repoPath, repoPath]
        args = sboxArgExpanded ++ piConfigBind ++ gitDirBind ++ nixBind ++ ["--"] ++ runnerArgs
        process =
            (proc (agentSboxCommand cfg) args)
                { cwd = Just worktreeDir
                , env = Just runnerEnv
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    (mIn, mOut, mErr, ph) <- createProcess process
    onProcessStarted ph
    case mIn of
        Nothing -> return ()
        Just hin -> hClose hin
    -- Drain stdout and stderr concurrently to prevent pipe buffer deadlock
    outDrainer <- async $ drainHandle mOut
    errDrainer <- async $ drainHandle mErr
    _ <- wait outDrainer
    _ <- wait errDrainer
    waitForProcess ph

drainHandle :: Maybe Handle -> IO ()
drainHandle Nothing = return ()
drainHandle (Just h) = do
    _ <- BS.hGetContents h
    return ()

expandBootstrapArg :: FilePath -> FilePath -> Text -> String
expandBootstrapArg worktreeDir home arg =
    T.unpack $
        T.replace "{worktree}" (T.pack worktreeDir) $
            T.replace "{home}" (T.pack home) $
                T.replace "{sessionRoot}" (T.pack (takeDirectory worktreeDir)) arg

findSessionFile :: FilePath -> IO (Maybe FilePath)
findSessionFile piSessionDir = do
    exists <- doesDirectoryExist piSessionDir
    if not exists
        then return Nothing
        else do
            allFiles <- findJsonlFiles piSessionDir
            case allFiles of
                [] -> return Nothing
                _ -> do
                    withTimes <-
                        mapM
                            ( \f -> do
                                t <- getModificationTime f
                                return (t, f)
                            )
                            allFiles
                    let sorted = sortOn (Down . fst) withTimes
                    return $ fmap snd (listToMaybe sorted)

-- | Recursively collect all .jsonl files under a directory.
findJsonlFiles :: FilePath -> IO [FilePath]
findJsonlFiles dir = do
    entries <- listDirectory dir
    fmap concat $
        mapM
            ( \name -> do
                let fullPath = dir </> name
                isDir <- doesDirectoryExist fullPath
                if isDir
                    then findJsonlFiles fullPath
                    else return $ if isSuffixOf ".jsonl" name then [fullPath] else []
            )
            entries
