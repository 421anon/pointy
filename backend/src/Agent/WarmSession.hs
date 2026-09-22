{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.WarmSession (
    WarmSessionMeta (..),
    getOrBuildWarmSession,
) where

import Agent.Policy (renderEmbeddedBootstrapPrompt)
import Agent.Sandbox (SandboxPaths (..), bindPathReadOnly, expandSandboxArg, nixDaemonBindArgs, piAgentConfigDir, runnerEnvironment)
import Agent.Session (agentSessionsRoot)
import Config (AgentConfig (..))
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (listToMaybe)
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
    getModificationTime,
    listDirectory,
    removePathForcibly,
 )
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
    result <- try (removePathForcibly templateDir) :: IO (Either IOException ())
    case result of
        Left err ->
            return $ Left $ "Failed to clean warm template dir: " ++ show err
        Right () -> do
            _ <- runGitIn repoPath ["worktree", "prune"]
            createDirectoryIfMissing True piSessionDir
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
    repoPath <- userRepoPath
    nixBind <- nixDaemonBindArgs
    piConfigDir <- piAgentConfigDir
    runnerEnv <-
        runnerEnvironment
            [ ("HOME", home)
            , ("PI_CODING_AGENT_DIR", piConfigDir)
            , ("PI_CODING_AGENT_SESSION_DIR", piSessionDir)
            ]
    let expand =
            expandSandboxArg
                SandboxPaths{sandboxWorktree = worktreeDir, sandboxHome = home, sandboxSessionId = ""}
        runnerArgs = [agentRunnerCommand cfg, "--tools", "read,grep,find,ls", "-p", T.unpack bootstrapPrompt]
        args =
            map expand (agentSboxArgs cfg)
                ++ bindPathReadOnly piConfigDir
                ++ bindPathReadOnly repoPath
                ++ nixBind
                ++ ["--"]
                ++ runnerArgs
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
