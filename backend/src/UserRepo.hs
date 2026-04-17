{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UserRepo (
    userRepoPath,
    ensureUserRepo,
    runNix,
    runNixInRepo,
    runGit,
    runGitIn,
    ReadRepoContext (..),
    WriteRepoContext (..),
    withReadRepoTransaction,
    withWriteRepoTransactionRaw,
    commitAndPushChanges,
    fetchRepo,
) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ProcessLimiter (readCreateProcessWithExitCodeL, readProcessWithExitCodeL)
import System.Directory (doesDirectoryExist, doesFileExist, getHomeDirectory, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (..))
import qualified System.FileLock
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (
    CreateProcess (..),
    proc,
 )

data ReadRepoContext = ReadRepoContext
    { readRepoPath :: FilePath
    , readCommitHash :: String
    }

newtype WriteRepoContext = WriteRepoContext
    { writeWorktreePath :: FilePath
    }

class RepoContext ctx where
    nixArgs :: ctx -> String -> String

instance RepoContext ReadRepoContext where
    nixArgs (ReadRepoContext repoPath commitHash) attr =
        let url = "git+file://" ++ repoPath ++ "?rev=" ++ commitHash ++ "&allRefs=true" ++ attr
         in url

instance RepoContext WriteRepoContext where
    nixArgs (WriteRepoContext worktreePath) attr =
        worktreePath ++ attr

runNix :: [String] -> ExceptT String IO String
runNix args = ExceptT $ do
    (exitCode, stdout, stderr) <- readProcessWithExitCodeL "nix" args ""
    return $ case exitCode of
        ExitSuccess -> Right stdout
        ExitFailure _ -> Left stderr

runNixInRepo :: (RepoContext ctx) => ctx -> [String] -> String -> ExceptT String IO String
runNixInRepo ctx args attr = ExceptT $ do
    (exitCode, stdout, stderr) <- readProcessWithExitCodeL "nix" (args ++ [nixArgs ctx attr]) ""
    return $ case exitCode of
        ExitSuccess -> Right stdout
        ExitFailure _ -> Left stderr

userRepoPath :: IO FilePath
userRepoPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> "user-repo.git"

userRepoLockPath :: IO FilePath
userRepoLockPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> "user-repo.lock"

-- Note: This function uses blocking file locks (flock) under the hood.
-- See comment at the `-threaded` flag in backend.cabal.
withFileLock :: FilePath -> RepoAccess -> IO a -> IO a
withFileLock lockPath access action = do
    let mode = case access of
            ReadOnly -> Shared
            ReadWrite -> Exclusive
    System.FileLock.withFileLock lockPath mode $ const action

data RepoAccess = ReadOnly | ReadWrite deriving (Eq, Show)

runGit :: [String] -> IO (ExitCode, String, String)
runGit args = do
    repoPath <- userRepoPath
    readCreateProcessWithExitCodeL (proc "git" ("-C" : repoPath : args)) ""

runGitIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runGitIn path args = do
    readCreateProcessWithExitCodeL (proc "git" ("-C" : path : args)) ""

runGitWithSshKey :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runGitWithSshKey keyfile path args = do
    let sshCommand = "ssh -i " ++ keyfile ++ " -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new"
    baseEnv <- getEnvironment
    let env_ = ("GIT_SSH_COMMAND", sshCommand) : baseEnv
        process = (proc "git" ("-C" : path : args)){env = Just env_}
    readCreateProcessWithExitCodeL process ""

getRemoteUrl :: IO (Maybe Text)
getRemoteUrl = do
    (exitCode, stdout, _) <- runGit ["remote", "get-url", "origin"]
    case exitCode of
        ExitSuccess -> return $ Just $ T.strip $ T.pack stdout
        ExitFailure _ -> return Nothing

ensureUserRepo :: Config -> IO ()
ensureUserRepo cfg = do
    repoPath <- userRepoPath
    lockPath <- userRepoLockPath
    let backupPath = repoPath ++ "-replaced"
        userRepo = configUserRepo cfg

    lockExists <- doesFileExist lockPath
    when lockExists $ do
        putStrLn "Removing stale lockfile..."
        removeFile lockPath

    exists <- doesDirectoryExist repoPath
    if exists
        then do
            mUrl <- getRemoteUrl
            let urlMatches = mUrl == Just (userRepoUrl userRepo)
            if urlMatches
                then do
                    putStrLn "User repo already configured correctly"
                    cleanWorktrees repoPath
                else replaceAndClone backupPath userRepo
        else cloneRepoFresh userRepo

cleanWorktrees :: FilePath -> IO ()
cleanWorktrees repoPath = do
    putStrLn "Cleaning up non-main worktrees..."
    (exitCode, stdout, stderr) <- runGitIn repoPath ["worktree", "list"]
    case exitCode of
        ExitSuccess -> do
            let worktreeLines = drop 1 (lines stdout)
                worktreePaths = mapMaybe (listToMaybe . words) worktreeLines
            forM_ worktreePaths $ \worktreePath -> do
                (removeCode, _, removeErr) <- runGitIn repoPath ["worktree", "remove", worktreePath]
                case removeCode of
                    ExitSuccess -> return ()
                    ExitFailure code ->
                        putStrLn $
                            "Warning: git worktree remove failed for "
                                ++ worktreePath
                                ++ " (exit "
                                ++ show code
                                ++ "): "
                                ++ removeErr
        ExitFailure code -> putStrLn $ "Warning: git worktree list failed (exit " ++ show code ++ "): " ++ stderr

replaceAndClone :: FilePath -> UserRepoConfig -> IO ()
replaceAndClone backupPath cfg = do
    repoPath <- userRepoPath
    putStrLn "User repo configuration mismatch, replacing..."
    backupExists <- doesDirectoryExist backupPath
    when backupExists $ do
        putStrLn "Removing old backup..."
        removeDirectoryRecursive backupPath
    putStrLn $ "Moving " ++ repoPath ++ " to " ++ backupPath
    renameDirectory repoPath backupPath
    cloneRepoFresh cfg

cloneRepoFresh :: UserRepoConfig -> IO ()
cloneRepoFresh cfg = do
    repoPath <- userRepoPath
    putStrLn $ "Cloning bare repo " ++ T.unpack (userRepoUrl cfg) ++ " branch " ++ T.unpack (userRepoBranch cfg)
    let action = do
            (exitCode, stdout, stderr) <-
                runGitWithSshKey
                    (userRepoKeyfile cfg)
                    "."
                    ["clone", "--bare", "--branch", T.unpack (userRepoBranch cfg), T.unpack (userRepoUrl cfg), repoPath]
            case exitCode of
                ExitSuccess -> return $ Right ()
                ExitFailure code -> return $ Left $ "Failed to clone repo (exit " ++ show code ++ "): " ++ stderr ++ stdout
    res <- retry 3 action
    case res of
        Right () -> putStrLn "User repo cloned successfully"
        Left err -> error err

fetchRepo :: ExceptT String IO ()
fetchRepo = ExceptT $ do
    cfg <- resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        keyfile = userRepoKeyfile userRepo
        branch = T.unpack $ userRepoBranch userRepo

    repoPath <- userRepoPath
    let refspec = branch ++ ":" ++ branch
        fetchFromRemote = runGitWithSshKey keyfile repoPath ["fetch", "origin", refspec]
        pushUnpushedLocalCommits = do
            putStrLn "Fetch rejected (non-fast-forward): pushing unpushed local commits..."
            runGitWithSshKey keyfile repoPath ["push", "origin", refspec]
        forceFetchFromRemote pushErr = do
            putStrLn $ "Push also failed (" ++ pushErr ++ "), force-fetching from remote..."
            runGitWithSshKey keyfile repoPath ["fetch", "origin", "+" ++ refspec]
        retryFetchAfterPush = do
            putStrLn "Push succeeded, retrying fetch..."
            fetchFromRemote
        action = do
            (fetchCode, _, fetchErr) <- fetchFromRemote
            case fetchCode of
                ExitSuccess -> return $ Right ()
                ExitFailure _ | "non-fast-forward" `isInfixOf` fetchErr -> do
                    (pushCode, _, pushErr) <- pushUnpushedLocalCommits
                    (fetchCode2, _, fetchErr2) <- case pushCode of
                        ExitSuccess -> retryFetchAfterPush
                        ExitFailure _ -> forceFetchFromRemote pushErr
                    case fetchCode2 of
                        ExitSuccess -> return $ Right ()
                        ExitFailure code2 -> return $ Left $ "git fetch failed with exit code " ++ show code2 ++ ": " ++ fetchErr2
                ExitFailure code -> return $ Left $ "git fetch failed with exit code " ++ show code ++ ": " ++ fetchErr
    retry 3 action

retry :: Int -> IO (Either String a) -> IO (Either String a)
retry 0 action = action
retry n action = do
    res <- action
    case res of
        Left err -> do
            putStrLn $ "Action failed, retrying (" ++ show n ++ " left): " ++ err
            threadDelay 1000000
            retry (n - 1) action
        Right val -> return $ Right val

withReadRepoTransaction :: (ReadRepoContext -> ExceptT String IO a) -> IO (Either String a)
withReadRepoTransaction action = do
    cfg <- resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        branch = T.unpack $ userRepoBranch userRepo

    repoPath <- userRepoPath
    lockPath <- userRepoLockPath

    withFileLock lockPath ReadOnly $ runExceptT $ do
        (_, revOut, _) <-
            ExceptT $
                runGitIn repoPath ["rev-parse", branch] >>= \case
                    (ExitFailure _, _, err) -> return $ Left $ "git rev-parse failed: " ++ err
                    (ExitSuccess, out, _) -> return $ Right (ExitSuccess, out, "" :: String)
        let commitHash = filter (`notElem` ("\n\r" :: String)) revOut
        action (ReadRepoContext repoPath commitHash)

fetchAndWarn :: String -> IO ()
fetchAndWarn context = do
    result <- runExceptT fetchRepo
    case result of
        Left err -> putStrLn $ "Warning: Failed to fetch " ++ context ++ ": " ++ err
        Right () -> return ()

withWriteRepoTransactionRaw :: (WriteRepoContext -> ExceptT String IO a) -> IO (Either String a)
withWriteRepoTransactionRaw action = do
    cfg <- resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        branch = T.unpack $ userRepoBranch userRepo

    repoPath <- userRepoPath
    lockPath <- userRepoLockPath

    res <- withFileLock lockPath ReadWrite $ withSystemTempDirectory "pointy-worktree" $ \worktreePath -> runExceptT $ do
        (addCode, _, addErr) <- liftIO $ runGitIn repoPath ["worktree", "add", worktreePath, branch]
        case addCode of
            ExitFailure _ -> ExceptT $ return $ Left $ "git worktree add failed: " ++ addErr
            ExitSuccess -> return ()

        _ <- liftIO $ runGitIn worktreePath ["config", "user.email", "backend@invalid.local"]
        _ <- liftIO $ runGitIn worktreePath ["config", "user.name", "backend"]

        -- Run the action, ensuring we clean up the worktree afterwards
        ExceptT $
            runExceptT (action (WriteRepoContext worktreePath)) `finally` do
                _ <- runGitIn repoPath ["worktree", "remove", "--force", worktreePath]
                return ()

    when (case res of Left e -> "Concurrent modification detected" `isInfixOf` e; _ -> False) $
        fetchAndWarn "after write transaction"

    case res of
        Right a -> return $ Right a
        Left e -> return $ Left e

commitAndPushChanges :: WriteRepoContext -> String -> ExceptT String IO ()
commitAndPushChanges (WriteRepoContext worktreePath) message = ExceptT $ do
    cfg <- resolveConfigPath >>= loadConfig
    let keyfile = userRepoKeyfile (configUserRepo cfg)
        branch = T.unpack $ userRepoBranch (configUserRepo cfg)
    _ <- runGitIn worktreePath ["add", "-A"]
    (statusCode, statusOut, _) <- runGitIn worktreePath ["status", "--porcelain"]
    if statusCode == ExitSuccess && not (null statusOut)
        then do
            _ <- runGitIn worktreePath ["commit", "-m", message]
            pushWithRetry worktreePath keyfile branch
        else return $ Right ()
  where
    pushWithRetry wp kf br = do
        (exitCode, _, stderr) <- runGitWithSshKey kf wp ["push", "origin", "HEAD:" ++ br]
        case exitCode of
            ExitSuccess -> return $ Right ()
            ExitFailure _ | isRejectedPush stderr -> do
                (pullCode, _, pullErr) <- runGitWithSshKey kf wp ["pull", "--rebase", "origin", br]
                case pullCode of
                    ExitSuccess -> pushWithRetry wp kf br
                    ExitFailure _
                        | hasConflict pullErr -> do
                            _ <- runGitWithSshKey kf wp ["rebase", "--abort"]
                            return $ Left "Concurrent modification detected. The remote repository was updated by another process. Please retry your operation."
                        | otherwise -> return $ Left $ "git pull --rebase failed: " ++ pullErr
            ExitFailure code -> return $ Left $ "git push failed with exit code " ++ show code ++ ": " ++ stderr

    isRejectedPush stderr = any (`isInfixOf` stderr) ["rejected", "non-fast-forward", "fetch first"]

    hasConflict err = any (`isInfixOf` err) ["could not apply", "CONFLICT"]
