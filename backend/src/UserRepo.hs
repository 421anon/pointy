{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module UserRepo (
    userRepoPath,
    ensureUserRepo,
    runNix,
    runNixEvalJsonInRepo,
    runNixEvalJsonInRepoBackground,
    runNixEvalRawInRepo,
    runNixEvalJsonApplyInRepo,
    runNixEvalImpureJsonExpr,
    rewarmRepoJsonExpressions,
    runGit,
    runGitIn,
    runGitWithSshKey,
    ReadRepoContext (..),
    WriteRepoContext (..),
    RepoContext,
    withReadRepoTransaction,
    withReadRepoTransactionIO,
    withWriteRepoTransactionRaw,
    withUserRepoExclusiveIO,
    withUserRepoSharedIO,
    commitAndPushChanges,
    commitContext,
    fetchRepo,
    fetchRepoStrict,
    ensureRepoCommit,
) where

import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Concurrent (threadDelay)
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (bracket, finally)
import Effects (App, Eval (..), Nix (..), evalJson, evalJsonApply, evalImpure, evalRaw, rewarm, runNixCli)
import NixEvaluator (EvalPriority (..), RepoExpression, RepoSource, jsonExpression, mutableRepoSource, repoSource)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, removeDirectoryRecursive, removeFile, renameDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FileLock (SharedExclusive (..))
import qualified System.FileLock
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, withSystemTempDirectory)
import System.Posix.Process (getProcessID)
import System.Process (
    CreateProcess (..),
    proc,
    readCreateProcessWithExitCode,
 )

data ReadRepoContext = ReadRepoContext
    { readRepoPath :: FilePath
    , readCommitHash :: String
    }

newtype WriteRepoContext = WriteRepoContext
    { writeWorktreePath :: FilePath
    }

class RepoContext ctx where
    evaluatorSource :: ctx -> RepoSource

instance RepoContext ReadRepoContext where
    evaluatorSource (ReadRepoContext repoPath commitHash) =
        repoSource $ "git+file://" ++ repoPath ++ "?rev=" ++ commitHash ++ "&allRefs=true"

instance RepoContext WriteRepoContext where
    evaluatorSource = mutableRepoSource . writeWorktreePath

runNix :: (Nix :> es) => [String] -> ExceptT String (Eff es) String
runNix args = ExceptT $ do
    (code, stdout, stderr) <- runNixCli args
    pure $ case code of
        ExitSuccess -> Right stdout
        ExitFailure _ -> Left stderr

runNixEvalJsonInRepo :: (RepoContext ctx, Eval :> es) => ctx -> String -> ExceptT String (Eff es) String
runNixEvalJsonInRepo ctx attr = ExceptT $ evalJson Interactive (evaluatorSource ctx) attr

runNixEvalJsonInRepoBackground :: (RepoContext ctx, Eval :> es) => ctx -> String -> ExceptT String (Eff es) String
runNixEvalJsonInRepoBackground ctx attr = ExceptT $ evalJson Background (evaluatorSource ctx) attr

runNixEvalRawInRepo :: (RepoContext ctx, Eval :> es) => ctx -> String -> ExceptT String (Eff es) String
runNixEvalRawInRepo ctx attr = ExceptT $ evalRaw (evaluatorSource ctx) attr

runNixEvalJsonApplyInRepo :: (RepoContext ctx, Eval :> es) => ctx -> String -> String -> ExceptT String (Eff es) String
runNixEvalJsonApplyInRepo ctx applyExpr attr = ExceptT $ evalJsonApply (evaluatorSource ctx) applyExpr attr

runNixEvalImpureJsonExpr :: (Eval :> es) => String -> ExceptT String (Eff es) String
runNixEvalImpureJsonExpr = ExceptT . evalImpure

rewarmRepoJsonExpressions :: (Eval :> es) => RepoSource -> [(Maybe Int, String)] -> Eff es (Either String [(Maybe Int, Either String String)])
rewarmRepoJsonExpressions = rewarm

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
withRepoLock :: (IOE :> es) => FilePath -> RepoAccess -> Eff es a -> Eff es a
withRepoLock lockPath access action =
    bracket
        (liftIO $ System.FileLock.lockFile lockPath (lockMode access))
        (liftIO . System.FileLock.unlockFile)
        (const action)

lockMode :: RepoAccess -> SharedExclusive
lockMode ReadOnly = Shared
lockMode ReadWrite = Exclusive

withUserRepoExclusiveIO :: ExceptT String IO a -> IO (Either String a)
withUserRepoExclusiveIO action = do
    lockPath <- userRepoLockPath
    System.FileLock.withFileLock lockPath Exclusive $ const (runExceptT action)

withUserRepoSharedIO :: IO a -> IO a
withUserRepoSharedIO action = do
    lockPath <- userRepoLockPath
    System.FileLock.withFileLock lockPath Shared (const action)

withReadRepoTransactionIO :: (ReadRepoContext -> ExceptT String IO a) -> IO (Either String a)
withReadRepoTransactionIO action = do
    cfg <- resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        branch = T.unpack $ userRepoBranch userRepo
    repoPath <- userRepoPath
    lockPath <- userRepoLockPath
    System.FileLock.withFileLock lockPath Shared $ \_ -> runExceptT $ do
        ctx <- repoHeadContext repoPath branch
        action ctx
data RepoAccess = ReadOnly | ReadWrite deriving (Eq, Show)

runGit :: [String] -> IO (ExitCode, String, String)
runGit args = do
    repoPath <- userRepoPath
    readCreateProcessWithExitCode (proc "git" ("-C" : repoPath : args)) ""

runGitIn :: FilePath -> [String] -> IO (ExitCode, String, String)
runGitIn path args = readCreateProcessWithExitCode (proc "git" ("-C" : path : args)) ""

runGitWithSshKey :: FilePath -> FilePath -> [String] -> IO (ExitCode, String, String)
runGitWithSshKey keyfile path args = do
    controlPath <- sshControlPath
    let sshOptions =
            [ "IdentitiesOnly=yes"
            , "StrictHostKeyChecking=accept-new"
            , "ControlMaster=auto"
            , "ControlPersist=600"
            , "ControlPath=" ++ controlPath
            ]
        sshCommand =
            unwords . map shellQuote $
                ["ssh", "-i", keyfile] ++ concatMap (\option -> ["-o", option]) sshOptions
    environment <- (("GIT_SSH_COMMAND", sshCommand) :) <$> getEnvironment
    readCreateProcessWithExitCode (proc "git" ("-C" : path : args)){env = Just environment} ""

sshControlPath :: IO FilePath
sshControlPath = do
    sshDir <- (</> ".ssh") <$> getHomeDirectory
    createDirectoryIfMissing True sshDir
    pid <- getProcessID
    pure $ sshDir </> ("pointy-user-repo-" ++ show pid ++ "-%C")

shellQuote :: String -> String
shellQuote value = "'" ++ concatMap escape value ++ "'"
  where
    escape '\'' = "'\\''"
    escape char = [char]

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
    putStrLn "Pruning stale git worktree metadata..."
    (exitCode, _, stderr) <- runGitIn repoPath ["worktree", "prune"]
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure code -> putStrLn $ "Warning: git worktree prune failed (exit " ++ show code ++ "): " ++ stderr

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

commitContext :: FilePath -> Text -> ExceptT String IO ReadRepoContext
commitContext repoPath hash = do
    let commit = T.unpack hash
    (exitCode, resolved, _) <- liftIO $ runGitIn repoPath ["rev-parse", "--verify", "--end-of-options", commit ++ "^{commit}"]
    when (exitCode /= ExitSuccess) $ throwError ("Commit " ++ commit ++ " is not in the local user repository.")
    pure $ ReadRepoContext repoPath (T.unpack (T.strip (T.pack resolved)))

{- | Ensure a pinned commit exists in the local bare repository. Fetch only
when the object is absent so cached project evaluations stay network-free.
-}
ensureRepoCommit :: String -> ExceptT String IO ()
ensureRepoCommit commit = do
    repoPath <- liftIO userRepoPath
    present <- liftIO $ repoContainsCommit repoPath commit
    when (not present) $ do
        fetchRepo
        presentAfterFetch <- liftIO $ repoContainsCommit repoPath commit
        when (not presentAfterFetch) $
            ExceptT $
                return $
                    Left $
                        "Git commit " ++ commit ++ " is unavailable after fetching the user repository"

repoContainsCommit :: FilePath -> String -> IO Bool
repoContainsCommit repoPath commit = do
    (exitCode, _, _) <- runGitIn repoPath ["cat-file", "-e", "--", commit ++ "^{commit}"]
    return $ exitCode == ExitSuccess

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

fetchRepoStrict :: ExceptT String IO ()
fetchRepoStrict = ExceptT $ do
    cfg <- resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        keyfile = userRepoKeyfile userRepo
        branch = T.unpack $ userRepoBranch userRepo

    repoPath <- userRepoPath
    let refspec = "+" ++ branch ++ ":" ++ branch
        action = do
            (fetchCode, fetchOut, fetchErr) <- runGitWithSshKey keyfile repoPath ["fetch", "origin", refspec]
            case fetchCode of
                ExitSuccess -> return $ Right ()
                ExitFailure code -> return $ Left $ "git fetch failed with exit code " ++ show code ++ formatGitOutput fetchOut fetchErr
    retry 3 action
  where
    formatGitOutput stdout stderr =
        (if null stdout then "" else "\nstdout:\n" ++ stdout)
            ++ (if null stderr then "" else "\nstderr:\n" ++ stderr)

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

repoHeadContext :: (MonadIO m) => FilePath -> String -> ExceptT String m ReadRepoContext
repoHeadContext repoPath branch = do
    (exitCode, revOut, err) <- liftIO (runGitIn repoPath ["rev-parse", branch])
    case exitCode of
        ExitFailure _ -> throwError ("git rev-parse failed: " ++ err)
        ExitSuccess -> pure (ReadRepoContext repoPath (filter (`notElem` ("\n\r" :: String)) revOut))

withReadRepoTransaction :: (IOE :> es) => (ReadRepoContext -> ExceptT String (Eff es) a) -> Eff es (Either String a)
withReadRepoTransaction action = do
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        branch = T.unpack $ userRepoBranch userRepo

    repoPath <- liftIO userRepoPath
    lockPath <- liftIO userRepoLockPath

    withRepoLock lockPath ReadOnly $ runExceptT $ do
        ctx <- repoHeadContext repoPath branch
        action ctx

fetchAndWarn :: String -> IO ()
fetchAndWarn context = do
    result <- runExceptT fetchRepo
    case result of
        Left err -> putStrLn $ "Warning: Failed to fetch " ++ context ++ ": " ++ err
        Right () -> return ()

withWriteRepoTransactionRaw :: (IOE :> es) => (WriteRepoContext -> ExceptT String (Eff es) a) -> Eff es (Either String a)
withWriteRepoTransactionRaw action = do
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        branch = T.unpack $ userRepoBranch userRepo

    repoPath <- liftIO userRepoPath
    lockPath <- liftIO userRepoLockPath

    withRepoLock lockPath ReadWrite $ do
        worktreePath <- liftIO $ createTempDirectory "/tmp" "pointy-worktree"
        let cleanup = liftIO $ void $ runGitIn repoPath ["worktree", "remove", "--force", worktreePath]
        res <-
            ( runExceptT $ do
                (addCode, _, addErr) <- liftIO $ runGitIn repoPath ["worktree", "add", worktreePath, branch]
                case addCode of
                    ExitFailure _ -> ExceptT $ return $ Left $ "git worktree add failed: " ++ addErr
                    ExitSuccess -> return ()

                _ <- liftIO $ runGitIn worktreePath ["config", "user.email", "backend@invalid.local"]
                _ <- liftIO $ runGitIn worktreePath ["config", "user.name", "backend"]

                action (WriteRepoContext worktreePath)
            )
                `finally` cleanup

        when (case res of Left e -> "Concurrent modification detected" `isInfixOf` e; _ -> False) $
            liftIO $ fetchAndWarn "after write transaction"

        pure res

commitAndPushChanges :: (IOE :> es) => WriteRepoContext -> String -> ExceptT String (Eff es) ()
commitAndPushChanges (WriteRepoContext worktreePath) message = ExceptT $ liftIO $ do
    cfg <- resolveConfigPath >>= loadConfig
    let keyfile = userRepoKeyfile (configUserRepo cfg)
        branch = T.unpack $ userRepoBranch (configUserRepo cfg)
    (addCode, addOut, addErr) <- runGitIn worktreePath ["add", "-A"]
    case addCode of
        ExitFailure code -> return $ Left $ formatGitFailure "git add" code addOut addErr
        ExitSuccess -> do
            (statusCode, statusOut, statusErr) <- runGitIn worktreePath ["status", "--porcelain"]
            case statusCode of
                ExitFailure code -> return $ Left $ formatGitFailure "git status" code statusOut statusErr
                ExitSuccess | null statusOut -> return $ Right ()
                ExitSuccess -> do
                    (commitCode, commitOut, commitErr) <- runGitIn worktreePath ["commit", "-m", message]
                    case commitCode of
                        ExitSuccess -> pushWithRetry worktreePath keyfile branch
                        ExitFailure code -> return $ Left $ formatGitFailure "git commit" code commitOut commitErr
  where
    formatGitFailure command code stdout stderr =
        command ++ " failed with exit code " ++ show code ++ formatGitOutput stdout stderr

    formatGitOutput stdout stderr =
        (if null stdout then "" else "\nstdout:\n" ++ stdout)
            ++ (if null stderr then "" else "\nstderr:\n" ++ stderr)

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
