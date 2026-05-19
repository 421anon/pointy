{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Git (
    AgentGitState (..),
    AgentSessionView (..),
    AgentUsage (..),
    createAgentSession,
    listAgentSessions,
    archiveAgentSession,
    purgeAgentSession,
    loadAgentSessionView,
    commitAgentTurnOutputs,
    prepareApplyCandidate,
    confirmApplyCandidate,
    discardAgentSession,
    getAgentUsage,
    sweepStaleRunningSessions,
) where

import Agent.Session (
    AgentSession (..),
    AgentTurn (..),
    PreparedApply (..),
    freshSessionLayout,
    listSessions,
    listTurns,
    loadSessionById,
    newSessionId,
    saveSession,
    sessionDir,
    touchSession,
 )
import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT (..), throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import UserRepo (fetchRepoStrict, runGitIn, runGitWithSshKey, userRepoPath)


data AgentGitState = AgentGitState
    { headCommit :: Text
    , commitLog :: Text
    , branchDiff :: Text
    , hasAgentCommits :: Bool
    }
    deriving (Show, Eq, Generic, ToJSON)


data AgentSessionView = AgentSessionView
    { session :: AgentSession
    , gitState :: AgentGitState
    , turns :: [AgentTurn]
    }
    deriving (Show, Eq, Generic, ToJSON)


data AgentUsage = AgentUsage
    { totalSessions :: Int
    , openSessions :: Int
    , runningSessions :: Int
    , appliedSessions :: Int
    , discardedSessions :: Int
    }
    deriving (Show, Eq, Generic, ToJSON)


createAgentSession :: ExceptT String IO AgentSessionView
createAgentSession = do
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    let target = userRepoBranch (configUserRepo cfg)
        targetBranchName = T.unpack target
    fetchRepoStrict
    repoPath <- liftIO userRepoPath
    base <- stripOutput <$> runGitChecked repoPath ["rev-parse", targetBranchName]
    sid <- liftIO newSessionId
    (_, worktree, _) <- liftIO $ freshSessionLayout sid
    let agentBranchName = "agent/" <> T.unpack sid
    _ <- runGitChecked repoPath ["branch", agentBranchName, T.unpack base]
    _ <- runGitChecked repoPath ["worktree", "add", worktree, agentBranchName]
    _ <- runGitChecked worktree ["config", "user.email", "agent@invalid.local"]
    _ <- runGitChecked worktree ["config", "user.name", "Pointy Agent"]
    now <- liftIO getCurrentTime
    let session_ =
            AgentSession
                { sessionId = sid
                , targetBranch = target
                , agentBranch = T.pack agentBranchName
                , baseCommit = base
                , worktreePath = worktree
                , status = "open"
                , preparedApply = Nothing
                , activeTurnId = Nothing
                , lastError = Nothing
                , createdAt = now
                , updatedAt = now
                }
    liftIO $ saveSession session_
    loadAgentSessionView sid


listAgentSessions :: ExceptT String IO [AgentSessionView]
listAgentSessions = do
    sessions <- liftIO listSessions
    let ordered = sortOn (Down . createdAt) sessions
    mapM (loadAgentSessionView . sessionId) ordered


loadAgentSessionView :: Text -> ExceptT String IO AgentSessionView
loadAgentSessionView sid = do
    session_ <- loadSessionOrThrow sid
    state <- collectGitState session_
    turns_ <- liftIO $ sortOn turnStartedAtCompat <$> listTurns sid
    return $ AgentSessionView session_ state turns_


commitAgentTurnOutputs :: AgentSession -> AgentTurn -> ExceptT String IO (Maybe Text, [Text])
commitAgentTurnOutputs session_ turn = do
    _ <- runGitChecked (worktreePath session_) ["reset", "-q", "--"]
    changedPaths <- changedWorktreePaths (worktreePath session_)
    let allowedPaths = filter isAgentOutputPath changedPaths
        skippedPaths = filter (not . isAgentOutputPath) changedPaths
    case allowedPaths of
        [] -> return (Nothing, skippedPaths)
        _ -> do
            _ <- runGitChecked (worktreePath session_) (["add", "-A", "--"] ++ map T.unpack allowedPaths)
            staged <- hasStagedChanges (worktreePath session_)
            if not staged
                then return (Nothing, skippedPaths)
                else do
                    _ <- runGitChecked (worktreePath session_) ["commit", "-m", "Agent turn " ++ T.unpack (turnId turn)]
                    head_ <- stripOutput <$> runGitChecked (worktreePath session_) ["rev-parse", "HEAD"]
                    return (Just head_, skippedPaths)


prepareApplyCandidate :: Text -> ExceptT String IO AgentSessionView
prepareApplyCandidate sid = do
    session_ <- requireEditableSession sid
    when (activeTurnId session_ /= Nothing) $ throwError "runner_active"
    state <- collectGitState session_
    unless (hasAgentCommits state) $ throwError "no_agent_commits"

    fetchRepoStrict
    repoPath <- liftIO userRepoPath
    let targetBranchName = T.unpack (targetBranch session_)
    targetHead_ <- stripOutput <$> runGitChecked repoPath ["rev-parse", targetBranchName]
    agentHead_ <- stripOutput <$> runGitChecked repoPath ["rev-parse", T.unpack (agentBranch session_)]
    sessionRoot <- liftIO $ sessionDir sid
    let applyWorktree = sessionRoot </> "apply-worktree"

    liftIO $ removeWorktreeIfExists repoPath applyWorktree
    _ <- runGitChecked repoPath ["worktree", "add", "--detach", applyWorktree, T.unpack targetHead_]
    _ <- runGitChecked applyWorktree ["config", "user.email", "backend@invalid.local"]
    _ <- runGitChecked applyWorktree ["config", "user.name", "Pointy Backend"]
    mergeResult <- liftIO $ runGitIn applyWorktree ["merge", "--squash", T.unpack (agentBranch session_)]
    case mergeResult of
        (ExitSuccess, _, _) -> do
            _ <- runGitChecked applyWorktree ["commit", "-m", "Apply agent session " ++ T.unpack sid]
            candidateHead_ <- stripOutput <$> runGitChecked applyWorktree ["rev-parse", "HEAD"]
            saveSessionUpdate
                session_
                    { status = "open"
                    , preparedApply =
                        Just
                            PreparedApply
                                { targetHead = targetHead_
                                , agentHead = agentHead_
                                , candidateHead = candidateHead_
                                , candidateWorktree = applyWorktree
                                }
                    , lastError = Nothing
                    }
            loadAgentSessionView sid
        (ExitFailure _, mergeOut, mergeErr) -> do
            conflictSummary <- collectConflictSummary applyWorktree mergeOut mergeErr
            liftIO $ removeWorktreeIfExists repoPath applyWorktree
            saveSessionUpdate session_{status = "prepare_conflict", preparedApply = Nothing, lastError = Just conflictSummary}
            loadAgentSessionView sid


confirmApplyCandidate :: Text -> Text -> Text -> ExceptT String IO AgentSessionView
confirmApplyCandidate sid requestedTarget requestedCandidate = do
    session_ <- requireEditableSession sid
    candidate <- case preparedApply session_ of
        Nothing -> throwError "candidate_missing"
        Just c -> return c
    when (targetHead candidate /= requestedTarget || candidateHead candidate /= requestedCandidate) $ throwError "candidate_mismatch"

    fetchRepoStrict
    repoPath <- liftIO userRepoPath
    cfg <- liftIO $ resolveConfigPath >>= loadConfig
    let userRepo = configUserRepo cfg
        branchName = T.unpack (targetBranch session_)
        candidateSha = T.unpack (candidateHead candidate)
    currentTarget <- stripOutput <$> runGitChecked repoPath ["rev-parse", branchName]
    when (currentTarget /= targetHead candidate) $ throwError "target_moved"

    worktreeHead <- stripOutput <$> runGitChecked (candidateWorktree candidate) ["rev-parse", "HEAD"]
    when (worktreeHead /= candidateHead candidate) $ throwError "candidate_mismatch"
    pushResult <- liftIO $ runGitWithSshKey (userRepoKeyfile userRepo) (candidateWorktree candidate) ["push", "origin", candidateSha ++ ":" ++ branchName]
    case pushResult of
        (ExitSuccess, _, _) -> do
            _ <- runGitChecked repoPath ["update-ref", "refs/heads/" ++ branchName, candidateSha]
            liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
            saveSessionUpdate session_{status = "applied", preparedApply = Nothing, lastError = Nothing}
            loadAgentSessionView sid
        (ExitFailure code, stdout, stderr) -> throwError $ "push_rejected: git push failed with exit code " ++ show code ++ formatGitOutput stdout stderr


discardAgentSession :: Text -> ExceptT String IO AgentSessionView
discardAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    repoPath <- liftIO userRepoPath
    liftIO $ removeWorktreeIfExists repoPath (worktreePath session_)
    case preparedApply session_ of
        Nothing -> return ()
        Just candidate -> liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
    _ <- liftIO $ runGitIn repoPath ["branch", "-D", T.unpack (agentBranch session_)]
    saveSessionUpdate session_{status = "discarded", activeTurnId = Nothing, preparedApply = Nothing}
    loadAgentSessionView sid


archiveAgentSession :: Text -> ExceptT String IO AgentSessionView
archiveAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    when (status session_ == "running") $ throwError "runner_active"
    saveSessionUpdate session_{status = "archived", activeTurnId = Nothing}
    loadAgentSessionView sid


purgeAgentSession :: Text -> ExceptT String IO ()
purgeAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    when (status session_ == "running") $ throwError "runner_active"
    repoPath <- liftIO userRepoPath
    liftIO $ removeWorktreeIfExists repoPath (worktreePath session_)
    case preparedApply session_ of
        Nothing -> return ()
        Just candidate -> liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
    _ <- liftIO $ runGitIn repoPath ["branch", "-D", T.unpack (agentBranch session_)]
    sessionRoot <- liftIO $ sessionDir sid
    liftIO $ removePathForcibly sessionRoot


getAgentUsage :: IO AgentUsage
getAgentUsage = do
    sessions <- listSessions
    let countStatus st = length $ filter ((== st) . status) sessions
    return
        AgentUsage
            { totalSessions = length sessions
            , openSessions = countStatus "open"
            , runningSessions = countStatus "running"
            , appliedSessions = countStatus "applied"
            , discardedSessions = countStatus "discarded"
            }


collectGitState :: AgentSession -> ExceptT String IO AgentGitState
collectGitState session_ = do
    exists <- liftIO $ doesDirectoryExist (worktreePath session_)
    if not exists
        then
            return
                AgentGitState
                    { headCommit = ""
                    , commitLog = ""
                    , branchDiff = ""
                    , hasAgentCommits = False
                    }
        else do
            head_ <- stripOutput <$> runGitChecked (worktreePath session_) ["rev-parse", "HEAD"]
            baseReachable <- baseCommitReachable (worktreePath session_) (baseCommit session_)
            ( log_, diff_, hasCommits ) <-
                if baseReachable then do
                    l <- runGitChecked (worktreePath session_) ["log", "--oneline", T.unpack (baseCommit session_) ++ "..HEAD"]
                    d <- runGitChecked (worktreePath session_) ["diff", T.unpack (baseCommit session_) ++ "..HEAD"]
                    return (l, d, not (T.null (T.strip l)))
                else
                    return
                        ( "(base commit " <> baseCommit session_ <> " is no longer reachable; cannot compute branch diff)"
                        , ""
                        , head_ /= baseCommit session_
                        )
            return
                AgentGitState
                    { headCommit = head_
                    , commitLog = log_
                    , branchDiff = diff_
                    , hasAgentCommits = hasCommits
                    }


baseCommitReachable :: FilePath -> Text -> ExceptT String IO Bool
baseCommitReachable worktree base = ExceptT $ do
    (code, _, _) <- runGitIn worktree ["cat-file", "-e", T.unpack base <> "^{commit}"]
    return $ case code of
        ExitSuccess -> Right True
        ExitFailure _ -> Right False




changedWorktreePaths :: FilePath -> ExceptT String IO [Text]
changedWorktreePaths worktree = do
    output <- runGitChecked worktree ["ls-files", "--modified", "--deleted", "--others", "--exclude-standard", "-z"]
    return $ nub $ filter (not . T.null) $ T.splitOn "\0" output


hasStagedChanges :: FilePath -> ExceptT String IO Bool
hasStagedChanges worktree = ExceptT $ do
    (exitCode, stdout, stderr) <- runGitIn worktree ["diff", "--cached", "--quiet", "--exit-code"]
    return $ case exitCode of
        ExitSuccess -> Right False
        ExitFailure 1 -> Right True
        ExitFailure code -> Left $ "git diff --cached --quiet --exit-code failed with exit code " ++ show code ++ formatGitOutput stdout stderr


isAgentOutputPath :: Text -> Bool
isAgentOutputPath path =
    case T.splitOn "/" path of
        ["projects", file] -> isNumberedNix file
        ["steps", file] -> isNumberedNix file
        "srcFiles" : stepId : rest -> isDigits stepId && not (null rest)
        _ -> False


isNumberedNix :: Text -> Bool
isNumberedNix file =
    case T.stripSuffix ".nix" file of
        Just stem -> isDigits stem
        Nothing -> False


isDigits :: Text -> Bool
isDigits text_ = not (T.null text_) && T.all isDigit text_


requireEditableSession :: Text -> ExceptT String IO AgentSession
requireEditableSession sid = do
    session_ <- loadSessionOrThrow sid
    when (status session_ == "applied") $ throwError "session_applied"
    when (status session_ == "discarded") $ throwError "session_discarded"
    when (status session_ == "archived") $ throwError "session_archived"
    return session_


loadSessionOrThrow :: Text -> ExceptT String IO AgentSession
loadSessionOrThrow sid = ExceptT $ loadSessionById sid




saveSessionUpdate :: AgentSession -> ExceptT String IO ()
saveSessionUpdate session_ = do
    touched <- liftIO $ touchSession session_
    liftIO $ saveSession touched


collectConflictSummary :: FilePath -> String -> String -> ExceptT String IO Text
collectConflictSummary worktree mergeOut mergeErr = do
    statusOut <- runGitChecked worktree ["status", "--porcelain"]
    return $ T.pack mergeErr <> T.pack mergeOut <> "\n" <> statusOut


runGitChecked :: FilePath -> [String] -> ExceptT String IO Text
runGitChecked path args = ExceptT $ do
    (exitCode, stdout, stderr) <- runGitIn path args
    return $ case exitCode of
        ExitSuccess -> Right $ T.pack stdout
        ExitFailure code -> Left $ "git " ++ unwords args ++ " failed with exit code " ++ show code ++ formatGitOutput stdout stderr


stripOutput :: Text -> Text
stripOutput = T.strip


formatGitOutput :: String -> String -> String
formatGitOutput stdout stderr =
    (if null stdout then "" else "\nstdout:\n" ++ stdout)
        ++ (if null stderr then "" else "\nstderr:\n" ++ stderr)


removeWorktreeIfExists :: FilePath -> FilePath -> IO ()
removeWorktreeIfExists repoPath path = do
    exists <- doesDirectoryExist path
    when exists $ do
        _ <- runGitIn repoPath ["worktree", "remove", "--force", path]
        stillExists <- doesDirectoryExist path
        when stillExists $ removePathForcibly path


turnStartedAtCompat :: AgentTurn -> String
turnStartedAtCompat = show . turnStartedAt



{- | Reset sessions left mid-run when the backend exited. Called once at startup;
the new process has no live runner attached, so the only honest status is "open"
(or whatever non-running status was set before). Active turn IDs are cleared.
-}
sweepStaleRunningSessions :: IO ()
sweepStaleRunningSessions = do
    sessions <- listSessions
    mapM_ resetIfRunning sessions
  where
    resetIfRunning session_
        | status session_ == "running" = do
            now <- getCurrentTime
            saveSession
                session_
                    { status = "open"
                    , activeTurnId = Nothing
                    , lastError = Just "runner exited while backend was offline"
                    , updatedAt = now
                    }
        | otherwise =
            case activeTurnId session_ of
                Just _ -> do
                    now <- getCurrentTime
                    saveSession session_{activeTurnId = Nothing, updatedAt = now}
                Nothing -> return ()