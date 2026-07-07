{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Git (
    AgentApplyView (..),
    AgentGitState (..),
    AgentSessionView (..),
    AgentUsage (..),
    createAgentSession,
    listAgentSessions,
    archiveAgentSession,
    purgeAgentSession,
    renameAgentSession,
    loadAgentSessionView,
    commitAgentTurnOutputs,
    refreshSessionBase,
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
    listTurnsWithLogs,
    loadSessionById,
    newSessionId,
    newTurnId,
    normalizeSessionName,
    saveSession,
    saveTurn,
    sessionDir,
    touchSession,
    turnLogFilePath,
 )
import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT (..), catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.List (nub, sortOn)
import Data.Maybe (isJust, mapMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Handlers.Statuses (broadcastProjectStatus, broadcastStatusForStepProjects)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getModificationTime, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
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

data AgentApplyView = AgentApplyView
    { sessionView :: AgentSessionView
    , invalidatedProjectIds :: [Int]
    , invalidatedStepIds :: [Int]
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
    _ <- runGitChecked worktree ["config", "user.name", "agent"]
    now <- liftIO getCurrentTime
    let session_ =
            AgentSession
                { sessionId = sid
                , sessionName = Nothing
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
    loadedTurns <- liftIO $ sortOn turnStartedAtCompat <$> listTurnsWithLogs sid
    turns_ <- liftIO $ mapM (repairInactiveRunningTurn session_) loadedTurns
    return $ AgentSessionView session_ state turns_

renameAgentSession :: Text -> Text -> ExceptT String IO AgentSessionView
renameAgentSession sid rawName = do
    session_ <- loadSessionOrThrow sid
    case normalizeSessionName rawName of
        Nothing -> throwError "empty_session_name"
        Just name -> do
            saveSessionUpdate session_{sessionName = Just name}
            loadAgentSessionView sid

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

refreshSessionBase :: AgentSession -> ExceptT String IO (AgentSession, [Text])
refreshSessionBase session_ = do
    fetchNotes <-
        (fetchRepoStrict >> pure [])
            `catchError` \err ->
                pure ["Warning: could not fetch latest repo state: " <> T.pack err]
    repoPath <- liftIO userRepoPath
    latest <- stripOutput <$> runGitChecked repoPath ["rev-parse", T.unpack (targetBranch session_)]
    worktreeExists <- liftIO $ doesDirectoryExist (worktreePath session_)
    if latest == baseCommit session_ || not worktreeExists
        then return (session_, fetchNotes)
        else do
            (updated, syncNote) <- syncWorktreeToTarget session_ latest
            return (updated, fetchNotes ++ [syncNote])

syncWorktreeToTarget :: AgentSession -> Text -> ExceptT String IO (AgentSession, Text)
syncWorktreeToTarget session_ latest = do
    head_ <- stripOutput <$> runGitChecked (worktreePath session_) ["rev-parse", "HEAD"]
    if head_ == baseCommit session_
        then do
            -- No agent commits yet: stray uncommitted files are disposable
            -- (same policy as confirmApplyCandidate), so jump straight to latest.
            _ <- runGitChecked (worktreePath session_) ["clean", "-fd"]
            _ <- runGitChecked (worktreePath session_) ["reset", "--hard", T.unpack latest]
            advanceBase $ "Updated session to latest `" <> targetBranch session_ <> "` state (" <> shortCommit latest <> ")"
        else do
            mergeResult <-
                liftIO $
                    runGitIn
                        (worktreePath session_)
                        ["merge", "-m", "Merge latest " ++ T.unpack (targetBranch session_) ++ " into agent session", T.unpack latest]
            case mergeResult of
                (ExitSuccess, _, _) ->
                    advanceBase $ "Merged latest `" <> targetBranch session_ <> "` state (" <> shortCommit latest <> ") into session"
                (ExitFailure _, mergeOut, mergeErr) -> do
                    _ <- liftIO $ runGitIn (worktreePath session_) ["merge", "--abort"]
                    return
                        ( session_
                        , "Warning: could not merge latest `"
                            <> targetBranch session_
                            <> "` ("
                            <> shortCommit latest
                            <> ") into session; continuing from "
                            <> shortCommit (baseCommit session_)
                            <> "."
                            <> T.pack (formatGitOutput mergeOut mergeErr)
                        )
  where
    advanceBase note = do
        let refreshed = session_{baseCommit = latest}
        saveSessionUpdate refreshed
        return (refreshed, note)

prepareApplyCandidate :: Text -> ExceptT String IO AgentSessionView
prepareApplyCandidate sid = do
    session_ <- requireEditableSession sid
    when (sessionHasActiveRunner session_) $ throwError "runner_active"
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
    _ <- runGitChecked applyWorktree ["config", "user.email", "agent@invalid.local"]
    _ <- runGitChecked applyWorktree ["config", "user.name", "agent"]
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

confirmApplyCandidate :: Text -> Text -> Text -> ExceptT String IO AgentApplyView
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
    changesetDiff <- runGitChecked (candidateWorktree candidate) ["diff", T.unpack (targetHead candidate) ++ ".." ++ candidateSha]
    pushResult <- liftIO $ runGitWithSshKey (userRepoKeyfile userRepo) (candidateWorktree candidate) ["push", "origin", candidateSha ++ ":" ++ branchName]
    case pushResult of
        (ExitSuccess, _, _) -> do
            _ <- runGitChecked repoPath ["update-ref", "refs/heads/" ++ branchName, candidateSha]
            _ <- runGitChecked (worktreePath session_) ["clean", "-fd"]
            _ <- runGitChecked (worktreePath session_) ["reset", "--hard", candidateSha]
            _ <- runGitChecked (worktreePath session_) ["clean", "-fd"]
            liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
            changedPaths <- T.lines <$> runGitChecked repoPath ["diff", "--name-only", T.unpack (targetHead candidate) ++ ".." ++ candidateSha]
            let projectIds = nub (mapMaybe appliedProjectId changedPaths)
                stepIds = nub (mapMaybe appliedStepId changedPaths)
            liftIO $ void $ forkIO $ broadcastAppliedStatuses (candidateHead candidate) projectIds stepIds
            appendLifecycleTurn
                sid
                "Apply proposed changeset"
                ("Applied changes to `" <> targetBranch session_ <> "` at " <> shortCommit (candidateHead candidate) <> ". You can continue from the applied state in this chat.")
                changesetDiff
            saveSessionUpdate
                session_
                    { status = "open"
                    , baseCommit = candidateHead candidate
                    , preparedApply = Nothing
                    , lastError = Nothing
                    }
            view_ <- loadAgentSessionView sid
            return AgentApplyView{sessionView = view_, invalidatedProjectIds = projectIds, invalidatedStepIds = stepIds}
        (ExitFailure code, stdout, stderr) -> throwError $ "push_rejected: git push failed with exit code " ++ show code ++ formatGitOutput stdout stderr

discardAgentSession :: Text -> ExceptT String IO AgentSessionView
discardAgentSession sid = do
    session_ <- requireEditableSession sid
    when (sessionHasActiveRunner session_) $ throwError "runner_active"
    changesetDiff <- branchDiff <$> collectGitState session_
    repoPath <- liftIO userRepoPath
    latest <- stripOutput <$> runGitChecked repoPath ["rev-parse", T.unpack (targetBranch session_)]
    _ <- runGitChecked (worktreePath session_) ["clean", "-fd"]
    _ <- runGitChecked (worktreePath session_) ["reset", "--hard", T.unpack latest]
    _ <- runGitChecked (worktreePath session_) ["clean", "-fd"]
    case preparedApply session_ of
        Nothing -> return ()
        Just candidate -> liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
    appendLifecycleTurn
        sid
        "Discard proposed changeset"
        ("Discarded this draft. No changes were applied to `" <> targetBranch session_ <> "`. You can continue from a clean state in this chat.")
        changesetDiff
    saveSessionUpdate session_{status = "open", baseCommit = latest, activeTurnId = Nothing, preparedApply = Nothing, lastError = Nothing}
    loadAgentSessionView sid

appendLifecycleTurn :: Text -> Text -> Text -> Text -> ExceptT String IO ()
appendLifecycleTurn sid prompt body changesetDiff = do
    tid <- liftIO newTurnId
    logPath <- liftIO $ turnLogFilePath sid tid
    now <- liftIO getCurrentTime
    let turn =
            AgentTurn
                { turnId = tid
                , turnSessionId = sid
                , turnPrompt = prompt
                , turnStatus = "succeeded"
                , turnExitCode = Just 0
                , turnStartedAt = now
                , turnFinishedAt = Just now
                , turnLogPath = logPath
                , turnLog = ""
                }
    liftIO $ createDirectoryIfMissing True (takeDirectory logPath)
    liftIO $ TIO.writeFile logPath (renderLifecycleLog body changesetDiff)
    liftIO $ saveTurn turn

renderLifecycleLog :: Text -> Text -> Text
renderLifecycleLog body changesetDiff =
    T.unlines $ ["[stdout] " <> body, "[system] changeset-diff"] ++ T.lines changesetDiff

archiveAgentSession :: Text -> ExceptT String IO AgentSessionView
archiveAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    when (sessionHasActiveRunner session_) $ throwError "runner_active"
    saveSessionUpdate session_{status = "archived", activeTurnId = Nothing}
    loadAgentSessionView sid

purgeAgentSession :: Text -> ExceptT String IO ()
purgeAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    when (sessionHasActiveRunner session_) $ throwError "runner_active"
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
            (log_, diff_, hasCommits) <-
                if baseReachable
                    then do
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
    isJust (appliedProjectId path) || isJust (appliedStepId path)

appliedProjectId :: Text -> Maybe Int
appliedProjectId path =
    case T.splitOn "/" path of
        ["projects", file] -> numberedNixId file
        _ -> Nothing

appliedStepId :: Text -> Maybe Int
appliedStepId path =
    case T.splitOn "/" path of
        ["steps", file] -> numberedNixId file
        "srcFiles" : stepId : _ : _ -> decimalId stepId
        _ -> Nothing

numberedNixId :: Text -> Maybe Int
numberedNixId file = T.stripSuffix ".nix" file >>= decimalId

decimalId :: Text -> Maybe Int
decimalId text_ =
    case TR.decimal text_ of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing

broadcastAppliedStatuses :: Text -> [Int] -> [Int] -> IO ()
broadcastAppliedStatuses commit projectIds stepIds = do
    mapM_ (\pid -> broadcastProjectStatus pid commit Nothing) projectIds
    mapM_ (\sid -> broadcastStatusForStepProjects sid commit Nothing) stepIds

sessionHasActiveRunner :: AgentSession -> Bool
sessionHasActiveRunner session_ =
    status session_ == "running" || activeTurnId session_ /= Nothing

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

shortCommit :: Text -> Text
shortCommit = T.take 12

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
    resetIfRunning session_ = do
        -- A turn is live only while the session points at it. Anything else is
        -- stale metadata from an interrupted final write; repair it from the log.
        turns_ <- listTurnsWithLogs (sessionId session_)
        let inactiveSession = session_{activeTurnId = Nothing}
        mapM_ (repairInactiveRunningTurn inactiveSession) turns_
        now <- getCurrentTime
        -- Fix the session itself.
        if status session_ == "running"
            then
                saveSession
                    session_
                        { status = "open"
                        , activeTurnId = Nothing
                        , lastError = Just "runner exited while backend was offline"
                        , updatedAt = now
                        }
            else case activeTurnId session_ of
                Just _ ->
                    saveSession session_{activeTurnId = Nothing, updatedAt = now}
                Nothing -> return ()

repairInactiveRunningTurn :: AgentSession -> AgentTurn -> IO AgentTurn
repairInactiveRunningTurn session_ turn
    | turnStatus turn /= "running" = return turn
    | activeTurnId session_ == Just (turnId turn) = return turn
    | otherwise = do
        finishedAt <- turnTerminalTime session_ turn
        let exitCode = inferTurnExitCode (turnLog turn)
            finalCode = maybe (-1) id exitCode
            finalStatus =
                if exitCode == Just 0
                    then "succeeded"
                    else "failed"
            repaired =
                turn
                    { turnStatus = finalStatus
                    , turnExitCode = Just finalCode
                    , turnFinishedAt = Just finishedAt
                    }
        saveTurn repaired
        return repaired

turnTerminalTime :: AgentSession -> AgentTurn -> IO UTCTime
turnTerminalTime session_ turn = do
    result <- try (getModificationTime (turnLogPath turn)) :: IO (Either IOException UTCTime)
    return $ case result of
        Right modified -> modified
        Left _ -> updatedAt session_

inferTurnExitCode :: Text -> Maybe Int
inferTurnExitCode logText = go (reverse (T.lines logText))
  where
    prefix = "[system] Agent turn finished with exit code "

    go [] = Nothing
    go (line : rest) =
        case T.stripPrefix prefix line of
            Just codeText ->
                case reads (T.unpack codeText) of
                    [(code, "")] -> Just code
                    _ -> go rest
            Nothing -> go rest
