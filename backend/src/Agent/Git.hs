{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Git (
    AgentGitState (..),
    AgentSessionView (..),
    AgentUsage (..),
    createAgentSession,
    listAgentSessions,
    archiveAgentSession,
    purgeAgentSession,
    renameAgentSession,
    nameUnnamedAgentSession,
    loadAgentSessionView,
    sessionHasActiveRunner,
    commitAgentTurnOutputs,
    refreshSessionBase,
    applyAgentChanges,
    discardStaleApplyConflict,
    finalizeApplyResolution,
    getAgentUsage,
    sweepStaleRunningSessions,
) where

import Agent.Policy (appliedProjectId, appliedStepId, isAgentOutputPath)
import Agent.Session (
    AgentSession (..),
    AgentSessionSummary (AgentSessionSummary),
    AgentTurn (..),
    PreparedApply (..),
    applyConflictsPending,
    forgetSessionTurns,
    freshSessionLayout,
    inferTurnExitCode,
    latestUnfinishedTurn,
    listSessions,
    listTurns,
    listTurnsWithLogs,
    loadSessionById,
    newSessionId,
    newTurnId,
    normalizeSessionName,
    saveSession,
    saveTurn,
    sessionDir,
    touchSession,
    turnIsUnfinished,
    turnLogFilePath,
    turnLogHasFinalizationFailure,
 )
import Config (Config (..), UserRepoConfig (..), loadConfig, resolveConfigPath)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Control.Monad (filterM, mfilter, unless, void, when)
import Control.Monad.Except (ExceptT (..), catchError, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Char (isControl)
import Data.List (intercalate, nub, sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Ord (Down (..))
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Certificates (checkRevision)
import Handlers.StepReview (readableStepReviews)
import Handlers.Statuses (broadcastProjectStatus, broadcastStatusForStepProjects)
import Interpreters.Production (runProduction)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getModificationTime, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import UserRepo (ReadRepoContext (..), fetchRepoStrict, runGitIn, runGitWithSshKey, userRepoPath)

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

createAgentSession :: ExceptT String IO Text
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
                , agentCurrentProjectId = Nothing
                }
    liftIO $ saveSession session_
    return sid

listAgentSessions :: ExceptT String IO [AgentSessionSummary]
listAgentSessions =
    mapM loadAgentSessionSummary . sortOn (Down . createdAt) =<< liftIO listSessions

loadAgentSessionSummary :: AgentSession -> ExceptT String IO AgentSessionSummary
loadAgentSessionSummary session_ = do
    turns_ <- liftIO $ sortOn turnStartedAtCompat <$> listTurns (sessionId session_)
    AgentSessionSummary (deriveSessionRuntime session_ turns_) (sessionDisplayTitle session_ turns_) (length turns_)
        <$> sessionHasAgentCommits session_

sessionDisplayTitle :: AgentSession -> [AgentTurn] -> Text
sessionDisplayTitle session_ turns_ =
    fromMaybe "" $ (normalizeSessionName =<< sessionName session_) <|> promptTitle turns_

promptTitle :: [AgentTurn] -> Maybe Text
promptTitle =
    listToMaybe . mapMaybe (normalizeSessionName . turnPrompt)

sessionHasAgentCommits :: AgentSession -> ExceptT String IO Bool
sessionHasAgentCommits session_ = do
    usable <- liftIO $ isWorktreeCheckout worktree
    if usable
        then commitsBeyondBase =<< baseCommitReachable worktree (baseCommit session_)
        else return False
  where
    worktree = worktreePath session_
    commitsBeyondBase reachable
        | reachable = not . T.null . T.strip <$> runGitChecked worktree ["rev-list", "--max-count=1", commitRange session_]
        | otherwise = (/= baseCommit session_) . stripOutput <$> runGitChecked worktree ["rev-parse", "HEAD"]

loadAgentSessionView :: Text -> ExceptT String IO AgentSessionView
loadAgentSessionView sid = do
    session_ <- loadSessionOrThrow sid
    state <- collectGitState session_
    turns_ <- liftIO $ loadRepairedSessionTurns session_
    return $ AgentSessionView (deriveSessionRuntime session_ turns_) state turns_

renameAgentSession :: Text -> Text -> ExceptT String IO Text
renameAgentSession sid rawName = do
    session_ <- loadSessionOrThrow sid
    case normalizeSessionName rawName of
        Nothing -> throwError "empty_session_name"
        Just name -> do
            saveSessionUpdate session_{sessionName = Just name}
            return sid

nameUnnamedAgentSession :: Text -> Text -> ExceptT String IO ()
nameUnnamedAgentSession sid title = do
    session_ <- loadSessionOrThrow sid
    case (sessionName session_ >>= normalizeSessionName, normalizeSessionName title) of
        (Nothing, Just name) -> saveSessionUpdate session_{sessionName = Just name}
        _ -> return ()

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
                    _ <- runGitChecked (worktreePath session_) ["commit", "-m", turnCommitSubject session_ turn, "-m", "Turn " ++ T.unpack (turnId turn)]
                    head_ <- stripOutput <$> runGitChecked (worktreePath session_) ["rev-parse", "HEAD"]
                    return (Just head_, skippedPaths)

commitSubjectMaxLength :: Int
commitSubjectMaxLength = 72

turnCommitSubject :: AgentSession -> AgentTurn -> String
turnCommitSubject session_ turn =
    agentCommitSubject "Agent" session_ (Just (turnPrompt turn))

applyCommitSubject :: AgentSession -> String
applyCommitSubject session_ =
    agentCommitSubject "Apply" session_ (sessionName session_)

agentCommitSubject :: Text -> AgentSession -> Maybe Text -> String
agentCommitSubject kind session_ rawSummary =
    T.unpack $ maybe base withSummary (rawSummary >>= normalizeCommitSummary)
  where
    base = kind <> " " <> sessionId session_
    prefix = base <> ": "
    withSummary summary =
        prefix <> truncateCommitSummary (commitSubjectMaxLength - T.length prefix) summary

normalizeCommitSummary :: Text -> Maybe Text
normalizeCommitSummary =
    normalizeSessionName . T.map (\char -> if isControl char then ' ' else char)

truncateCommitSummary :: Int -> Text -> Text
truncateCommitSummary available summary
    | T.length summary <= available = summary
    | available <= 1 = T.take (max 0 available) "…"
    | otherwise =
        let window = T.take available summary
            (wholeWords, _) = T.breakOnEnd " " window
            shortened =
                if T.null wholeWords
                    then T.take (available - 1) summary
                    else T.stripEnd wholeWords
         in shortened <> "…"

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

applyAgentChanges :: (Text -> IO ()) -> Text -> ExceptT String IO ()
applyAgentChanges announce sid =
    applyPendingChanges announce sid `catchError` \err -> do
        session_ <- loadSessionOrThrow sid
        saveSessionUpdate session_{lastError = Just (T.pack err)}
        throwError err

applyPendingChanges :: (Text -> IO ()) -> Text -> ExceptT String IO ()
applyPendingChanges announce sid = do
    session_ <- requireEditableSession sid
    branchState <- collectGitState session_
    when (hasAgentCommits branchState) $ do
        unless (T.null (T.strip (branchDiff branchState))) $ liftIO $ announce (branchDiff branchState)
        fetchRepoStrict
        repoPath <- liftIO userRepoPath
        targetHead_ <- stripOutput <$> runGitChecked repoPath ["rev-parse", T.unpack (targetBranch session_)]
        agentHead_ <- stripOutput <$> runGitChecked repoPath ["rev-parse", T.unpack (agentBranch session_)]
        current <- maybe (return False) (candidateCurrent targetHead_ agentHead_) (preparedApply session_)
        prepared <-
            if current
                then return (preparedApply session_)
                else mergeCandidate repoPath session_ targetHead_ agentHead_
        mapM_ (pushCandidate repoPath session_) (mfilter (not . applyConflictsPending) prepared)

candidateCurrent :: Text -> Text -> PreparedApply -> ExceptT String IO Bool
candidateCurrent targetHead_ agentHead_ candidate
    | targetHead candidate /= targetHead_ || agentHead candidate /= agentHead_ = return False
    | otherwise = do
        worktreeExists <- liftIO $ doesDirectoryExist (candidateWorktree candidate)
        if not worktreeExists
            then return False
            else
                if applyConflictsPending candidate
                    then return True
                    else (== candidateHead candidate) . stripOutput <$> runGitChecked (candidateWorktree candidate) ["rev-parse", "HEAD"]

mergeCandidate :: FilePath -> AgentSession -> Text -> Text -> ExceptT String IO (Maybe PreparedApply)
mergeCandidate repoPath session_ targetHead_ agentHead_ = do
    sessionRoot <- liftIO $ sessionDir (sessionId session_)
    let applyWorktree = sessionRoot </> "apply-worktree"
        candidate = PreparedApply{targetHead = targetHead_, agentHead = agentHead_, candidateHead = "", candidateWorktree = applyWorktree}
    liftIO $ removeWorktreeIfExists repoPath applyWorktree
    _ <- runGitChecked repoPath ["worktree", "add", "--detach", applyWorktree, T.unpack targetHead_]
    _ <- runGitChecked applyWorktree ["config", "user.email", "agent@invalid.local"]
    _ <- runGitChecked applyWorktree ["config", "user.name", "agent"]
    mergeResult <- liftIO $ runGitIn applyWorktree ["merge", "--squash", T.unpack (agentBranch session_)]
    case mergeResult of
        (ExitFailure _, mergeOut, mergeErr) -> do
            conflictSummary <- collectConflictSummary applyWorktree mergeOut mergeErr
            saveSessionUpdate session_{status = "prepare_conflict", preparedApply = Just candidate, lastError = Just conflictSummary}
            return (Just candidate)
        (ExitSuccess, _, _) -> do
            staged <- hasStagedChanges applyWorktree
            if staged
                then do
                    _ <- runGitChecked applyWorktree ["commit", "-m", applyCommitSubject session_]
                    candidateHead_ <- stripOutput <$> runGitChecked applyWorktree ["rev-parse", "HEAD"]
                    let committed = candidate{candidateHead = candidateHead_}
                    saveSessionUpdate session_{status = "open", preparedApply = Just committed, lastError = Nothing}
                    return (Just committed)
                else do
                    liftIO $ removeWorktreeIfExists repoPath applyWorktree
                    resetWorktree (worktreePath session_) targetHead_
                    saveSessionUpdate session_{status = "open", baseCommit = targetHead_, preparedApply = Nothing, lastError = Nothing}
                    return Nothing

verifyCandidate :: FilePath -> PreparedApply -> [Int] -> IO (Either String ())
verifyCandidate repoPath candidate changedSteps =
    runProduction $ runExceptT $ checkRevision (atCommit (targetHead candidate)) (atCommit (candidateHead candidate)) changedSteps
  where
    atCommit commit = ReadRepoContext repoPath (T.unpack commit)

rejectCandidate :: FilePath -> AgentSession -> PreparedApply -> String -> ExceptT String IO ()
rejectCandidate repoPath session_ candidate failures = do
    liftIO $ do
        putStrLn $
            "Agent apply refused for session "
                ++ T.unpack (sessionId session_)
                ++ ": changeset "
                ++ T.unpack (shortCommit (candidateHead candidate))
                ++ " onto "
                ++ T.unpack (targetBranch session_)
                ++ " at "
                ++ T.unpack (shortCommit (targetHead candidate))
                ++ " introduces evaluation failures: "
                ++ intercalate "; " (lines failures)
        removeWorktreeIfExists repoPath (candidateWorktree candidate)
    saveSessionUpdate session_{status = "evaluation_failed", preparedApply = Nothing, lastError = Just (T.pack failures)}

discardStaleApplyConflict :: AgentSession -> ExceptT String IO AgentSession
discardStaleApplyConflict session_ =
    case preparedApply session_ of
        Just candidate
            | applyConflictsPending candidate -> do
                repoPath <- liftIO userRepoPath
                worktreeExists <- liftIO $ doesDirectoryExist (candidateWorktree candidate)
                currentAgentHead <- stripOutput <$> runGitChecked repoPath ["rev-parse", T.unpack (agentBranch session_)]
                if worktreeExists && currentAgentHead == agentHead candidate
                    then return session_
                    else do
                        liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
                        return session_{status = "open", preparedApply = Nothing}
        _ -> return session_

finalizeApplyResolution :: AgentSession -> ExceptT String IO (AgentSession, Maybe Text)
finalizeApplyResolution session_ = do
    current <- discardStaleApplyConflict session_
    case preparedApply current of
        Just candidate
            | applyConflictsPending candidate -> do
                let applyWorktree = candidateWorktree candidate
                unmerged <- worktreeUnmergedPaths applyWorktree
                markers <- liftIO $ filterM (fileHasConflictMarkers applyWorktree) unmerged
                if not (null markers)
                    then do
                        conflictSummary <- collectConflictSummary applyWorktree "" ""
                        return (current{status = "prepare_conflict", lastError = Just conflictSummary}, Nothing)
                    else do
                        edited <- filter isAgentOutputPath <$> changedWorktreePaths applyWorktree
                        let resolvedPaths = nub (unmerged ++ edited)
                        unless (null resolvedPaths) $
                            void $
                                runGitChecked applyWorktree (["add", "-A", "--"] ++ map T.unpack resolvedPaths)
                        staged <- hasStagedChanges applyWorktree
                        when staged $
                            void $
                                runGitChecked applyWorktree ["commit", "-m", applyCommitSubject current]
                        candidateHead_ <- stripOutput <$> runGitChecked applyWorktree ["rev-parse", "HEAD"]
                        return
                            ( current{status = "open", preparedApply = Just candidate{candidateHead = candidateHead_}}
                            , Just candidateHead_
                            )
        _ -> return (current, Nothing)

worktreeUnmergedPaths :: FilePath -> ExceptT String IO [Text]
worktreeUnmergedPaths worktree = do
    output <- runGitChecked worktree ["diff", "--name-only", "--diff-filter=U"]
    return $ nub $ filter (not . T.null) $ T.lines output

fileHasConflictMarkers :: FilePath -> Text -> IO Bool
fileHasConflictMarkers worktree path = do
    let fullPath = worktree </> T.unpack path
    exists <- doesFileExist fullPath
    if not exists
        then return False
        else do
            content <- BS.readFile fullPath
            return $ any (`BS.isInfixOf` content) conflictMarkerBytes
  where
    conflictMarkerBytes = ["<<<<<<<", "=======", ">>>>>>>"] :: [BS.ByteString]

pushCandidate :: FilePath -> AgentSession -> PreparedApply -> ExceptT String IO ()
pushCandidate repoPath session_ candidate = do
    changedPaths <- T.lines <$> runGitChecked repoPath ["diff", "--name-only", changesetRange]
    let stepIds = nub (mapMaybe appliedStepId changedPaths)
    verdict <- liftIO $ verifyCandidate repoPath candidate stepIds
    case verdict of
        Left failures -> rejectCandidate repoPath session_ candidate failures
        Right () -> do
            reviews <- ExceptT $ runProduction $ runExceptT $ readableStepReviews (ReadRepoContext repoPath (T.unpack (targetHead candidate))) stepIds
            let reviewedSteps = Map.keys (Map.filter isJust reviews)
            unless (null reviewedSteps) $ throwError (reviewedStepsError reviewedSteps)
            cfg <- liftIO $ resolveConfigPath >>= loadConfig
            changesetDiff <- runGitChecked repoPath ["diff", changesetRange]
            pushResult <- liftIO $ runGitWithSshKey (userRepoKeyfile (configUserRepo cfg)) (candidateWorktree candidate) ["push", "origin", candidateSha ++ ":" ++ branchName]
            case pushResult of
                (ExitFailure code, stdout, stderr) ->
                    throwError $ "git push failed with exit code " ++ show code ++ formatGitOutput stdout stderr
                (ExitSuccess, _, _) -> do
                    _ <- runGitChecked repoPath ["update-ref", "refs/heads/" ++ branchName, candidateSha]
                    resetWorktree (worktreePath session_) (candidateHead candidate)
                    liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
                    liftIO $ void $ forkIO $ broadcastAppliedStatuses (candidateHead candidate) (nub (mapMaybe appliedProjectId changedPaths)) stepIds
                    appendAppliedTurn
                        (sessionId session_)
                        ("Applied to `" <> targetBranch session_ <> "` at " <> shortCommit (candidateHead candidate) <> ".")
                        changesetDiff
                    saveSessionUpdate
                        session_
                            { status = "open"
                            , baseCommit = candidateHead candidate
                            , preparedApply = Nothing
                            , lastError = Nothing
                            }
  where
    branchName = T.unpack (targetBranch session_)
    candidateSha = T.unpack (candidateHead candidate)
    changesetRange = T.unpack (targetHead candidate) ++ ".." ++ candidateSha

reviewedStepsError :: [Int] -> String
reviewedStepsError stepIds =
    "Reviewed steps cannot be changed: " ++ intercalate ", " (map (("step " ++) . show) stepIds) ++ "."

resetWorktree :: FilePath -> Text -> ExceptT String IO ()
resetWorktree worktree commit =
    mapM_ (runGitChecked worktree) [["clean", "-fd"], ["reset", "--hard", T.unpack commit], ["clean", "-fd"]]

appendAppliedTurn :: Text -> Text -> Text -> ExceptT String IO ()
appendAppliedTurn sid body changesetDiff = do
    tid <- liftIO newTurnId
    logPath <- liftIO $ turnLogFilePath sid tid
    now <- liftIO getCurrentTime
    let turn =
            AgentTurn
                { turnId = tid
                , turnSessionId = sid
                , turnPrompt = "Apply proposed changeset"
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

archiveAgentSession :: Text -> ExceptT String IO ()
archiveAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    hasRunner <- sessionHasActiveRunner session_
    when hasRunner $ throwError "runner_active"
    saveSessionUpdate session_{status = "archived", activeTurnId = Nothing}
    return ()

purgeAgentSession :: Text -> ExceptT String IO ()
purgeAgentSession sid = do
    session_ <- loadSessionOrThrow sid
    hasRunner <- sessionHasActiveRunner session_
    when hasRunner $ throwError "runner_active"
    repoPath <- liftIO userRepoPath
    liftIO $ removeWorktreeIfExists repoPath (worktreePath session_)
    case preparedApply session_ of
        Nothing -> return ()
        Just candidate -> liftIO $ removeWorktreeIfExists repoPath (candidateWorktree candidate)
    _ <- liftIO $ runGitIn repoPath ["branch", "-D", T.unpack (agentBranch session_)]
    sessionRoot <- liftIO $ sessionDir sid
    liftIO $ removePathForcibly sessionRoot
    liftIO $ forgetSessionTurns sid

getAgentUsage :: IO AgentUsage
getAgentUsage = do
    sessions <- listSessions
    viewSessions <- mapM deriveUsageSession sessions
    let countStatus st = length $ filter ((== st) . status) viewSessions
    return
        AgentUsage
            { totalSessions = length viewSessions
            , openSessions = countStatus "open"
            , runningSessions = countStatus "running"
            , appliedSessions = countStatus "applied"
            , discardedSessions = countStatus "discarded"
            }
  where
    deriveUsageSession session_ = do
        turns_ <- loadRepairedSessionTurns session_
        return $ deriveSessionRuntime session_ turns_

collectGitState :: AgentSession -> ExceptT String IO AgentGitState
collectGitState session_ = do
    usable <- liftIO $ isWorktreeCheckout (worktreePath session_)
    if not usable
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
                        l <- runGitChecked (worktreePath session_) ["log", "--oneline", commitRange session_]
                        d <- runGitChecked (worktreePath session_) ["diff", commitRange session_]
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


commitRange :: AgentSession -> String
commitRange =
    (++ "..HEAD") . T.unpack . baseCommit


isWorktreeCheckout :: FilePath -> IO Bool
isWorktreeCheckout path = do
    (exitCode, out, _) <- runGitIn path ["rev-parse", "--is-inside-work-tree"]
    return $ exitCode == ExitSuccess && T.strip (T.pack out) == "true"


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

broadcastAppliedStatuses :: Text -> [Int] -> [Int] -> IO ()
broadcastAppliedStatuses commit projectIds stepIds = do
    mapM_ (\pid -> runProduction $ broadcastProjectStatus pid commit Nothing) projectIds
    mapM_ (\sid -> runProduction $ broadcastStatusForStepProjects sid commit Nothing) stepIds

sessionHasActiveRunner :: AgentSession -> ExceptT String IO Bool
sessionHasActiveRunner session_ = do
    turns_ <- liftIO $ loadRepairedSessionTurns session_
    return $ isJust (latestUnfinishedTurn turns_)

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

loadRepairedSessionTurns :: AgentSession -> IO [AgentTurn]
loadRepairedSessionTurns session_ = do
    loadedTurns <- sortOn turnStartedAtCompat <$> listTurnsWithLogs (sessionId session_)
    mapM (repairLoggedTerminalTurn session_) loadedTurns

deriveSessionRuntime :: AgentSession -> [AgentTurn] -> AgentSession
deriveSessionRuntime session_ turns_ =
    let baseSession =
            session_
                { status =
                    if status session_ == "running"
                        then "open"
                        else status session_
                , activeTurnId = Nothing
                }
     in case latestUnfinishedTurn turns_ of
            Just turn
                | sessionAllowsRunner baseSession ->
                    baseSession{status = "running", activeTurnId = Just (turnId turn)}
            _ -> baseSession

sessionAllowsRunner :: AgentSession -> Bool
sessionAllowsRunner session_ =
    status session_ `notElem` ["applied", "discarded", "archived"]

repairLoggedTerminalTurn :: AgentSession -> AgentTurn -> IO AgentTurn
repairLoggedTerminalTurn session_ turn
    | not (turnIsUnfinished turn) = return turn
    | not (turnLogHasFinalizationFailure (turnLog turn)) = return turn
    | otherwise =
        case inferTurnExitCode (turnLog turn) of
            Nothing -> return turn
            Just exitCode -> finalizeTurnWithExitCode session_ turn exitCode

finalizeTurnWithExitCode :: AgentSession -> AgentTurn -> Int -> IO AgentTurn
finalizeTurnWithExitCode session_ turn exitCode = do
    finishedAt <- turnTerminalTime session_ turn
    let finalStatus =
            if exitCode == 0
                then "succeeded"
                else "failed"
        repaired =
            turn
                { turnStatus = finalStatus
                , turnExitCode = Just exitCode
                , turnFinishedAt = Just finishedAt
                }
    saveTurnBestEffort repaired
    return repaired

saveTurnBestEffort :: AgentTurn -> IO ()
saveTurnBestEffort turn =
    void (try (saveTurn turn) :: IO (Either IOException ()))

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

sweepStaleRunningSessions :: IO ()
sweepStaleRunningSessions = do
    sessions <- listSessions
    mapM_ resetStaleSession sessions
  where
    resetStaleSession session_ = do
        turns_ <- listTurnsWithLogs (sessionId session_)
        let unfinishedTurns = filter turnIsUnfinished turns_
        mapM_ (repairStaleUnfinishedTurn session_) unfinishedTurns
        now <- getCurrentTime
        let hadPersistedRunner = status session_ == "running" || activeTurnId session_ /= Nothing
            staleFailure = any ((/= Just 0) . inferTurnExitCode . turnLog) unfinishedTurns
            shouldSave = hadPersistedRunner || staleFailure
            nextStatus =
                if status session_ == "running"
                    then "open"
                    else status session_
            nextError =
                if staleFailure
                    then Just "runner exited while backend was offline"
                    else lastError session_
        when shouldSave $
            saveSession
                session_
                    { status = nextStatus
                    , activeTurnId = Nothing
                    , lastError = nextError
                    , updatedAt = now
                    }

    repairStaleUnfinishedTurn session_ turn =
        let exitCode = maybe (-1) id (inferTurnExitCode (turnLog turn))
         in void $ finalizeTurnWithExitCode session_ turn exitCode

turnTerminalTime :: AgentSession -> AgentTurn -> IO UTCTime
turnTerminalTime session_ turn = do
    result <- try (getModificationTime (turnLogPath turn)) :: IO (Either IOException UTCTime)
    return $ case result of
        Right modified -> modified
        Left _ -> updatedAt session_
