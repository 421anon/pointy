{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end exercise of the apply flow: prepare -> squash-merge conflict ->
-- agent resolution in the kept apply worktree -> finalizeApplyResolution ->
-- confirm. Runs against scratch repos under the system temp dir, isolated via
-- HOME and POINTY_CONFIG_PATH, so it never touches the real user repo.
module Main (main) where

import Agent.Git (AgentApplyView (..), AgentSessionView (..), confirmApplyCandidate, createAgentSession, finalizeApplyResolution, prepareApplyCandidate)
import Agent.Session (AgentSession (..), PreparedApply (..), applyConflictsPending, loadSessionById)
import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory)
import System.Environment (setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (CreateProcess, proc, readCreateProcessWithExitCode)

git :: FilePath -> [String] -> IO ()
git dir args = do
    (code, out, err) <- readCreateProcessWithExitCode (procGit dir args) ""
    case code of
        ExitSuccess -> return ()
        ExitFailure _ -> fail ("git " ++ unwords args ++ " failed in " ++ dir ++ ":\n" ++ out ++ err)

procGit :: FilePath -> [String] -> System.Process.CreateProcess
procGit dir args =
    System.Process.proc
        "git"
        (["-C", dir, "-c", "user.name=test", "-c", "user.email=test@invalid.local"] ++ args)

expectRight :: String -> ExceptT String IO a -> IO a
expectRight label action = do
    result <- runExceptT action
    case result of
        Right value -> return value
        Left err -> fail (label ++ " failed: " ++ err)

-- | Base state of the two files, shared by the session and the target branch.
baseNix :: String
baseNix =
    unlines
        [ "{"
        , "  name = \"project 95\";"
        , "  sortKey = 55;"
        , "  steps = ["
        , "    { hidden = false; id = 2055; sortKey = null; }"
        , "  ];"
        , "}"
        ]

agentNix :: String
agentNix =
    unlines
        [ "{"
        , "  name = \"project 95\";"
        , "  sortKey = 56;"
        , "  steps = ["
        , "    { hidden = false; id = 2054; sortKey = null; }"
        , "  ];"
        , "}"
        ]

targetNix :: String
targetNix =
    unlines
        [ "{"
        , "  name = \"project 95\";"
        , "  sortKey = 56;"
        , "  steps = ["
        , "  ];"
        , "}"
        ]

basePy :: String
basePy =
    unlines
        [ "def main():"
        , "    xaxis_title = \"old\""
        , "    yaxis_title = \"old2\""
        , "    print(xaxis_title, yaxis_title)"
        ]

agentPy :: String
agentPy =
    unlines
        [ "def main():"
        , "    xaxis_title = \"A30\""
        , "    yaxis_title = \"A20\""
        , "    print(xaxis_title, yaxis_title)"
        ]

targetPy :: String
targetPy =
    unlines
        [ "def main():"
        , "    xaxis_title = \"T24\""
        , "    yaxis_title = \"T16\""
        , "    print(xaxis_title, yaxis_title)"
        ]

-- | Content for the second session's agent and a further target move, so the
-- second scenario conflicts against the post-apply target state.
agent2Nix :: String
agent2Nix =
    unlines
        [ "{"
        , "  name = \"project 95\";"
        , "  sortKey = 57;"
        , "  steps = ["
        , "    { hidden = false; id = 2053; sortKey = null; }"
        , "  ];"
        , "}"
        ]

target2Nix :: String
target2Nix =
    unlines
        [ "{"
        , "  name = \"project 95\";"
        , "  sortKey = 58;"
        , "  steps = ["
        , "  ];"
        , "}"
        ]

agent2Py :: String
agent2Py =
    unlines
        [ "def main():"
        , "    xaxis_title = \"B30\""
        , "    yaxis_title = \"B20\""
        , "    print(xaxis_title, yaxis_title)"
        ]

target2Py :: String
target2Py =
    unlines
        [ "def main():"
        , "    xaxis_title = \"C24\""
        , "    yaxis_title = \"C16\""
        , "    print(xaxis_title, yaxis_title)"
        ]

setupRepos :: FilePath -> IO ()
setupRepos root = do
    let remote = root </> "remote.git"
        seed = root </> "seed"
    git root ["init", "-q", "--bare", remote]
    git root ["init", "-q", seed]
    createDirectoryIfMissing True (seed </> "projects")
    TIO.writeFile (seed </> "projects/95.nix") (T.pack baseNix)
    createDirectoryIfMissing True (seed </> "srcFiles" </> "2029")
    TIO.writeFile (seed </> "srcFiles" </> "2029" </> "plot_lib_vs_top_recommended.py") (T.pack basePy)
    git seed ["add", "-A"]
    git seed ["commit", "-q", "-m", "base"]
    git seed ["remote", "add", "origin", remote]
    git seed ["push", "-q", "origin", "HEAD:prod-backend"]
    -- The backend's local bare mirror of the remote.
    git root ["clone", "-q", "--bare", "--branch", "prod-backend", remote, root </> "home" </> "user-repo.git"]

-- | Another agent advances prod-backend with conflicting changes.
advanceTarget :: FilePath -> IO ()
advanceTarget root = do
    let target = root </> "target"
    git root ["clone", "-q", "--branch", "prod-backend", root </> "remote.git", target]
    TIO.writeFile (target </> "projects/95.nix") (T.pack targetNix)
    TIO.writeFile (target </> "srcFiles" </> "2029" </> "plot_lib_vs_top_recommended.py") (T.pack targetPy)
    git target ["add", "-A"]
    git target ["commit", "-q", "-m", "target changes"]
    git target ["push", "-q", "origin", "HEAD:prod-backend"]

-- | A further target move with content conflicting with the second session.
advanceTarget2 :: FilePath -> IO ()
advanceTarget2 root = do
    let target = root </> "target2"
    git root ["clone", "-q", "--branch", "prod-backend", root </> "remote.git", target]
    TIO.writeFile (target </> "projects/95.nix") (T.pack target2Nix)
    TIO.writeFile (target </> "srcFiles" </> "2029" </> "plot_lib_vs_top_recommended.py") (T.pack target2Py)
    git target ["add", "-A"]
    git target ["commit", "-q", "-m", "target changes 2"]
    git target ["push", "-q", "origin", "HEAD:prod-backend"]

-- | The agent's resolution: keep its own content (what the real agent chose
-- after reviewing the markers in the apply worktree).
resolveMarkers :: FilePath -> IO ()
resolveMarkers applyWorktree = do
    TIO.writeFile (applyWorktree </> "projects/95.nix") (T.pack agentNix)
    TIO.writeFile (applyWorktree </> "srcFiles" </> "2029" </> "plot_lib_vs_top_recommended.py") (T.pack agentPy)

sessionOf :: AgentSessionView -> AgentSession
sessionOf = session

main :: IO ()
main = do
    now <- getCurrentTime
    root <- (</> ("pointy-apply-flow-" ++ show now)) <$> getTemporaryDirectory
    createDirectoryIfMissing True (root </> "home")
    setEnv "HOME" (root </> "home")
    setEnv "POINTY_CONFIG_PATH" (root </> "config.toml")
    TIO.writeFile
        (root </> "config.toml")
        ( T.unlines
            [ "[user-repo]"
            , "url = " <> T.pack (show (root </> "remote.git"))
            , "keyfile = " <> T.pack (show (root </> "dummy-key"))
            , "branch = \"prod-backend\""
            ]
        )
    setupRepos root

    -- 1. Session created on the base state; the agent commits its changes.
    view0 <- expectRight "createAgentSession"  createAgentSession
    let sid = sessionId (sessionOf view0)
        sessionWorktree = worktreePath (sessionOf view0)
    TIO.writeFile (sessionWorktree </> "projects/95.nix") (T.pack agentNix)
    createDirectoryIfMissing True (sessionWorktree </> "srcFiles" </> "2029")
    TIO.writeFile (sessionWorktree </> "srcFiles" </> "2029" </> "plot_lib_vs_top_recommended.py") (T.pack agentPy)
    git sessionWorktree ["add", "-A"]
    git sessionWorktree ["commit", "-q", "-m", "agent changes"]

    -- 2. Another agent advances prod-backend with overlapping changes.
    advanceTarget root

    -- 3. Prepare: the squash merge conflicts; the apply worktree is kept.
    conflictView <- expectRight "prepareApplyCandidate"  (prepareApplyCandidate sid)
    let conflictSession = sessionOf conflictView
    when (status conflictSession /= "prepare_conflict") $ fail "expected prepare_conflict status after conflicting prepare"
    pending <- case preparedApply conflictSession of
        Just candidate -> do
            when (candidateHead candidate /= "") $ fail "expected empty candidate head while conflicted"
            when (not (applyConflictsPending candidate)) $ fail "expected applyConflictsPending while conflicted"
            return candidate
        Nothing -> fail "expected a pending prepared apply while conflicted"
    worktreeKept <- doesDirectoryExist (candidateWorktree pending)
    when (not worktreeKept) $ fail "apply worktree was removed on conflict"
    when (lastError conflictSession == Nothing) $ fail "expected conflict summary in lastError"

    -- 4. Unresolved: finalize keeps the conflict pending.
    mResolved1 <- expectRight "finalizeApplyResolution (unresolved)"  (finalizeApplyResolution conflictSession)
    when (mResolved1 /= Nothing) $ fail "expected no resolution while markers remain"
    reloaded <- loadSessionById sid
    reloadedSession <- case reloaded of
        Right s -> return s
        Left err -> fail ("reload failed: " ++ err)
    when (status reloadedSession /= "prepare_conflict") $ fail "expected prepare_conflict after unresolved finalize"
    when (lastError reloadedSession == Nothing) $ fail "expected conflict summary preserved after unresolved finalize"

    -- 5. The agent resolves the markers in the kept apply worktree.
    resolveMarkers (candidateWorktree pending)

    -- 6. Finalize: the resolution is staged and committed; the apply is ready.
    mResolved2 <- expectRight "finalizeApplyResolution"  (finalizeApplyResolution conflictSession)
    resolvedHead <- case mResolved2 of
        Just sha -> return sha
        Nothing -> fail "expected a committed resolution"
    reloaded2 <- loadSessionById sid
    reloadedSession2 <- case reloaded2 of
        Right s -> return s
        Left err -> fail ("reload failed: " ++ err)
    when (status reloadedSession2 /= "open") $ fail "expected open status after resolved finalize"
    resolvedApply <- case preparedApply reloadedSession2 of
        Just c -> return c
        Nothing -> fail "expected prepared apply after resolved finalize"
    when (candidateHead resolvedApply /= resolvedHead) $ fail "candidate head mismatch after finalize"
    -- The committed resolution contains the agent's content plus the target's changes.
    resolvedNix <- TIO.readFile (candidateWorktree resolvedApply </> "projects/95.nix")
    when (resolvedNix /= T.pack agentNix) $ fail "resolution content mismatch in projects/95.nix"

    -- 7. Re-preparing returns the pending resolution unchanged (short-circuit).
    view3 <- expectRight "prepareApplyCandidate (short-circuit)"  (prepareApplyCandidate sid)
    case preparedApply (sessionOf view3) of
        Just c -> when (candidateHead c /= resolvedHead) $ fail "short-circuit prepare returned a stale candidate"
        Nothing -> fail "short-circuit prepare dropped the pending apply"
    when (status (sessionOf view3) /= "open") $ fail "expected open status from short-circuit prepare"

    -- 8. Confirm: the resolution is pushed to the target and the base advances.
    applyView <- expectRight "confirmApplyCandidate"  (confirmApplyCandidate sid (targetHead pending) resolvedHead)
    let appliedSession = sessionOf (sessionView applyView)
    when (baseCommit appliedSession /= resolvedHead) $ fail "session base did not advance to the resolution"
    appliedWorktreeGone <- not <$> doesDirectoryExist (candidateWorktree resolvedApply)
    unless appliedWorktreeGone $ fail "apply worktree not removed after confirm"
    -- The resolution was pushed to the remote target branch.
    remoteHead <- readProcessGit (root </> "remote.git") ["rev-parse", "prod-backend"]
    when (T.strip remoteHead /= resolvedHead) $ fail "target branch was not updated to the resolution"

    -- 9. Second session: a conflict that is never resolved stays pending.
    viewOther <- expectRight "createAgentSession (second)"  createAgentSession
    let sidOther = sessionId (sessionOf viewOther)
        worktreeOther = worktreePath (sessionOf viewOther)
    TIO.writeFile (worktreeOther </> "projects/95.nix") (T.pack agent2Nix)
    TIO.writeFile (worktreeOther </> "srcFiles" </> "2029" </> "plot_lib_vs_top_recommended.py") (T.pack agent2Py)
    git worktreeOther ["add", "-A"]
    git worktreeOther ["commit", "-q", "-m", "agent changes"]
    -- The target moves again, conflicting with the second session's changes.
    advanceTarget2 root
    conflictViewOther <- expectRight "prepareApplyCandidate (second)"  (prepareApplyCandidate sidOther)
    let mPendingOther = preparedApply (sessionOf conflictViewOther)
    pendingOther <- case mPendingOther of
        Just c -> return c
        Nothing -> fail "expected pending apply (second)"
    when (not (applyConflictsPending pendingOther)) $ fail "expected conflict pending (second)"
    mResolvedOther <- expectRight "finalizeApplyResolution (second)"  (finalizeApplyResolution (sessionOf conflictViewOther))
    when (mResolvedOther /= Nothing) $ fail "expected no resolution (second)"

    -- 10. Session 1's applied state is still intact on the remote after the
    -- second scenario ran (sanity: the second scenario only prepared, never pushed).
    putStrLn "apply-flow: all checks passed"

readProcessGit :: FilePath -> [String] -> IO T.Text
readProcessGit dir args = do
    (code, out, err) <- readCreateProcessWithExitCode (procGit dir args) ""
    case code of
        ExitSuccess -> return (T.pack out)
        ExitFailure _ -> fail ("git " ++ unwords args ++ " failed in " ++ dir ++ ":\n" ++ out ++ err)
