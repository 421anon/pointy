{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Git (finalizeApplyResolution)
import Agent.Session (AgentSession (..), PreparedApply (..))
import Control.Monad (unless, void)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, removeFile)
import System.Environment (setEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = withSystemTempDirectory "apply-resolution-test" $ \home -> do
    setEnv "HOME" home
    agentMovedAfterConflict home
    markersResolvedInApplyWorktree home

agentMovedAfterConflict :: FilePath -> IO ()
agentMovedAfterConflict home = do
    (session_, applyWorktree) <- conflictedSession home "moved"
    git (worktreePath session_) ["rm", "-q", "steps/2994.nix"]
    writeFile (worktreePath session_ </> "steps" </> "2997.nix") "{ name = \"agent\"; }\n"
    git (worktreePath session_) ["add", "-A"]
    git (worktreePath session_) ["commit", "-q", "-m", "renumber"]
    (finalized, resolved) <- finalizeOrFail session_
    assertEqual "renumbered session leaves conflict state" "open" (status finalized)
    assertEqual "stale conflicted candidate is dropped" Nothing (preparedApply finalized)
    assertEqual "no resolution is committed" Nothing resolved
    exists <- doesDirectoryExist applyWorktree
    assertEqual "stale apply worktree is removed" False exists

markersResolvedInApplyWorktree :: FilePath -> IO ()
markersResolvedInApplyWorktree home = do
    (session_, applyWorktree) <- conflictedSession home "resolved"
    writeFile (applyWorktree </> "steps" </> "2994.nix") "{ name = \"target\"; }\n"
    writeFile (applyWorktree </> "steps" </> "2997.nix") "{ name = \"agent\"; }\n"
    (finalized, resolved) <- finalizeOrFail session_
    assertEqual "resolved session is applicable" "open" (status finalized)
    assertEqual "candidate carries the resolution commit" resolved (candidateHead <$> preparedApply finalized)
    commit <- maybe (fail "resolution commit is reported") (return . T.unpack) resolved
    kept <- gitOut applyWorktree ["show", commit ++ ":steps/2994.nix"]
    assertEqual "target keeps its step id" "{ name = \"target\"; }" kept
    moved <- gitOut applyWorktree ["show", commit ++ ":steps/2997.nix"]
    assertEqual "renumbered agent step is committed" "{ name = \"agent\"; }" moved

conflictedSession :: FilePath -> String -> IO (AgentSession, FilePath)
conflictedSession home name = do
    let root = home </> name
        repo = root </> "user-repo.git"
        seed = root </> "seed"
        worktree = root </> "worktree"
        applyWorktree = root </> "apply-worktree"
    createDirectoryIfMissing True (seed </> "steps")
    git root ["init", "-q", "--bare", "-b", "prod-backend", repo]
    git seed ["init", "-q", "-b", "prod-backend"]
    identity seed
    writeFile (seed </> "steps" </> "1.nix") "{ name = \"base\"; }\n"
    git seed ["add", "-A"]
    git seed ["commit", "-q", "-m", "base"]
    git seed ["push", "-q", repo, "prod-backend"]
    base <- gitOut repo ["rev-parse", "prod-backend"]
    git repo ["branch", "agent/" ++ name, T.unpack base]
    git repo ["worktree", "add", "-q", worktree, "agent/" ++ name]
    identity worktree
    writeFile (worktree </> "steps" </> "2994.nix") "{ name = \"agent\"; }\n"
    git worktree ["add", "-A"]
    git worktree ["commit", "-q", "-m", "agent"]
    writeFile (seed </> "steps" </> "2994.nix") "{ name = \"target\"; }\n"
    git seed ["add", "-A"]
    git seed ["commit", "-q", "-m", "target"]
    git seed ["push", "-q", repo, "prod-backend"]
    targetHead_ <- gitOut repo ["rev-parse", "prod-backend"]
    agentHead_ <- gitOut repo ["rev-parse", "agent/" ++ name]
    git repo ["worktree", "add", "-q", "--detach", applyWorktree, T.unpack targetHead_]
    identity applyWorktree
    void $ readProcessWithExitCode "git" ["-C", applyWorktree, "merge", "--squash", "agent/" ++ name] ""
    removeFile (seed </> "steps" </> "2994.nix")
    now <- getCurrentTime
    setEnv "HOME" root
    let session_ =
            AgentSession
                { sessionId = T.pack name
                , sessionName = Nothing
                , targetBranch = "prod-backend"
                , agentBranch = T.pack ("agent/" ++ name)
                , baseCommit = base
                , worktreePath = worktree
                , status = "prepare_conflict"
                , preparedApply =
                    Just
                        PreparedApply
                            { targetHead = targetHead_
                            , agentHead = agentHead_
                            , candidateHead = ""
                            , candidateWorktree = applyWorktree
                            }
                , activeTurnId = Nothing
                , lastError = Just "conflict"
                , createdAt = now
                , updatedAt = now
                }
    return (session_, applyWorktree)

finalizeOrFail :: AgentSession -> IO (AgentSession, Maybe Text)
finalizeOrFail session_ = runExceptT (finalizeApplyResolution session_) >>= either fail return

identity :: FilePath -> IO ()
identity dir = do
    git dir ["config", "user.email", "test@invalid.local"]
    git dir ["config", "user.name", "test"]

git :: FilePath -> [String] -> IO ()
git dir args = void (gitOut dir args)

gitOut :: FilePath -> [String] -> IO Text
gitOut dir args = do
    (code, out, err) <- readProcessWithExitCode "git" ("-C" : dir : args) ""
    case code of
        ExitSuccess -> return (T.strip (T.pack out))
        ExitFailure _ -> fail ("git " ++ unwords args ++ ": " ++ err)

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
