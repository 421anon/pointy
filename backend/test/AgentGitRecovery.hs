{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Agent.Git (advanceCleanWorktreeToCommit, attachAgentWorktreeToBranch, commitAgentTurnOutputs)
import Agent.Session (AgentSession (..), AgentTurn (..))
import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (setEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

main :: IO ()
main = withSystemTempDirectory "agent-git-recovery" $ \root -> do
    setEnv "HOME" root
    let repo = root </> "user-repo.git"
        seed = root </> "seed"
        worktree = root </> "worktree"
    setupRepo repo seed worktree
    base <- gitOut repo ["rev-parse", "main"]
    now <- getCurrentTime
    let session_ =
            AgentSession
                { sessionId = "test"
                , sessionName = Nothing
                , targetBranch = "main"
                , agentBranch = "agent/test"
                , baseCommit = base
                , worktreePath = worktree
                , status = "open"
                , preparedApply = Nothing
                , activeTurnId = Nothing
                , lastError = Nothing
                , createdAt = now
                , updatedAt = now
                , agentCurrentProjectId = Nothing
                , autoApply = True
                , autoApplyClient = Nothing
                }
        turn = turnFor now worktree
    detachedCommitReattachesBeforeCommit session_ turn worktree repo
    detachedCommitReattachesBeforeApply session_ worktree repo
    dirtyWorktreeSurvivesApplyAdvance session_ worktree

detachedCommitReattachesBeforeCommit :: AgentSession -> AgentTurn -> FilePath -> FilePath -> IO ()
detachedCommitReattachesBeforeCommit session_ turn worktree repo = do
    git worktree ["checkout", "-q", "--detach"]
    writeFile (worktree </> "steps" </> "2.nix") "{ name = \"detached output\"; }\n"
    (commit, skipped) <- commitOrFail session_ turn
    assertEqual "detached commit keeps all output changes" [] skipped
    assertBool "detached commit produces a commit" (commit /= Nothing)
    branchHead <- gitOut repo ["rev-parse", "agent/test"]
    head_ <- gitOut worktree ["rev-parse", "HEAD"]
    attachedBranch <- gitOut worktree ["symbolic-ref", "--short", "HEAD"]
    assertEqual "agent branch follows the detached commit" head_ branchHead
    assertEqual "worktree reattaches to the agent branch" "agent/test" attachedBranch
    committed <- gitOut repo ["show", "agent/test:steps/2.nix"]
    assertEqual "agent output lands on the agent branch" "{ name = \"detached output\"; }" committed

detachedCommitReattachesBeforeApply :: AgentSession -> FilePath -> FilePath -> IO ()
detachedCommitReattachesBeforeApply session_ worktree repo = do
    git worktree ["checkout", "-q", "--detach"]
    writeFile (worktree </> "steps" </> "3.nix") "{ name = \"detached commit\"; }\n"
    git worktree ["add", "steps/3.nix"]
    git worktree ["commit", "-q", "-m", "detached apply commit"]
    detachedHead <- gitOut worktree ["rev-parse", "HEAD"]
    attached <- attachOrFail session_
    assertEqual "apply preparation reports the worktree head" (Just detachedHead) attached
    branchHead <- gitOut repo ["rev-parse", "agent/test"]
    attachedBranch <- gitOut worktree ["symbolic-ref", "--short", "HEAD"]
    assertEqual "apply preparation updates the agent branch" detachedHead branchHead
    assertEqual "apply preparation attaches the worktree" "agent/test" attachedBranch

dirtyWorktreeSurvivesApplyAdvance :: AgentSession -> FilePath -> IO ()
dirtyWorktreeSurvivesApplyAdvance session_ worktree = do
    branchHead <- gitOut worktree ["rev-parse", "HEAD"]
    clean <- advanceOrFail session_ branchHead branchHead
    assertBool "a clean worktree advances" clean
    writeFile (worktree </> "steps" </> "4.nix") "{ name = \"uncommitted\"; }\n"
    advanced <- advanceOrFail session_ branchHead branchHead
    exists <- doesFileExist (worktree </> "steps" </> "4.nix")
    assertEqual "a dirty worktree is not advanced" False advanced
    assertBool "uncommitted agent output survives apply" exists

setupRepo :: FilePath -> FilePath -> FilePath -> IO ()
setupRepo repo seed worktree = do
    createDirectoryIfMissing True (seed </> "steps")
    git (takeDirectory repo) ["init", "-q", "--bare", "-b", "main", repo]
    git seed ["init", "-q", "-b", "main"]
    identity seed
    writeFile (seed </> "steps" </> "1.nix") "{ name = \"base\"; }\n"
    git seed ["add", "-A"]
    git seed ["commit", "-q", "-m", "base"]
    git seed ["push", "-q", repo, "main"]
    base <- gitOut repo ["rev-parse", "main"]
    git repo ["branch", "agent/test", T.unpack base]
    git repo ["worktree", "add", "-q", worktree, "agent/test"]
    identity worktree

turnFor :: UTCTime -> FilePath -> AgentTurn
turnFor now worktree =
    AgentTurn
        { turnId = "turn-test"
        , turnSessionId = "test"
        , turnPrompt = "recover detached worktree"
        , turnImages = []
        , turnAreaCaptures = []
        , turnAutomatic = False
        , turnStatus = "running"
        , turnExitCode = Nothing
        , turnStartedAt = now
        , turnFinishedAt = Nothing
        , turnLogPath = takeDirectory worktree </> "turn.log"
        , turnLog = ""
        }

commitOrFail :: AgentSession -> AgentTurn -> IO (Maybe Text, [Text])
commitOrFail session_ turn =
    runExceptT (commitAgentTurnOutputs session_ turn) >>= either fail return

attachOrFail :: AgentSession -> IO (Maybe Text)
attachOrFail session_ =
    runExceptT (attachAgentWorktreeToBranch session_) >>= either fail return

advanceOrFail :: AgentSession -> Text -> Text -> IO Bool
advanceOrFail session_ expectedHead targetCommit =
    runExceptT (advanceCleanWorktreeToCommit session_ expectedHead targetCommit) >>= either fail return

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
assertBool label ok = if ok then pure () else fail label

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
