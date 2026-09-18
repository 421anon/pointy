{-# LANGUAGE OverloadedStrings #-}

module Agent.Sandbox (
    SandboxPaths (..),
    sessionPaths,
    expandSandboxArg,
    bindPath,
    bindPathReadOnly,
    nixDaemonBindArgs,
    piAgentConfigDir,
    runnerEnvironment,
    runnerConfigArgs,
) where

import Agent.Session (AgentSession (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist, getHomeDirectory)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))

nixCompatSocket :: FilePath
nixCompatSocket = "/run/nix-daemon-socket"

nixDaemonBindArgs :: IO [String]
nixDaemonBindArgs = do
    exists <- doesFileExist nixCompatSocket
    return $
        if exists
            then ["--bind", nixCompatSocket, "/var/run/nix-daemon-socket"]
            else []

bindPath :: FilePath -> [String]
bindPath path = ["--bind", path, path]

bindPathReadOnly :: FilePath -> [String]
bindPathReadOnly path = ["--ro-bind", path, path]

piAgentConfigDir :: IO FilePath
piAgentConfigDir = (\home -> home </> ".pi" </> "agent") <$> getHomeDirectory

data SandboxPaths = SandboxPaths
    { sandboxWorktree :: FilePath
    , sandboxHome :: FilePath
    , sandboxSessionId :: Text
    }

sessionPaths :: AgentSession -> SandboxPaths
sessionPaths session_ =
    SandboxPaths
        { sandboxWorktree = worktreePath session_
        , sandboxHome = takeDirectory (worktreePath session_) </> "home"
        , sandboxSessionId = sessionId session_
        }

expandSandboxArg :: SandboxPaths -> Text -> String
expandSandboxArg paths = T.unpack . flip (foldr (uncurry T.replace)) placeholders
  where
    placeholders =
        [ ("{worktree}", T.pack (sandboxWorktree paths))
        , ("{home}", T.pack (sandboxHome paths))
        , ("{sessionRoot}", T.pack (takeDirectory (sandboxWorktree paths)))
        , ("{sessionId}", sandboxSessionId paths)
        ]

runnerEnvironment :: [(String, String)] -> IO [(String, String)]
runnerEnvironment ownVars = do
    baseEnv <- getEnvironment
    return $
        ("PATH", fromMaybe fallbackPath (lookup "PATH" baseEnv))
            : ownVars
            ++ filter ((`elem` passthroughKeys) . fst) baseEnv
  where
    fallbackPath = "/run/current-system/sw/bin:/usr/bin:/bin"
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

runnerConfigArgs :: (Text -> String) -> [Text] -> [String]
runnerConfigArgs expand = strip
  where
    strip [] = []
    strip (arg : rest)
        | arg `elem` managedWithValue = strip (drop 1 rest)
        | arg `elem` managedFlags = strip rest
        | otherwise = expand arg : strip rest
    managedWithValue = ["--mode", "--session", "--fork"]
    managedFlags = ["-c", "--continue", "--no-session", "-p", "--print", "{prompt}"]
