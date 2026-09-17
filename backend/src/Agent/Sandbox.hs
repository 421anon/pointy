{-# LANGUAGE OverloadedStrings #-}

module Agent.Sandbox (
    nixDaemonBindArgs,
    runnerEnvironment,
    runnerConfigArgs,
    expandSessionArg,
) where

import Agent.Session (AgentSession (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory)

nixCompatSocket :: FilePath
nixCompatSocket = "/run/nix-daemon-socket"

nixDaemonBindArgs :: IO [String]
nixDaemonBindArgs = do
    exists <- doesFileExist nixCompatSocket
    return $
        if exists
            then ["--bind", "/run/nix-daemon-socket", "/var/run/nix-daemon-socket"]
            else []

fallbackPath :: String
fallbackPath = "/run/current-system/sw/bin:/usr/bin:/bin"

{- | The environment a sandboxed runner gets: a PATH, the caller's own
variables, then the host's identity and provider keys. sbox-inner runs
`set -euo pipefail` and references USER/SHELL/etc., so those have to survive;
everything else is dropped so the runner never inherits the backend's Git or
SSH credentials.
-}
runnerEnvironment :: [(String, String)] -> IO [(String, String)]
runnerEnvironment ownVars = do
    baseEnv <- getEnvironment
    let pathValue = fromMaybe fallbackPath (lookup "PATH" baseEnv)
    return $
        ("PATH", pathValue)
            : ownVars
            ++ [(name, value) | (name, value) <- baseEnv, name `elem` passthroughKeys]
  where
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

{- | Configured runner arguments with the flags the backend owns removed: it
picks the mode, the conversation to continue and how the prompt arrives. What
is left describes the model, and is expanded by the caller.
-}
runnerConfigArgs :: (Text -> String) -> [Text] -> [String]
runnerConfigArgs expand = strip
  where
    strip [] = []
    strip (flag : _value : rest)
        | flag `elem` ["--mode", "--session", "--fork"] = strip rest
    strip (arg : rest)
        | arg `elem` ["-c", "--continue", "--no-session", "-p", "--print", "{prompt}"] = strip rest
        | otherwise = expand arg : strip rest

-- | Fill a configured argument's placeholders with this chat's paths.
expandSessionArg :: AgentSession -> Text -> Text -> String
expandSessionArg session_ promptText arg =
    let sessionRoot = T.pack (takeDirectory (worktreePath session_))
        runnerHome = sessionRoot <> "/home"
     in T.unpack $
            T.replace "{prompt}" promptText $
                T.replace "{worktree}" (T.pack (worktreePath session_)) $
                    T.replace "{home}" runnerHome $
                        T.replace "{sessionRoot}" sessionRoot $
                            T.replace "{sessionId}" (sessionId session_) arg
