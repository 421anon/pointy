{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Title (generateSessionTitle) where

import Agent.Model (chatModelArgs, prepareChatModel)
import Agent.Sandbox (bindPath, expandSandboxArg, piIsolationFlags, runnerConfigArgs, runnerEnvironment, sandboxPiHome, sandboxProcess, seedPiConfig, sessionPaths)
import Agent.Session (AgentSession (..))
import Config (AgentConfig (..))
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, try)
import Control.Lens (filtered, folded, lastOf, to)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.Process (CreateProcess (..), StdStream (..), createProcess, terminateProcess, waitForProcess)
import System.Timeout (timeout)

titleTimeoutSeconds :: Int
titleTimeoutSeconds = 60

titleMaxLength :: Int
titleMaxLength = 60

titleMaxWords :: Int
titleMaxWords = 10

titleSystemPrompt :: Text
titleSystemPrompt = "You name chat sessions. Reply with the title only."

generateSessionTitle :: AgentConfig -> AgentSession -> Text -> IO (Either String Text)
generateSessionTitle cfg session_ request = do
    chat <- prepareChatModel
    let paths = sessionPaths session_
        piHome = sandboxPiHome paths
        expand = expandSandboxArg paths
        titlePrompt = agentTitlePrompt cfg <> "\n\nUser request:\n" <> request
        runnerArgs =
            agentRunnerCommand cfg
                : ["--no-session", "--no-tools"]
                ++ piIsolationFlags
                ++ chatModelArgs chat
                ++ runnerConfigArgs expand (agentRunnerArgs cfg)
                ++ ["--system-prompt", T.unpack titleSystemPrompt]
                ++ ["-p", T.unpack titlePrompt]
        args = bindPath piHome ++ ["--"] ++ runnerArgs
    runnerEnv <- runnerEnvironment [("HOME", piHome)]
    seedPiConfig piHome
    sandbox <- sandboxProcess cfg args
    let process =
            sandbox
                { cwd = Just piHome
                , env = Just runnerEnv
                , std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    spawned <- try (createProcess process)
    case spawned of
        Left (err :: IOException) -> return $ Left ("runner failed to start: " ++ show err)
        Right (_, Just hout, Just herr, ph) -> do
            outReader <- async (readHandleText hout)
            errReader <- async (readHandleText herr)
            finished <- timeout (titleTimeoutSeconds * 1000000) (waitForProcess ph)
            output <- wait outReader
            _ <- wait errReader
            case finished of
                Nothing -> do
                    terminateProcess ph
                    _ <- waitForProcess ph
                    return $ Left "runner timed out"
                Just (ExitFailure code) ->
                    return $ Left ("runner exited with code " ++ show code)
                Just ExitSuccess ->
                    return $ maybe (Left ("runner answered with no usable title: " ++ show (T.strip output))) Right (titleFromOutput output)
        Right _ -> return $ Left "runner pipes unavailable"

readHandleText :: Handle -> IO Text
readHandleText handle =
    fromRight "" <$> (try (TIO.hGetContents handle) :: IO (Either IOException Text))

titleFromOutput :: Text -> Maybe Text
titleFromOutput output = do
    line <- lastOf (folded . to T.strip . filtered (not . T.null)) (T.lines output)
    let titleWords = T.words (T.dropAround isNoise line)
        title = T.unwords titleWords
    if T.null title || T.length title > titleMaxLength || length titleWords > titleMaxWords
        then Nothing
        else Just title
  where
    isNoise char = char `elem` ("\"'`*#_.,:;- \t" :: String)
