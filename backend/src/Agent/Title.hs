{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Title (generateSessionTitle) where

import Agent.Sandbox (bindPathReadOnly, expandSandboxArg, nixDaemonBindArgs, piAgentConfigDir, runnerConfigArgs, runnerEnvironment, sandboxHome, sessionPaths)
import Agent.Session (AgentSession (..))
import Config (AgentConfig (..))
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, try)
import Control.Lens (filtered, folded, lastOf, to)
import Data.Either (fromRight)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

titleTimeoutSeconds :: Int
titleTimeoutSeconds = 60

titleMaxLength :: Int
titleMaxLength = 60

titleMaxWords :: Int
titleMaxWords = 10

generateSessionTitle :: AgentConfig -> AgentSession -> Text -> IO (Either String Text)
generateSessionTitle cfg session_ request = do
    nixBind <- nixDaemonBindArgs
    piConfigDir <- piAgentConfigDir
    let paths = sessionPaths session_
        runnerHome = sandboxHome paths
        expand = expandSandboxArg paths
        runnerArgs =
            agentRunnerCommand cfg
                : ["--no-session", "--no-tools"]
                ++ runnerConfigArgs expand (agentRunnerArgs cfg)
                ++ ["-p", T.unpack (agentTitlePrompt cfg)]
        args =
            map expand (agentSboxArgs cfg)
                ++ bindPathReadOnly piConfigDir
                ++ nixBind
                ++ ["--"]
                ++ runnerArgs
    runnerEnv <-
        runnerEnvironment
            [ ("HOME", runnerHome)
            , ("PI_CODING_AGENT_DIR", piConfigDir)
            ]
    createDirectoryIfMissing True runnerHome
    let process =
            (proc (agentSboxCommand cfg) args)
                { cwd = Just runnerHome
                , env = Just runnerEnv
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    spawned <- try (createProcess process)
    case spawned of
        Left (err :: IOException) -> return $ Left ("runner failed to start: " ++ show err)
        Right (Just hin, Just hout, Just herr, ph) -> do
            writer <- async $ do
                _ <- try (TIO.hPutStr hin request) :: IO (Either IOException ())
                try (hClose hin) :: IO (Either IOException ())
            outReader <- async (readHandleText hout)
            errReader <- async (readHandleText herr)
            finished <- timeout (titleTimeoutSeconds * 1000000) (waitForProcess ph)
            _ <- wait writer
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
    -- Models wrap a title in quotes, bullets, bold markers and punctuation.
    isNoise char = char `elem` ("\"'`*#_.,:;- \t" :: String)
