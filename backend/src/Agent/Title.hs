{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Title (generateSessionTitle) where

import Agent.Sandbox (expandSessionArg, nixDaemonBindArgs, runnerConfigArgs, runnerEnvironment)
import Agent.Session (AgentSession (..))
import Config (AgentConfig (..))
import Control.Concurrent.Async (async, wait)
import Control.Exception (IOException, try)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, hClose)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import System.Timeout (timeout)

titleTimeoutSeconds :: Int
titleTimeoutSeconds = 60

-- | An answer longer than this is prose, not a title.
titleMaxLength :: Int
titleMaxLength = 60

titleMaxWords :: Int
titleMaxWords = 10

{- | Ask the runner to name a chat after the request that opened it.

This is a throwaway completion: no tools, no session file, and the request
arrives on stdin (pi merges piped stdin into the print-mode prompt), so a long
or quote-heavy request never has to survive an argument list. It runs in the
chat's own sandbox but touches nothing the chat owns.
-}
generateSessionTitle :: AgentConfig -> AgentSession -> Text -> IO (Either String Text)
generateSessionTitle cfg session_ request = do
    realHome <- getHomeDirectory
    nixBind <- nixDaemonBindArgs
    let piConfigDir = realHome </> ".pi" </> "agent"
        runnerHome = takeDirectory (worktreePath session_) </> "home"
        expand = expandSessionArg session_ ""
        runnerArgs =
            agentRunnerCommand cfg
                : ["--no-session", "--no-tools"]
                ++ runnerConfigArgs expand (agentRunnerArgs cfg)
                ++ ["-p", T.unpack (agentTitlePrompt cfg)]
        args =
            map expand (agentSboxArgs cfg)
                ++ ["--ro-bind", piConfigDir, piConfigDir]
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
readHandleText handle = do
    result <- try (TIO.hGetContents handle) :: IO (Either IOException Text)
    return $ either (const "") id result

{- | The one short line a model's print-mode answer is supposed to be, with the
decoration models add anyway removed. Anything wordier is rejected: falling
back to the opening request beats showing a paragraph as a title.
-}
titleFromOutput :: Text -> Maybe Text
titleFromOutput output = do
    line <- listToMaybe (reverse (filter (not . T.null) (map T.strip (T.lines output))))
    let title = T.unwords (T.words (T.dropAround isNoise line))
    if T.null title || T.length title > titleMaxLength || length (T.words title) > titleMaxWords
        then Nothing
        else Just title
  where
    -- Models wrap a title in quotes, bullets, bold markers and punctuation.
    isNoise char = char `elem` ("\"'`*#_.,:;- \t" :: String)
