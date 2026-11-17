module Main (main) where

import Control.Exception (IOException, SomeException, displayException, try)
import Data.Maybe (fromMaybe)
import SlurmBroker
import SlurmEvents
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitSuccess, exitWith, ExitCode (..))
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

defaultSocket :: FilePath
defaultSocket = "/run/pointy-slurm-events/broker.sock"

defaultHeartbeatSeconds :: Int
defaultHeartbeatSeconds = 10

defaultTrustedUsers :: [String]
defaultTrustedUsers = ["root", "slurm"]

defaultStateFile :: FilePath
defaultStateFile = "/var/lib/pointy-slurm-events/terminal-state.json"

main :: IO ()
main = do
    arguments <- getArgs
    configuredSocket <- fromMaybe defaultSocket <$> lookupEnv "POINTY_SLURM_EVENT_SOCKET"
    case arguments of
        "serve" : rest -> serve configuredSocket rest
        "watch" : rest -> watch configuredSocket rest
        "notify" : rest -> notify configuredSocket rest
        _ -> usage

serve :: FilePath -> [String] -> IO ()
serve defaultPath arguments =
    case parseServe defaultPath arguments of
        Left problem -> usageFailure problem
        Right (path, heartbeatSeconds, trustedUsers, accountWatchUsers, scopePrefix, stateFile) -> do
            outcome <-
                try $
                    runBrokerServer
                        BrokerConfig
                            { brokerSocketPath = path
                            , brokerHeartbeatMicros = max 1 heartbeatSeconds * 1000000
                            , brokerTrustedUsers = trustedUsers
                            , brokerAccountWatchUsers = accountWatchUsers
                            , brokerScopePrefix = scopePrefix
                            , brokerStateFile = Just stateFile
                            , brokerQuery = defaultSnapshotQuery
                            }
            case outcome of
                Left exception ->
                    usageFailure (displayException (exception :: SomeException))
                Right () -> pure ()

watch :: FilePath -> [String] -> IO ()
watch defaultPath arguments = do
    let (path, filterArguments) = parseSocket defaultPath arguments
    outcome <- try (runWatchClient path filterArguments)
    case outcome of
        Left exception -> do
            hPutStrLn stderr ("pointy-slurm-broker: " <> displayException (exception :: IOException))
            exitWith (ExitFailure 1)
        Right () -> exitSuccess

notify :: FilePath -> [String] -> IO ()
notify defaultPath arguments =
    case parseNotify defaultPath arguments of
        Left problem -> hPutStrLn stderr ("pointy-slurm-broker: " <> problem)
        Right (path, event) -> notifyBroker path event

parseServe :: FilePath -> [String] -> Either String (FilePath, Int, [String], [String], Maybe String, FilePath)
parseServe defaultPath = go defaultPath defaultHeartbeatSeconds defaultTrustedUsers [] Nothing defaultStateFile
  where
    go path heartbeat trusted accountWatchers scopePrefix stateFile [] =
        Right (path, heartbeat, trusted, accountWatchers, scopePrefix, stateFile)
    go _ heartbeat trusted accountWatchers scopePrefix stateFile ("--socket" : value : rest) =
        go value heartbeat trusted accountWatchers scopePrefix stateFile rest
    go path _ trusted accountWatchers scopePrefix stateFile ("--heartbeat" : value : rest) =
        case readMaybe value of
            Just seconds | seconds > 0 -> go path seconds trusted accountWatchers scopePrefix stateFile rest
            _ -> Left "--heartbeat must be a positive integer"
    go path heartbeat _ accountWatchers scopePrefix stateFile ("--trusted-users" : value : rest) =
        go path heartbeat (filter (not . null) (splitComma value)) accountWatchers scopePrefix stateFile rest
    go path heartbeat trusted _ scopePrefix stateFile ("--account-watch-users" : value : rest) =
        go path heartbeat trusted (filter (not . null) (splitComma value)) scopePrefix stateFile rest
    go path heartbeat trusted accountWatchers _ stateFile ("--scope-prefix" : value : rest)
        | not (null value) = go path heartbeat trusted accountWatchers (Just value) stateFile rest
        | otherwise = Left "--scope-prefix must not be empty"
    go path heartbeat trusted accountWatchers scopePrefix _ ("--state-file" : value : rest)
        | not (null value) = go path heartbeat trusted accountWatchers scopePrefix value rest
        | otherwise = Left "--state-file must not be empty"
    go _ _ _ _ _ _ (unknown : _) = Left ("unknown serve argument: " <> unknown)

splitComma :: String -> [String]
splitComma value = case break (== ',') value of
    (part, []) -> [part]
    (part, _ : rest) -> part : splitComma rest

parseSocket :: FilePath -> [String] -> (FilePath, [String])
parseSocket defaultPath ("--socket" : value : rest) = (value, rest)
parseSocket defaultPath arguments = (defaultPath, arguments)

data NotifyOptions = NotifyOptions
    { optionEvent :: String
    , optionAccount :: Maybe String
    , optionJob :: Maybe String
    , optionName :: Maybe String
    , optionState :: Maybe String
    }

parseNotify :: FilePath -> [String] -> Either String (FilePath, Event)
parseNotify defaultPath arguments = do
    let (path, remaining) = parseSocket defaultPath arguments
    options <- parseNotifyOptions (NotifyOptions "" Nothing Nothing Nothing Nothing) remaining
    kind <- maybe (Left ("unknown event: " <> optionEvent options)) Right (parseEventKind (optionEvent options))
    Right
        ( path
        , Event
            { eventKind = kind
            , eventAccount = optionAccount options
            , eventJobId = optionJob options
            , eventJobName = optionName options
            , eventState = optionState options
            }
        )

parseNotifyOptions :: NotifyOptions -> [String] -> Either String NotifyOptions
parseNotifyOptions options [] = Right options
parseNotifyOptions options ("--event" : value : rest) =
    parseNotifyOptions (options {optionEvent = value}) rest
parseNotifyOptions options ("--account" : value : rest) =
    parseNotifyOptions (options {optionAccount = present value}) rest
parseNotifyOptions options ("--job" : value : rest) =
    parseNotifyOptions (options {optionJob = present value}) rest
parseNotifyOptions options ("--name" : value : rest) =
    parseNotifyOptions (options {optionName = present value}) rest
parseNotifyOptions options ("--state" : value : rest) =
    parseNotifyOptions (options {optionState = present value}) rest
parseNotifyOptions _ (unknown : _) = Left ("unknown notify argument: " <> unknown)

present :: String -> Maybe String
present "" = Nothing
present value = Just value

usage :: IO ()
usage = do
    hPutStrLn stderr "usage: pointy-slurm-broker serve [--socket PATH] [--heartbeat SECONDS] [--trusted-users root,slurm] [--account-watch-users USER] [--scope-prefix PREFIX] [--state-file PATH]"
    hPutStrLn stderr "       pointy-slurm-broker watch [--socket PATH] native"
    hPutStrLn stderr "       pointy-slurm-broker watch [--socket PATH] account ACCOUNT"
    hPutStrLn stderr "       pointy-slurm-broker notify [--socket PATH] --event EVENT [--account ACCOUNT] [--job ID] [--name NAME] [--state STATE]"
    exitWith (ExitFailure 2)

usageFailure :: String -> IO a
usageFailure message = do
    hPutStrLn stderr ("pointy-slurm-broker: " <> message)
    exitWith (ExitFailure 2)
