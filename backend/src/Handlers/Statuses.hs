{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Handlers.Statuses (
    getProjectOutPathsHandler,
    checkStatus,
    getStatuses,
    addDependencyRunningOverrides,
    removeDependencyRunningOverrides,
    broadcastProjectStatus,
    broadcastStatusForStepProjects,
    forkBroadcastProjectStatusAtHead,
    forkBroadcastStatusForStepProjectsAtHead,
) where

import BuildLog (ResolvedLog (..), lastMeaningfulLine, resolveBuildLog)
import Bus (broadcastSnapshot)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), getProjectOutPaths)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, throwError)
import Servant.Server (err500, errBody)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), runNix, runNixEvalJsonInRepo, withReadRepoTransaction)

{-# NOINLINE dependencyRunningOverrides #-}
dependencyRunningOverrides :: TVar (Map (Text, Int) Int)
dependencyRunningOverrides = unsafePerformIO $ newTVarIO Map.empty

getProjectOutPathsHandler :: Int -> Maybe Text -> Handler (Map Int Text)
getProjectOutPathsHandler pid commit = do
    eitherCommit <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return $ fromMaybe (pack hash) commit
    case eitherCommit of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right targetCommit -> do
            result <- liftIO $ getProjectOutPaths pid targetCommit
            case result of
                Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
                Right outPaths -> return outPaths

checkStatus :: FilePath -> IO (Text, Maybe Text)
checkStatus path = do
    let sanitizedPath = map (\c -> if c == '/' then '-' else c) (dropWhile (== '/') path)
    let unitName = "nix-build-" ++ sanitizedPath

    (exitCode, _, _) <- readProcessWithExitCodeL "systemctl" ["is-active", unitName] ""
    if exitCode == ExitSuccess
        then return ("running", Nothing)
        else do
            -- Note: systemd-run is invoked with --collect, which GCs the unit shortly
            -- after exit. is-failed is therefore racy on fast failures; the BFS over input
            -- derivations performed by resolveBuildLog is the durable signal.
            (failedCode, _, _) <- readProcessWithExitCodeL "systemctl" ["is-failed", unitName] ""
            if failedCode == ExitSuccess
                then resolveFailure path "failure"
                else do
                    result <- runExceptT $ runNix ["path-info", path]
                    case result of
                        Right _ -> return ("success", Nothing)
                        Left _ -> resolveFailure path "not-started"

{- | Look up a failure log via 'resolveBuildLog'. The fallback state is used
when no log can be resolved: when systemd already confirmed failure we
still report "failure" (without a message); when there is no other
evidence the step might simply never have been queued.
-}
resolveFailure :: FilePath -> Text -> IO (Text, Maybe Text)
resolveFailure path fallback = do
    mResolved <- resolveBuildLog path
    case mResolved of
        Just rl -> return ("failure", lastMeaningfulLine (resolvedLog rl))
        Nothing -> return (fallback, Nothing)

getStatuses :: Text -> Map Int Text -> IO (Map Int (Text, Maybe Text))
getStatuses targetCommit outPaths = do
    rawStatuses <- Map.fromList <$> mapConcurrently getStatusForStep (Map.toList outPaths)
    applyDependencyRunningOverrides targetCommit rawStatuses
  where
    getStatusForStep (sid, path) = do
        status_ <-
            checkStatus (unpack path)
                `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
        pure (sid, status_)

addDependencyRunningOverrides :: Text -> [Int] -> IO ()
addDependencyRunningOverrides targetCommit stepIds =
    atomically $
        modifyTVar' dependencyRunningOverrides $
            \overrides ->
                foldl' (\acc sid -> Map.insertWith (+) (targetCommit, sid) 1 acc) overrides stepIds

removeDependencyRunningOverrides :: Text -> [Int] -> IO ()
removeDependencyRunningOverrides targetCommit stepIds =
    atomically $
        modifyTVar' dependencyRunningOverrides $
            \overrides ->
                foldl'
                    (\acc sid -> Map.update decrement (targetCommit, sid) acc)
                    overrides
                    stepIds
  where
    decrement count
        | count <= 1 = Nothing
        | otherwise = Just (count - 1)

applyDependencyRunningOverrides :: Text -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
applyDependencyRunningOverrides targetCommit statuses = do
    overrides <- readTVarIO dependencyRunningOverrides
    let blockedStepIds =
            [ sid
            | ((commitHash, sid), count) <- Map.toList overrides
            , commitHash == targetCommit
            , count > 0
            ]
    return $ foldl' applyBlockedRunning statuses blockedStepIds
  where
    applyBlockedRunning acc sid =
        Map.adjust
            (\status_@(state, _) -> if state == "not-started" then ("running", Nothing) else status_)
            sid
            acc

broadcastProjectStatus :: Int -> Text -> Maybe (Int, (Text, Maybe Text)) -> IO ()
broadcastProjectStatus pid targetCommit mStatusOverride = do
    result <- getProjectOutPaths pid targetCommit
    case result of
        Left err -> putStrLn $ "broadcastProjectStatus skipped: " ++ err
        Right outPaths -> do
            stats <- getStatuses targetCommit outPaths
            let finalStats = case mStatusOverride of
                    Just (sid, st) -> Map.insert sid st stats
                    Nothing -> stats
            broadcastSnapshot pid targetCommit finalStats outPaths

broadcastStatusForStepProjects :: Int -> Text -> Maybe (Text, Maybe Text) -> IO ()
broadcastStatusForStepProjects sid targetCommit mStatusOverride = do
    result <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
        let ctx = ReadRepoContext repoPath (unpack targetCommit)
        output <- runNixEvalJsonInRepo ctx "#pointy.projects"
        let decodeResult = eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Map String ProjectDef)
        case decodeResult of
            Left err -> do
                liftIO $ putStrLn $ "Error parsing json in broadcastStatusForStepProjects for #pointy.projects: " ++ err
                return ()
            Right projects -> do
                let targetProjects = filter (projectContainsStep sid) (Map.elems projects)
                liftIO $ forM_ targetProjects $ \p ->
                    forkIO $ broadcastProjectStatus (projectDefId p) targetCommit (fmap (sid,) mStatusOverride)
                return ()
    case result of
        Left err -> putStrLn $ "Error broadcasting statuses for step " ++ show sid ++ ": " ++ err
        Right _ -> return ()

forkBroadcastProjectStatusAtHead :: Int -> IO ()
forkBroadcastProjectStatusAtHead pid = do
    eHead <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "forkBroadcastProjectStatusAtHead skipped: " ++ err
        Right c -> void $ forkIO $ broadcastProjectStatus pid c Nothing

forkBroadcastStatusForStepProjectsAtHead :: Int -> IO ()
forkBroadcastStatusForStepProjectsAtHead sid = do
    eHead <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "forkBroadcastStatusForStepProjectsAtHead skipped: " ++ err
        Right c -> void $ forkIO $ broadcastStatusForStepProjects sid c Nothing

projectContainsStep :: Int -> ProjectDef -> Bool
projectContainsStep sid p =
    not (projectDefHidden p) && any isTargetStep (projectDefSteps p)
  where
    isTargetStep s = not (stepRefHidden s) && stepDefId (stepRefDef s) == sid
