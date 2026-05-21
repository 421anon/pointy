{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Handlers.Statuses (
    
    checkStatus,
    getStatuses,
    addDependencyRunningOverrides,
    removeDependencyRunningOverrides,
    broadcastProjectStatus,
    broadcastStatusForStepProjects,
    forkBroadcastProjectStatusAtHead,
    forkBroadcastStatusForStepProjectsAtHead,
    resolveFailureLogs,
) where

import BuildLog (ResolvedLog (..), lastMeaningfulLine, resolveBuildLog)
import Bus (broadcastSnapshot)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM, forM_, void)
import Control.Monad.Except (runExceptT)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), getProjectOutPaths)
import ProcessLimiter (readProcessWithExitCodeL)
import System.Directory (doesPathExist)

import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), runNixEvalJsonInRepo, runNixEvalRawInRepo, userRepoPath, withReadRepoTransaction)

{-# NOINLINE dependencyRunningOverrides #-}
dependencyRunningOverrides :: TVar (Map (Text, Int) Int)
dependencyRunningOverrides = unsafePerformIO $ newTVarIO Map.empty

checkStatus :: FilePath -> IO (Text, Maybe Text)
checkStatus path = do
    exists <- doesPathExist path
    if exists
        then return ("success", Nothing)
        else do
            let sanitizedPath = map (\c -> if c == '/' then '-' else c) (dropWhile (== '/') path)
            let unitName = "nix-build-" ++ sanitizedPath
            (exitCode, _, _) <- readProcessWithExitCodeL "systemctl" ["is-active", unitName] ""
            if exitCode == ExitSuccess
                then return ("running", Nothing)
                else do
                    (failedCode, _, _) <- readProcessWithExitCodeL "systemctl" ["is-failed", unitName] ""
                    if failedCode == ExitSuccess
                        then return ("failure", Nothing)
                        else return ("not-started", Nothing)

{- | Resolve build logs for all steps with @"failure"@ status, returning
updated statuses with log summaries. Called asynchronously after the
initial SSE snapshot so log resolution doesn't block the first message.
-}
resolveFailureLogs :: ReadRepoContext -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveFailureLogs ctx statuses = do
    let failed = [(sid, st) | (sid, st) <- Map.toList statuses, fst st == "failure"]
    updates <- forM failed $ \(sid, _) -> do
        result <- runExceptT $ runNixEvalRawInRepo ctx ("#pointy.steps." ++ show sid ++ ".outPath")
        case result of
            Left _ -> return (sid, ("failure", Nothing))
            Right outPath -> do
                mResolved <- resolveBuildLog outPath
                let mMsg = case mResolved of
                        Just rl -> lastMeaningfulLine (resolvedLog rl)
                        Nothing -> Nothing
                return (sid, ("failure", mMsg))
    return $ foldl' (\acc (sid, st) -> Map.insert sid st acc) statuses updates

resolveFailureLogsAtCommit :: Text -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveFailureLogsAtCommit targetCommit statuses = do
    repoPath <- userRepoPath
    resolveFailureLogs (ReadRepoContext repoPath (unpack targetCommit)) statuses

getStatuses :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text)))
getStatuses pid targetCommit = do
    result <- getProjectOutPaths pid targetCommit
    case result of
        Left err -> return $ Left err
        Right outPaths -> do
            rawStatuses <- Map.fromList <$> mapConcurrently getStatusForStep (Map.toList outPaths)
            Right <$> applyDependencyRunningOverrides targetCommit rawStatuses
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
    statusesResult <- getStatuses pid targetCommit
    case statusesResult of
        Left err -> putStrLn $ "broadcastProjectStatus skipped: " ++ err
        Right stats -> do
            let finalStats = case mStatusOverride of
                    Just (sid, st) -> Map.insert sid st stats
                    Nothing -> stats
            broadcastSnapshot pid targetCommit finalStats
            void $ forkIO $ do
                updatedStatuses <- resolveFailureLogsAtCommit targetCommit finalStats
                broadcastSnapshot pid targetCommit updatedStatuses

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
