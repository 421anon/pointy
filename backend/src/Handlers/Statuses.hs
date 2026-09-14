{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Handlers.Statuses (
    checkStatus,
    getRawStatuses,
    getRawStatusesWithPaths,
    getStatuses,
    partitionImmediateStatuses,
    resolveStepStatus,
    broadcastProjectStatus,
    broadcastSingleStepForProjects,
    broadcastFailedStepForProjects,
    broadcastKnownStepStatus,
    broadcastStatusForStepProjects,
    forkBroadcastProjectStatusAtHead,
    forkBroadcastStatusForStepProjectsAtHead,
    restoreRunningStatuses,
) where

import BuildLog (ResolvedLog (..), StepStore, buildStepStore, lastMeaningfulLine, logDirectoryAvailable, rawStatusesBatched, resolveBuildLog, resolveStatusesBatched)
import BuildRunner (BuildKey (..), BuildState (..), buildKeyForOutPath, queryState, querySlurmJobs, slurmJobName)
import Bus (broadcastSnapshot)
import ClusterBus (restoreRunningStepIds)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void, when)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import NixUtils (isValidStorePath)
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), getProjectOutPaths)
import UserRepo (ReadRepoContext (..), runNixEvalJsonInRepo, userRepoPath, withReadRepoTransaction)

checkStatus :: FilePath -> IO (Text, Maybe Text)
checkStatus path = do
    valid <- isValidStorePath path
    if valid
        then return ("success", Nothing)
        else do
            state <- queryState $ buildKeyForOutPath path
            return $ case state of
                BRunning -> ("running", Nothing)
                BAbsent -> ("not-started", Nothing)
                BSucceeded -> ("success", Nothing)
                BFailed -> ("failure", Nothing)

isImmediateStatus :: (Text, Maybe Text) -> Bool
isImmediateStatus (state, _) = state == "success" || state == "running"

partitionImmediateStatuses :: Map Int (Text, Maybe Text) -> (Map Int (Text, Maybe Text), Map Int (Text, Maybe Text))
partitionImmediateStatuses = Map.partition isImmediateStatus

resolveStepStatus :: ReadRepoContext -> Maybe FilePath -> (Int, (Text, Maybe Text)) -> IO (Int, (Text, Maybe Text))
resolveStepStatus _ _ entry@(_, status_)
    | isImmediateStatus status_ = return entry
resolveStepStatus _ Nothing entry@(_, (state, _))
    | state == "failure" || state == "not-started" = return entry
resolveStepStatus _ Nothing entry = return entry
resolveStepStatus _ (Just outPath) entry@(sid, (state, _))
    | state == "failure" || state == "not-started" = do
        mResolved <- resolveBuildLog outPath
        return $ case mResolved of
            Just rl -> (sid, ("failure", lastMeaningfulLine (resolvedLog rl)))
            Nothing -> entry
    | otherwise = return entry

resolveStatusesAtCommitWithPaths :: Text -> Map Int Text -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveStatusesAtCommitWithPaths targetCommit outPaths statuses = do
    repoPath <- userRepoPath
    let ctx = ReadRepoContext repoPath (unpack targetCommit)
    Map.fromList <$> mapConcurrently (resolveOneWithPath ctx outPaths) (Map.toList statuses)
  where
    resolveOneWithPath ctx outPaths (sid, entry) =
        resolveStepStatus ctx (fmap unpack $ Map.lookup sid outPaths) (sid, entry)

-- | Slurm job names of every queued or running build, in one query.
runningBuildKeys :: IO (Set String)
runningBuildKeys = do
    jobs <- querySlurmJobs `catch` \(_ :: SomeException) -> return []
    return $ Set.fromList (map slurmJobName jobs)

isRunningOutPath :: Set String -> Text -> Bool
isRunningOutPath runningKeys outPath =
    Set.member (unBuildKey (buildKeyForOutPath (unpack outPath))) runningKeys

{- | Raw statuses and out paths for every step of a project. One derivation
lookup, one build-plan query and one slurm query answer all steps at once; if
any of those fail, the per-step probes are used instead.
-}
getRawStatusesWithPaths :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text), Map Int Text))
getRawStatusesWithPaths pid targetCommit =
    fmap (\(statuses, outPaths, _) -> (statuses, outPaths)) <$> getBatchedStatuses pid targetCommit

getBatchedStatuses :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text), Map Int Text, Maybe StepStore))
getBatchedStatuses pid targetCommit = do
    result <- getProjectOutPaths pid targetCommit
    case result of
        Left err -> return $ Left err
        Right outPaths -> do
            batched <-
                (Right <$> buildBatched outPaths)
                    `catch` \(err :: SomeException) -> do
                        putStrLn $ "Batched status probe failed, falling back to per-step probes: " ++ show err
                        statuses <- Map.fromList <$> mapConcurrently getStatusForStep (Map.toList outPaths)
                        return (Left statuses)
            case batched of
                Right (statuses, store) -> return $ Right (statuses, outPaths, Just store)
                Left statuses -> return $ Right (statuses, outPaths, Nothing)
  where
    buildBatched outPaths = do
        store <- buildStepStore outPaths
        runningKeys <- runningBuildKeys
        statuses <- rawStatusesBatched store (isRunningOutPath runningKeys) outPaths
        return (statuses, store)

    getStatusForStep (sid, path) = do
        status_ <-
            checkStatus (unpack path)
                `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
        pure (sid, status_)

getRawStatuses :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text)))
getRawStatuses pid targetCommit = do
    result <- getRawStatusesWithPaths pid targetCommit
    return $ fmap fst result
getStatuses :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text)))
getStatuses pid targetCommit = do
    rawResult <- getBatchedStatuses pid targetCommit
    case rawResult of
        Left err -> return $ Left err
        Right (statuses, outPaths, store) -> Right <$> resolveStatusesFor targetCommit outPaths store statuses

{- | Resolve the failure logs behind pending statuses. The batched walk answers
every step with a handful of processes; when the local build-log tree is not
readable it falls back to the per-step walk. The log endpoint keeps its own
online fetch through 'resolveBuildLog'.
-}
resolveStatusesFor :: Text -> Map Int Text -> Maybe StepStore -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveStatusesFor targetCommit outPaths mStore statuses = case mStore of
    Just store -> do
        batchable <- logDirectoryAvailable
        if batchable
            then resolveStatusesBatched store statuses
            else resolveStatusesAtCommitWithPaths targetCommit outPaths statuses
    Nothing -> resolveStatusesAtCommitWithPaths targetCommit outPaths statuses

broadcastProjectStatus :: Int -> Text -> Maybe (Int, (Text, Maybe Text)) -> IO ()
broadcastProjectStatus pid targetCommit mStatusOverride = do
    result <- getBatchedStatuses pid targetCommit
    case result of
        Left err -> putStrLn $ "broadcastProjectStatus skipped: " ++ err
        Right (stats, outPaths, store) -> do
            let finalStats = case mStatusOverride of
                    Just (sid, st) -> Map.insert sid st stats
                    Nothing -> stats
            let (immediate, pending) = partitionImmediateStatuses finalStats
            broadcastSnapshot pid targetCommit immediate
            when (not (Map.null pending)) $
                void $
                    forkIO $ do
                        resolved <- resolveStatusesFor targetCommit outPaths store pending
                        forM_ (Map.toList resolved) $ \(sid, status_) ->
                            broadcastSnapshot pid targetCommit (Map.singleton sid status_)

withStepProjects :: Int -> Text -> (Int -> ReadRepoContext -> IO ()) -> IO ()
withStepProjects sid targetCommit action = do
    result <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
        let ctx = ReadRepoContext repoPath (unpack targetCommit)
        output <- runNixEvalJsonInRepo ctx "#pointy.projects"
        let decodeResult = eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Map String ProjectDef)
        case decodeResult of
            Left err -> liftIO $ putStrLn $ "Error parsing #pointy.projects for step " ++ show sid ++ ": " ++ err
            Right projects -> do
                let targetProjects = filter (projectContainsStep sid) (Map.elems projects)
                liftIO $ forM_ targetProjects $ \p -> forkIO $ action (projectDefId p) ctx
    case result of
        Left err -> putStrLn $ "Error in withStepProjects for step " ++ show sid ++ ": " ++ err
        Right _ -> return ()

broadcastStatusForStepProjects :: Int -> Text -> Maybe (Text, Maybe Text) -> IO ()
broadcastStatusForStepProjects sid targetCommit mStatusOverride =
    withStepProjects sid targetCommit $ \pid _ ->
        broadcastProjectStatus pid targetCommit (fmap (sid,) mStatusOverride)

broadcastSingleStepForProjects :: Int -> Text -> FilePath -> IO ()
broadcastSingleStepForProjects sid targetCommit outPath = do
    rawStatus <- checkStatus outPath `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
    withStepProjects sid targetCommit $ \pid ctx -> do
        (_, resolvedStatus) <- resolveStepStatus ctx (Just outPath) (sid, rawStatus)
        broadcastSnapshot pid targetCommit (Map.singleton sid resolvedStatus)

broadcastFailedStepForProjects :: Int -> Text -> IO ()
broadcastFailedStepForProjects sid targetCommit =
    withStepProjects sid targetCommit $ \pid ctx -> do
        outPathsResult <- getProjectOutPaths pid targetCommit
        let mOutPath = case outPathsResult of
                Right outPaths -> fmap unpack (Map.lookup sid outPaths)
                Left _ -> Nothing
        (_, status) <- resolveStepStatus ctx mOutPath (sid, ("failure", Nothing))
        broadcastSnapshot pid targetCommit (Map.singleton sid status)

broadcastKnownStepStatus :: Int -> Text -> (Text, Maybe Text) -> IO ()
broadcastKnownStepStatus sid targetCommit status =
    withStepProjects sid targetCommit $ \pid _ ->
        broadcastSnapshot pid targetCommit (Map.singleton sid status)

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

-- | Evaluate #pointy.projects, query raw statuses for every visible project,
-- and collect step IDs whose sampled state is @running@.  Those IDs are passed
-- through 'restoreRunningStepIds' so live updates that overlap are never
-- clobbered.  Per-project and top-level errors are logged; individual failures
-- do not prevent remaining projects from being processed.
restoreRunningStatuses :: IO ()
restoreRunningStatuses = do
    eHead <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "restoreRunningStatuses: cannot read HEAD: " ++ err
        Right targetCommit ->
            restoreRunningStepIds $ do
                eProjects <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
                    let ctx = ReadRepoContext repoPath (unpack targetCommit)
                    output <- runNixEvalJsonInRepo ctx "#pointy.projects"
                    let decodeResult = eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Map String ProjectDef)
                    case decodeResult of
                        Left err -> do
                            liftIO $ putStrLn $ "restoreRunningStatuses: error parsing projects: " ++ err
                            return []
                        Right projects -> return $ filter (not . projectDefHidden) (Map.elems projects)
                case eProjects of
                    Left err -> do
                        putStrLn $ "restoreRunningStatuses: transaction error: " ++ err
                        return Set.empty
                    Right projects
                        | null projects -> return Set.empty
                        | otherwise -> do
                            results <- mapConcurrently (\p -> do
                                let pid = projectDefId p
                                rawResult <- getRawStatuses pid targetCommit
                                    `catch` \(e :: SomeException) -> do
                                        putStrLn $ "restoreRunningStatuses: error for project " ++ show pid ++ ": " ++ show e
                                        return (Right Map.empty)
                                case rawResult of
                                    Left err -> do
                                        putStrLn $ "restoreRunningStatuses: raw status error for project " ++ show pid ++ ": " ++ err
                                        return Set.empty
                                    Right statuses ->
                                        return $ Map.keysSet $ Map.filter (\(st, _) -> st == pack "running") statuses
                                ) projects
                            return $ Set.unions results

projectContainsStep :: Int -> ProjectDef -> Bool
projectContainsStep sid p =
    not (projectDefHidden p) && any isTargetStep (projectDefSteps p)
  where
    isTargetStep s = not (stepRefHidden s) && stepDefId (stepRefDef s) == sid
