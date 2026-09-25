{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Statuses (
    checkStatus,
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

import BuildLog (ResolvedLog (..), StepStore, buildStepStore, lastMeaningfulLine, lookupDeriver, rawStatusesBatched, resolveBuildLog, resolveStatusesBatched)
import BuildRunner (BuildKey (..), BuildState (..), buildKeyForOutPath, queryState, querySlurmJobs, slurmJobName)
import Bus (broadcastSnapshot)
import ClusterBus (restoreRunningStepIds)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException)
import Control.Monad (filterM, forM_, void, when)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import qualified Data.Set as Set
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift), (:>), withEffToIO)
import Effectful.Exception (catch)
import Effects (App, AppEffects, Nix, Slurm, pathValid)
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), decodeProjectDefinitions, evalProjectDefinitions, getProjectCertificates, getStepCertificate)
import UserRepo (ReadRepoContext (..), userRepoPath, withReadRepoTransaction)

checkStatus :: (Nix :> es, Slurm :> es) => FilePath -> Eff es (Text, Maybe Text)
checkStatus certificate = do
    valid <- pathValid certificate
    if valid
        then return ("success", Nothing)
        else do
            state <- queryState $ buildKeyForOutPath certificate
            return $ case state of
                BRunning -> ("running", Nothing)
                BAbsent -> ("not-started", Nothing)
                BSucceeded -> ("success", Nothing)
                BFailed -> ("failure", Nothing)

isImmediateStatus :: (Text, Maybe Text) -> Bool
isImmediateStatus (state, _) = state == "success" || state == "running"

partitionImmediateStatuses :: Map Int (Text, Maybe Text) -> (Map Int (Text, Maybe Text), Map Int (Text, Maybe Text))
partitionImmediateStatuses = Map.partition isImmediateStatus

resolveStepStatus :: (Nix :> es, IOE :> es) => ReadRepoContext -> Maybe FilePath -> (Int, (Text, Maybe Text)) -> Eff es (Int, (Text, Maybe Text))
resolveStepStatus _ _ entry@(_, status_)
    | isImmediateStatus status_ = return entry
resolveStepStatus _ Nothing entry@(_, (state, _))
    | state == "failure" || state == "not-started" = return entry
resolveStepStatus _ Nothing entry = return entry
resolveStepStatus _ (Just certificate) entry@(sid, (state, _))
    | state == "failure" || state == "not-started" = do
        mDrv <- lookupDeriver certificate
        mResolved <- maybe (return Nothing) resolveBuildLog mDrv
        return $ case mResolved of
            Just rl -> (sid, ("failure", lastMeaningfulLine (resolvedLog rl)))
            Nothing -> entry
    | otherwise = return entry

resolveStatusesAtCommitWithCertificates :: (Nix :> es, IOE :> es) => Text -> Map Int Text -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesAtCommitWithCertificates targetCommit certificates statuses = do
    repoPath <- liftIO userRepoPath
    let ctx = ReadRepoContext repoPath (unpack targetCommit)
    Map.fromList <$> liftIO (mapConcurrently (runAppEffects . resolveOne ctx certificates) (Map.toList statuses))
  where
    resolveOne :: ReadRepoContext -> Map Int Text -> (Int, (Text, Maybe Text)) -> Eff AppEffects (Int, (Text, Maybe Text))
    resolveOne ctx' certificates' (sid, entry) =
        resolveStepStatus ctx' (fmap unpack $ Map.lookup sid certificates') (sid, entry)

runningBuildKeys :: (Slurm :> es) => Eff es (Set String)
runningBuildKeys = do
    jobs <- querySlurmJobs
    return $ Set.fromList (map slurmJobName jobs)

isCertificateBuilding :: Set String -> Text -> Bool
isCertificateBuilding runningKeys certificate =
    Set.member (unBuildKey (buildKeyForOutPath (unpack certificate))) runningKeys

getBatchedStatuses :: App es => Int -> Text -> Eff es (Either String (Map Int (Text, Maybe Text), Map Int Text, Maybe StepStore))
getBatchedStatuses pid targetCommit = do
    result <- getProjectCertificates pid targetCommit
    case result of
        Left err -> return $ Left err
        Right certificates -> do
            batched <-
                (Right <$> buildBatched certificates)
                    `catch` \(err :: SomeException) -> do
                        liftIO $ putStrLn $ "Batched status probe failed, falling back to per-step probes: " ++ show err
                        statuses <- Map.fromList <$> liftIO (mapConcurrently (runAppEffects . getStatusForStep) (Map.toList certificates))
                        return (Left statuses)
            case batched of
                Right (statuses, store) -> return $ Right (statuses, certificates, Just store)
                Left statuses -> return $ Right (statuses, certificates, Nothing)
  where
    buildBatched :: (Nix :> es', Slurm :> es', IOE :> es') => Map Int Text -> Eff es' (Map Int (Text, Maybe Text), StepStore)
    buildBatched certificates = do
        store <- buildStepStore certificates
        runningKeys <- runningBuildKeys
        statuses <- rawStatusesBatched store (isCertificateBuilding runningKeys) certificates
        return (statuses, store)

    getStatusForStep :: (Int, Text) -> Eff AppEffects (Int, (Text, Maybe Text))
    getStatusForStep (sid, certificate) = do
        status_ <-
            checkStatus (unpack certificate)
                `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
        pure (sid, status_)

resolveStatusesFor :: (Nix :> es, IOE :> es) => Text -> Map Int Text -> Maybe StepStore -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesFor targetCommit certificates mStore statuses = case mStore of
    Just store -> resolveStatusesBatched store statuses
    Nothing -> resolveStatusesAtCommitWithCertificates targetCommit certificates statuses

broadcastProjectStatus :: App es => Int -> Text -> Maybe (Int, (Text, Maybe Text)) -> Eff es ()
broadcastProjectStatus pid targetCommit mStatusOverride = do
    result <- getBatchedStatuses pid targetCommit
    case result of
        Left err -> liftIO $ putStrLn $ "broadcastProjectStatus skipped: " ++ err
        Right (stats, certificates, store) -> do
            let finalStats = case mStatusOverride of
                    Just (sid, st) -> Map.insert sid st stats
                    Nothing -> stats
            let (immediate, pending) = partitionImmediateStatuses finalStats
            liftIO $ broadcastSnapshot pid targetCommit immediate
            when (not (Map.null pending)) $
                void $
                    liftIO $
                        forkIO $
                            runAppEffects $ do
                                resolved <- resolveStatusesFor targetCommit certificates store pending
                                liftIO $
                                    forM_ (Map.toList resolved) $ \(sid, status_) ->
                                        broadcastSnapshot pid targetCommit (Map.singleton sid status_)

withStepProjects :: App es => Int -> Text -> (Int -> ReadRepoContext -> Eff es ()) -> Eff es ()
withStepProjects sid targetCommit action = do
    result <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
        let ctx = ReadRepoContext repoPath (unpack targetCommit)
        output <- evalProjectDefinitions ctx
        case decodeProjectDefinitions output of
            Left err -> liftIO $ putStrLn $ "Error for step " ++ show sid ++ ": " ++ err
            Right projects -> do
                let targetProjects = filter (projectContainsStep sid) (Map.elems projects)
                lift $
                    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
                        forM_ targetProjects $ \p -> void $ forkIO $ unlift $ action (projectDefId p) ctx
    case result of
        Left err -> liftIO $ putStrLn $ "Error in withStepProjects for step " ++ show sid ++ ": " ++ err
        Right _ -> return ()

broadcastStepCertificateForProjects :: App es => Int -> Text -> Eff es ()
broadcastStepCertificateForProjects sid targetCommit = do
    eCertificate <- getStepCertificate sid targetCommit
    case eCertificate of
        Left err -> liftIO $ putStrLn $ "Step certificate probe skipped for step " ++ show sid ++ ": " ++ err
        Right certificate -> do
            rawStatus <- case certificate of
                Just path -> checkStatus (unpack path) `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
                Nothing -> pure ("not-started", Nothing)
            withStepProjects sid targetCommit $ \pid ctx -> do
                (_, resolvedStatus) <- resolveStepStatus ctx (unpack <$> certificate) (sid, rawStatus)
                liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid resolvedStatus)

broadcastStatusForStepProjects :: App es => Int -> Text -> Maybe (Text, Maybe Text) -> Eff es ()
broadcastStatusForStepProjects sid targetCommit mStatusOverride =
    withStepProjects sid targetCommit $ \pid _ ->
        broadcastProjectStatus pid targetCommit (fmap (sid,) mStatusOverride)

broadcastSingleStepForProjects :: App es => Int -> Text -> FilePath -> Eff es ()
broadcastSingleStepForProjects sid targetCommit certificate = do
    rawStatus <- checkStatus certificate `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
    withStepProjects sid targetCommit $ \pid ctx -> do
        (_, resolvedStatus) <- resolveStepStatus ctx (Just certificate) (sid, rawStatus)
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid resolvedStatus)

broadcastFailedStepForProjects :: App es => Int -> Text -> Eff es ()
broadcastFailedStepForProjects sid targetCommit =
    withStepProjects sid targetCommit $ \pid ctx -> do
        certificatesResult <- getProjectCertificates pid targetCommit
        let mCertificate = case certificatesResult of
                Right certificates -> fmap unpack (Map.lookup sid certificates)
                Left _ -> Nothing
        (_, status) <- resolveStepStatus ctx mCertificate (sid, ("failure", Nothing))
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid status)

broadcastKnownStepStatus :: App es => Int -> Text -> (Text, Maybe Text) -> Eff es ()
broadcastKnownStepStatus sid targetCommit status =
    withStepProjects sid targetCommit $ \pid _ ->
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid status)

forkBroadcastProjectStatusAtHead :: Int -> IO ()
forkBroadcastProjectStatusAtHead pid = do
    eHead <- runAppEffects $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "forkBroadcastProjectStatusAtHead skipped: " ++ err
        Right c -> void $ forkIO $ runAppEffects $ broadcastProjectStatus pid c Nothing

forkBroadcastStatusForStepProjectsAtHead :: Int -> IO ()
forkBroadcastStatusForStepProjectsAtHead sid = do
    eHead <- runAppEffects $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "forkBroadcastStatusForStepProjectsAtHead skipped: " ++ err
        Right c ->
            void $
                forkIO $
                    runAppEffects $ do
                        broadcastStepCertificateForProjects sid c
                        broadcastStatusForStepProjects sid c Nothing

restoreRunningStatuses :: App es => Eff es ()
restoreRunningStatuses = do
    eHead <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> liftIO $ putStrLn $ "restoreRunningStatuses: cannot read HEAD: " ++ err
        Right targetCommit ->
            liftIO $
                restoreRunningStepIds $
                    runAppEffects $ do
                        eProjects <-
                            withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
                                let ctx = ReadRepoContext repoPath (unpack targetCommit)
                                output <- evalProjectDefinitions ctx
                                case decodeProjectDefinitions output of
                                    Left err -> do
                                        liftIO $ putStrLn $ "restoreRunningStatuses: error parsing projects: " ++ err
                                        return []
                                    Right projects -> return $ filter (not . projectDefHidden) (Map.elems projects)
                        case eProjects of
                            Left err -> do
                                liftIO $ putStrLn $ "restoreRunningStatuses: transaction error: " ++ err
                                return Set.empty
                            Right projects -> buildingStepIds targetCommit projects

buildingStepIds :: App es => Text -> [ProjectDef] -> Eff es (Set Int)
buildingStepIds targetCommit projects = do
    runningKeys <- runningBuildKeys
    if Set.null runningKeys
        then return Set.empty
        else do
            candidates <- Map.unions <$> mapM (buildingCertificates targetCommit runningKeys) projects
            uncertified <- filterM (fmap not . pathValid . unpack . snd) (Map.toList candidates)
            return $ Set.fromList (map fst uncertified)

buildingCertificates :: App es => Text -> Set String -> ProjectDef -> Eff es (Map Int Text)
buildingCertificates targetCommit runningKeys project = do
    result <-
        getProjectCertificates pid targetCommit
            `catch` \(err :: SomeException) -> return (Left (show err))
    case result of
        Left err -> do
            liftIO $ putStrLn $ "restoreRunningStatuses: certificates unavailable for project " ++ show pid ++ ": " ++ err
            return Map.empty
        Right certificates -> return $ Map.filter (isCertificateBuilding runningKeys) certificates
  where
    pid = projectDefId project

projectContainsStep :: Int -> ProjectDef -> Bool
projectContainsStep sid p =
    not (projectDefHidden p) && any isTargetStep (projectDefSteps p)
  where
    isTargetStep s = not (stepRefHidden s) && stepDefId (stepRefDef s) == sid
