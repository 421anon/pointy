{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Statuses (
    broadcastProjectStatus,
    broadcastSingleStepForProjects,
    broadcastFailedStepForProjects,
    broadcastKnownStepStatus,
    broadcastStatusForStepProjects,
    forkBroadcastProjectStatusAtHead,
    forkBroadcastStatusForStepProjectsAtHead,
    forkReporting,
    restoreRunningStatuses,
    projectContainsStep,
) where

import BuildLog (StepStore, buildStepStore, rawStatusesBatched, resolveStatusesBatched)
import BuildRunner (BuildKey (..), buildKeyForOutPath, querySlurmJobs, slurmJobName)
import BuildStatus (checkStatus, partitionImmediateStatuses, resolveStepStatus)
import Bus (broadcastSnapshot)
import Certificates (ProjectDef (..), StepDef (..), StepRef (..), decodeProjectDefinitions, evalProjectDefinitions, getProjectCertificates, getStepCertificate)
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
import Effectful.Exception (catch, try)
import Effects (App, AppEffects, Nix, Slurm, pathValid)
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

resolveStatusesAtCommitWithCertificates :: (Nix :> es, IOE :> es) => Map Int Text -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesAtCommitWithCertificates certificates statuses =
    Map.fromList <$> liftIO (mapConcurrently (runAppEffects . resolveOne certificates) (Map.toList statuses))
  where
    resolveOne :: Map Int Text -> (Int, (Text, Maybe Text)) -> Eff AppEffects (Int, (Text, Maybe Text))
    resolveOne certificates' (sid, entry) =
        resolveStepStatus (fmap unpack $ Map.lookup sid certificates') (sid, entry)

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

resolveStatusesFor :: (Nix :> es, IOE :> es) => Map Int Text -> Maybe StepStore -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesFor certificates mStore statuses = case mStore of
    Just store -> resolveStatusesBatched store statuses
    Nothing -> resolveStatusesAtCommitWithCertificates certificates statuses

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
                liftIO $
                    forkReporting ("Pending status resolution for project " ++ show pid) $ do
                        resolved <- resolveStatusesFor certificates store pending
                        liftIO $
                            forM_ (Map.toList resolved) $ \(sid, status_) ->
                                broadcastSnapshot pid targetCommit (Map.singleton sid status_)

withStepProjects :: App es => Int -> Text -> (Int -> Eff es ()) -> Eff es ()
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
                        forM_ targetProjects $ \p ->
                            void $
                                forkIO $
                                    unlift $ do
                                        outcome <- try (action (projectDefId p))
                                        case outcome of
                                            Left (err :: SomeException) -> liftIO $ putStrLn $ "Step status broadcast for step " ++ show sid ++ " failed: " ++ show err
                                            Right () -> return ()
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
            withStepProjects sid targetCommit $ \pid -> do
                (_, resolvedStatus) <- resolveStepStatus (unpack <$> certificate) (sid, rawStatus)
                liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid resolvedStatus)

broadcastStatusForStepProjects :: App es => Int -> Text -> Maybe (Text, Maybe Text) -> Eff es ()
broadcastStatusForStepProjects sid targetCommit mStatusOverride =
    withStepProjects sid targetCommit $ \pid ->
        broadcastProjectStatus pid targetCommit (fmap (sid,) mStatusOverride)

broadcastSingleStepForProjects :: App es => Int -> Text -> FilePath -> Eff es ()
broadcastSingleStepForProjects sid targetCommit certificate = do
    rawStatus <- checkStatus certificate `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
    withStepProjects sid targetCommit $ \pid -> do
        (_, resolvedStatus) <- resolveStepStatus (Just certificate) (sid, rawStatus)
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid resolvedStatus)

broadcastFailedStepForProjects :: App es => Int -> Text -> Eff es ()
broadcastFailedStepForProjects sid targetCommit =
    withStepProjects sid targetCommit $ \pid -> do
        certificatesResult <- getProjectCertificates pid targetCommit
        let mCertificate = case certificatesResult of
                Right certificates -> fmap unpack (Map.lookup sid certificates)
                Left _ -> Nothing
        (_, status) <- resolveStepStatus mCertificate (sid, ("failure", Nothing))
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid status)

broadcastKnownStepStatus :: App es => Int -> Text -> (Text, Maybe Text) -> Eff es ()
broadcastKnownStepStatus sid targetCommit status =
    withStepProjects sid targetCommit $ \pid ->
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid status)

forkReporting :: String -> Eff AppEffects () -> IO ()
forkReporting label action =
    void $
        forkIO $
            runAppEffects $ do
                outcome <- try action
                case outcome of
                    Left (err :: SomeException) -> liftIO $ putStrLn $ label ++ " failed: " ++ show err
                    Right () -> return ()

forkBroadcastProjectStatusAtHead :: Int -> IO ()
forkBroadcastProjectStatusAtHead pid = do
    eHead <- runAppEffects $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "forkBroadcastProjectStatusAtHead skipped: " ++ err
        Right c -> forkReporting ("Project status broadcast for " ++ show pid) (broadcastProjectStatus pid c Nothing)

forkBroadcastStatusForStepProjectsAtHead :: Int -> IO ()
forkBroadcastStatusForStepProjectsAtHead sid = do
    eHead <- runAppEffects $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> putStrLn $ "forkBroadcastStatusForStepProjectsAtHead skipped: " ++ err
        Right c ->
            forkReporting ("Step status broadcast for step " ++ show sid) $ do
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
                                    Right projects -> return (Map.elems projects)
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
projectContainsStep sid p = any ((== sid) . stepDefId . stepRefDef) (projectDefSteps p)
