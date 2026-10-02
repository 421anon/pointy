{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Statuses (
    rawStatusesFor,
    broadcastProjectStatus,
    broadcastSingleStepForProjects,
    broadcastFailedStepForProjects,
    broadcastKnownStepStatus,
    broadcastStatusForStepProjects,
    broadcastStepCertificateAtMovedHead,
    forkBroadcastProjectStatusAtHead,
    forkBroadcastStatusForStepProjectsAtHead,
    forkWarmStepCertificate,
    forkReporting,
    restoreRunningStatuses,
    trackBuild,
    projectContainsStep,
) where

import BuildLog (StepStore, resolveStatusesBatched)
import BuildRunner (buildKeyForOutPath, waitForCompletion)
import BuildStatus (StepPaths (..), checkStatus, markBuiltOutputs, partitionImmediateStatuses, resolveStatuses, resolveStepStatus)
import Bus (broadcastSnapshot)
import Certificates (ProjectDef (..), cachedProjectDefinitions, getProjectCertificates, getStepCertificate, isCertificateBuilding, rawStatusesFor, runningBuildKeys)
import ClusterBus (beginBuild, endBuild)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException)
import Control.Monad (filterM, forM_, void, when)
import Control.Monad.Except (runExceptT)

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Either (fromRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import qualified Data.Set as Set
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift), (:>), withEffToIO)
import Effectful.Exception (bracket_, catch, try)
import Effects (App, AppEffects, Nix, pathValid)
import UserRepo (ReadRepoContext (..), userRepoPath, withReadRepoTransaction)

resolveStatusesAtCommitWithCertificates :: (Nix :> es, IOE :> es) => Map Int Text -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesAtCommitWithCertificates certificates statuses =
    Map.fromList <$> liftIO (mapConcurrently (runAppEffects . resolveOne certificates) (Map.toList statuses))
  where
    resolveOne :: Map Int Text -> (Int, (Text, Maybe Text)) -> Eff AppEffects (Int, (Text, Maybe Text))
    resolveOne certificates' (sid, entry) =
        resolveStepStatus (fmap unpack $ Map.lookup sid certificates') (sid, entry)

getBatchedStatuses :: App es => Int -> Text -> Eff es (Either String (Map Int (Text, Maybe Text), Map Int StepPaths, Maybe StepStore))
getBatchedStatuses pid targetCommit = do
    result <- getProjectCertificates pid targetCommit
    case result of
        Left err -> return $ Left err
        Right certificates -> do
            batched <-
                (Right <$> rawStatusesFor (Map.map (pack . stepCertificate) certificates))
                    `catch` \(err :: SomeException) -> do
                        liftIO $ putStrLn $ "Batched status probe failed, falling back to per-step probes: " ++ show err
                        statuses <- Map.fromList <$> liftIO (mapConcurrently (runAppEffects . getStatusForStep) (Map.toList certificates))
                        return (Left statuses)
            case batched of
                Right (statuses, store) -> return $ Right (statuses, certificates, Just store)
                Left statuses -> return $ Right (statuses, certificates, Nothing)
  where
    getStatusForStep :: (Int, StepPaths) -> Eff AppEffects (Int, (Text, Maybe Text))
    getStatusForStep (sid, paths) = do
        status_ <-
            checkStatus (stepCertificate paths)
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
                        resolved <- markBuiltOutputs certificates =<< resolveStatusesFor (Map.map (pack . stepCertificate) certificates) store pending
                        liftIO $
                            forM_ (Map.toList resolved) $ \(sid, status_) ->
                                broadcastSnapshot pid targetCommit (Map.singleton sid status_)

withStepProjects :: App es => Int -> Text -> (Int -> Eff es ()) -> Eff es ()
withStepProjects sid targetCommit action = do
    result <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
        let ctx = ReadRepoContext repoPath (unpack targetCommit)
        projects <- cachedProjectDefinitions ctx
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
                Just paths -> checkStatus (stepCertificate paths) `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
                Nothing -> pure ("not-started", Nothing)
            withStepProjects sid targetCommit $ \pid ->
                broadcastStepStatusResolved pid targetCommit sid certificate rawStatus

broadcastStepStatusResolved :: App es => Int -> Text -> Int -> Maybe StepPaths -> (Text, Maybe Text) -> Eff es ()
broadcastStepStatusResolved pid targetCommit sid certificate rawStatus = do
    broadcastMarked rawStatus
    (_, resolvedStatus) <- resolveStepStatus (stepCertificate <$> certificate) (sid, rawStatus)
    when (resolvedStatus /= rawStatus) $
        broadcastMarked resolvedStatus
  where
    broadcastMarked status = liftIO . broadcastSnapshot pid targetCommit =<< markBuiltOutputs (foldMap (Map.singleton sid) certificate) (Map.singleton sid status)

broadcastStatusForStepProjects :: App es => Int -> Text -> Maybe (Text, Maybe Text) -> Eff es ()
broadcastStatusForStepProjects sid targetCommit mStatusOverride =
    withStepProjects sid targetCommit $ \pid ->
        broadcastProjectStatus pid targetCommit (fmap (sid,) mStatusOverride)

broadcastSingleStepForProjects :: App es => Int -> Text -> FilePath -> Eff es ()
broadcastSingleStepForProjects sid targetCommit certificate = do
    rawStatus <- checkStatus certificate `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
    withStepProjects sid targetCommit $ \pid ->
        broadcastStepStatusResolved pid targetCommit sid (Just (StepPaths certificate certificate)) rawStatus

broadcastFailedStepForProjects :: App es => Int -> Text -> Eff es ()
broadcastFailedStepForProjects sid targetCommit =
    withStepProjects sid targetCommit $ \pid -> do
        certificates <- fromRight Map.empty <$> getProjectCertificates pid targetCommit
        liftIO . broadcastSnapshot pid targetCommit =<< resolveStatuses certificates (Map.singleton sid ("failure", Nothing))

broadcastKnownStepStatus :: App es => Int -> Text -> (Text, Maybe Text) -> Eff es ()
broadcastKnownStepStatus sid targetCommit status =
    withStepProjects sid targetCommit $ \pid ->
        liftIO $ broadcastSnapshot pid targetCommit (Map.singleton sid status)

trackBuild :: (IOE :> es) => Int -> Text -> Eff es a -> Eff es a
trackBuild sid commit = bracket_ (liftIO $ beginBuild sid commit) (liftIO $ endBuild sid commit)

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

broadcastStepCertificateAtMovedHead :: App es => Int -> Text -> Eff es ()
broadcastStepCertificateAtMovedHead sid buildCommit = do
    eHead <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> liftIO $ putStrLn $ "broadcastStepCertificateAtMovedHead skipped: " ++ err
        Right c -> when (c /= buildCommit) $ broadcastStepCertificateForProjects sid c

warmStepCertificate :: Int -> Text -> Eff AppEffects ()
warmStepCertificate sid targetCommit = do
    repoPath <- liftIO userRepoPath
    let ctx = ReadRepoContext repoPath (unpack targetCommit)
    membership <- runExceptT $ cachedProjectDefinitions ctx
    certificate <- getStepCertificate sid targetCommit
    liftIO $
        case (membership, certificate) of
            (_, Left err) -> putStrLn $ "Step warm skipped for step " ++ show sid ++ ": " ++ err
            (Left err, _) -> putStrLn $ "Step membership warm skipped for step " ++ show sid ++ ": " ++ err
            _ -> return ()

forkWarmStepCertificate :: Int -> Text -> IO ()
forkWarmStepCertificate sid targetCommit =
    forkReporting ("Step warm for step " ++ show sid) (warmStepCertificate sid targetCommit)

restoreRunningStatuses :: App es => Eff es ()
restoreRunningStatuses = do
    eHead <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (pack hash)
    case eHead of
        Left err -> liftIO $ putStrLn $ "restoreRunningStatuses: cannot read HEAD: " ++ err
        Right targetCommit -> do
            eProjects <-
                withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> do
                    let ctx = ReadRepoContext repoPath (unpack targetCommit)
                    Map.elems <$> cachedProjectDefinitions ctx
            case eProjects of
                Left err -> liftIO $ putStrLn $ "restoreRunningStatuses: transaction error: " ++ err
                Right projects -> trackRunningBuilds targetCommit projects

trackRunningBuilds :: App es => Text -> [ProjectDef] -> Eff es ()
trackRunningBuilds targetCommit projects = do
    runningKeys <- runningBuildKeys
    if Set.null runningKeys
        then return ()
        else do
            candidates <- Map.unions <$> mapM (buildingCertificates targetCommit runningKeys) projects
            uncertified <- filterM (fmap not . pathValid . unpack . snd) (Map.toList candidates)
            liftIO $
                forM_ uncertified $ \(sid, certificate) ->
                    forkReporting ("Restored build watch for step " ++ show sid) $
                        trackBuild sid targetCommit (waitForCompletion (buildKeyForOutPath (unpack certificate)))

buildingCertificates :: App es => Text -> Set String -> ProjectDef -> Eff es (Map Int Text)
buildingCertificates targetCommit runningKeys project = do
    result <-
        getProjectCertificates pid targetCommit
            `catch` \(err :: SomeException) -> return (Left (show err))
    case result of
        Left err -> do
            liftIO $ putStrLn $ "restoreRunningStatuses: certificates unavailable for project " ++ show pid ++ ": " ++ err
            return Map.empty
        Right certificates -> return $ Map.filter (isCertificateBuilding runningKeys) (Map.map (pack . stepCertificate) certificates)
  where
    pid = projectDefId project

projectContainsStep :: Int -> ProjectDef -> Bool
projectContainsStep sid p = sid `elem` projectDefStepIds p
