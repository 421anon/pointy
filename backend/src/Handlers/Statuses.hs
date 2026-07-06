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
) where

import BuildLog (ResolvedLog (..), lastMeaningfulLine, resolveBuildLog)
import BuildRunner (BuildState (..), buildKeyForOutPath, queryState)
import Bus (broadcastSnapshot)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently, mapConcurrently_)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void, when)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
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
getRawStatusesWithPaths :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text), Map Int Text))
getRawStatusesWithPaths pid targetCommit = do
    result <- getProjectOutPaths pid targetCommit
    case result of
        Left err -> return $ Left err
        Right outPaths -> do
            rawStatuses <- Map.fromList <$> mapConcurrently getStatusForStep (Map.toList outPaths)
            return $ Right (rawStatuses, outPaths)
  where
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
    rawResult <- getRawStatusesWithPaths pid targetCommit
    case rawResult of
        Left err -> return $ Left err
        Right (statuses, outPaths) -> Right <$> resolveStatusesAtCommitWithPaths targetCommit outPaths statuses

broadcastResolvedStatusesAtCommit :: Int -> Text -> Map Int Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastResolvedStatusesAtCommit pid targetCommit outPaths statuses = do
    repoPath <- userRepoPath
    let ctx = ReadRepoContext repoPath (unpack targetCommit)
    mapConcurrently_ (broadcastResolvedStatus ctx outPaths) (Map.toList statuses)
  where
    broadcastResolvedStatus ctx outPaths (sid, entry) = do
        (sid', status_) <- resolveStepStatus ctx (fmap unpack $ Map.lookup sid outPaths) (sid, entry)
        broadcastSnapshot pid targetCommit (Map.singleton sid' status_)

broadcastProjectStatus :: Int -> Text -> Maybe (Int, (Text, Maybe Text)) -> IO ()
broadcastProjectStatus pid targetCommit mStatusOverride = do
    result <- getRawStatusesWithPaths pid targetCommit
    case result of
        Left err -> putStrLn $ "broadcastProjectStatus skipped: " ++ err
        Right (stats, outPaths) -> do
            let finalStats = case mStatusOverride of
                    Just (sid, st) -> Map.insert sid st stats
                    Nothing -> stats
            let (immediate, pending) = partitionImmediateStatuses finalStats
            broadcastSnapshot pid targetCommit immediate
            when (not (Map.null pending)) $
                void $
                    forkIO $
                        broadcastResolvedStatusesAtCommit pid targetCommit outPaths pending

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

projectContainsStep :: Int -> ProjectDef -> Bool
projectContainsStep sid p =
    not (projectDefHidden p) && any isTargetStep (projectDefSteps p)
  where
    isTargetStep s = not (stepRefHidden s) && stepDefId (stepRefDef s) == sid
