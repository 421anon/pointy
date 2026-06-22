{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Handlers.Statuses (
    checkStatus,
    getRawStatuses,
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
import Control.Monad.Except (runExceptT)

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import NixUtils (isValidStorePath)
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), getProjectOutPaths)
import UserRepo (ReadRepoContext (..), runNixEvalJsonInRepo, runNixEvalRawInRepo, userRepoPath, withReadRepoTransaction)

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

{- | Resolve build logs only for statuses whose durable Nix log can refine them.
Successful and running steps are already authoritative, so they return without
derivation traversal. A @"not-started"@ step is upgraded to @"failure"@ only
when the resolver finds a recorded failed build after the scheduler job has
disappeared.
-}
resolveStatuses :: ReadRepoContext -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveStatuses ctx statuses =
    Map.fromList <$> mapConcurrently (resolveStepStatus ctx) (Map.toList statuses)

resolveStepStatus :: ReadRepoContext -> (Int, (Text, Maybe Text)) -> IO (Int, (Text, Maybe Text))
resolveStepStatus _ entry@(_, status_)
    | isImmediateStatus status_ = return entry
resolveStepStatus ctx entry@(sid, (state, _))
    | state == "failure" || state == "not-started" = do
        result <- runExceptT $ runNixEvalRawInRepo ctx ("#pointy.steps." ++ show sid ++ ".outPath")
        case result of
            Left _ -> return entry
            Right outPath -> do
                mResolved <- resolveBuildLog outPath
                return $ case mResolved of
                    Just rl -> (sid, ("failure", lastMeaningfulLine (resolvedLog rl)))
                    Nothing -> entry
    | otherwise = return entry

resolveStatusesAtCommit :: Text -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveStatusesAtCommit targetCommit statuses = do
    repoPath <- userRepoPath
    resolveStatuses (ReadRepoContext repoPath (unpack targetCommit)) statuses

getRawStatuses :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text)))
getRawStatuses pid targetCommit = do
    result <- getProjectOutPaths pid targetCommit
    case result of
        Left err -> return $ Left err
        Right outPaths -> do
            rawStatuses <- Map.fromList <$> mapConcurrently getStatusForStep (Map.toList outPaths)
            return $ Right rawStatuses
  where
    getStatusForStep (sid, path) = do
        status_ <-
            checkStatus (unpack path)
                `catch` \(_ :: SomeException) -> pure ("not-started", Nothing)
        pure (sid, status_)

getStatuses :: Int -> Text -> IO (Either String (Map Int (Text, Maybe Text)))
getStatuses pid targetCommit = do
    rawResult <- getRawStatuses pid targetCommit
    case rawResult of
        Left err -> return $ Left err
        Right statuses -> Right <$> resolveStatusesAtCommit targetCommit statuses

broadcastResolvedStatusesAtCommit :: Int -> Text -> Map Int (Text, Maybe Text) -> IO ()
broadcastResolvedStatusesAtCommit pid targetCommit statuses = do
    repoPath <- userRepoPath
    let ctx = ReadRepoContext repoPath (unpack targetCommit)
    mapConcurrently_ (broadcastResolvedStatus ctx) (Map.toList statuses)
  where
    broadcastResolvedStatus ctx entry = do
        (sid, status_) <- resolveStepStatus ctx entry
        broadcastSnapshot pid targetCommit (Map.singleton sid status_)

broadcastProjectStatus :: Int -> Text -> Maybe (Int, (Text, Maybe Text)) -> IO ()
broadcastProjectStatus pid targetCommit mStatusOverride = do
    statusesResult <- getRawStatuses pid targetCommit
    case statusesResult of
        Left err -> putStrLn $ "broadcastProjectStatus skipped: " ++ err
        Right stats -> do
            let finalStats = case mStatusOverride of
                    Just (sid, st) -> Map.insert sid st stats
                    Nothing -> stats
            let (immediate, pending) = partitionImmediateStatuses finalStats
            broadcastSnapshot pid targetCommit immediate
            when (not (Map.null pending)) $
                void $
                    forkIO $
                        broadcastResolvedStatusesAtCommit pid targetCommit pending

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
        (_, resolvedStatus) <- resolveStepStatus ctx (sid, rawStatus)
        broadcastSnapshot pid targetCommit (Map.singleton sid resolvedStatus)

broadcastFailedStepForProjects :: Int -> Text -> IO ()
broadcastFailedStepForProjects sid targetCommit =
    withStepProjects sid targetCommit $ \pid ctx -> do
        (_, status) <- resolveStepStatus ctx (sid, ("failure", Nothing))
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
