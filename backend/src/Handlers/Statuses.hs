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
) where

import Bus (broadcastSnapshot)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT (..), runExceptT)
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
import UserRepo (ReadRepoContext (..), runNix, runNixInRepo, withReadRepoTransaction)

{-# NOINLINE dependencyRunningOverrides #-}
dependencyRunningOverrides :: TVar (Map (Text, Int) Int)
dependencyRunningOverrides = unsafePerformIO $ newTVarIO Map.empty

getProjectOutPathsHandler :: Int -> Maybe Text -> Handler (Map Int Text)
getProjectOutPathsHandler pid commit = do
    eitherCommit <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return $ fromMaybe (pack hash) commit
    case eitherCommit of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right targetCommit -> liftIO $ getProjectOutPaths pid targetCommit

checkStatus :: FilePath -> IO (Text, Maybe Text)
checkStatus path = do
    let sanitizedPath = map (\c -> if c == '/' then '-' else c) (dropWhile (== '/') path)
    let unitName = "nix-build-" ++ sanitizedPath

    (exitCode, _, _) <- readProcessWithExitCodeL "systemctl" ["is-active", unitName] ""
    if exitCode == ExitSuccess
        then return ("running", Nothing)
        else do
            (failedCode, _, _) <- readProcessWithExitCodeL "systemctl" ["is-failed", unitName] ""
            if failedCode == ExitSuccess
                then do
                    (st, mErr) <- checkNixLogForFailure path
                    if st == "failure"
                        then return ("failure", mErr)
                        else return ("failure", Nothing)
                else do
                    result <- runExceptT $ runNix ["path-info", path]
                    case result of
                        Right _ -> return ("success", Nothing)
                        Left _ -> checkNixLogForFailure path

checkNixLogForFailure :: FilePath -> IO (Text, Maybe Text)
checkNixLogForFailure path = do
    derivationResult <- runExceptT $ runNix ["path-info", "--derivation", path]
    case derivationResult of
        Right derivationPath ->
            case lines derivationPath of
                derivation : _ -> do
                    logResult <- runExceptT $ runNix ["log", derivation]
                    case logResult of
                        Right logOutput -> return ("failure", lastMeaningfulLine logOutput)
                        Left _ -> return ("not-started", Nothing)
                [] -> return ("not-started", Nothing)
        Left _ -> return ("not-started", Nothing)

lastMeaningfulLine :: String -> Maybe Text
lastMeaningfulLine output =
    case filter (not . null) (lines output) of
        [] -> Nothing
        ls -> Just (pack (last ls))

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

broadcastProjectStatus :: Int -> Maybe (Int, (Text, Maybe Text)) -> IO ()
broadcastProjectStatus pid mStatusOverride = do
    result <- withReadRepoTransaction $ \(ReadRepoContext _ hash) -> ExceptT $ do
        let targetCommit = pack hash
        outPaths <- getProjectOutPaths pid targetCommit
        stats <- getStatuses targetCommit outPaths
        let finalStats = case mStatusOverride of
                Just (sid, st) -> Map.insert sid st stats
                Nothing -> stats
        broadcastSnapshot pid finalStats outPaths
        return $ Right ()
    case result of
        Left err -> putStrLn $ "Error broadcasting project status: " ++ err
        Right _ -> return ()

broadcastStatusForStepProjects :: Int -> Maybe (Text, Maybe Text) -> IO ()
broadcastStatusForStepProjects sid mStatusOverride = do
    result <- withReadRepoTransaction $ \ctx -> do
        output <- runNixInRepo ctx ["eval", "--json"] "#pointy.projects"
        let decodeResult = eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Map String ProjectDef)
        case decodeResult of
            Left err -> do
                liftIO $ putStrLn $ "Error parsing json in broadcastStatusForStepProjects for #pointy.projects: " ++ err
                return ()
            Right projects -> do
                let targetProjects = filter (projectContainsStep sid) (Map.elems projects)
                liftIO $ forM_ targetProjects $ \p ->
                    forkIO $ broadcastProjectStatus (projectDefId p) (fmap (sid,) mStatusOverride)
                return ()
    case result of
        Left err -> putStrLn $ "Error broadcasting statuses for step " ++ show sid ++ ": " ++ err
        Right _ -> return ()

projectContainsStep :: Int -> ProjectDef -> Bool
projectContainsStep sid p =
    not (projectDefHidden p) && any isTargetStep (projectDefSteps p)
  where
    isTargetStep s = not (stepRefHidden s) && stepDefId (stepRefDef s) == sid
