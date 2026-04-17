{-# LANGUAGE OverloadedStrings #-}

module Handlers.RunStep (
    runStepHandler,
    stopStepHandler,
) where

import Cache (getOutPathFromCache, memoizeStepOutPaths)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (bracket_)

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Statuses (addDependencyRunningOverrides, broadcastStatusForStepProjects, removeDependencyRunningOverrides)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, NoContent (..))
import System.Directory (createDirectoryIfMissing, findExecutable, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import UserRepo (ReadRepoContext (..), runNixInRepo, withReadRepoTransaction)

runStepHandler :: Int -> Maybe T.Text -> Handler NoContent
runStepHandler eid commit = do
    _ <- liftIO $ forkIO $ runStepSync eid commit
    return NoContent

stopStepHandler :: Int -> Maybe T.Text -> Handler NoContent
stopStepHandler eid commit = do
    liftIO $ stopStepSync eid commit
    return NoContent

runStepSync :: Int -> Maybe T.Text -> IO ()
runStepSync eid commit = do
    result <- runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
        depIds <- getDependencies ctx eid
        let stepIds = depIds ++ [eid]
        let targetCommitText = T.pack targetCommit
        liftIO $ putStrLn $ "runStep " ++ show eid ++ " dependencies: " ++ show depIds

        ensureOutPathsCached ctx stepIds
        liftIO $
            bracket_
                (addDependencyRunningOverrides targetCommitText stepIds)
                ( do
                    removeDependencyRunningOverrides targetCommitText stepIds
                    mapM_ (`broadcastStatusForStepProjects` Nothing) stepIds
                )
                ( do
                    mapM_ (`broadcastStatusForStepProjects` Nothing) stepIds
                    mapConcurrently_ (buildStep ctx) stepIds
                )

    case result of
        Left err -> putStrLn $ "runStepAsync error: " ++ err
        Right _ -> return ()

buildStep :: ReadRepoContext -> Int -> IO ()
buildStep ctx@(ReadRepoContext repoPath targetCommit) eid = do
    result <- runExceptT $ do
        let flakeRefBase = "git+file://" ++ repoPath ++ "?rev=" ++ targetCommit ++ "&allRefs=true"

        mGitExe <- liftIO $ findExecutable "git"
        gitExe <- case mGitExe of
            Nothing -> throwError "git executable not found"
            Just exe -> return exe

        let pathEnvArg = ["--setenv=PATH=" ++ takeDirectory gitExe]

        outPathText <- requireOutPathFromCache ctx eid
        let outPath = T.unpack outPathText
        built <- liftIO $ isBuilt outPath
        if built
            then liftIO $ broadcastStatusForStepProjects eid Nothing
            else do
                let unitName = outPathToUnitName outPath
                _ <- liftIO $ readProcessWithExitCodeL "systemctl" ["reset-failed", unitName] ""
                liftIO $ broadcastStatusForStepProjects eid (Just ("running", Nothing))
                _ <-
                    liftIO $
                        readProcessWithExitCodeL
                            "systemd-run"
                            ( [ "--uid=backend"
                              , "--gid=backend"
                              , "--slice=pointy-builds.slice"
                              , "--unit=" ++ unitName
                              , "--collect"
                              , "--wait"
                              ]
                                ++ pathEnvArg
                                ++ ["nix", "build", "--no-link", "--no-eval-cache", flakeRefBase ++ "#trotter.steps." ++ show eid]
                            )
                            ""
                _ <- liftIO $ registerGcRootForOutPath outPath
                liftIO $ broadcastStatusForStepProjects eid Nothing

    case result of
        Left err -> putStrLn $ "buildStep error: " ++ err
        Right _ -> return ()

ensureOutPathsCached :: ReadRepoContext -> [Int] -> ExceptT String IO ()
ensureOutPathsCached ctx@(ReadRepoContext _ targetCommit) stepIds = do
    let targetCommitText = T.pack targetCommit
    cachedOutPaths <- liftIO $ mapM (getOutPathFromCache targetCommitText) stepIds
    when (any isNothing cachedOutPaths) $ cacheProjectOutPathsForCommit ctx

requireOutPathFromCache :: ReadRepoContext -> Int -> ExceptT String IO T.Text
requireOutPathFromCache (ReadRepoContext _ targetCommit) eid = do
    let targetCommitText = T.pack targetCommit
    mOutPath <- liftIO $ getOutPathFromCache targetCommitText eid
    case mOutPath of
        Just outPathText -> return outPathText
        Nothing -> throwError $ "outPath not found in cache for step " ++ show eid

cacheProjectOutPathsForCommit :: ReadRepoContext -> ExceptT String IO ()
cacheProjectOutPathsForCommit ctx@(ReadRepoContext _ targetCommit) = do
    output <- runNixInRepo ctx ["eval", "--json"] "#trotter.projectOutPaths"
    projectOutPaths <-
        ExceptT $ do
            return $
                case eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Map.Map Int (Map.Map Int FilePath)) of
                    Left err -> Left $ "Failed to parse #trotter.projectOutPaths: " ++ err
                    Right paths -> Right paths

    liftIO $
        mapM_
            ( \(pid, paths) -> do
                let textPaths = Map.map T.pack paths
                _ <- memoizeStepOutPaths pid (T.pack targetCommit) (return textPaths)
                return ()
            )
            (Map.toList projectOutPaths)

getDependencies :: ReadRepoContext -> Int -> ExceptT String IO [Int]
getDependencies ctx stepId = do
    result <- liftIO $ runExceptT $ runNixInRepo ctx ["eval", "--json"] ("#trotter.dependencies." ++ show stepId)
    case result of
        Left _ -> return []
        Right stdout ->
            case eitherDecode (TLE.encodeUtf8 (TL.pack stdout)) :: Either String [String] of
                Left _ -> return []
                Right ids -> return $ map read ids

isBuilt :: FilePath -> IO Bool
isBuilt path = do
    (exitCode, _, _) <- readProcessWithExitCodeL "nix" ["path-info", path] ""
    return $ exitCode == ExitSuccess

registerGcRootForOutPath :: FilePath -> IO ()
registerGcRootForOutPath outPath = do
    home <- getHomeDirectory
    let gcRootDir = home </> ".local" </> "state" </> "pointy" </> "gc-roots"
        gcRootPath = gcRootDir </> takeFileName outPath
    createDirectoryIfMissing True gcRootDir
    _ <- readProcessWithExitCodeL "nix-store" ["--add-root", gcRootPath, "--realise", outPath] ""
    return ()

stopStepSync :: Int -> Maybe T.Text -> IO ()
stopStepSync eid commit = do
    result <- runExceptT $ do
        targetCommit <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext _ commitHash) ->
                    return (fromMaybe (T.pack commitHash) commit)

        mOutPath <- liftIO $ getOutPathFromCache targetCommit eid
        case mOutPath of
            Nothing -> throwError "outPath not found in cache"
            Just outPathText -> do
                let unitName = outPathToUnitName $ T.unpack outPathText
                _ <- liftIO $ readProcessWithExitCodeL "systemctl" ["stop", unitName] ""
                liftIO $ removeDependencyRunningOverrides targetCommit [eid]
                liftIO $ broadcastStatusForStepProjects eid Nothing

    case result of
        Left err -> putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()

outPathToUnitName :: String -> String
outPathToUnitName outPath =
    let sanitizedPath = map (\c -> if c == '/' then '-' else c) (dropWhile (== '/') outPath)
     in "nix-build-" ++ sanitizedPath
