{-# LANGUAGE OverloadedStrings #-}

module Handlers.RunStep (
    runStepHandler,
    stepLogHandler,
    stopStepHandler,
) where

import BuildLog (LogSource (..), ResolvedLog (..), resolveBuildLog)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (bracket_)

import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Statuses (addDependencyRunningOverrides, broadcastStatusForStepProjects, removeDependencyRunningOverrides)
import OutPaths (warmProjectOutPathsForCommit)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, NoContent (..), err404, err500, errBody)
import System.Directory (createDirectoryIfMissing, findExecutable, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import UserRepo (ReadRepoContext (..), runNixEvalJsonInRepo, runNixEvalRawInRepo, withReadRepoTransaction)

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

        warmProjectOutPathsForCommit ctx
        liftIO $
            bracket_
                (addDependencyRunningOverrides targetCommitText stepIds)
                ( do
                    removeDependencyRunningOverrides targetCommitText stepIds
                    mapM_ (\sid -> broadcastStatusForStepProjects sid targetCommitText Nothing) stepIds
                )
                ( do
                    mapM_ (\sid -> broadcastStatusForStepProjects sid targetCommitText Nothing) stepIds
                    mapConcurrently_ (buildStep ctx) stepIds
                )

    case result of
        Left err -> putStrLn $ "runStepAsync error: " ++ err
        Right _ -> return ()

stepLogHandler :: Int -> Maybe T.Text -> Handler T.Text
stepLogHandler eid commit = do
    result <- liftIO $ runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
        liftIO $ resolveBuildLog (stepInstallable ctx eid)

    case result of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right Nothing ->
            throwError $
                err404
                    { errBody =
                        TLE.encodeUtf8 (TL.pack ("No build log available for step " ++ show eid))
                    }
        Right (Just rl) -> return (renderResolvedLog rl)

{- | Render a resolved log for the wire. Logs that come from an input
derivation (rather than the step itself) are prefixed so the user knows
the failure originated in a build prerequisite.
-}
renderResolvedLog :: ResolvedLog -> T.Text
renderResolvedLog (ResolvedLog _ logText StepDrv) = T.pack logText
renderResolvedLog (ResolvedLog drv logText (InputDrv _ _)) =
    T.pack ("Build prerequisite failed: " ++ drv ++ "\n-----\n" ++ logText)

stepInstallable :: ReadRepoContext -> Int -> String
stepInstallable (ReadRepoContext repoPath targetCommit) eid =
    "git+file://" ++ repoPath ++ "?rev=" ++ targetCommit ++ "&allRefs=true#pointy.steps." ++ show eid

buildStep :: ReadRepoContext -> Int -> IO ()
buildStep ctx eid = do
    result <- runExceptT $ do
        mGitExe <- liftIO $ findExecutable "git"
        gitExe <- case mGitExe of
            Nothing -> throwError "git executable not found"
            Just exe -> return exe

        let pathEnvArg = ["--setenv=PATH=" ++ takeDirectory gitExe]

        outPathText <- getStepOutPath ctx eid
        let outPath = T.unpack outPathText
        let targetCommitText = T.pack (readCommitHash ctx)
        built <- liftIO $ isBuilt outPath
        if built
            then liftIO $ broadcastStatusForStepProjects eid targetCommitText Nothing
            else do
                let unitName = outPathToUnitName outPath
                _ <- liftIO $ readProcessWithExitCodeL "systemctl" ["reset-failed", unitName] ""
                liftIO $ broadcastStatusForStepProjects eid targetCommitText (Just ("running", Nothing))
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
                                ++ ["nix", "build", "--no-link", "--no-eval-cache", stepInstallable ctx eid]
                            )
                            ""
                _ <- liftIO $ registerGcRootForOutPath outPath
                liftIO $ broadcastStatusForStepProjects eid targetCommitText Nothing

    case result of
        Left err -> putStrLn $ "buildStep error: " ++ err
        Right _ -> return ()

getStepOutPath :: ReadRepoContext -> Int -> ExceptT String IO T.Text
getStepOutPath ctx eid = do
    output <- runNixEvalRawInRepo ctx ("#pointy.steps." ++ show eid ++ ".outPath")
    return $ T.pack output

getDependencies :: ReadRepoContext -> Int -> ExceptT String IO [Int]
getDependencies ctx stepId = do
    result <- liftIO $ runExceptT $ runNixEvalJsonInRepo ctx ("#pointy.dependencies." ++ show stepId)
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
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    let targetCommit = fromMaybe (T.pack commitHash) commit
                     in return (repoPath, targetCommit)

        outPathText <- getStepOutPath (ReadRepoContext repoPath (T.unpack targetCommit)) eid
        let unitName = outPathToUnitName $ T.unpack outPathText
        _ <- liftIO $ readProcessWithExitCodeL "systemctl" ["stop", unitName] ""
        liftIO $ removeDependencyRunningOverrides targetCommit [eid]
        liftIO $ broadcastStatusForStepProjects eid targetCommit Nothing

    case result of
        Left err -> putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()

outPathToUnitName :: String -> String
outPathToUnitName outPath =
    let sanitizedPath = map (\c -> if c == '/' then '-' else c) (dropWhile (== '/') outPath)
     in "nix-build-" ++ sanitizedPath
