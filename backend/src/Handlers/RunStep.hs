{-# LANGUAGE OverloadedStrings #-}

module Handlers.RunStep (
    runStepHandler,
    stepLogHandler,
    stopStepHandler,
) where

import BuildLog (LogSource (..), ResolvedLog (..), resolveBuildLog)
import EvalError (cachedEvalStepDrv, cleanEvalError, getCachedEvalError, shortEvalError)
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
import Servant (Handler, NoContent (..), err404, err500, errBody)
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
        mResolved <- liftIO $ resolveBuildLog (stepInstallable ctx eid)
        case mResolved of
            Just rl -> return (Just (renderResolvedLog rl))
            Nothing -> do
                mEvalErr <- liftIO $ getCachedEvalError (T.pack targetCommit) eid
                case mEvalErr of
                    Just (Left stderrTxt) -> do
                        mSrc <- liftIO $ readStepSource repoPath targetCommit eid
                        return (Just (renderEvalError eid mSrc (cleanEvalError stderrTxt)))
                    _ -> return Nothing

    case result of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right Nothing ->
            throwError $
                err404
                    { errBody =
                        TLE.encodeUtf8 (TL.pack ("No build log available for step " ++ show eid))
                    }
        Right (Just txt) -> return txt

{- | Render a resolved log for the wire. Logs that come from an input
derivation (rather than the step itself) are prefixed so the user knows
the failure originated in a build prerequisite.
-}
renderResolvedLog :: ResolvedLog -> T.Text
renderResolvedLog (ResolvedLog _ logText StepDrv) = T.pack logText
renderResolvedLog (ResolvedLog drv logText (InputDrv _ _)) =
    T.pack ("Build prerequisite failed: " ++ drv ++ "\n-----\n" ++ logText)

-- | @git show <commit>:steps/<eid>.nix@. 'Nothing' iff the path does
-- not exist at that revision or git exits non-zero.
readStepSource :: FilePath -> String -> Int -> IO (Maybe T.Text)
readStepSource repoPath targetCommit eid = do
    let spec = targetCommit ++ ":steps/" ++ show eid ++ ".nix"
    (ec, out, _) <-
        readProcessWithExitCodeL "git" ["-C", repoPath, "show", spec] ""
    case ec of
        ExitSuccess -> return (Just (T.pack out))
        _ -> return Nothing

-- | Prepend the step's saved configuration (when available) to the
-- cleaned eval-error trace.
renderEvalError :: Int -> Maybe T.Text -> T.Text -> T.Text
renderEvalError _ Nothing err = err
renderEvalError eid (Just src) err =
    "Step configuration (steps/"
        <> T.pack (show eid)
        <> ".nix):\n"
        <> src
        <> "\nError:\n"
        <> err

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

        outPathText <- requireOutPathFromCache ctx eid
        let outPath = T.unpack outPathText
        let targetCommitText = T.pack (readCommitHash ctx)
        built <- liftIO $ isBuilt outPath
        if built
            then liftIO $ broadcastStatusForStepProjects eid targetCommitText Nothing
            else do
                evalResult <- liftIO $ cachedEvalStepDrv ctx eid
                case evalResult of
                    Left stderrTxt ->
                        liftIO $
                            broadcastStatusForStepProjects
                                eid
                                targetCommitText
                                (Just ("failure", shortEvalError stderrTxt))
                    Right _ -> do
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
    output <- runNixInRepo ctx ["eval", "--json"] "#pointy.projectOutPaths"
    projectOutPaths <-
        ExceptT $ do
            return $
                case eitherDecode (TLE.encodeUtf8 (TL.pack output)) :: Either String (Map.Map Int (Map.Map Int FilePath)) of
                    Left err -> Left $ "Failed to parse #pointy.projectOutPaths: " ++ err
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
    result <- liftIO $ runExceptT $ runNixInRepo ctx ["eval", "--json"] ("#pointy.dependencies." ++ show stepId)
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
                liftIO $ broadcastStatusForStepProjects eid targetCommit Nothing

    case result of
        Left err -> putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()

outPathToUnitName :: String -> String
outPathToUnitName outPath =
    let sanitizedPath = map (\c -> if c == '/' then '-' else c) (dropWhile (== '/') outPath)
     in "nix-build-" ++ sanitizedPath
