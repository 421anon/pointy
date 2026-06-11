{-# LANGUAGE OverloadedStrings #-}

module Handlers.RunStep (
    buildExtras,
    runStepHandler,
    stepLogHandler,
    stopStepHandler,
) where

import BuildLog (LogSource (..), ResolvedLog (..), resolveBuildLog)
import BuildRunner (StepRequirements (..), buildKeyForOutPath, cancel, submitAndWait)
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
import Handlers.Statuses (addDependencyRunningOverrides, broadcastFailedStepForProjects, broadcastKnownStepStatus, broadcastSingleStepForProjects, broadcastStatusForStepProjects, removeDependencyRunningOverrides)
import NixUtils (isValidStorePath)
import OutPaths (warmProjectOutPathsForCommit)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, NoContent (..), err404, err500, errBody)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
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

        warmProjectOutPathsForCommit ctx
        liftIO $
            bracket_
                (addDependencyRunningOverrides targetCommitText stepIds)
                (removeDependencyRunningOverrides targetCommitText stepIds)
                ( do
                    mapM_ (\sid -> broadcastKnownStepStatus sid targetCommitText ("running", Nothing)) stepIds
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

extrasInstallable :: ReadRepoContext -> Int -> String
extrasInstallable ctx eid =
    stepInstallable ctx eid ++ ".meta.pointy.extras"

buildStep :: ReadRepoContext -> Int -> IO ()
buildStep ctx eid = do
    let targetCommitText = T.pack (readCommitHash ctx)
    result <- runExceptT $ do
        outPathText <- getStepOutPath ctx eid
        let outPath = T.unpack outPathText
        built <- liftIO $ isBuilt outPath
        if built
            then liftIO $ broadcastSingleStepForProjects eid targetCommitText outPath
            else do
                let buildKey = buildKeyForOutPath outPath
                requirements <- getStepRequirements ctx eid
                exitCode <-
                    liftIO $
                        submitAndWait
                            requirements
                            buildKey
                            ["nix", "build", "--no-link", "--no-eval-cache", stepInstallable ctx eid]
                case exitCode of
                    ExitSuccess -> do
                        nowBuilt <- liftIO $ isBuilt outPath
                        if nowBuilt
                            then liftIO $ registerGcRootForOutPath outPath
                            else return ()
                        liftIO $ broadcastSingleStepForProjects eid targetCommitText outPath
                    -- The scheduler job is already gone when the status is
                    -- re-checked, so checkStatus would report "not-started"
                    -- and the dependency-running override (still held by
                    -- runStepSync) would mask the failure as "running".
                    -- The exit code is authoritative: broadcast the failure.
                    ExitFailure _ ->
                        liftIO $ broadcastFailedStepForProjects eid targetCommitText

        -- Build extras derivation if present, independently of main step status.
        liftIO $ buildExtras ctx eid

    case result of
        Left err -> do
            putStrLn $ "buildStep error: " ++ err
            broadcastKnownStepStatus eid targetCommitText ("failure", Just (T.pack err))
        Right _ -> return ()

{- | Attempt to build the extras derivation for a step.  Errors are non-fatal
(logged only) because extras are supplementary metadata.
-}
buildExtras :: ReadRepoContext -> Int -> IO ()
buildExtras ctx eid = do
    result <- runExceptT $ do
        mExtrasPath <- getExtrasOutPath ctx eid
        case mExtrasPath of
            Nothing -> return ()
            Just extrasPath -> do
                built <- liftIO $ isBuilt extrasPath
                if built
                    then liftIO $ registerGcRootForOutPath extrasPath
                    else do
                        requirements <- getExtrasRequirements ctx eid
                        let buildKey = buildKeyForOutPath extrasPath
                        exitCode <-
                            liftIO $
                                submitAndWait
                                    requirements
                                    buildKey
                                    ["nix", "build", "--no-link", "--no-eval-cache", extrasInstallable ctx eid]
                        case exitCode of
                            ExitSuccess -> do
                                nowBuilt <- liftIO $ isBuilt extrasPath
                                if nowBuilt then liftIO $ registerGcRootForOutPath extrasPath else return ()
                            ExitFailure _ -> return ()
    case result of
        Left err -> putStrLn $ "buildExtras error for step " ++ show eid ++ ": " ++ err
        Right _ -> return ()

getStepOutPath :: ReadRepoContext -> Int -> ExceptT String IO T.Text
getStepOutPath ctx eid = do
    output <- runNixEvalRawInRepo ctx ("#pointy.steps." ++ show eid ++ ".outPath")
    return $ T.pack output

{- | Resolve the extras outPath.  Returns Nothing when the step has no extras
attribute (i.e. the eval result is not a valid store path).
-}
getExtrasOutPath :: ReadRepoContext -> Int -> ExceptT String IO (Maybe FilePath)
getExtrasOutPath ctx eid = do
    result <-
        liftIO $
            runExceptT $
                runNixEvalRawInRepo ctx ("#pointy.steps." ++ show eid ++ ".meta.pointy.extras.outPath")
    case result of
        Left _ -> return Nothing
        Right path ->
            if null path
                then return Nothing
                else return (Just path)

getExtrasRequirements :: ReadRepoContext -> Int -> ExceptT String IO StepRequirements
getExtrasRequirements ctx eid = do
    let attr = "#pointy.steps." ++ show eid ++ ".meta.pointy.extras.requirements"
    result <- liftIO $ runExceptT $ runNixEvalJsonInRepo ctx attr
    case result of
        Left _ ->
            -- No extras.requirements: use conservative defaults.
            return StepRequirements{cpu = 1, ram = "1G", ior = "0", iow = "0"}
        Right output ->
            decodeAndValidateRequirements attr output

getStepRequirements :: ReadRepoContext -> Int -> ExceptT String IO StepRequirements
getStepRequirements ctx eid = do
    let attr = "#pointy.steps." ++ show eid ++ ".requirements"
    output <- runNixEvalJsonInRepo ctx attr
    decodeAndValidateRequirements attr output

{- | Decode a JSON-encoded `StepRequirements` payload and reject values that
would produce malformed slurm arguments (negative cpu, delimiters in
ram/ior/iow). Used by both the main step and extras paths so they share
the same validation contract.
-}
decodeAndValidateRequirements :: String -> String -> ExceptT String IO StepRequirements
decodeAndValidateRequirements attr output = do
    requirements <-
        case eitherDecode (TLE.encodeUtf8 (TL.pack output)) of
            Left err -> throwError $ "Failed to decode " ++ attr ++ ": " ++ err
            Right decoded -> return decoded
    case validateStepRequirements requirements of
        Left err -> throwError $ "Invalid " ++ attr ++ ": " ++ err
        Right () -> return requirements

validateStepRequirements :: StepRequirements -> Either String ()
validateStepRequirements requirements
    | cpu requirements <= 0 = Left $ "cpu must be positive, got " ++ show (cpu requirements)
    | hasExportDelimiter (ior requirements) = Left "ior must not contain comma, newline, or NUL"
    | hasExportDelimiter (iow requirements) = Left "iow must not contain comma, newline, or NUL"
    | hasExportDelimiter (ram requirements) = Left "ram must not contain comma, newline, or NUL"
    | otherwise = Right ()
  where
    hasExportDelimiter = T.any (\c -> c == ',' || c == '\n' || c == '\r' || c == '\0')

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
isBuilt = isValidStorePath

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

        let ctx = ReadRepoContext repoPath (T.unpack targetCommit)

        -- Cancel extras first: if we cancelled main only, Nix dependency
        -- resolution in the extras build could restart the main build.
        mExtrasPath <- getExtrasOutPath ctx eid
        liftIO $ case mExtrasPath of
            Just extrasPath -> cancel (buildKeyForOutPath extrasPath)
            Nothing -> return ()

        outPathText <- getStepOutPath ctx eid
        liftIO $ cancel $ buildKeyForOutPath $ T.unpack outPathText
        liftIO $ removeDependencyRunningOverrides targetCommit [eid]
        liftIO $ broadcastStatusForStepProjects eid targetCommit Nothing

    case result of
        Left err -> putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()
