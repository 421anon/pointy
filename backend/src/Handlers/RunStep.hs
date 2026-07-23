{-# LANGUAGE OverloadedStrings #-}

module Handlers.RunStep (
    buildExtras,
    runStepHandler,
    stepLogHandler,
    stopStepHandler,
) where

import BuildLog (LogSource (..), ResolvedLog (..), resolveBuildLog)
import BuildRunner (BuildKey, JobId, StepRequirements (..), buildKeyForOutPath, cancel, queryJobIds, submitAndWait, submitJob, waitForCompletion)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad (foldM)

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.List (foldl', nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Statuses (broadcastFailedStepForProjects, broadcastKnownStepStatus, broadcastSingleStepForProjects, broadcastStatusForStepProjects)
import NixUtils (isValidStorePath)
import System.Process (readProcessWithExitCode)
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
        graph <- getDependencyGraph ctx eid
        stepIds <- liftEither $ topoOrder graph

        liftIO $ do
            outcomes <- submitGraph ctx graph stepIds
            mapConcurrently_ (finishStep ctx) (Map.toList outcomes)

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

-- | Submission-phase result for a step.
data SubmitOutcome
    = -- | Store path is already valid; nothing to schedule.
      AlreadyBuilt FilePath
    | -- | A slurm job (new or already in flight) produces the path; carries
      -- its job ids for dependents' @afterok@ edges.
      Enqueued FilePath BuildKey [JobId]
    | -- | Evaluation or submission failed; dependents are not scheduled.
      NotSubmitted String

{- | Submit one slurm job per unbuilt step, dependencies first, wired with
@afterok@ edges so a dependent never builds a dependency's derivation
inside its own allocation.
-}
submitGraph :: ReadRepoContext -> Map.Map Int [Int] -> [Int] -> IO (Map.Map Int SubmitOutcome)
submitGraph ctx graph = foldM submitOne Map.empty
  where
    submitOne outcomes sid = do
        outcome <- submitStep ctx outcomes (Map.findWithDefault [] sid graph) sid
        return $ Map.insert sid outcome outcomes

submitStep :: ReadRepoContext -> Map.Map Int SubmitOutcome -> [Int] -> Int -> IO SubmitOutcome
submitStep ctx outcomes deps sid
    | not (null blockedOn) =
        return $ NotSubmitted $ "dependency step(s) " ++ show blockedOn ++ " could not be scheduled"
    | otherwise = do
        result <- runExceptT $ do
            outPathText <- getStepOutPath ctx sid
            let outPath = T.unpack outPathText
            built <- liftIO $ isBuilt outPath
            let buildKey = buildKeyForOutPath outPath
            if built
                then return $ AlreadyBuilt outPath
                else do
                    existing <- liftIO $ queryJobIds buildKey
                    if not (null existing)
                        then return $ Enqueued outPath buildKey existing
                        else do
                            requirements <- getStepRequirements ctx sid
                            submitted <-
                                liftIO $
                                    submitJob
                                        requirements
                                        buildKey
                                        depJobIds
                                        ["nix", "build", "--no-link", "--no-eval-cache", stepInstallable ctx sid]
                            case submitted of
                                Left err -> throwError err
                                Right jobId -> return $ Enqueued outPath buildKey [jobId]
        return $ either NotSubmitted id result
  where
    blockedOn = [d | d <- deps, isBlocked (Map.lookup d outcomes)]
    isBlocked (Just NotSubmitted{}) = True
    isBlocked Nothing = True -- unreachable given topological order; fail closed
    isBlocked _ = False
    depJobIds = nub [jobId | d <- deps, Just (Enqueued _ _ jobIds) <- [Map.lookup d outcomes], jobId <- jobIds]

{- | Wait for a step's job to leave the queue and broadcast the result.
Success is judged by the store path; slurm reports no usable exit status
for jobs not submitted with @--wait@.
-}
finishStep :: ReadRepoContext -> (Int, SubmitOutcome) -> IO ()
finishStep ctx (sid, outcome) = case outcome of
    AlreadyBuilt outPath -> do
        broadcastSingleStepForProjects sid targetCommitText outPath
        buildExtras ctx sid
    Enqueued outPath buildKey _ -> do
        -- The job is already submitted; broadcast "running" immediately
        -- instead of querying squeue which may not have registered it yet.
        broadcastKnownStepStatus sid targetCommitText ("running", Nothing)
        waitForCompletion buildKey
        nowBuilt <- isBuilt outPath
        if nowBuilt
            then do
                registerGcRootForOutPath outPath
                broadcastSingleStepForProjects sid targetCommitText outPath
                buildExtras ctx sid
            else broadcastFailedStepForProjects sid targetCommitText
    NotSubmitted err -> do
        putStrLn $ "buildStep error: " ++ err
        broadcastKnownStepStatus sid targetCommitText ("failure", Just (T.pack err))
  where
    targetCommitText = T.pack (readCommitHash ctx)

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

{- | Transitive step dependency graph rooted at a step: every reachable
step mapped to its direct dependencies.
-}
getDependencyGraph :: ReadRepoContext -> Int -> ExceptT String IO (Map.Map Int [Int])
getDependencyGraph ctx root = go Map.empty [root]
  where
    go acc [] = return acc
    go acc (sid : rest)
        | Map.member sid acc = go acc rest
        | otherwise = do
            deps <- nub <$> getDependencies ctx sid
            go (Map.insert sid deps acc) (rest ++ deps)

-- | Dependencies-first ordering of the step graph; fails on cycles.
topoOrder :: Map.Map Int [Int] -> Either String [Int]
topoOrder graph = go Set.empty [] (Map.keys graph)
  where
    go _ ordered [] = Right ordered
    go done ordered pending
        | null ready = Left $ "dependency cycle detected among steps " ++ show pending
        | otherwise = go (foldl' (flip Set.insert) done ready) (ordered ++ ready) blocked
      where
        (ready, blocked) = partition (all (`Set.member` done) . depsOf) pending
        depsOf sid = Map.findWithDefault [] sid graph

{- | Direct dependencies of a step. A missing @pointy.dependencies@ attribute
(or one that fails to decode) is treated as "no dependencies".
-}
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
    _ <- readProcessWithExitCode "nix-store" ["--add-root", gcRootPath, "--realise", outPath] ""
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
        liftIO $ broadcastStatusForStepProjects eid targetCommit Nothing

    case result of
        Left err -> putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()
