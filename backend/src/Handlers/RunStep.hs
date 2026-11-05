{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.RunStep (
    buildExtras,
    jobEndedHandler,
    restoreJobsFromSlurm,
    runStepHandler,
    stepLogHandler,
    stopStepHandler,
) where

import Bisect (bisectBatch)
import BuildLog (LogAccess (..), LogSource (..), ResolvedLog (..), resolveBuildLog, validPaths)
import BuildRunner (BuildKey (..), JobComment (..), JobId, SlurmJob (..), StepRequirements (..), buildKeyForOutPath, cancel, decodeJobComment, encodeJobComment, isDerivationPath, isRunningState, notifyJobEnded, queryJobIds, querySlurmJobs, realiseCommand, runningJobNames, submitAndWait, submitJob, waitForCompletion)
import Certificates (renderIdList)
import ClusterBus (buildingSteps, requestStop, stopRequested, takeStopRequest)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, catch)
import Control.Monad (foldM, forM_, unless, void, when)

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.List (foldl', isPrefixOf, nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effects (App, AppM, Eval, Nix, listDerivationRootJobs, pathValid, rootDerivationForJob, rootStorePath, unrootDerivationRoot, unrootDerivationRootsForJob)
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (finally, onException)
import Handlers.Statuses (broadcastFailedStepForProjects, broadcastKnownStepStatus, broadcastSingleStepForProjects, broadcastStatusForStepProjects, broadcastStepCertificateAtMovedHead, trackBuild)
import Servant (NoContent (..), err404, err500, errBody)
import System.Exit (ExitCode (..))
import StepEventBus (StepEvent (..), StepEventKind (..), publishStepEvent)
import UserRepo (ReadRepoContext (..), ensureRepoCommit, runNixEvalJsonApplyInRepo, runNixEvalJsonInRepo, runNixEvalRawInRepo, withReadRepoTransaction)

runStepHandler :: Int -> Maybe T.Text -> Maybe T.Text -> AppM NoContent
runStepHandler eid commit appInstance = do
    _ <- liftIO $ forkIO $ runAppEffects $ runStepSync eid commit appInstance
    return NoContent

stopStepHandler :: Int -> Maybe T.Text -> AppM NoContent
stopStepHandler eid commit = do
    lift $ stopStepSync eid commit
    return NoContent

jobEndedHandler :: String -> AppM NoContent
jobEndedHandler name = do
    lift $ notifyJobEnded (BuildKey name)
    return NoContent

runStepSync :: App es => Int -> Maybe T.Text -> Maybe T.Text -> Eff es ()
runStepSync eid commit appInstance = do
    result <- runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
            halted = stopRequested eid
            announce kind = liftIO $ publishStepEvent (StepEvent kind eid (T.pack targetCommit) appInstance)
        lift $ announce BuildStarted
        ExceptT $ (`finally` announce BuildFinished) $ runExceptT $ do
            outcomes <- ExceptT $ trackBuild eid (T.pack targetCommit) $ runExceptT $ do
                _ <- liftIO $ takeStopRequest eid
                graph <- getDependencyGraph halted ctx eid
                stepIds <- liftEither $ topoOrder graph
                submitted <- lift $ submitGraph halted ctx graph stepIds
                stopped <- liftIO $ takeStopRequest eid
                when stopped $ lift $ mapM_ cancel [buildKey | Just (Enqueued _ buildKey _ _) <- [Map.lookup eid submitted]]
                return submitted
            unless (Map.member eid outcomes) $ lift $ broadcastStatusForStepProjects eid (T.pack targetCommit) Nothing
            liftIO $ mapConcurrently_ (runAppEffects . finishStep ctx) (Map.toList outcomes)

    case result of
        Left err -> liftIO $ putStrLn $ "runStepAsync error: " ++ err
        Right _ -> return ()

stepLogHandler :: Int -> Maybe T.Text -> AppM T.Text
stepLogHandler eid commit = do
    result <- lift $ runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
        target <- resolveStepTarget ctx eid
        lift $ resolveBuildLog RemoteLogs (stepTargetDrv target)

    case result of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right Nothing ->
            throwError $
                err404
                    { errBody =
                        TLE.encodeUtf8 (TL.pack ("No build log available for step " ++ show eid))
                    }
        Right (Just rl) -> return (renderResolvedLog rl)

renderResolvedLog :: ResolvedLog -> T.Text
renderResolvedLog (ResolvedLog _ logText StepDrv) = TE.decodeUtf8Lenient logText
renderResolvedLog (ResolvedLog drv logText (InputDrv _ _)) =
    "Build prerequisite failed: " <> T.pack drv <> "\n-----\n" <> TE.decodeUtf8Lenient logText

stepAttr :: Int -> String
stepAttr eid = "pointy.steps." ++ show eid

data StepTarget = StepTarget
    { stepTargetPath :: FilePath
    , stepTargetDrv :: FilePath
    }

instance FromJSON StepTarget where
    parseJSON = withObject "StepTarget" $ \o -> StepTarget <$> o .: "path" <*> o .: "drv"

resolveStepTarget :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) StepTarget
resolveStepTarget ctx eid = do
    output <- runNixEvalJsonApplyInRepo ctx certificateOrLegacyStep ('#' : stepAttr eid)
    either (throwError . (("Failed to decode the build target of step " ++ show eid ++ ": ") ++)) pure $
        eitherDecode (TLE.encodeUtf8 (TL.pack output))

resolveStepTargets :: (Eval :> es) => ReadRepoContext -> [Int] -> Eff es (Map.Map Int (Either String StepTarget))
resolveStepTargets ctx = bisectBatch evaluateTargets (\_ single -> Map.singleton single <$> runExceptT (resolveStepTarget ctx single))
  where
    evaluateTargets ids = do
        outcome <- runExceptT $ runNixEvalJsonApplyInRepo ctx (certificatesOrLegacySteps ids) "#pointy.steps"
        pure $ case eitherDecode . TLE.encodeUtf8 . TL.pack <$> outcome of
            Right (Right targets) | length targets == length ids -> Right (Map.fromList (zip ids (map Right targets)))
            _ -> Left ()
    certificatesOrLegacySteps ids = "steps: map (id: (" ++ certificateOrLegacyStep ++ ") steps.${id}) " ++ renderIdList ids

certificateOrLegacyStep :: String
certificateOrLegacyStep = "step: { path = (step.certificate or step).outPath; drv = (step.certificate or step).drvPath; }"

data SubmitOutcome
    = AlreadyCertified FilePath
    | Enqueued FilePath BuildKey [JobId] FilePath
    | NotSubmitted String

submitGraph :: App es => IO Bool -> ReadRepoContext -> Map.Map Int [Int] -> [Int] -> Eff es (Map.Map Int SubmitOutcome)
submitGraph halted ctx graph stepIds = do
    targets <- resolveStepTargets ctx stepIds
    built <- validPaths (Set.toList (Set.fromList [stepTargetPath target | Right target <- Map.elems targets]))
    let isCertified certificate = maybe (isBuilt certificate) (return . Set.member certificate) built
        submitOne outcomes (sid, target) = do
            stop <- liftIO halted
            if stop
                then return outcomes
                else do
                    outcome <- submitStep ctx isCertified outcomes (Map.findWithDefault [] sid graph) sid target
                    return $ Map.insert sid outcome outcomes
    foldM submitOne Map.empty [(sid, target) | sid <- stepIds, Just target <- [Map.lookup sid targets]]

submitStep :: App es => ReadRepoContext -> (FilePath -> Eff es Bool) -> Map.Map Int SubmitOutcome -> [Int] -> Int -> Either String StepTarget -> Eff es SubmitOutcome
submitStep ctx isCertified outcomes deps sid resolvedTarget
    | not (null blockedOn) =
        return $ NotSubmitted $ "dependency step(s) " ++ show blockedOn ++ " could not be scheduled"
    | otherwise = do
        result <- runExceptT $ do
            target <- liftEither resolvedTarget
            liftEither $ validateStepTarget sid target
            let certificate = stepTargetPath target
            certified <- lift $ isCertified certificate
            let buildKey = buildKeyForOutPath certificate
            if certified
                then return $ AlreadyCertified certificate
                else do
                    existing <- lift $ queryJobIds buildKey
                    if not (null existing)
                        then do
                            (rootedTarget, root) <- ExceptT $ ensureRooted ctx sid target
                            return $ Enqueued (stepTargetPath rootedTarget) buildKey existing root
                        else do
                            requirements <- getStepRequirements ctx sid
                            command <- liftIO realiseCommand
                            (rootedTarget, root) <- ExceptT $ ensureRooted ctx sid target
                            submitted <-
                                lift $
                                    submitJob
                                        requirements
                                        buildKey
                                        depJobIds
                                        (encodeJobComment (JobComment "step" sid (readCommitHash ctx) certificate))
                                        [command, stepTargetDrv rootedTarget]
                                        `onException` unrootDerivationRoot root
                            case submitted of
                                Left err -> do
                                    lift $ unrootDerivationRoot root
                                    throwError err
                                Right jobId -> return $ Enqueued certificate buildKey [jobId] root
        return $ either NotSubmitted id result
  where
    blockedOn = [d | d <- deps, isBlocked (Map.lookup d outcomes)]
    isBlocked (Just NotSubmitted{}) = True
    isBlocked Nothing = True
    isBlocked _ = False
    depJobIds = nub [jobId | d <- deps, Just (Enqueued _ _ jobIds _) <- [Map.lookup d outcomes], jobId <- jobIds]

validateStepTarget :: Int -> StepTarget -> Either String ()
validateStepTarget sid = validateTarget ("step " ++ show sid) . stepTargetDrv

validateTarget :: String -> FilePath -> Either String ()
validateTarget label drvPath
    | isDerivationPath drvPath = Right ()
    | otherwise =
        Left $
            label
                ++ " evaluated to an invalid derivation path "
                ++ show drvPath
                ++ "; expected /nix/store/<hash>-<name>.drv"

ensureRooted :: App es => ReadRepoContext -> Int -> StepTarget -> Eff es (Either String (StepTarget, FilePath))
ensureRooted ctx sid target =
    case validateStepTarget sid target of
        Left err -> return (Left err)
        Right () -> do
            firstAttempt <- tryRoot target
            case firstAttempt of
                Right root -> return (Right (target, root))
                Left firstError -> do
                    reevaluated <- runExceptT $ resolveStepTarget ctx sid
                    case reevaluated of
                        Left reevaluationError ->
                            return $ Left (firstError ++ "; re-evaluating the step failed: " ++ reevaluationError)
                        Right target' ->
                            case validateStepTarget sid target' of
                                Left invalid -> return $ Left (firstError ++ "; " ++ invalid)
                                Right ()
                                    | stepTargetPath target' /= stepTargetPath target ->
                                        return $
                                            Left
                                                ( firstError
                                                    ++ "; the step now evaluates to "
                                                    ++ stepTargetPath target'
                                                    ++ ", so it must be retried"
                                                )
                                    | otherwise -> do
                                        secondAttempt <- tryRoot target'
                                        case secondAttempt of
                                            Right root -> return (Right (target', root))
                                            Left secondError -> return $ Left (firstError ++ "; " ++ secondError)
  where
    tryRoot candidate = do
        let drv = stepTargetDrv candidate
        rooted <- rootDerivationForJob (unBuildKey (buildKeyForOutPath (stepTargetPath candidate))) drv
        case rooted of
            Left error -> return (Left error)
            Right root -> do
                stillValid <- pathValid drv
                if stillValid
                    then return (Right root)
                    else do
                        unrootDerivationRoot root
                        return (Left ("the derivation " ++ drv ++ " disappeared before submission"))

finishStep :: App es => ReadRepoContext -> (Int, SubmitOutcome) -> Eff es ()
finishStep ctx (sid, outcome) = case outcome of
    AlreadyCertified certificate -> do
        rootStorePath certificate
        broadcastSingleStepForProjects sid targetCommitText certificate
        buildExtras ctx sid
    Enqueued certificate buildKey _ root -> do
        trackBuild sid targetCommitText $ do
            broadcastKnownStepStatus sid targetCommitText ("running", Nothing)
            finally (waitForCompletion buildKey) (unrootDerivationRoot root)
        certified <- isBuilt certificate
        if certified
            then do
                registerCertificationRoots ctx sid certificate
                broadcastSingleStepForProjects sid targetCommitText certificate
            else broadcastFailedStepForProjects sid targetCommitText
        broadcastStepCertificateAtMovedHead sid targetCommitText
        when certified $ buildExtras ctx sid
    NotSubmitted err -> do
        liftIO $ putStrLn $ "buildStep error: " ++ err
        broadcastKnownStepStatus sid targetCommitText ("failure", Just (T.pack err))
  where
    targetCommitText = T.pack (readCommitHash ctx)

registerCertificationRoots :: App es => ReadRepoContext -> Int -> FilePath -> Eff es ()
registerCertificationRoots ctx sid certificate = do
    rootStorePath certificate
    result <- runExceptT $ getStepOutPath ctx sid
    case result of
        Right outPath -> rootStorePath (T.unpack outPath)
        Left _ -> return ()

buildExtras :: App es => ReadRepoContext -> Int -> Eff es ()
buildExtras ctx eid = do
    result <- runExceptT $ do
        mTarget <- getExtrasTarget ctx eid
        case mTarget of
            Nothing -> return ()
            Just target -> do
                liftEither $ validateTarget ("extras of step " ++ show eid) (extrasTargetDrv target)
                let extrasPath = extrasTargetPath target
                built <- lift $ isBuilt extrasPath
                if built
                    then lift $ rootStorePath extrasPath
                    else do
                        requirements <- getExtrasRequirements ctx eid
                        command <- liftIO realiseCommand
                        let buildKey = buildKeyForOutPath extrasPath
                        root <-
                            lift $
                                rootDerivationForJob
                                    (unBuildKey buildKey)
                                    (extrasTargetDrv target)
                        rootPath <- liftEither root
                        exitCode <-
                            lift $
                                finally
                                    ( submitAndWait
                                        requirements
                                        buildKey
                                        (encodeJobComment (JobComment "extras" eid (readCommitHash ctx) extrasPath))
                                        [command, extrasTargetDrv target]
                                    )
                                    (unrootDerivationRoot rootPath)
                        case exitCode of
                            ExitSuccess -> do
                                nowBuilt <- lift $ isBuilt extrasPath
                                if nowBuilt then lift $ rootStorePath extrasPath else return ()
                            ExitFailure _ -> return ()
    case result of
        Left err -> liftIO $ putStrLn $ "buildExtras error for step " ++ show eid ++ ": " ++ err
        Right _ -> return ()

getStepOutPath :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) T.Text
getStepOutPath ctx eid = do
    output <- runNixEvalRawInRepo ctx ("#pointy.steps." ++ show eid ++ ".outPath")
    return $ T.pack output

data ExtrasTarget = ExtrasTarget
    { extrasTargetPath :: FilePath
    , extrasTargetDrv :: FilePath
    }

instance FromJSON ExtrasTarget where
    parseJSON = withObject "ExtrasTarget" $ \o -> ExtrasTarget <$> o .: "path" <*> o .: "drv"

getExtrasTarget :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) (Maybe ExtrasTarget)
getExtrasTarget ctx eid = do
    result <-
        lift $
            runExceptT $
                runNixEvalJsonApplyInRepo
                    ctx
                    "extras: { path = extras.outPath; drv = extras.drvPath; }"
                    ("#pointy.steps." ++ show eid ++ ".meta.pointy.extras")
    case result of
        Left _ -> return Nothing
        Right output ->
            case eitherDecode (TLE.encodeUtf8 (TL.pack output)) of
                Left _ -> return Nothing
                Right target -> return (Just target)

getExtrasRequirements :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) StepRequirements
getExtrasRequirements ctx eid = do
    let attr = "#pointy.steps." ++ show eid ++ ".meta.pointy.extras.requirements"
    result <- lift $ runExceptT $ runNixEvalJsonInRepo ctx attr
    case result of
        Left _ ->
            return StepRequirements{cpu = 1, ram = "1G", ior = "0", iow = "0"}
        Right output ->
            decodeAndValidateRequirements attr output

getStepRequirements :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) StepRequirements
getStepRequirements ctx eid = do
    let attr = "#pointy.steps." ++ show eid ++ ".requirements"
    output <- runNixEvalJsonInRepo ctx attr
    decodeAndValidateRequirements attr output

decodeAndValidateRequirements :: String -> String -> ExceptT String (Eff es) StepRequirements
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

getDependencyGraph :: (Eval :> es, IOE :> es) => IO Bool -> ReadRepoContext -> Int -> ExceptT String (Eff es) (Map.Map Int [Int])
getDependencyGraph halted ctx root = go Map.empty [root]
  where
    go acc [] = return acc
    go acc (sid : rest)
        | Map.member sid acc = go acc rest
        | otherwise = do
            stop <- liftIO halted
            if stop
                then return Map.empty
                else do
                    deps <- nub <$> getDependencies ctx sid
                    go (Map.insert sid deps acc) (rest ++ deps)

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

getDependencies :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) [Int]
getDependencies ctx stepId = do
    result <- lift $ runExceptT $ runNixEvalJsonInRepo ctx ('#' : stepAttr stepId ++ ".dependencies")
    case result of
        Left _ -> return []
        Right stdout ->
            case eitherDecode (TLE.encodeUtf8 (TL.pack stdout)) :: Either String [String] of
                Left _ -> return []
                Right ids -> return $ map read ids

isBuilt :: (Nix :> es) => FilePath -> Eff es Bool
isBuilt = pathValid

stopStepSync :: App es => Int -> Maybe T.Text -> Eff es ()
stopStepSync eid commit = do
    liftIO $ requestStop eid
    result <- runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    let targetCommit = fromMaybe (T.pack commitHash) commit
                     in return (repoPath, targetCommit)

        let ctx = ReadRepoContext repoPath (T.unpack targetCommit)

        mExtrasTarget <- getExtrasTarget ctx eid
        lift $ case mExtrasTarget of
            Just extrasTarget -> cancel (buildKeyForOutPath (extrasTargetPath extrasTarget))
            Nothing -> return ()

        target <- resolveStepTarget ctx eid
        building <- liftIO $ atomically buildingSteps
        lift $ cancel $ buildKeyForOutPath $ stepTargetPath target
        unless (Set.member eid building) $ lift $ broadcastStatusForStepProjects eid targetCommit Nothing

    case result of
        Left err -> liftIO $ putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()

restoreJobsFromSlurm :: App es => Eff es ()
restoreJobsFromSlurm = do
    eRepo <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> return repoPath
    case eRepo of
        Left err -> liftIO $ putStrLn $ "restoreJobsFromSlurm: cannot access repo: " ++ err
        Right repoPath -> do
            liftIO $
                scanJobs repoPath
                    `catch` \e -> putStrLn $ "restoreJobsFromSlurm failed: " ++ show (e :: SomeException)
            sweepDerivationRoots
  where
    scanJobs :: FilePath -> IO ()
    scanJobs repoPath = do
        jobs <- runAppEffects querySlurmJobs
        let pointyJobs = [job | job <- jobs, isPointyJob (slurmJobName job)]
        attachWatchers repoPath pointyJobs

    attachWatchers :: FilePath -> [SlurmJob] -> IO ()
    attachWatchers repoPath = go Set.empty
      where
        go _ [] = return ()
        go seen (job : rest)
            | Set.member (slurmJobName job) seen = go seen rest
            | otherwise = do
                attachOne repoPath job
                go (Set.insert (slurmJobName job) seen) rest

    attachOne :: FilePath -> SlurmJob -> IO ()
    attachOne repoPath job = case decodeJobComment =<< slurmJobComment job of
        Nothing -> do
            putStrLn $ "restoreJobsFromSlurm: no job comment on " ++ slurmJobName job ++ ", skipping"
            runAppEffects $ unrootDerivationRootsForJob (slurmJobName job)
        Just comment
            | jobCommentKind comment == "extras" ->
                void $
                    forkIO $
                        runAppEffects $
                            watchRestoredExtrasJob (slurmJobName job) (slurmJobState job)
            | jobCommentKind comment /= "step" -> do
                putStrLn $
                    "restoreJobsFromSlurm: unknown job kind on "
                        ++ slurmJobName job
                        ++ ", skipping"
                runAppEffects $ unrootDerivationRootsForJob (slurmJobName job)
            | buildKeyForOutPath (jobCommentOutPath comment) /= BuildKey (slurmJobName job) -> do
                putStrLn $
                    "restoreJobsFromSlurm: job name mismatch for step "
                        ++ show (jobCommentStep comment)
                        ++ ", skipping"
                runAppEffects $ do
                    unrootDerivationRootsForJob (slurmJobName job)
                    unrootDerivationRootsForJob (unBuildKey (buildKeyForOutPath (jobCommentOutPath comment)))
            | otherwise -> do
                eReady <- runExceptT $ ensureRepoCommit (jobCommentCommit comment)
                case eReady of
                    Left err -> do
                        putStrLn $
                            "restoreJobsFromSlurm: commit "
                                ++ jobCommentCommit comment
                                ++ " unavailable for step "
                                ++ show (jobCommentStep comment)
                                ++ ": "
                                ++ err
                        runAppEffects $ unrootDerivationRootsForJob (slurmJobName job)
                    Right () -> do
                        let ctx = ReadRepoContext repoPath (jobCommentCommit comment)
                        void $ forkIO $ runAppEffects $ watchRestoredJob ctx comment job

sweepDerivationRoots :: App es => Eff es ()
sweepDerivationRoots = do
    running <- runningJobNames
    case running of
        Nothing -> return ()
        Just names -> do
            roots <- liftIO listDerivationRootJobs
            forM_ roots $ \jobName ->
                unless (Set.member jobName names) $ unrootDerivationRootsForJob jobName

watchRestoredExtrasJob :: App es => String -> String -> Eff es ()
watchRestoredExtrasJob jobName state = do
    when (isRunningState state) $ waitForCompletion (BuildKey jobName)
    unrootDerivationRootsForJob jobName

watchRestoredJob :: App es => ReadRepoContext -> JobComment -> SlurmJob -> Eff es ()
watchRestoredJob ctx comment job = do
    let stepId = jobCommentStep comment
        commitText = T.pack (jobCommentCommit comment)
        certificate = jobCommentOutPath comment
        buildKey = buildKeyForOutPath certificate
        jobName = unBuildKey buildKey
    if isRunningState (slurmJobState job)
        then trackBuild stepId commitText $ do
            broadcastKnownStepStatus stepId commitText ("running", Nothing)
            finally (waitForCompletion buildKey) (unrootDerivationRootsForJob jobName)
        else unrootDerivationRootsForJob jobName
    certified <- isBuilt certificate
    if certified
        then do
            registerCertificationRoots ctx stepId certificate
            broadcastSingleStepForProjects stepId commitText certificate
        else broadcastFailedStepForProjects stepId commitText
    broadcastStepCertificateAtMovedHead stepId commitText
    when certified $ buildExtras ctx stepId

isPointyJob :: String -> Bool
isPointyJob name = "pointy-nix-build-" `isPrefixOf` name
