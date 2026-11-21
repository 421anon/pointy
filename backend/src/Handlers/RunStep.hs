{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.RunStep (
    StepTarget (..),
    buildExtras,
    getInputGraph,
    resolveStepTargets,
    restoreJobsFromSlurm,
    runStepHandler,
    stepLogHandler,
    stopStepHandler,
) where

import Bisect (bisectBatch)
import BuildLog (LogAccess (..), resolveBuildLog, validPaths)
import BuildRunner (BuildKey (..), JobComment (..), JobId (..), SlurmJob (..), StepRequirements (..), buildKeyForOutPath, cancel, decodeJobComment, encodeJobComment, isDerivationPath, isRunningState, queryJobIds, querySlurmJobs, realiseCommand, runningJobNames, submitAndWait, submitJob, waitForCompletion)
import Certificates (renderIdList)
import ClusterBus (buildingSteps, clearBuildRequest, markQueued, requestBuild, requestStop, stopRequested, takeStopRequest)
import ColdRefusal (clearColdRefusals, recordColdRefusal)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Exception (SomeException, catch)
import Control.Monad (foldM, forM_, unless, void, when)

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.List (intercalate, isPrefixOf, nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effects (App, AppM, Eval, Nix, coldStorageConfigured, listDerivationRootJobs, pathValid, rootDerivationForJob, rootStorePath, unrootDerivationRoot, unrootDerivationRootsForJob)
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (finally, onException)
import Handlers.Cold (coldFilesFor, mapColdProvider)
import Handlers.Statuses (broadcastFailedStepForProjects, broadcastKnownStepStatus, broadcastSingleStepForProjects, broadcastStatusForStepProjects, broadcastStepCertificateAtMovedHead, trackBuild)
import JobOutcome (jobOutcome, outcomeMessage)
import Servant (NoContent (..), err404, err500, errBody)
import StepLog (StepBuild, StepLogView, stepLogView, stepLogWithoutLog)
import System.Exit (ExitCode (..))
import UserRepo (ReadRepoContext (..), ensureRepoCommit, runNixEvalJsonApplyInRepo, runNixEvalJsonInRepo, runNixEvalRawInRepo, withReadRepoTransaction)
import StorageBus (outputsBuilt)

runStepHandler :: Int -> Maybe T.Text -> AppM NoContent
runStepHandler eid commit = do
    liftIO $ requestBuild eid
    _ <- liftIO $ forkIO $ runAppEffects $ runStepSync eid commit `finally` liftIO (clearBuildRequest eid)
    return NoContent

stopStepHandler :: Int -> Maybe T.Text -> AppM NoContent
stopStepHandler eid commit = do
    lift $ stopStepSync eid commit
    return NoContent

runStepSync :: App es => Int -> Maybe T.Text -> Eff es ()
runStepSync eid commit = do
    result <- runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
            halted = stopRequested eid
        outcomes <- ExceptT $ trackBuild eid (T.pack targetCommit) $ runExceptT $ do
            _ <- liftIO $ takeStopRequest eid
            graph <- getInputGraph halted ctx eid
            stepIds <- liftEither $ topoOrder graph
            liftIO $ clearColdRefusals (T.pack targetCommit) stepIds
            submitted <- lift $ submitGraph halted ctx graph stepIds
            stopped <- liftIO $ takeStopRequest eid
            when stopped $ lift $ mapM_ cancel [buildKey | Just (Enqueued _ buildKey _ _) <- [Map.lookup eid submitted]]
            return submitted
        unless (Map.member eid outcomes) $ lift $ broadcastStatusForStepProjects eid (T.pack targetCommit) Nothing
        liftIO $ mapConcurrently_ (runAppEffects . finishStep ctx) (Map.toList outcomes)

    case result of
        Left err -> liftIO $ putStrLn $ "runStepAsync error: " ++ err
        Right _ -> return ()

stepLogHandler :: Int -> Maybe T.Text -> AppM StepLogView
stepLogHandler eid commit = do
    result <- lift $ runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
        target <- resolveStepTarget ctx eid
        outcome <- liftIO $ jobOutcome (unBuildKey (buildKeyForOutPath (stepTargetPath target)))
        resolved <- lift $ resolveBuildLog RemoteLogs (stepTargetDrv target)
        let reason = outcomeMessage <$> outcome
        builds <- stepBuildsUpstreamOf ctx eid
        return $ case resolved of
            Just resolvedLog -> Just (stepLogView eid builds reason resolvedLog)
            Nothing -> stepLogWithoutLog eid builds <$> reason

    case result of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right Nothing ->
            throwError $
                err404
                    { errBody =
                        TLE.encodeUtf8 (TL.pack ("No build log available for step " ++ show eid))
                    }
        Right (Just view) -> return view

stepBuildsUpstreamOf :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) [StepBuild]
stepBuildsUpstreamOf ctx eid = do
    output <- runNixEvalJsonApplyInRepo ctx expression "#pointy.steps"
    either (throwError . (("Failed to decode the builds upstream of step " ++ show eid ++ ": ") ++)) pure $
        eitherDecode (TLE.encodeUtf8 (TL.pack output))
  where
    expression =
        unlines
            [ "steps:"
            , "let"
            , "  closure = builtins.genericClosure {"
            , "    startSet = [ { key = \"" ++ show eid ++ "\"; } ];"
            , "    operator = entry: map (id: { key = id; }) (steps.${entry.key}.dependencies or [ ]);"
            , "  };"
            , "  stdlibPhases = [ \"unpackPhase\" ];"
            , "  templatePhases = step: builtins.filter (attr: builtins.match \".*Phase\" attr != null && !(builtins.elem attr stdlibPhases)) (builtins.attrNames step.drvAttrs);"
            , "  describe = entry:"
            , "    let step = steps.${entry.key}; in {"
            , "      id = entry.key;"
            , "      name = step.def.name or \"\";"
            , "      build = step.drvPath;"
            , "      check = if step ? certificate then step.certificate.drvPath else null;"
            , "      phases = templatePhases step;"
            , "    };"
            , "in map describe closure"
            ]

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
    | NotSubmittedCold [Int] String

data InputCheck = InputsLocal | InputsCold String

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
    | not (null failedOn) =
        return $ NotSubmitted $ "dependency step(s) " ++ show failedOn ++ " could not be scheduled"
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
                            check <- checkOffloadedInputs ctx sid builtInputs
                            case (check, coldRoots) of
                                (InputsLocal, []) -> submitBuild ctx sid depJobIds certificate target buildKey
                                (InputsLocal, _) -> return (coldRefusal coldRoots [])
                                (InputsCold ownReads, _) -> return (coldRefusal (sid : coldRoots) [ownReads])
        return $ either NotSubmitted id result
  where
    builtInputs = [dep | dep <- deps, Just AlreadyCertified{} <- [Map.lookup dep outcomes]]
    failedOn = [d | d <- deps, depFailed (Map.lookup d outcomes)]
    depFailed (Just NotSubmitted{}) = True
    depFailed Nothing = True
    depFailed _ = False
    coldRoots = nub (concat [readers | d <- deps, Just (NotSubmittedCold readers _) <- [Map.lookup d outcomes]])
    coldRefusal readers ownReads =
        NotSubmittedCold readers $
            unwords (ownReads ++ [dependsOnColdReaders | not (null coldRoots)] ++ ["Copy them back from the step's status, then run again."])
    dependsOnColdReaders =
        "Step " ++ show sid ++ " depends on " ++ stepList coldRoots ++ ", which " ++ (if length coldRoots == 1 then "reads" else "read") ++ " files offloaded to cold storage."
    depJobIds = nub [jobId | d <- deps, Just (Enqueued _ _ jobIds _) <- [Map.lookup d outcomes], jobId <- jobIds]

submitBuild :: App es => ReadRepoContext -> Int -> [JobId] -> FilePath -> StepTarget -> BuildKey -> ExceptT String (Eff es) SubmitOutcome
submitBuild ctx sid depJobIds certificate target buildKey = do
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

checkOffloadedInputs :: App es => ReadRepoContext -> Int -> [Int] -> ExceptT String (Eff es) InputCheck
checkOffloadedInputs ctx sid builtInputs = do
    configured <- lift coldStorageConfigured
    if not configured
        then pure InputsLocal
        else do
            outPaths <- mapM (getStepOutPath ctx) builtInputs
            inputColdFiles <- lift $ zip builtInputs <$> mapColdProvider coldFilesFor outPaths
            let unknown = [input | (input, Nothing) <- inputColdFiles]
                offloaded = [(input, Set.toList files) | (input, Just files) <- inputColdFiles, not (Set.null files)]
            unless (null unknown) $
                throwError $
                    "Cannot check whether " ++ stepList unknown ++ ", read by step " ++ show sid ++ ", " ++ hasOrHave unknown ++ " files offloaded to cold storage. Run again when storage status is available."
            pure $
                if null offloaded
                    then InputsLocal
                    else
                        InputsCold $
                            "Step " ++ show sid ++ " reads files offloaded to cold storage: "
                                ++ intercalate "; " [fileList files ++ " of step " ++ show input | (input, files) <- offloaded]
                                ++ "."
  where
    hasOrHave inputs = if length inputs == 1 then "has" else "have"
    fileList files = case splitAt 3 files of
        (named, []) -> intercalate ", " (map T.unpack named)
        (named, rest) -> intercalate ", " (map T.unpack named) ++ " and " ++ show (length rest) ++ " more"

stepList :: [Int] -> String
stepList steps = "step" ++ (if length steps == 1 then " " else "s ") ++ intercalate ", " (map show steps)

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
            Left err -> return (Left err)
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
        liftIO outputsBuilt
        broadcastSingleStepForProjects sid targetCommitText certificate
        buildExtras ctx sid
    Enqueued certificate buildKey jobIds root -> do
        trackBuild sid targetCommitText $ do
            case jobIds of
                jobId : _ -> liftIO $ markQueued sid (unJobId jobId)
                [] -> pure ()
            broadcastKnownStepStatus sid targetCommitText ("running", Nothing)
            finally (waitForCompletion buildKey jobIds) (unrootDerivationRoot root)
        certified <- isBuilt certificate
        if certified
            then do
                registerCertificationRoots ctx sid certificate
                liftIO outputsBuilt
                broadcastSingleStepForProjects sid targetCommitText certificate
            else broadcastFailedStepForProjects sid targetCommitText
        broadcastStepCertificateAtMovedHead sid targetCommitText
        when certified $ buildExtras ctx sid
    NotSubmitted err -> do
        liftIO $ putStrLn $ "buildStep error: " ++ err
        broadcastKnownStepStatus sid targetCommitText ("failure", Just (T.pack err))
    NotSubmittedCold _ err -> do
        liftIO $ recordColdRefusal sid targetCommitText (T.pack err)
        liftIO $ putStrLn $ "buildStep inputs in cold storage: " ++ err
        broadcastKnownStepStatus sid targetCommitText ("inputs-cold", Just (T.pack err))
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

getInputGraph :: (Eval :> es, IOE :> es) => IO Bool -> ReadRepoContext -> Int -> ExceptT String (Eff es) (Map.Map Int [Int])
getInputGraph halted ctx root = do
    stop <- liftIO halted
    if stop
        then return Map.empty
        else do
            output <- runNixEvalJsonApplyInRepo ctx expression "#pointy.steps"
            entries <-
                either (throwError . (("Failed to decode the steps upstream of step " ++ show root ++ ": ") ++)) pure $
                    eitherDecode (TLE.encodeUtf8 (TL.pack output))
            return $ Map.fromList [(sid, nub inputs) | (sid, inputs) <- entries :: [(Int, [Int])]]
  where
    expression =
        unlines
            [ "steps:"
            , "let"
            , "  stepRefsIn = value:"
            , "    if builtins.isAttrs value && (value.type or null) != \"derivation\" then"
            , "      (if value ? step && (builtins.isInt value.step || builtins.isString value.step) then [ (toString value.step) ] else [ ])"
            , "      ++ builtins.concatMap stepRefsIn (builtins.attrValues value)"
            , "    else if builtins.isList value then builtins.concatMap stepRefsIn value"
            , "    else [ ];"
            , "  inputsOf = id:"
            , "    let upstream = steps.${id}.dependencies or [ ];"
            , "    in builtins.filter (ref: builtins.elem ref upstream) (stepRefsIn (steps.${id}.def.args or { }));"
            , "  closure = builtins.genericClosure {"
            , "    startSet = [ { key = \"" ++ show root ++ "\"; } ];"
            , "    operator = entry: map (id: { key = id; }) (inputsOf entry.key);"
            , "  };"
            , "in map (entry: [ (builtins.fromJSON entry.key) (map builtins.fromJSON (inputsOf entry.key)) ]) closure"
            ]

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
                            watchRestoredExtrasJob (slurmJobName job) (slurmJobState job) (slurmJobId job)
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

watchRestoredExtrasJob :: App es => String -> String -> JobId -> Eff es ()
watchRestoredExtrasJob jobName state jobId = do
    when (isRunningState state) $ waitForCompletion (BuildKey jobName) [jobId]
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
            finally (waitForCompletion buildKey [slurmJobId job]) (unrootDerivationRootsForJob jobName)
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
