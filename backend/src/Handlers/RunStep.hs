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

import BuildLog (LogSource (..), ResolvedLog (..), resolveBuildLog)
import BuildRunner (BuildKey (..), JobComment (..), JobId, SlurmJob (..), StepRequirements (..), buildKeyForOutPath, cancel, decodeJobComment, encodeJobComment, isRunningState, notifyJobEnded, queryJobIds, querySlurmJobs, submitAndWait, submitJob, waitForCompletion)
import ClusterBus (restoreRunningStepIds)
import Control.Concurrent (forkIO, forkIOWithUnmask)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (SomeException, catch)
import Control.Monad (foldM, void, when)

import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), eitherDecode, withObject, (.:))
import Data.List (foldl', isPrefixOf, nub, partition)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effectful (Eff, (:>))
import Effects (App, AppM, Eval, Nix, pathValid, rootStorePath)
import Handlers.Statuses (broadcastFailedStepForProjects, broadcastKnownStepStatus, broadcastSingleStepForProjects, broadcastStatusForStepProjects)
import Servant (NoContent (..), err404, err500, errBody)
import System.Exit (ExitCode (..))
import UserRepo (ReadRepoContext (..), ensureRepoCommit, runNixEvalJsonApplyInRepo, runNixEvalJsonInRepo, runNixEvalRawInRepo, withReadRepoTransaction)

runStepHandler :: Int -> Maybe T.Text -> AppM NoContent
runStepHandler eid commit = do
    _ <- liftIO $ forkIO $ runAppEffects $ runStepSync eid commit
    return NoContent

stopStepHandler :: Int -> Maybe T.Text -> AppM NoContent
stopStepHandler eid commit = do
    lift $ stopStepSync eid commit
    return NoContent

jobEndedHandler :: String -> AppM NoContent
jobEndedHandler name = do
    lift $ notifyJobEnded (BuildKey name)
    return NoContent

runStepSync :: App es => Int -> Maybe T.Text -> Eff es ()
runStepSync eid commit = do
    result <- runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    return (repoPath, maybe commitHash T.unpack commit)

        let ctx = ReadRepoContext repoPath targetCommit
        graph <- getDependencyGraph ctx eid
        stepIds <- liftEither $ topoOrder graph

        outcomes <- lift $ submitGraph ctx graph stepIds
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
        lift $ resolveBuildLog (stepTargetDrv target)

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
renderResolvedLog (ResolvedLog _ logText StepDrv) = T.pack logText
renderResolvedLog (ResolvedLog drv logText (InputDrv _ _)) =
    T.pack ("Build prerequisite failed: " ++ drv ++ "\n-----\n" ++ logText)

repoInstallable :: ReadRepoContext -> String -> String
repoInstallable (ReadRepoContext repoPath targetCommit) fragment =
    "git+file://" ++ repoPath ++ "?rev=" ++ targetCommit ++ "&allRefs=true#" ++ fragment

stepAttr :: Int -> String
stepAttr eid = "pointy.steps." ++ show eid

data StepTarget = StepTarget
    { stepTargetCertified :: Bool
    , stepTargetPath :: FilePath
    , stepTargetDrv :: FilePath
    }

instance FromJSON StepTarget where
    parseJSON = withObject "StepTarget" $ \o -> StepTarget <$> o .: "certified" <*> o .: "path" <*> o .: "drv"

resolveStepTarget :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) StepTarget
resolveStepTarget ctx eid = do
    output <- runNixEvalJsonApplyInRepo ctx certificateOrLegacyStep ('#' : stepAttr eid)
    either (throwError . (("Failed to decode the build target of step " ++ show eid ++ ": ") ++)) pure $
        eitherDecode (TLE.encodeUtf8 (TL.pack output))
  where
    certificateOrLegacyStep = "step: { certified = step ? certificate; path = builtins.unsafeDiscardStringContext (step.certificate or step).outPath; drv = builtins.unsafeDiscardStringContext (step.certificate or step).drvPath; }"

stepTargetInstallable :: ReadRepoContext -> Int -> StepTarget -> String
stepTargetInstallable ctx eid target =
    repoInstallable ctx $ stepAttr eid ++ if stepTargetCertified target then ".certificate" else ""

extrasInstallable :: ReadRepoContext -> Int -> String
extrasInstallable ctx eid = repoInstallable ctx (stepAttr eid ++ ".meta.pointy.extras")

data SubmitOutcome
    = AlreadyCertified FilePath
    | Enqueued FilePath BuildKey [JobId]
    | NotSubmitted String

submitGraph :: App es => ReadRepoContext -> Map.Map Int [Int] -> [Int] -> Eff es (Map.Map Int SubmitOutcome)
submitGraph ctx graph = foldM submitOne Map.empty
  where
    submitOne outcomes sid = do
        outcome <- submitStep ctx outcomes (Map.findWithDefault [] sid graph) sid
        return $ Map.insert sid outcome outcomes

submitStep :: App es => ReadRepoContext -> Map.Map Int SubmitOutcome -> [Int] -> Int -> Eff es SubmitOutcome
submitStep ctx outcomes deps sid
    | not (null blockedOn) =
        return $ NotSubmitted $ "dependency step(s) " ++ show blockedOn ++ " could not be scheduled"
    | otherwise = do
        result <- runExceptT $ do
            target <- resolveStepTarget ctx sid
            let certificate = stepTargetPath target
            certified <- lift $ isBuilt certificate
            let buildKey = buildKeyForOutPath certificate
            if certified
                then return $ AlreadyCertified certificate
                else do
                    existing <- lift $ queryJobIds buildKey
                    if not (null existing)
                        then return $ Enqueued certificate buildKey existing
                        else do
                            requirements <- getStepRequirements ctx sid
                            submitted <-
                                lift $
                                    submitJob
                                        requirements
                                        buildKey
                                        depJobIds
                                        (encodeJobComment (JobComment "step" sid (readCommitHash ctx) certificate))
                                        ["nix", "build", "--no-link", "--no-eval-cache", stepTargetInstallable ctx sid target]
                            case submitted of
                                Left err -> throwError err
                                Right jobId -> return $ Enqueued certificate buildKey [jobId]
        return $ either NotSubmitted id result
  where
    blockedOn = [d | d <- deps, isBlocked (Map.lookup d outcomes)]
    isBlocked (Just NotSubmitted{}) = True
    isBlocked Nothing = True
    isBlocked _ = False
    depJobIds = nub [jobId | d <- deps, Just (Enqueued _ _ jobIds) <- [Map.lookup d outcomes], jobId <- jobIds]

finishStep :: App es => ReadRepoContext -> (Int, SubmitOutcome) -> Eff es ()
finishStep ctx (sid, outcome) = case outcome of
    AlreadyCertified certificate -> do
        rootStorePath certificate
        broadcastSingleStepForProjects sid targetCommitText certificate
        buildExtras ctx sid
    Enqueued certificate buildKey _ -> do
        broadcastKnownStepStatus sid targetCommitText ("running", Nothing)
        waitForCompletion buildKey
        certified <- isBuilt certificate
        if certified
            then do
                registerCertificationRoots ctx sid certificate
                broadcastSingleStepForProjects sid targetCommitText certificate
                buildExtras ctx sid
            else broadcastFailedStepForProjects sid targetCommitText
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
        mExtrasPath <- getExtrasOutPath ctx eid
        case mExtrasPath of
            Nothing -> return ()
            Just extrasPath -> do
                built <- lift $ isBuilt extrasPath
                if built
                    then lift $ rootStorePath extrasPath
                    else do
                        requirements <- getExtrasRequirements ctx eid
                        let buildKey = buildKeyForOutPath extrasPath
                        exitCode <-
                            lift $
                                submitAndWait
                                    requirements
                                    buildKey
                                    (encodeJobComment (JobComment "extras" eid (readCommitHash ctx) extrasPath))
                                    ["nix", "build", "--no-link", "--no-eval-cache", extrasInstallable ctx eid]
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

getExtrasOutPath :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) (Maybe FilePath)
getExtrasOutPath ctx eid = do
    result <-
        lift $
            runExceptT $
                runNixEvalRawInRepo ctx ("#pointy.steps." ++ show eid ++ ".meta.pointy.extras.outPath")
    case result of
        Left _ -> return Nothing
        Right path ->
            if null path
                then return Nothing
                else return (Just path)

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

getDependencyGraph :: (Eval :> es) => ReadRepoContext -> Int -> ExceptT String (Eff es) (Map.Map Int [Int])
getDependencyGraph ctx root = go Map.empty [root]
  where
    go acc [] = return acc
    go acc (sid : rest)
        | Map.member sid acc = go acc rest
        | otherwise = do
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
    result <- runExceptT $ do
        (repoPath, targetCommit) <-
            ExceptT $
                withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) ->
                    let targetCommit = fromMaybe (T.pack commitHash) commit
                     in return (repoPath, targetCommit)

        let ctx = ReadRepoContext repoPath (T.unpack targetCommit)

        mExtrasPath <- getExtrasOutPath ctx eid
        lift $ case mExtrasPath of
            Just extrasPath -> cancel (buildKeyForOutPath extrasPath)
            Nothing -> return ()

        target <- resolveStepTarget ctx eid
        lift $ cancel $ buildKeyForOutPath $ stepTargetPath target
        lift $ broadcastStatusForStepProjects eid targetCommit Nothing

    case result of
        Left err -> liftIO $ putStrLn $ "stopStep error: " ++ err
        Right _ -> return ()

restoreJobsFromSlurm :: App es => Eff es ()
restoreJobsFromSlurm = do
    eRepo <- withReadRepoTransaction $ \(ReadRepoContext repoPath _) -> return repoPath
    case eRepo of
        Left err -> liftIO $ putStrLn $ "restoreJobsFromSlurm: cannot access repo: " ++ err
        Right repoPath ->
            liftIO $
                void $
                    restoreRunningStepIds $
                        scanJobs repoPath
                            `catch` \e -> do
                                putStrLn $ "restoreJobsFromSlurm failed: " ++ show (e :: SomeException)
                                return Set.empty
  where
    scanJobs :: FilePath -> IO (Set Int)
    scanJobs repoPath = do
        jobs <- runAppEffects querySlurmJobs
        let pointyJobs = [job | job <- jobs, isPointyJob (slurmJobName job)]
        attachWatchers repoPath pointyJobs

    attachWatchers :: FilePath -> [SlurmJob] -> IO (Set Int)
    attachWatchers repoPath = go Set.empty
      where
        go _ [] = return Set.empty
        go seen (job : rest)
            | Set.member (slurmJobName job) seen = go seen rest
            | otherwise = do
                recovered <- attachOne repoPath job
                restRecovered <- go (Set.insert (slurmJobName job) seen) rest
                return (Set.union recovered restRecovered)

    attachOne :: FilePath -> SlurmJob -> IO (Set Int)
    attachOne repoPath job = case decodeJobComment =<< slurmJobComment job of
        Nothing -> do
            putStrLn $ "restoreJobsFromSlurm: no job comment on " ++ slurmJobName job ++ ", skipping"
            return Set.empty
        Just comment
            | jobCommentKind comment /= "step" -> return Set.empty
            | buildKeyForOutPath (jobCommentOutPath comment) /= BuildKey (slurmJobName job) -> do
                putStrLn $
                    "restoreJobsFromSlurm: job name mismatch for step "
                        ++ show (jobCommentStep comment)
                        ++ ", skipping"
                return Set.empty
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
                        return Set.empty
                    Right () -> do
                        let ctx = ReadRepoContext repoPath (jobCommentCommit comment)
                        void $ forkIOWithUnmask $ \unmask -> unmask $ runAppEffects $ watchRestoredJob ctx comment job
                        return $
                            if isRunningState (slurmJobState job)
                                then Set.singleton (jobCommentStep comment)
                                else Set.empty

watchRestoredJob :: App es => ReadRepoContext -> JobComment -> SlurmJob -> Eff es ()
watchRestoredJob ctx comment job = do
    let commitText = T.pack (jobCommentCommit comment)
        certificate = jobCommentOutPath comment
        buildKey = buildKeyForOutPath certificate
    when (isRunningState (slurmJobState job)) $ do
        broadcastKnownStepStatus (jobCommentStep comment) commitText ("running", Nothing)
        waitForCompletion buildKey
    certified <- isBuilt certificate
    if certified
        then do
            registerCertificationRoots ctx (jobCommentStep comment) certificate
            broadcastSingleStepForProjects (jobCommentStep comment) commitText certificate
            buildExtras ctx (jobCommentStep comment)
        else broadcastFailedStepForProjects (jobCommentStep comment) commitText

isPointyJob :: String -> Bool
isPointyJob name = "pointy-nix-build-" `isPrefixOf` name
