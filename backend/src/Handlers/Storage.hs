{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Storage (
    deleteRevisionHandler,
    offloadHandler,
    offloadStepHandler,
    onloadRevisionHandler,
    pathOwners,
    HeadStep (..),
    runStorageCenter,
    storageHandler,
) where

import BuildRunner (JobComment (..), SlurmJob (..), decodeJobComment, isRunningState, parseSlurmJobLine)
import ClusterBus (latestSlurmJobs)
import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..), OffloadProgress (..))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, retry, writeTVar)
import Control.Exception (SomeException, bracket_, displayException, try)
import Control.Monad (join, unless, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.!=), (.:), (.:?))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (isPrefixOf, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, UnliftStrategy (SeqUnlift), liftIO, withEffToIO, (:>))
import EvalOutput (evalOutputText)
import Effects (AppEffects, AppM, Eval, Nix, SlurmQuery (..), coldStorageConfigured, coldStorageDelete, coldStorageOffload, pathValidity, querySlurm, rootStorePath, runNixCli, runNixStoreCli, unrootStorePath)
import Handlers.Cold (coldFilesFor, coldStatusFor, mapColdProvider, onloadable, startOnload)
import Handlers.StepOutput (resolveStepOutPath)
import NixEvaluator (EvalFailure (..), historyEvaluatorWorkers)
import NixStore (rootedPath)
import Processes (cli)
import Servant (NoContent (..), ServerError (..), err400, err409, err500, err502, err503)
import StepEvaluations (CachedStep (..), StepEvaluations, appendEvaluationRecords, evaluatedCommit, loadStepEvaluations, recordEvaluation)
import StorageBus (ColdRecheck (..), Invalidation (..), OffloadState (..), StorageBlocker (..), StorageCapacity (..), StorageRevision (..), StorageView (..), beginOffload, currentView, endOffload, invalidate, knownColdCapacity, knownColdStatuses, offloadStates, parseStorageCapacity, publishView, recordOffloadProgress, recheckCold, revisionMovable, takeInvalidation, trackPaths, viewInputsChanged)
import StorageHistory (ColdSummary (..), RevisionSeed (..), directReadersOf, parseHistory, revisionsFromHistory, summarizeCold)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import UserRepo (commitContext, commitContextEff, runGitIn, runHistoryEvalJsonApplyInRepo, runNixEvalJsonApplyInRepo, runNixEvalRawInRepo, userRepoPath)

data HeadStep = HeadStep
    { headStepName :: Text
    , headStepInputs :: Set Int
    , headStepOutput :: Text
    , headStepCertificate :: Text
    }
    deriving (Eq, Show)

instance FromJSON HeadStep where
    parseJSON = withObject "head step" $ \fields ->
        HeadStep
            <$> fields .:? "name" .!= ""
            <*> (Set.fromList <$> fields .:? "inputs" .!= [])
            <*> fields .: "output"
            <*> fields .: "certificate"

newtype PathInfo = PathInfo {pathNarSize :: Integer}

instance FromJSON PathInfo where
    parseJSON = withObject "path info" $ \fields -> PathInfo <$> fields .: "narSize"

newtype EvalEntry = EvalEntry (Int, CachedStep)

instance FromJSON EvalEntry where
    parseJSON = withObject "step evaluation" $ \fields -> do
        stepId <- fields .: "id"
        name <- fields .:? "name" .!= ""
        path <- fields .:? "path"
        kind <- fields .: "kind"
        pure (EvalEntry (stepId, CachedStep name path kind))

data Caches = Caches
    { cachedHead :: Maybe Text
    , cachedRelevant :: [(Text, UTCTime)]
    , cachedHeadSteps :: Either Text (Map Int HeadStep)
    , cachedSizes :: Map Text (Maybe Integer)
    , cachedHistoryError :: Maybe Text
    }

storageHistoryLimit :: Int
storageHistoryLimit = 5000

scanRetryDelaySeconds :: Int
scanRetryDelaySeconds = 60

pathInfoChunkSize :: Int
pathInfoChunkSize = 200

coalesceMicros :: Int
coalesceMicros = 500000

capacityCacheSeconds :: NominalDiffTime
capacityCacheSeconds = 30

capacityCommandTimeout :: Int
capacityCommandTimeout = 5000000

{-# NOINLINE cachesVar #-}
cachesVar :: TVar Caches
cachesVar = unsafePerformIO (newTVarIO (Caches Nothing [] (Left "The repository has not been read yet.") Map.empty Nothing))

{-# NOINLINE evaluationsRef #-}
evaluationsRef :: MVar (Maybe StepEvaluations)
evaluationsRef = unsafePerformIO (newMVar Nothing)

{-# NOINLINE movablePathsVar #-}
movablePathsVar :: TVar (Map Int (Set Text))
movablePathsVar = unsafePerformIO (newTVarIO Map.empty)

data ScanState = ScanState
    { scanRunning :: Bool
    , scanUnavailable :: Maybe (UTCTime, Text)
    }

data ScanQueue = ScanQueue
    { queuePending :: [Text]
    , queueFailure :: Maybe (UTCTime, Text)
    }

{-# NOINLINE scanStateRef #-}
scanStateRef :: MVar ScanState
scanStateRef = unsafePerformIO (newMVar (ScanState False Nothing))

{-# NOINLINE capacityCacheRef #-}
capacityCacheRef :: MVar (Maybe (UTCTime, Maybe StorageCapacity))
capacityCacheRef = unsafePerformIO (newMVar Nothing)

storageHandler :: AppM StorageView
storageHandler = liftIO $ do
    invalidate mempty{invalidView = True, invalidBroadcast = True}
    atomically (currentView >>= maybe retry pure)

offloadHandler :: Int -> Maybe Text -> AppM NoContent
offloadHandler stepId mCommit = do
    configured <- lift coldStorageConfigured
    unless configured $ throwError err400{errBody = "Cold storage is not configured."}
    outPath <- resolveStepOutPath stepId mCommit
    withPathLock outPath $ do
        refusePathBusy outPath
        started <- liftIO (beginOffload outPath)
        when started $ liftIO $ void $ forkIO $ do
            outcome <- try (runAppEffects (coldStorageOffload outPath (recordOffloadProgress outPath)))
            endOffload outPath (either (\(err :: SomeException) -> Left (T.pack (displayException err))) id outcome)
        pure NoContent

offloadStepHandler :: Int -> AppM NoContent
offloadStepHandler stepId = do
    configured <- lift coldStorageConfigured
    unless configured $ throwError err400{errBody = "Cold storage is not configured."}
    withPathLock (T.pack (show stepId)) $ do
        paths <- liftIO (Map.findWithDefault Set.empty stepId <$> readTVarIO movablePathsVar)
        mapM_ refusePathBusy (Set.toList paths)
        unless (Set.null paths) $
            liftIO $ void $ forkIO $ mapM_ offloadPath (Set.toList paths)
        pure NoContent
  where
    offloadPath outPath = do
        outcome <- runAppEffects (runExceptT (withPathLock outPath $ do
            refusePathBusy outPath
            liftIO (beginOffload outPath)))
        case outcome of
            Left refused -> endOffload outPath (Left (TE.decodeUtf8Lenient (LBS.toStrict (errBody refused))))
            Right started -> when started $ do
                result <- try (runAppEffects (coldStorageOffload outPath (recordOffloadProgress outPath)))
                endOffload outPath (either (\(err :: SomeException) -> Left (T.pack (displayException err))) id result)

onloadRevisionHandler :: Int -> Maybe Text -> AppM NoContent
onloadRevisionHandler stepId mCommit = do
    configured <- lift coldStorageConfigured
    unless configured $ throwError err400{errBody = "Cold storage is not configured."}
    outPath <- resolveStepOutPath stepId mCommit
    withPathLock outPath $ do
        refusePathBusy outPath
        refuseOffloadRunning outPath
        status <- lift (coldStatusFor outPath)
        case status of
            Nothing -> throwError err503{errBody = "Cannot read the cold storage status. Try again when storage status is available."}
            Just cold -> do
                let files = filter onloadable (coldStatusFiles cold)
                outcomes <- lift $ mapColdProvider (startOnload outPath . T.unpack . coldFilePath) files
                liftIO (followOnloads outPath)
                let failures =
                        [ coldFilePath file <> ": " <> message
                        | (file, Left message) <- zip files outcomes
                        ]
                unless (null failures) $
                    throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 ("Could not start copying back: " <> T.intercalate "; " failures))}
                pure NoContent

{-# NOINLINE onloadWatchersRef #-}
onloadWatchersRef :: MVar (Set Text)
onloadWatchersRef = unsafePerformIO (newMVar Set.empty)

followOnloads :: Text -> IO ()
followOnloads outPath = do
    start <- modifyMVar onloadWatchersRef (\watchers -> pure (Set.insert outPath watchers, Set.notMember outPath watchers))
    when start $ void $ forkIO $ do
        threadDelay 1000000
        settled <- runAppEffects $ do
            status <- coldStatusFor outPath
            pure $ maybe False (all (notOnloading . coldFileState) . coldStatusFiles) status
        modifyMVar_ onloadWatchersRef (pure . Set.delete outPath)
        if settled then recheckCold outPath else followOnloads outPath
  where
    notOnloading Onloading{} = False
    notOnloading _ = True

deleteRevisionHandler :: Int -> Maybe Text -> AppM NoContent
deleteRevisionHandler stepId mCommit = do
    outPath <- resolveStepOutPath stepId mCommit
    withPathLock outPath $ do
        refusePathBusy outPath
        refuseOffloadRunning outPath
        certificate <- certificateForPath stepId mCommit outPath
        let path = T.unpack outPath
            certificatePath = T.unpack certificate
        validity <- lift (pathValidity path)
        case validity of
            Left err -> throwError err503{errBody = LBS.fromStrict (TE.encodeUtf8 ("Cannot check whether the output exists: " <> TE.decodeUtf8Lenient (LBS.toStrict err)))}
            Right False -> pure ()
            Right True -> do
                cold <- lift (coldFilesFor outPath)
                case cold of
                    Nothing -> throwError err503{errBody = "Cannot check whether this output has files in cold storage. Try again when storage status is available."}
                    Just files
                        | Set.null files -> pure ()
                        | otherwise -> do
                            deleted <- lift (coldStorageDelete outPath)
                            case deleted of
                                Left message -> throwError err502{errBody = LBS.fromStrict (TE.encodeUtf8 ("Could not delete the output from cold storage: " <> message))}
                                Right () -> liftIO (recheckCold outPath)
                lift (removeCertificationEvidence path certificatePath)
                (code, _, err) <- lift (runNixStoreCli ["--delete", path])
                case code of
                    ExitSuccess -> pure ()
                    ExitFailure _ -> do
                        lift (rootStorePath path)
                        lift (rootStorePath certificatePath)
                        let message = "The store refused to delete this output: " <> storeErrorLine (TE.decodeUtf8Lenient err)
                        throwError err409{errBody = LBS.fromStrict (TE.encodeUtf8 message)}
        liftIO $ do
            recheckCold outPath
            invalidate mempty{invalidOutputs = True}
        pure NoContent

{-# NOINLINE pathMutationSemaphore #-}
pathMutationSemaphore :: QSem
pathMutationSemaphore = unsafePerformIO (newQSem 1)

withPathLock :: Text -> AppM a -> AppM a
withPathLock _ action = ExceptT $ withEffToIO SeqUnlift $ \unlift ->
    bracket_ (waitQSem pathMutationSemaphore) (signalQSem pathMutationSemaphore) (unlift (runExceptT action))

storeErrorLine :: Text -> Text
storeErrorLine stderr =
    case [T.strip line | line <- T.lines stderr, "error:" `T.isInfixOf` line] of
        [] -> T.strip stderr
        errors -> last errors

removeCertificationEvidence :: IOE :> es => FilePath -> FilePath -> Eff es ()
removeCertificationEvidence outPath certificate = do
    unrootStorePath outPath
    unrootStorePath certificate

certificatePreparationError :: String -> ServerError
certificatePreparationError reason = err500{errBody = LBS.fromStrict (TE.encodeUtf8 ("Cannot prepare the output's certificate revision: " <> T.pack reason))}

certificateForPath :: Int -> Maybe Text -> Text -> AppM Text
certificateForPath stepId mCommit outPath = case mCommit of
    Just commit -> do
        repoPath <- liftIO userRepoPath
        ctx <- ExceptT $ either (Left . certificatePreparationError) Right <$> runExceptT (commitContextEff repoPath commit)
        result <- lift $ runExceptT $ runNixEvalRawInRepo ctx ("#pointy.steps." ++ show stepId ++ ".certificate.outPath")
        either (const (pure outPath)) (pure . evalOutputText) result
    Nothing -> do
        caches <- liftIO (readTVarIO cachesVar)
        case cachedHeadSteps caches of
            Left err -> throwError err503{errBody = LBS.fromStrict (TE.encodeUtf8 err)}
            Right steps -> case headStepCertificate <$> Map.lookup stepId steps of
                Just certificate -> pure certificate
                Nothing -> throwError err503{errBody = "Cannot resolve the output's certificate."}

refusePathBusy :: Text -> AppM ()
refusePathBusy outPath = do
    caches <- liftIO (readTVarIO cachesVar)
    running <- runningSteps
    case (cachedHeadSteps caches, running) of
        (Left err, _) -> throwError err503{errBody = LBS.fromStrict (TE.encodeUtf8 err)}
        (_, Left err) -> throwError err503{errBody = LBS.fromStrict (TE.encodeUtf8 err)}
        (Right steps, Right states) ->
            case [(owner, blocker) | owner <- pathOwners steps outPath, Just blocker <- [runningRefusal steps states owner]] of
                (owner, blocker) : _ -> throwError err409{errBody = LBS.fromStrict (TE.encodeUtf8 (storageBlockerMessage owner blocker))}
                [] -> pure ()

pathOwners :: Map Int HeadStep -> Text -> [Int]
pathOwners steps outPath = Map.keys (Map.filter ((== outPath) . headStepOutput) steps)

refuseOffloadRunning :: Text -> AppM ()
refuseOffloadRunning outPath = do
    offloads <- liftIO offloadStates
    case Map.lookup outPath offloads of
        Just (OffloadRunning _) ->
            throwError err409{errBody = "This output is being moved to cold storage. Wait for the move to finish, then try again."}
        _ -> pure ()

runningSteps :: AppM (Either Text (Map Int Text))
runningSteps = do
    result <- lift (querySlurm AllJobs)
    pure $ case result of
        Left err -> Left (T.pack err)
        Right output -> Right (runningStepStates (mapMaybe parseSlurmJobLine (lines output)))

runningStepStates :: [SlurmJob] -> Map Int Text
runningStepStates jobs =
    Map.fromList
        [ (jobCommentStep comment, T.pack (slurmJobState job))
        | job <- jobs
        , isRunningState (slurmJobState job)
        , Just comment <- [decodeJobComment =<< slurmJobComment job]
        , jobCommentKind comment == "step" || jobCommentKind comment == "extras"
        ]

runningRefusal :: Map Int HeadStep -> Map Int Text -> Int -> Maybe StorageBlocker
runningRefusal headSteps running stepId =
    case Map.lookup stepId running of
        Just jobState -> Just (blockerFor stepId jobState)
        Nothing ->
            case sortOn fst [(reader, jobState) | reader <- Set.toList readers, Just jobState <- [Map.lookup reader running]] of
                [] -> Nothing
                (reader, jobState) : _ -> Just (blockerFor reader jobState)
  where
    readers = directReadersOf (Map.map headStepInputs headSteps) stepId
    blockerFor blockerId jobState =
        StorageBlockerStep
            { blockerStep = blockerId
            , blockerName = stepName blockerId
            , blockerState = if jobState `elem` ["PENDING", "CONFIGURING"] then "queued" else "running"
            }
    stepName blockerId = case Map.lookup blockerId headSteps of
        Just step | not (T.null (headStepName step)) -> headStepName step
        _ -> "step " <> T.pack (show blockerId)

storageBlockerMessage :: Int -> StorageBlocker -> Text
storageBlockerMessage owner blocker = case blocker of
    StorageBlockerStep step name state
        | step == owner -> "This step is " <> state <> "."
        | otherwise -> "Step " <> T.pack (show step) <> " (" <> name <> ") is " <> state <> " and reads this output."
    StorageBlockerError message -> message

runStorageCenter :: IO ()
runStorageCenter = do
    invalidate mempty{invalidHead = True}
    loop Map.empty
  where
    loop previousRunning = do
        (work, running) <- atomically $ do
            work <- takeInvalidation
            running <- runningStepStates <$> latestSlurmJobs
            when (work == mempty && running == previousRunning) retry
            pure (work, running)
        outcome <- try (runAppEffects (refresh work running))
        either reportFailure pure outcome
        threadDelay coalesceMicros
        loop running
    reportFailure (err :: SomeException) = do
        putStrLn ("Storage refresh failed: " ++ displayException err)
        now <- getCurrentTime
        published <- atomically currentView
        when (isNothing published) $
            publishView
                False
                ( StorageView
                    { storageColdConfigured = False
                    , storageLocal = 0
                    , storageCold = 0
                    , storageCapacity = Nothing
                    , storageCapacityFreeBytes = Nothing
                    , storageColdCapacity = Nothing
                    , storageScanning = False
                    , storageScanned = 0
                    , storageHistory = 0
                    , storageTruncated = False
                    , storageRevisionCount = 0
                    , storageRevisions = []
                    , storageError = Just "Storage usage is unavailable. The server could not read it."
                    , storageUpdatedAt = now
                    }
                )

refresh :: Invalidation -> Map Int Text -> Eff AppEffects ()
refresh work running = do
    repoPath <- liftIO userRepoPath
    previous <- liftIO (readTVarIO cachesVar)
    caches <-
        if invalidHead work || isNothing (cachedHead previous)
            then refreshHistory repoPath previous
            else pure previous
    evaluations <- liftIO readEvaluations
    let relevant = cachedRelevant caches
        seeds = revisionsFromHistory evaluations relevant
        scannedCount = length [() | (commit, _) <- relevant, evaluatedCommit commit evaluations]
        pending = [commit | (commit, _) <- relevant, not (evaluatedCommit commit evaluations)]
        toScan = take (max 0 (storageHistoryLimit - scannedCount)) pending
        seedPaths = Set.fromList (map seedPath seeds)
        unknown = Set.filter (unknownSize (invalidOutputs work) (cachedSizes caches)) seedPaths
    liftIO (startScan repoPath toScan)
    sizeOutcome <- pathSizes (Set.toList unknown)
    let sizes = Map.restrictKeys (either (const (cachedSizes caches)) (\found -> Map.union (Map.fromSet (`Map.lookup` found) unknown) (cachedSizes caches)) sizeOutcome) seedPaths
        present = Map.mapMaybe id sizes
    liftIO $ atomically $ writeTVar cachesVar caches{cachedSizes = sizes}
    liftIO (trackPaths (Map.keysSet present))
    coldConfigured <- coldStorageConfigured
    known <- liftIO knownColdStatuses
    let recheck = case invalidCold work of
            RecheckAll -> Map.keysSet present
            RecheckPaths paths -> Set.intersection paths (Map.keysSet present)
            RecheckNone -> Set.empty
        toCheck = Set.union recheck (Map.keysSet (Map.difference present known))
    when coldConfigured $ void $ mapColdProvider coldStatusFor (Set.toList toCheck)
    colds <- liftIO knownColdStatuses
    coldCapacityBytes <- liftIO knownColdCapacity
    offloads <- liftIO offloadStates
    capacity <- liftIO (capacityUsage (invalidBroadcast work))
    now <- liftIO getCurrentTime
    scanFailure <- liftIO (fmap (fmap snd . scanUnavailable) (readMVar scanStateRef))
    let blockedReason = case cachedHeadSteps caches of
            Right headSteps -> runningRefusal headSteps running
            Left _ -> const (Just (StorageBlockerError "Cannot tell which steps read this output until the current revision evaluates."))
        revisionsWithSeeds =
            [ (seed, storageRevision coldConfigured (Map.lookup (seedPath seed) colds) (Map.lookup (seedPath seed) offloads) (blockedReason (seedStep seed)) (headStepName <$> either (const Nothing) (Map.lookup (seedStep seed)) (cachedHeadSteps caches)) seed narSize)
            | seed <- seeds
            , Just narSize <- [Map.lookup (seedPath seed) present]
            ]
        revisions =
            sortOn
                (\revision -> (negate (revisionLocal revision), negate (revisionCold revision)))
                (map snd revisionsWithSeeds)
        movablePaths =
            Map.fromListWith Set.union
                [ (seedStep seed, Set.singleton (seedPath seed))
                | (seed, revision) <- revisionsWithSeeds
                , revisionMovable revision
                ]
        localBytes = sum (map revisionLocal revisions)
        coldBytes = sum (map revisionCold revisions)
    liftIO $ atomically $ writeTVar movablePathsVar movablePaths
    liftIO $
        publishView
            (invalidBroadcast work)
            StorageView
                { storageColdConfigured = coldConfigured
                , storageLocal = localBytes
                , storageCold = coldBytes
                , storageCapacity = stepOutputCapacity localBytes capacity
                , storageCapacityFreeBytes = if maybe False ((== "filesystem") . capacityKind) capacity then capacityAvailableBytes <$> capacity else Nothing
                , storageColdCapacity = if coldConfigured then coldCapacityBytes else Nothing
                , storageScanning = not (null toScan)
                , storageScanned = scannedCount
                , storageHistory = length relevant
                , storageTruncated = null toScan && not (null pending)
                , storageRevisionCount = length revisions
                , storageRevisions = revisions
                , storageError =
                    firstError
                        [ cachedHistoryError caches
                        , either Just (const Nothing) sizeOutcome
                        , either Just (const Nothing) (cachedHeadSteps caches)
                        , if null toScan
                            then Nothing
                            else fmap (\reason -> "Some revisions could not be evaluated yet and will be retried: " <> reason) scanFailure
                        ]
                , storageUpdatedAt = now
                }
  where
    firstError errors = case [message | Just message <- errors] of
        message : _ -> Just message
        [] -> Nothing

unknownSize :: Bool -> Map Text (Maybe Integer) -> Text -> Bool
unknownSize recheckAll sizes path = recheckAll || Map.notMember path sizes

stepOutputCapacity :: Integer -> Maybe StorageCapacity -> Maybe StorageCapacity
stepOutputCapacity used capacity =
    fmap
        ( \reported ->
            if capacityKind reported == "quota"
                then
                    reported
                        { capacityUsedBytes = used
                        , capacityAvailableBytes = max 0 (capacityTotalBytes reported - used)
                        }
                else reported
        )
        capacity

storageRevision :: Bool -> Maybe (Maybe ColdStatus) -> Maybe OffloadState -> Maybe StorageBlocker -> Maybe Text -> RevisionSeed -> Integer -> StorageRevision
storageRevision coldConfigured status offload blocked currentName seed narSize =
    StorageRevision
        { revisionStep = seedStep seed
        , revisionCurrentName = currentName
        , revisionName = if T.null (seedName seed) then "Step " <> T.pack (show (seedStep seed)) else seedName seed
        , revisionKind = seedKind seed
        , revisionCommit = seedCommit seed
        , revisionCommittedAt = seedCommitTime seed
        , revisionLocal = coldSummaryLocal summary
        , revisionCold = coldSummaryCold summary
        , revisionState = coldSummaryState summary
        , revisionDone = coldSummaryDone summary
        , revisionTotal = coldSummaryTotal summary
        , revisionError = coldSummaryError summary
        , revisionBlocked = blocked
        }
  where
    provided
        | coldConfigured = summarizeCold narSize (join status)
        | otherwise = ColdSummary "local" narSize 0 0 0 Nothing
    summary = case offload of
        Just (OffloadRunning (OffloadProgress done total)) -> provided{coldSummaryState = "offloading", coldSummaryDone = done, coldSummaryTotal = total}
        Just (OffloadFailed message) -> provided{coldSummaryError = Just ("Moving to cold storage failed: " <> message)}
        Nothing -> provided

refreshHistory :: FilePath -> Caches -> Eff AppEffects Caches
refreshHistory repoPath caches = do
    headResult <- liftIO (repoHead repoPath)
    case headResult of
        Left err -> pure caches{cachedHistoryError = Just ("Cannot read the user repository's HEAD: " <> err)}
        Right headHash
            | Just headHash == cachedHead caches -> pure caches{cachedHistoryError = Nothing}
            | otherwise -> do
                history <- liftIO (gitHistory repoPath)
                case history of
                    Left err -> pure caches{cachedHistoryError = Just ("Cannot read the user repository's history: " <> err)}
                    Right entries -> do
                        headSteps <- runExceptT (evalHeadSteps repoPath headHash)
                        pure
                            caches
                                { cachedHead = Just headHash
                                , cachedRelevant = [(commit, time) | (commit, time, files) <- entries, commitAffectsOutputs files]
                                , cachedHeadSteps = first T.pack headSteps
                                , cachedHistoryError = Nothing
                                }

evalHeadSteps :: (Eval :> es, IOE :> es) => FilePath -> Text -> ExceptT String (Eff es) (Map Int HeadStep)
evalHeadSteps repoPath headHash = do
    ctx <- commitContextEff repoPath headHash
    output <- runNixEvalJsonApplyInRepo ctx headStepsExpression "#pointy.steps"
    either (throwError . ("Cannot decode step information: " ++)) (pure . Map.mapMaybe id) (eitherDecodeStrict' output :: Either String (Map Int (Maybe HeadStep)))

headStepsExpression :: String
headStepsExpression =
    unlines
        [ "steps:"
        , "let"
        , "  stepRefsOf = value:"
        , "    if builtins.isAttrs value && (value.type or null) != \"derivation\" then"
        , "      (if value ? step && (builtins.isInt value.step || builtins.isString value.step) then [ (toString value.step) ] else [ ])"
        , "      ++ builtins.concatMap stepRefsOf (builtins.attrValues value)"
        , "    else if builtins.isList value then builtins.concatMap stepRefsOf value"
        , "    else [ ];"
        , "  inputsOf = id:"
        , "    let upstream = steps.${id}.dependencies or [ ];"
        , "    in builtins.filter (ref: builtins.elem ref upstream) (stepRefsOf (steps.${id}.def.args or { }));"
        , "in builtins.mapAttrs (id: s:"
        , "  let name = builtins.tryEval (s.def.name or (s.name or \"\"));"
        , "      output = builtins.tryEval s.outPath;"
        , "      certificate = builtins.tryEval ((s.certificate or s).outPath);"
        , "  in if output.success && certificate.success then {"
        , "    name = if name.success && builtins.isString name.value then name.value else \"\";"
        , "    inputs = builtins.map builtins.fromJSON (inputsOf id);"
        , "    output = output.value;"
        , "    certificate = certificate.value;"
        , "  } else null) steps"
        ]

pathSizes :: (Nix :> es) => [Text] -> Eff es (Either Text (Map Text Integer))
pathSizes paths = go paths
  where
    go [] = pure (Right Map.empty)
    go remaining = do
        let (chunk, rest) = splitAt pathInfoChunkSize remaining
        (code, output, err) <- runNixCli (["--offline", "path-info", "--json"] ++ map T.unpack chunk)
        case code of
            ExitFailure _ -> pure (Left ("Cannot read store sizes: " <> T.strip (TE.decodeUtf8Lenient err)))
            ExitSuccess -> case eitherDecodeStrict' output of
                Left message -> pure (Left ("Cannot decode store sizes: " <> T.pack message))
                Right infos -> do
                    restSizes <- go rest
                    pure $ Map.union (Map.mapMaybe (fmap pathNarSize) (infos :: Map Text (Maybe PathInfo))) <$> restSizes

capacityUsage :: Bool -> IO (Maybe StorageCapacity)
capacityUsage force = do
    command <- lookupEnv "POINTY_STORAGE_CAPACITY_COMMAND"
    case command of
        Just executable -> cachedCommandCapacity executable force
        Nothing -> filesystemCapacity

cachedCommandCapacity :: FilePath -> Bool -> IO (Maybe StorageCapacity)
cachedCommandCapacity executable force = do
    now <- getCurrentTime
    modifyMVar capacityCacheRef $ \cached ->
        case cached of
            Just (measuredAt, capacity)
                | not force && diffUTCTime now measuredAt < capacityCacheSeconds -> pure (cached, capacity)
            _ -> do
                capacity <- commandCapacity executable
                pure (Just (now, capacity), capacity)

commandCapacity :: FilePath -> IO (Maybe StorageCapacity)
commandCapacity executable = do
    outcome <- timeout capacityCommandTimeout (cli executable [])
    pure $ case outcome of
        Nothing -> Nothing
        Just (ExitSuccess, output, _) -> either (const Nothing) Just (parseStorageCapacity (LBS.fromStrict (TE.encodeUtf8 (T.pack output))))
        Just (ExitFailure _, _, _) -> Nothing

filesystemCapacity :: IO (Maybe StorageCapacity)
filesystemCapacity = do
    diskPath <- fromMaybe (rootedPath "/nix/store") <$> lookupEnv "POINTY_STORAGE_DISK_PATH"
    outcome <- timeout capacityCommandTimeout (cli "df" ["-B1", "--output=avail,size", diskPath])
    pure $ case outcome of
        Nothing -> Nothing
        Just (ExitFailure _, _, _) -> Nothing
        Just (ExitSuccess, output, _) -> case reverse (lines output) of
            line : _ -> case words line of
                [available, total]
                    | Just totalBytes <- readInteger total
                    , Just availableBytes <- readInteger available
                    , totalBytes > 0 ->
                        let clamped = max 0 (min totalBytes availableBytes)
                         in Just
                                StorageCapacity
                                    { capacityKind = "filesystem"
                                    , capacityLabel = "disk"
                                    , capacityTotalBytes = totalBytes
                                    , capacityUsedBytes = totalBytes - clamped
                                    , capacityAvailableBytes = clamped
                                    }
                _ -> Nothing
            [] -> Nothing
  where
    readInteger text = case reads text of
        [(value, "")] -> Just value
        _ -> Nothing

repoHead :: FilePath -> IO (Either Text Text)
repoHead repoPath = do
    (code, output, err) <- runGitIn repoPath ["rev-parse", "--verify", "HEAD"]
    pure $ case code of
        ExitSuccess -> Right (T.strip (T.pack output))
        ExitFailure _ -> Left (T.pack err)

gitHistory :: FilePath -> IO (Either Text [(Text, UTCTime, [FilePath])])
gitHistory repoPath = do
    (code, output, err) <- runGitIn repoPath ["log", "--first-parent", "--format=%H %ct", "--name-only"]
    pure $ case code of
        ExitSuccess -> Right (parseHistory output)
        ExitFailure _ -> Left (T.pack err)

commitAffectsOutputs :: [FilePath] -> Bool
commitAffectsOutputs files =
    null files
        || any (`elem` globalPaths) files
        || any (\file -> any (`isPrefixOf` file) relevantPrefixes) files
  where
    globalPaths = ["flake.nix", "flake.lock", "pointy.nix", "main.pointy"]
    relevantPrefixes = ["steps/", "srcFiles/", "templates/", "packages/"]

startScan :: FilePath -> [Text] -> IO ()
startScan _ [] = pure ()
startScan repoPath pending = do
    now <- getCurrentTime
    shouldStart <- modifyMVar scanStateRef $ \state ->
        let retryBlocked = case scanUnavailable state of
                Just (failedAt, _) -> diffUTCTime now failedAt < fromIntegral scanRetryDelaySeconds
                Nothing -> False
         in if scanRunning state || retryBlocked
                then pure (state, False)
                else pure (state{scanRunning = True}, True)
    when shouldStart $
        void $
            forkIO $ do
                outcome <- try (scanCommits repoPath pending)
                unavailable <- case outcome of
                    Right unavailable -> pure unavailable
                    Left (err :: SomeException) -> do
                        failedAt <- getCurrentTime
                        pure (Just (failedAt, T.pack (displayException err)))
                modifyMVar_ scanStateRef $ \state -> pure state{scanRunning = False, scanUnavailable = unavailable}
                mapM_
                    ( \(_, reason) -> do
                        putStrLn ("Storage history scan paused: " ++ T.unpack reason)
                        void $ forkIO (threadDelay (scanRetryDelaySeconds * 1000000) >> viewInputsChanged)
                    )
                    unavailable
                viewInputsChanged

scanCommits :: FilePath -> [Text] -> IO (Maybe (UTCTime, Text))
scanCommits repoPath pending = do
    queue <- newMVar (ScanQueue pending Nothing)
    replicateConcurrently_ historyEvaluatorWorkers (scanWorker queue)
    queueFailure <$> readMVar queue
  where
    scanWorker queue = do
        next <- takeScanCommit queue
        case next of
            Nothing -> pure ()
            Just commit -> do
                outcome <- try @SomeException (evaluateCommit repoPath commit)
                case outcome of
                    Left err -> stopScanQueue queue (T.pack (displayException err))
                    Right (Left failure) -> stopScanQueue queue (evalFailureMessage failure)
                    Right (Right steps) -> do
                        insertEvaluation commit steps
                        viewInputsChanged
                        scanWorker queue

takeScanCommit :: MVar ScanQueue -> IO (Maybe Text)
takeScanCommit queue =
    modifyMVar queue $ \state ->
        case (queueFailure state, queuePending state) of
            (Nothing, commit : rest) -> pure (state{queuePending = rest}, Just commit)
            _ -> pure (state, Nothing)

stopScanQueue :: MVar ScanQueue -> Text -> IO ()
stopScanQueue queue reason = do
    now <- getCurrentTime
    modifyMVar_ queue $ \state -> pure state{queueFailure = Just (now, reason)}

insertEvaluation :: Text -> Map Int CachedStep -> IO ()
insertEvaluation commit steps =
    modifyMVar_ evaluationsRef $ \cached -> do
        current <- maybe loadStepEvaluations pure cached
        let (updated, records) = recordEvaluation commit steps current
        appendEvaluationRecords records
        pure (Just updated)

evalFailureMessage :: EvalFailure -> Text
evalFailureMessage failure = case failure of
    EvalError message -> "Evaluation error: " <> T.pack message
    EvalUnavailable message -> T.pack message

evaluateCommit :: FilePath -> Text -> IO (Either EvalFailure (Map Int CachedStep))
evaluateCommit repoPath commit = do
    context <- runExceptT (commitContext repoPath commit)
    case context of
        Left message -> pure (Left (EvalUnavailable message))
        Right ctx -> do
            outcome <- applyHistoryEval ctx stepEvaluationsExpression
            case outcome of
                Left (EvalUnavailable message) -> pure (Left (EvalUnavailable message))
                Left (EvalError _) -> evaluateFallback ctx
                Right output -> pure (decodeStepEvaluations output)
  where
    applyHistoryEval ctx expression = runAppEffects (runHistoryEvalJsonApplyInRepo ctx expression "#pointy.steps")
    evaluateFallback ctx = do
        names <- applyHistoryEval ctx stepNamesExpression
        case names of
            Left (EvalUnavailable message) -> pure (Left (EvalUnavailable message))
            Left (EvalError _) -> pure (Right Map.empty)
            Right output ->
                case decodeStepNames output of
                    Left failure -> pure (Left failure)
                    Right stepIds -> evaluateStepIds ctx stepIds Map.empty
    evaluateStepIds _ [] steps = pure (Right steps)
    evaluateStepIds ctx (stepId : rest) steps = do
        outcome <- applyHistoryEval ctx (stepEvaluationExpression [stepId])
        case outcome of
            Left (EvalUnavailable message) -> pure (Left (EvalUnavailable message))
            Left (EvalError _) -> evaluateStepIds ctx rest (Map.insert stepId (CachedStep "" Nothing "derived") steps)
            Right output ->
                case decodeStepEvaluations output of
                    Left failure -> pure (Left failure)
                    Right evaluated ->
                        let step = Map.findWithDefault (CachedStep "" Nothing "derived") stepId evaluated
                         in evaluateStepIds ctx rest (Map.insert stepId step steps)

decodeStepEvaluations :: ByteString -> Either EvalFailure (Map Int CachedStep)
decodeStepEvaluations output =
    case eitherDecodeStrict' output :: Either String [EvalEntry] of
        Left message -> Left (EvalUnavailable ("Cannot decode step evaluations: " ++ message))
        Right entries -> Right (Map.fromList (map (\(EvalEntry entry) -> entry) entries))

decodeStepNames :: ByteString -> Either EvalFailure [Int]
decodeStepNames output =
    case eitherDecodeStrict' output :: Either String [Text] of
        Left message -> Left (EvalUnavailable ("Cannot decode step identifiers: " ++ message))
        Right names -> Right (mapMaybe (readMaybe . T.unpack) names)

stepEvaluationsExpression :: String
stepEvaluationsExpression =
    "steps: map " ++ stepEvaluationMapper ++ " (builtins.attrNames steps)"

stepNamesExpression :: String
stepNamesExpression = "steps: builtins.attrNames steps"

stepEvaluationExpression :: [Int] -> String
stepEvaluationExpression stepIds =
    "steps: map "
        ++ stepEvaluationMapper
        ++ " [ "
        ++ unwords (map (show . show) stepIds)
        ++ " ]"

stepEvaluationMapper :: String
stepEvaluationMapper =
    "(id:"
        ++ " let s = steps.${id};"
        ++ " path = builtins.tryEval (toString s.outPath);"
        ++ " name = builtins.tryEval (s.def.name or (s.name or \"\"));"
        ++ " uploaded = builtins.tryEval (s.def.args.uploaded.hash or null);"
        ++ " downloaded = builtins.tryEval (s.def.args.downloaded.hash or null);"
        ++ " kind = if uploaded.success && uploaded.value != null then \"upload\" else if downloaded.success && downloaded.value != null then \"download\" else \"derived\";"
        ++ " in { id = builtins.fromJSON id;"
        ++ " name = if name.success && builtins.isString name.value then name.value else \"\";"
        ++ " kind = kind;"
        ++ " path = if path.success then path.value else null; })"

readEvaluations :: IO StepEvaluations
readEvaluations = modifyMVar evaluationsRef $ \cached -> do
    evaluations <- maybe loadStepEvaluations pure cached
    pure (Just evaluations, evaluations)
