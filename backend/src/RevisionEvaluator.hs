{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RevisionEvaluator (
    RevisionEvaluator,
    EvalPriority (..),
    RepoSource,
    RepoExpression,
    defaultRevisionEvaluator,
    repoSource,
    mutableRepoSource,
    jsonExpression,
    rawExpression,
    jsonAppliedExpression,
    evaluate,
    evaluateImpure,
    rewarmRevision,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, readMVar, tryPutMVar)
import Control.Concurrent.STM (TMVar, TQueue, atomically, newEmptyTMVarIO, newTQueueIO, orElse, putTMVar, readTQueue, takeTMVar, writeTQueue)
import Control.Exception (SomeException, catch, finally, try)
import Control.Monad (forM, forever, unless, void, when)
import Data.Char (ord)
import Data.Either (isLeft, isRight)
import Data.Foldable (for_)
import Data.List (find, foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import RevisionEvaluator.NixRepl (NixEvalOutput (..), NixEvalRequest (..), NixEvalTarget (..), ReplKind (..), ReplOutcome (..), ReplSession, closeSession, openSession, outcomeResult, readSessionMemoryBytes, runRequest)
import System.IO.Unsafe (unsafePerformIO)

data RepoSource = RepoSource Bool String
    deriving (Eq, Ord)

data ExpressionId
    = AttributeId NixEvalOutput String (Maybe String)
    | ImpureId String
    deriving (Eq, Ord)

data RepoExpression = RepoExpression
    { expressionId :: ExpressionId
    , expressionOutput :: NixEvalOutput
    , expressionApply :: Maybe String
    , expressionAttr :: String
    }

data Evaluation = Evaluation
    { evaluationId :: ExpressionId
    , evaluationRequest :: NixEvalRequest
    }

data EvalPriority = Interactive | Background
    deriving (Eq, Show)

data RevisionEvaluator = RevisionEvaluator
    { pureWorker :: ReplWorker
    , impureWorker :: ReplWorker
    , revisionResults :: MVar RevisionResultCache
    }

data ReplShard = ReplShard
    { replShardId :: Int
    , replShardSession :: MVar (Maybe ReplSession)
    , replShardInteractive :: TQueue QueuedEval
    , replShardBackground :: TQueue QueuedEval
    , replShardMemoryLimitBytes :: MVar Integer
    , replShardSessionGeneration :: MVar Int
    , replShardReplacementActive :: MVar Bool
    }

data ReplWorker = ReplWorker
    { replWorkerKind :: ReplKind
    , replWorkerShards :: [ReplShard]
    , replWorkerWarmRevision :: MVar (Int, Maybe WarmRevision)
    , replWorkerInitialWarm :: MVar Bool
    }

data WarmRevision = WarmRevision RepoSource (IO (Either String (NonEmpty RepoExpression)))

type ResultSlot = MVar (Either String String)

data RevisionResultCache = RevisionResultCache
    { resultCurrentRevision :: Maybe RepoSource
    , resultRevisionOrder :: [RepoSource]
    , resultRevisions :: Map.Map RepoSource (Map.Map ExpressionId ResultSlot)
    }

data QueuedEval = QueuedEval Evaluation (TMVar (Either String String))

data WarmEvaluation = WarmEvaluation
    { warmEvaluation :: Evaluation
    , warmCallback :: Maybe (Either String String -> IO ())
    }

data SwapResult
    = SwapRetry
    | SwapObsolete
    | SwapComplete (Maybe ReplSession)

pureReplShardCount :: Int
pureReplShardCount = 4

initialReplMemoryLimitBytes :: Integer
initialReplMemoryLimitBytes = 512 * 1024 * 1024

maxCachedRevisionCount :: Int
maxCachedRevisionCount = 8

{-# NOINLINE defaultRevisionEvaluator #-}
defaultRevisionEvaluator :: RevisionEvaluator
defaultRevisionEvaluator = unsafePerformIO newRevisionEvaluator

newRevisionEvaluator :: IO RevisionEvaluator
newRevisionEvaluator =
    RevisionEvaluator
        <$> newWorker PureRepl pureReplShardCount
        <*> newWorker ImpureRepl 1
        <*> newMVar (RevisionResultCache Nothing [] Map.empty)

repoSource :: String -> RepoSource
repoSource = RepoSource True

mutableRepoSource :: String -> RepoSource
mutableRepoSource = RepoSource False

jsonExpression :: String -> RepoExpression
jsonExpression = expression EvalJson Nothing

rawExpression :: String -> RepoExpression
rawExpression = expression EvalRaw Nothing

jsonAppliedExpression :: String -> String -> RepoExpression
jsonAppliedExpression applyExpr = expression EvalJson $ Just applyExpr

expression :: NixEvalOutput -> Maybe String -> String -> RepoExpression
expression output applyExpr attr =
    RepoExpression (AttributeId output attr applyExpr) output applyExpr attr

evaluate :: RevisionEvaluator -> EvalPriority -> RepoSource -> RepoExpression -> IO (Either String String)
evaluate evaluator priority source@(RepoSource cacheable _) repoExpr
    | not cacheable = evaluateRequest evaluator priority evaluation
    | otherwise = do
        (resultSlot, ownsResult) <- claimRevisionResult evaluator source $ evaluationId evaluation
        if ownsResult
            then do
                result <-
                    evaluateRequest evaluator priority evaluation
                        `catch` \(err :: SomeException) -> pure $ Left $ "revision evaluation failed: " ++ show err
                completeRevisionResult evaluator source (evaluationId evaluation) resultSlot result
                pure result
            else readMVar resultSlot
  where
    evaluation = repoEvaluation source repoExpr

evaluateImpure :: RevisionEvaluator -> String -> IO (Either String String)
evaluateImpure evaluator expr =
    evaluateRequest evaluator Interactive $
        Evaluation (ImpureId expr) (NixEvalRequest True EvalJson Nothing $ EvalExpr expr)

rewarmRevision :: RevisionEvaluator -> RepoSource -> IO (Either String (NonEmpty (key, RepoExpression))) -> IO (Either String (NonEmpty (key, Either String String)))
rewarmRevision evaluator source resolveExpressions = do
    let worker = pureWorker evaluator
        revision = WarmRevision source $ fmap (fmap $ fmap snd) resolveExpressions
    activateRevisionResults evaluator source
    modifyMVar_ (replWorkerWarmRevision worker) $ \(version, _) -> pure (version + 1, Just revision)
    resolved <- resolveExpressions `catch` \(err :: SomeException) -> pure $ Left $ "revision expression discovery failed: " ++ show err
    case resolved of
        Left err -> pure $ Left err
        Right expressions -> do
            pending <- forM expressions $ \(key, repoExpr) -> do
                response <- newEmptyTMVarIO
                let evaluation = repoEvaluation source repoExpr
                (resultSlot, ownsResult) <- claimRevisionResult evaluator source $ evaluationId evaluation
                let publish result = do
                        when ownsResult $ completeRevisionResult evaluator source (evaluationId evaluation) resultSlot result
                        atomically $ putTMVar response result
                pure (key, WarmEvaluation evaluation $ Just publish, response)
            warmed <- rewarmWorker worker $ fmap (\(_, warm, _) -> warm) pending
            results <- forM pending (\(key, _, response) -> fmap ((,) key) $ atomically $ takeTMVar response)
            initialWarm <- readMVar $ replWorkerInitialWarm worker
            when (initialWarm && warmed && all (isRight . snd) results) $ finishInitialWarm worker
            pure $ Right results

activateRevisionResults :: RevisionEvaluator -> RepoSource -> IO ()
activateRevisionResults evaluator source =
    modifyMVar_ (revisionResults evaluator) $ \cache ->
        pure $
            pruneRevisionResults $
                (touchResultRevision source cache){resultCurrentRevision = Just source}

claimRevisionResult :: RevisionEvaluator -> RepoSource -> ExpressionId -> IO (ResultSlot, Bool)
claimRevisionResult evaluator source exprId =
    modifyMVar (revisionResults evaluator) $ \cache -> do
        let cache' = pruneRevisionResults $ touchResultRevision source cache
            results = Map.findWithDefault Map.empty source $ resultRevisions cache'
        case Map.lookup exprId results of
            Just result -> pure (cache', (result, False))
            Nothing -> do
                result <- newEmptyMVar
                let revisions = Map.insert source (Map.insert exprId result results) $ resultRevisions cache'
                pure (cache'{resultRevisions = revisions}, (result, True))

completeRevisionResult :: RevisionEvaluator -> RepoSource -> ExpressionId -> ResultSlot -> Either String String -> IO ()
completeRevisionResult evaluator source exprId resultSlot result = do
    completed <- tryPutMVar resultSlot result
    when (completed && isLeft result) $
        discardRevisionResult evaluator source exprId resultSlot

discardRevisionResult :: RevisionEvaluator -> RepoSource -> ExpressionId -> ResultSlot -> IO ()
discardRevisionResult evaluator source exprId resultSlot =
    modifyMVar_ (revisionResults evaluator) $ \cache ->
        let revisions = Map.update discard source $ resultRevisions cache
         in pure
                cache
                    { resultRevisionOrder = filter (`Map.member` revisions) $ resultRevisionOrder cache
                    , resultRevisions = revisions
                    }
  where
    discard results
        | Map.lookup exprId results /= Just resultSlot = Just results
        | Map.null remaining = Nothing
        | otherwise = Just remaining
      where
        remaining = Map.delete exprId results

touchResultRevision :: RepoSource -> RevisionResultCache -> RevisionResultCache
touchResultRevision source cache =
    cache
        { resultRevisionOrder = filter (/= source) (resultRevisionOrder cache) ++ [source]
        , resultRevisions = Map.insertWith (\_ existing -> existing) source Map.empty $ resultRevisions cache
        }

pruneRevisionResults :: RevisionResultCache -> RevisionResultCache
pruneRevisionResults cache
    | Map.size (resultRevisions cache) <= maxCachedRevisionCount = cache
    | Just oldest <- find ((/= resultCurrentRevision cache) . Just) $ resultRevisionOrder cache =
        pruneRevisionResults
            cache
                { resultRevisionOrder = filter (/= oldest) $ resultRevisionOrder cache
                , resultRevisions = Map.delete oldest $ resultRevisions cache
                }
    | otherwise = cache

repoEvaluation :: RepoSource -> RepoExpression -> Evaluation
repoEvaluation (RepoSource _ installable) repoExpr =
    Evaluation
        (expressionId repoExpr)
        NixEvalRequest
            { evalImpure = False
            , evalOutput = expressionOutput repoExpr
            , evalApply = expressionApply repoExpr
            , evalTarget = EvalInstallable installable $ expressionAttr repoExpr
            }

newWorker :: ReplKind -> Int -> IO ReplWorker
newWorker kind shardCount = do
    shards <- mapM newShard [0 .. shardCount - 1]
    warmRevision <- newMVar (0, Nothing)
    initialWarm <- newMVar $ kind == PureRepl
    let worker = ReplWorker kind shards warmRevision initialWarm
    mapM_ (void . forkIO . replShardLoop worker) shards
    pure worker
  where
    newShard shardId =
        ReplShard shardId
            <$> newMVar Nothing
            <*> newTQueueIO
            <*> newTQueueIO
            <*> newMVar initialReplMemoryLimitBytes
            <*> newMVar 0
            <*> newMVar False

replShardLoop :: ReplWorker -> ReplShard -> IO ()
replShardLoop worker shard = forever $ do
    QueuedEval evaluation response <-
        atomically $
            readTQueue (replShardInteractive shard)
                `orElse` readTQueue (replShardBackground shard)
    result <-
        runWithShard True worker shard evaluation
            `catch` \(err :: SomeException) -> pure $ Left $ "revision evaluator shard failed: " ++ show err
    atomically $ putTMVar response result

evaluateRequest :: RevisionEvaluator -> EvalPriority -> Evaluation -> IO (Either String String)
evaluateRequest evaluator priority evaluation = do
    let worker = workerForEvaluation evaluator evaluation
        shard = evaluationShard worker evaluation
    response <- newEmptyTMVarIO
    atomically $ writeTQueue (shardQueue priority shard) $ QueuedEval evaluation response
    atomically $ takeTMVar response

workerForEvaluation :: RevisionEvaluator -> Evaluation -> ReplWorker
workerForEvaluation evaluator evaluation
    | evalImpure $ evaluationRequest evaluation = impureWorker evaluator
    | otherwise = pureWorker evaluator

shardQueue :: EvalPriority -> ReplShard -> TQueue QueuedEval
shardQueue Interactive = replShardInteractive
shardQueue Background = replShardBackground

evaluationShard :: ReplWorker -> Evaluation -> ReplShard
evaluationShard worker evaluation =
    replWorkerShards worker !! evaluationShardIndex worker evaluation

evaluationShardIndex :: ReplWorker -> Evaluation -> Int
evaluationShardIndex worker evaluation =
    fromIntegral $ expressionHash (evaluationId evaluation) `mod` fromIntegral (length $ replWorkerShards worker)

expressionHash :: ExpressionId -> Word
expressionHash (AttributeId output attr applyExpr) = maybe base (hashString base) applyExpr
  where
    base = hashString (case output of EvalJson -> 1; EvalRaw -> 2) attr
expressionHash (ImpureId expr) = hashString 3 expr

hashString :: Word -> String -> Word
hashString = foldl' $ \hash c -> hash * 33 + fromIntegral (ord c)

rewarmWorker :: ReplWorker -> NonEmpty WarmEvaluation -> IO Bool
rewarmWorker worker evaluations =
    and <$> mapConcurrently rewarm (replWorkerShards worker)
  where
    rewarm shard =
        fmap (all isRight) $
            mapM (runWarmEvaluation worker shard) $
                shardWarmItems worker shard warmEvaluation (\pending -> pending{warmCallback = Nothing}) evaluations

shardWarmItems :: ReplWorker -> ReplShard -> (item -> Evaluation) -> (item -> item) -> NonEmpty item -> [item]
shardWarmItems worker shard evaluationOf fallback items =
    case filter ((== replShardId shard) . evaluationShardIndex worker . evaluationOf) $ NonEmpty.toList items of
        [] -> [fallback $ NonEmpty.head items]
        assigned -> assigned

resolveWarmRevision :: WarmRevision -> IO (Either String (NonEmpty Evaluation))
resolveWarmRevision (WarmRevision source resolveExpressions) =
    fmap (fmap $ fmap $ repoEvaluation source) resolveExpressions
        `catch` \(err :: SomeException) -> pure $ Left $ "revision expression discovery failed: " ++ show err

runWarmEvaluation :: ReplWorker -> ReplShard -> WarmEvaluation -> IO (Either String String)
runWarmEvaluation worker shard pending = do
    result <-
        runWithShard True worker shard (warmEvaluation pending)
            `catch` \(err :: SomeException) -> pure $ Left $ "revision evaluator rewarm failed: " ++ show err
    for_ (warmCallback pending) $ \callback ->
        callback result `catch` \(_ :: SomeException) -> pure ()
    pure result

finishInitialWarm :: ReplWorker -> IO ()
finishInitialWarm worker = do
    mapM_ raiseLimit $ replWorkerShards worker
    modifyMVar_ (replWorkerInitialWarm worker) $ const $ pure False
  where
    raiseLimit shard =
        readMVar (replShardSession shard)
            >>= mapM_ (\session -> readSessionMemoryBytes session >>= mapM_ (growShardMemoryLimit worker shard "initial warm"))

scheduleReplacementCheck :: ReplWorker -> ReplShard -> IO ()
scheduleReplacementCheck worker shard = do
    initialWarm <- readMVar $ replWorkerInitialWarm worker
    unless initialWarm $ do
        started <- modifyMVar (replShardReplacementActive shard) $ \active ->
            pure (True, not active)
        when started $
            void $
                forkIO $
                    checkShardMemory worker shard
                        `finally` modifyMVar_ (replShardReplacementActive shard) (const $ pure False)

checkShardMemory :: ReplWorker -> ReplShard -> IO ()
checkShardMemory worker shard = do
    snapshot <- modifyMVar (replShardSession shard) $ \mSession -> do
        generation <- readMVar $ replShardSessionGeneration shard
        pure (mSession, (\session -> (generation, session)) <$> mSession)
    for_ snapshot $ \(generation, session) -> do
        memoryBytes <- readSessionMemoryBytes session
        memoryLimit <- readMVar $ replShardMemoryLimitBytes shard
        for_ memoryBytes $ \bytes ->
            when (bytes > memoryLimit) $
                replaceSession worker shard generation bytes

replaceSession :: ReplWorker -> ReplShard -> Int -> Integer -> IO ()
replaceSession worker shard oldGeneration oldMemoryBytes =
    try (openSession $ replWorkerKind worker) >>= \case
        Left (err :: SomeException) ->
            logWarning $ replacementPrefix ++ "failed to start: " ++ show err
        Right standby ->
            warmAndSwap standby >>= \case
                Left err -> do
                    closeQuietly standby
                    logWarning $ replacementPrefix ++ "failed to warm: " ++ err
                Right Nothing -> closeQuietly standby
                Right (Just oldSession) -> do
                    closeQuietly oldSession
                    logWarning $ replacementPrefix ++ "replaced at " ++ formatMiB oldMemoryBytes
  where
    warmAndSwap standby = do
        (version, mRevision) <- readMVar $ replWorkerWarmRevision worker
        warmResult <- maybe (pure $ Right ()) (warmStandbyRevision standby) mRevision
        case warmResult of
            Left err -> pure $ Left err
            Right () -> do
                growLimitForStandby standby
                swapResult <- modifyMVar (replShardSession shard) $ \mSession -> do
                    generation <- readMVar $ replShardSessionGeneration shard
                    (latestVersion, _) <- readMVar $ replWorkerWarmRevision worker
                    if generation /= oldGeneration
                        then pure (mSession, SwapObsolete)
                        else
                            if latestVersion /= version
                                then pure (mSession, SwapRetry)
                                else do
                                    bumpSessionGeneration shard
                                    pure (Just standby, SwapComplete mSession)
                case swapResult of
                    SwapRetry -> warmAndSwap standby
                    SwapObsolete -> pure $ Right Nothing
                    SwapComplete oldSession -> pure $ Right oldSession

    warmStandbyRevision standby revision =
        resolveWarmRevision revision >>= \case
            Left err -> pure $ Left err
            Right evaluations -> warmEvaluations $ shardWarmItems worker shard id id evaluations
      where
        warmEvaluations [] = pure $ Right ()
        warmEvaluations (evaluation : rest) =
            fmap outcomeResult (runStandby standby evaluation) >>= \case
                Left err -> pure $ Left err
                Right _ -> warmEvaluations rest

    runStandby standby evaluation =
        runRequest standby (evaluationRequest evaluation)
            `catch` \(err :: SomeException) -> pure (ReplDied $ show err)

    growLimitForStandby standby =
        readSessionMemoryBytes standby >>= mapM_ (growShardMemoryLimit worker shard "warmed replacement")

    replacementPrefix =
        show (replWorkerKind worker)
            ++ " shard "
            ++ show (replShardId shard)
            ++ " RAM replacement "

growShardMemoryLimit :: ReplWorker -> ReplShard -> String -> Integer -> IO ()
growShardMemoryLimit worker shard reason memoryBytes =
    modifyMVar_ (replShardMemoryLimitBytes shard) $ \memoryLimit ->
        if memoryBytes <= memoryLimit
            then pure memoryLimit
            else do
                let grownLimit = until (> memoryBytes) (* 2) memoryLimit
                logWarning $
                    show (replWorkerKind worker)
                        ++ " shard "
                        ++ show (replShardId shard)
                        ++ " "
                        ++ reason
                        ++ " uses "
                        ++ formatMiB memoryBytes
                        ++ "; growing limit from "
                        ++ formatMiB memoryLimit
                        ++ " to "
                        ++ formatMiB grownLimit
                pure grownLimit

bumpSessionGeneration :: ReplShard -> IO ()
bumpSessionGeneration shard =
    modifyMVar_ (replShardSessionGeneration shard) $ pure . (+ 1)

runWithShard :: Bool -> ReplWorker -> ReplShard -> Evaluation -> IO (Either String String)
runWithShard mayRetry worker shard evaluation = do
    outcome <- modifyMVar (replShardSession shard) $ \mSession -> do
        eSession <- maybe openActiveSession (pure . Right) mSession
        case eSession of
            Left (err :: SomeException) ->
                pure (Nothing, ReplDied $ "failed to start " ++ show (replWorkerKind worker) ++ " nix repl: " ++ show err)
            Right session -> do
                result <-
                    runRequest session (evaluationRequest evaluation)
                        `catch` \(err :: SomeException) -> pure $ ReplDied $ "nix repl session failed: " ++ show err
                case result of
                    ReplDied _ -> do
                        closeSession session
                        bumpSessionGeneration shard
                        pure (Nothing, result)
                    ReplSucceeded _ -> pure (Just session, result)
                    ReplFailed _ -> pure (Just session, result)
    case outcome of
        ReplSucceeded output -> do
            scheduleReplacementCheck worker shard
            pure $ Right output
        ReplFailed err -> pure $ Left err
        ReplDied err
            | mayRetry -> runWithShard False worker shard evaluation
            | otherwise -> pure $ Left err
  where
    openActiveSession = do
        result <- try $ openSession $ replWorkerKind worker
        for_ result $ const $ bumpSessionGeneration shard
        pure result

closeQuietly :: ReplSession -> IO ()
closeQuietly session = closeSession session `catch` \(_ :: SomeException) -> pure ()

logWarning :: String -> IO ()
logWarning message = putStrLn ("Warning: " ++ message) `catch` \(_ :: SomeException) -> pure ()

formatMiB :: Integer -> String
formatMiB bytes = show (bytes `div` (1024 * 1024)) ++ " MiB"
