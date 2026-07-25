{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RevisionEvaluator (
    RevisionEvaluator,
    EvalPriority (..),
    RepoSource,
    RepoExpression,
    defaultRevisionEvaluator,
    repoSource,
    jsonExpression,
    rawExpression,
    jsonAppliedExpression,
    evaluate,
    evaluateImpure,
    rewarmRevision,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TMVar, TQueue, atomically, newEmptyTMVarIO, newTQueueIO, orElse, putTMVar, readTQueue, takeTMVar, writeTQueue)
import Control.Exception (SomeException, catch, finally, try)
import Control.Monad (forM, forever, void, when)
import Data.Char (ord)
import Data.Foldable (for_)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import RevisionEvaluator.NixRepl (NixEvalOutput (..), NixEvalRequest (..), NixEvalTarget (..), ReplKind (..), ReplOutcome (..), ReplSession, closeSession, openSession, outcomeResult, readSessionMemoryBytes, runRequest)
import System.IO.Unsafe (unsafePerformIO)

newtype RepoSource = RepoSource String

data ExpressionId
    = AttributeId String (Maybe String)
    | ImpureId String

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
    }

data ReplShard = ReplShard
    { replShardId :: Int
    , replShardSession :: MVar (Maybe ReplSession)
    , replShardInteractive :: TQueue QueuedEval
    , replShardBackground :: TQueue QueuedEval
    , replShardWarmEvaluation :: MVar (Int, Maybe Evaluation)
    , replShardMemoryLimitBytes :: MVar Integer
    , replShardSessionGeneration :: MVar Int
    , replShardReplacementActive :: MVar Bool
    }

data ReplWorker = ReplWorker
    { replWorkerKind :: ReplKind
    , replWorkerShards :: [ReplShard]
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

{-# NOINLINE defaultRevisionEvaluator #-}
defaultRevisionEvaluator :: RevisionEvaluator
defaultRevisionEvaluator = unsafePerformIO newRevisionEvaluator

newRevisionEvaluator :: IO RevisionEvaluator
newRevisionEvaluator = RevisionEvaluator <$> newWorker PureRepl pureReplShardCount <*> newWorker ImpureRepl 1

repoSource :: String -> RepoSource
repoSource = RepoSource

jsonExpression :: String -> RepoExpression
jsonExpression = expression EvalJson Nothing

rawExpression :: String -> RepoExpression
rawExpression = expression EvalRaw Nothing

jsonAppliedExpression :: String -> String -> RepoExpression
jsonAppliedExpression applyExpr = expression EvalJson $ Just applyExpr

expression :: NixEvalOutput -> Maybe String -> String -> RepoExpression
expression output applyExpr attr =
    RepoExpression (AttributeId attr applyExpr) output applyExpr attr

evaluate :: RevisionEvaluator -> EvalPriority -> RepoSource -> RepoExpression -> IO (Either String String)
evaluate evaluator priority source = evaluateRequest evaluator priority . repoEvaluation source

evaluateImpure :: RevisionEvaluator -> String -> IO (Either String String)
evaluateImpure evaluator expr =
    evaluateRequest evaluator Interactive $
        Evaluation (ImpureId expr) (NixEvalRequest True EvalJson Nothing $ EvalExpr expr)

rewarmRevision :: RevisionEvaluator -> RepoSource -> NonEmpty RepoExpression -> IO [Either String String]
rewarmRevision evaluator source expressions = do
    pending <- forM (map (repoEvaluation source) $ NonEmpty.toList expressions) $ \evaluation -> do
        response <- newEmptyTMVarIO
        pure (WarmEvaluation evaluation $ Just $ atomically . putTMVar response, response)
    rewarmWorker (pureWorker evaluator) $ map fst pending
    mapM (atomically . takeTMVar . snd) pending

repoEvaluation :: RepoSource -> RepoExpression -> Evaluation
repoEvaluation (RepoSource installable) repoExpr =
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
    let worker = ReplWorker kind shards
    mapM_ (void . forkIO . replShardLoop worker) shards
    pure worker
  where
    newShard shardId =
        ReplShard shardId
            <$> newMVar Nothing
            <*> newTQueueIO
            <*> newTQueueIO
            <*> newMVar (0, Nothing)
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
expressionHash (AttributeId attr applyExpr) = maybe base (hashString base) applyExpr
  where
    base = hashString 1 attr
expressionHash (ImpureId expr) = hashString 2 expr

hashString :: Word -> String -> Word
hashString = foldl' $ \hash c -> hash * 33 + fromIntegral (ord c)

rewarmWorker :: ReplWorker -> [WarmEvaluation] -> IO ()
rewarmWorker _ [] = pure ()
rewarmWorker worker evaluations@(first : _) =
    mapConcurrently_ rewarm $ replWorkerShards worker
  where
    fallback = first{warmCallback = Nothing}
    rewarm shard =
        mapM_ (runWarmEvaluation worker shard) $
            case filter ((== replShardId shard) . evaluationShardIndex worker . warmEvaluation) evaluations of
                [] -> [fallback]
                assigned -> assigned

runWarmEvaluation :: ReplWorker -> ReplShard -> WarmEvaluation -> IO ()
runWarmEvaluation worker shard pending = do
    result <-
        runWithShard True worker shard (warmEvaluation pending)
            `catch` \(err :: SomeException) -> pure $ Left $ "revision evaluator rewarm failed: " ++ show err
    for_ (warmCallback pending) $ \callback ->
        callback result `catch` \(_ :: SomeException) -> pure ()

rememberWarmEvaluation :: ReplShard -> Evaluation -> IO ()
rememberWarmEvaluation shard evaluation =
    modifyMVar_ (replShardWarmEvaluation shard) $ \(version, _) ->
        pure (version + 1, Just evaluation)

scheduleReplacementCheck :: ReplWorker -> ReplShard -> IO ()
scheduleReplacementCheck worker shard = do
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
        (version, mEvaluation) <- readMVar $ replShardWarmEvaluation shard
        warmResult <- maybe (pure $ Right ()) (fmap (void . outcomeResult) . runStandby standby) mEvaluation
        case warmResult of
            Left err -> pure $ Left err
            Right () -> do
                growLimitForStandby standby
                swapResult <- modifyMVar (replShardSession shard) $ \mSession -> do
                    generation <- readMVar $ replShardSessionGeneration shard
                    (latestVersion, _) <- readMVar $ replShardWarmEvaluation shard
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

    runStandby standby evaluation =
        runRequest standby (evaluationRequest evaluation)
            `catch` \(err :: SomeException) -> pure (ReplDied $ show err)

    growLimitForStandby standby =
        readSessionMemoryBytes standby >>= mapM_ adjust
      where
        adjust standbyMemoryBytes =
            modifyMVar_ (replShardMemoryLimitBytes shard) $ \memoryLimit ->
                if standbyMemoryBytes <= memoryLimit
                    then pure memoryLimit
                    else do
                        let grownLimit = until (> standbyMemoryBytes) (* 2) memoryLimit
                        logWarning $
                            replacementPrefix
                                ++ "warmed replacement uses "
                                ++ formatMiB standbyMemoryBytes
                                ++ "; growing limit from "
                                ++ formatMiB memoryLimit
                                ++ " to "
                                ++ formatMiB grownLimit
                        pure grownLimit

    replacementPrefix =
        show (replWorkerKind worker)
            ++ " shard "
            ++ show (replShardId shard)
            ++ " RAM replacement "

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
                    ReplSucceeded _ -> do
                        rememberWarmEvaluation shard evaluation
                        pure (Just session, result)
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
