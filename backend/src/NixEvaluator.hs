{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixEvaluator (
    NixEvaluator,
    HistoryEvaluator,
    RepoSource (..),
    RepoExpression,
    EvalFailure (..),
    defaultNixEvaluator,
    historyEvaluatorWorkers,
    historyNixEvaluator,
    jsonExpression,
    rawExpression,
    jsonAppliedExpression,
    evaluate,
    evaluateHistory,
    evaluateImpure,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TQueue, TMVar, atomically, check, isEmptyTQueue, newEmptyTMVarIO, newTQueueIO, orElse, putTMVar, readTQueue, readTVar, registerDelay, takeTMVar, writeTQueue)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forever, replicateM_, void, when)
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import NixEvaluator.NixRepl (NixEvalOutput (..), NixEvalRequest (..), NixEvalTarget (..), ReplKind (..), ReplOutcome (..), ReplSession, closeSession, openSession, readSessionMemoryBytes, runRequest)
import System.IO.Unsafe (unsafePerformIO)

data RepoSource
    = RepoRef String
    | RepoWorktree FilePath
    deriving (Eq, Ord)

data RepoExpression = RepoExpression
    { expressionOutput :: NixEvalOutput
    , expressionApply :: Maybe String
    , expressionAttr :: String
    }

data EvalFailure
    = EvalError String
    | EvalUnavailable String
    deriving (Eq, Show)

data NixEvaluator = NixEvaluator
    { pureWorker :: ReplWorker
    , impureWorker :: ReplWorker
    }

newtype HistoryEvaluator = HistoryEvaluator (TQueue QueuedEval)

data ReplWorker = ReplWorker
    { replWorkerKind :: ReplKind
    , replWorkerSession :: MVar (Maybe ReplSession)
    , replWorkerQueue :: TQueue QueuedEval
    , replWorkerRelease :: ReleasePolicy
    }

data ReleasePolicy = ReleaseWhenIdle | ReleaseAfterRequest

data QueuedEval = QueuedEval NixEvalRequest (TMVar (Either EvalFailure ByteString))

replMemoryLimitBytes :: Integer
replMemoryLimitBytes = 2 * 1024 * 1024 * 1024

historyEvaluatorWorkers :: Int
historyEvaluatorWorkers = 4

historySessionIdleSeconds :: Int
historySessionIdleSeconds = 60

unavailableFailureMarkers :: [String]
unavailableFailureMarkers =
    [ "while fetching the input"
    , "unable to download"
    , "could not resolve host"
    , "couldn't resolve host"
    , "connection refused"
    , "timed out"
    , "http error"
    , "cannot connect to socket"
    , "no space left on device"
    , "out of memory"
    , "unable to fork"
    ]

{-# NOINLINE defaultNixEvaluator #-}
defaultNixEvaluator :: NixEvaluator
defaultNixEvaluator = unsafePerformIO newNixEvaluator

newNixEvaluator :: IO NixEvaluator
newNixEvaluator = do
    pureQueue <- newTQueueIO
    impureQueue <- newTQueueIO
    NixEvaluator
        <$> newWorker PureRepl pureQueue ReleaseWhenIdle
        <*> newWorker ImpureRepl impureQueue ReleaseWhenIdle

{-# NOINLINE historyNixEvaluator #-}
historyNixEvaluator :: HistoryEvaluator
historyNixEvaluator = unsafePerformIO newHistoryEvaluator

newHistoryEvaluator :: IO HistoryEvaluator
newHistoryEvaluator = do
    queue <- newTQueueIO
    replicateM_ historyEvaluatorWorkers $ newWorker PureRepl queue ReleaseAfterRequest
    pure $ HistoryEvaluator queue

jsonExpression :: String -> RepoExpression
jsonExpression = expression EvalJson Nothing

rawExpression :: String -> RepoExpression
rawExpression = expression EvalRaw Nothing

jsonAppliedExpression :: String -> String -> RepoExpression
jsonAppliedExpression applyExpr = expression EvalJson $ Just applyExpr

expression :: NixEvalOutput -> Maybe String -> String -> RepoExpression
expression output applyExpr attr = RepoExpression output applyExpr attr

evaluate :: NixEvaluator -> RepoSource -> RepoExpression -> IO (Either String ByteString)
evaluate evaluator source repoExpr =
    eitherFailureMessage <$> evaluateRequest evaluator (requestForSource source repoExpr)

evaluateHistory :: HistoryEvaluator -> RepoSource -> RepoExpression -> IO (Either EvalFailure ByteString)
evaluateHistory (HistoryEvaluator queue) source repoExpr =
    enqueueEval queue (requestForSource source repoExpr)

requestForSource :: RepoSource -> RepoExpression -> NixEvalRequest
requestForSource source repoExpr =
    NixEvalRequest
        { evalImpure = sourceImpure source
        , evalOutput = expressionOutput repoExpr
        , evalApply = expressionApply repoExpr
        , evalTarget = EvalInstallable (sourceInstallable source) (expressionAttr repoExpr)
        }

sourceInstallable :: RepoSource -> String
sourceInstallable = \case
    RepoRef reference -> reference
    RepoWorktree path -> path

sourceImpure :: RepoSource -> Bool
sourceImpure = \case
    RepoRef _ -> False
    RepoWorktree _ -> True

evaluateImpure :: NixEvaluator -> String -> IO (Either String ByteString)
evaluateImpure evaluator expr =
    eitherFailureMessage <$> evaluateRequest evaluator (NixEvalRequest True EvalJson Nothing $ EvalExpr expr)

evaluateRequest :: NixEvaluator -> NixEvalRequest -> IO (Either EvalFailure ByteString)
evaluateRequest evaluator request =
    enqueueEval (replWorkerQueue $ workerForRequest evaluator request) request

enqueueEval :: TQueue QueuedEval -> NixEvalRequest -> IO (Either EvalFailure ByteString)
enqueueEval queue request = do
    response <- newEmptyTMVarIO
    atomically $ writeTQueue queue (QueuedEval request response)
    atomically $ takeTMVar response

workerForRequest :: NixEvaluator -> NixEvalRequest -> ReplWorker
workerForRequest evaluator request
    | evalImpure request = impureWorker evaluator
    | otherwise = pureWorker evaluator

newWorker :: ReplKind -> TQueue QueuedEval -> ReleasePolicy -> IO ReplWorker
newWorker kind queue release = do
    session <- newMVar Nothing
    let worker = ReplWorker kind session queue release
    void $ forkIO $ replWorkerLoop worker
    pure worker

replWorkerLoop :: ReplWorker -> IO ()
replWorkerLoop worker = forever $ do
    QueuedEval request response <- nextQueuedEval worker
    result <-
        runRequestWithSession True worker request
            `catch` \(err :: SomeException) -> pure $ Left $ EvalUnavailable $ "nix repl request failed: " ++ show err
    atomically $ putTMVar response result
    releaseWorkerSession worker

nextQueuedEval :: ReplWorker -> IO QueuedEval
nextQueuedEval worker = case replWorkerRelease worker of
    ReleaseWhenIdle -> atomically $ readTQueue (replWorkerQueue worker)
    ReleaseAfterRequest -> do
        idleTimeout <- registerDelay (historySessionIdleSeconds * 1000000)
        next <- atomically $ (Just <$> readTQueue (replWorkerQueue worker)) `orElse` (readTVar idleTimeout >>= check >> pure Nothing)
        case next of
            Just queued -> pure queued
            Nothing -> do
                modifyMVar_ (replWorkerSession worker) $ \session -> mapM_ closeQuietly session >> pure Nothing
                nextQueuedEval worker

runRequestWithSession :: Bool -> ReplWorker -> NixEvalRequest -> IO (Either EvalFailure ByteString)
runRequestWithSession mayRetry worker request = do
    outcome <- modifyMVar (replWorkerSession worker) $ \mSession -> do
        eSession <- maybe openActiveSession (pure . Right) mSession
        case eSession of
            Left (err :: SomeException) ->
                pure (Nothing, ReplDied $ "failed to start " ++ show (replWorkerKind worker) ++ " nix repl: " ++ show err)
            Right session -> do
                result <-
                    runRequest session request
                        `catch` \(err :: SomeException) -> pure $ ReplDied $ "nix repl session failed: " ++ show err
                case result of
                    ReplDied err -> do
                        closeQuietly session
                        pure (Nothing, ReplDied err)
                    ReplSucceeded output -> pure (Just session, ReplSucceeded output)
                    ReplFailed err -> pure (Just session, ReplFailed err)
    case outcome of
        ReplSucceeded output -> pure $ Right output
        ReplFailed err -> pure $ Left (classifyFailure err)
        ReplDied err
            | mayRetry -> runRequestWithSession False worker request
            | otherwise -> pure $ Left $ EvalUnavailable err
  where
    openActiveSession = try $ openSession $ replWorkerKind worker

classifyFailure :: String -> EvalFailure
classifyFailure message
    | any (`isInfixOf` map toLower message) unavailableFailureMarkers = EvalUnavailable message
    | otherwise = EvalError message

eitherFailureMessage :: Either EvalFailure ByteString -> Either String ByteString
eitherFailureMessage = either (Left . failureMessage) Right

failureMessage :: EvalFailure -> String
failureMessage = \case
    EvalError message -> message
    EvalUnavailable message -> message

releaseWorkerSession :: ReplWorker -> IO ()
releaseWorkerSession worker =
    case replWorkerRelease worker of
        ReleaseWhenIdle -> do
            idle <- atomically $ isEmptyTQueue (replWorkerQueue worker)
            when idle $ releaseOverMemory worker
        ReleaseAfterRequest -> releaseOverMemory worker

releaseOverMemory :: ReplWorker -> IO ()
releaseOverMemory worker =
    modifyMVar_ (replWorkerSession worker) $ \case
        Nothing -> pure Nothing
        Just session -> do
            memoryBytes <- readSessionMemoryBytes session
            if maybe False (> replMemoryLimitBytes) memoryBytes
                then do
                    closeQuietly session
                    logWarning $
                        show (replWorkerKind worker)
                            ++ " nix repl released at "
                            ++ formatMiB (fromMaybe 0 memoryBytes)
                            ++ " RSS"
                    pure Nothing
                else pure $ Just session

closeQuietly :: ReplSession -> IO ()
closeQuietly session = closeSession session `catch` \(_ :: SomeException) -> pure ()

logWarning :: String -> IO ()
logWarning message = putStrLn ("Warning: " ++ message) `catch` \(_ :: SomeException) -> pure ()

formatMiB :: Integer -> String
formatMiB bytes = show (bytes `div` (1024 * 1024)) ++ " MiB"
