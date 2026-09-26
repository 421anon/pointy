{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixEvaluator (
    NixEvaluator,
    RepoSource (..),
    RepoExpression,
    defaultNixEvaluator,
    jsonExpression,
    rawExpression,
    jsonAppliedExpression,
    evaluate,
    evaluateImpure,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TQueue, TMVar, atomically, isEmptyTQueue, newEmptyTMVarIO, newTQueueIO, putTMVar, readTQueue, takeTMVar, writeTQueue)
import Control.Exception (SomeException, catch, try)
import Control.Monad (forever, void, when)
import Data.Maybe (fromMaybe)
import NixEvaluator.NixRepl (NixEvalOutput (..), NixEvalRequest (..), NixEvalTarget (..), ReplKind (..), ReplOutcome (..), ReplSession, closeSession, openSession, readSessionMemoryBytes, runRequest)
import System.IO.Unsafe (unsafePerformIO)

newtype RepoSource = RepoSource String
    deriving (Eq, Ord)

data RepoExpression = RepoExpression
    { expressionOutput :: NixEvalOutput
    , expressionApply :: Maybe String
    , expressionAttr :: String
    }

data NixEvaluator = NixEvaluator
    { pureWorker :: ReplWorker
    , impureWorker :: ReplWorker
    }

data ReplWorker = ReplWorker
    { replWorkerKind :: ReplKind
    , replWorkerSession :: MVar (Maybe ReplSession)
    , replWorkerQueue :: TQueue QueuedEval
    }

data QueuedEval = QueuedEval NixEvalRequest (TMVar (Either String String))

replMemoryLimitBytes :: Integer
replMemoryLimitBytes = 2 * 1024 * 1024 * 1024

{-# NOINLINE defaultNixEvaluator #-}
defaultNixEvaluator :: NixEvaluator
defaultNixEvaluator = unsafePerformIO newNixEvaluator

newNixEvaluator :: IO NixEvaluator
newNixEvaluator =
    NixEvaluator
        <$> newWorker PureRepl
        <*> newWorker ImpureRepl

jsonExpression :: String -> RepoExpression
jsonExpression = expression EvalJson Nothing

rawExpression :: String -> RepoExpression
rawExpression = expression EvalRaw Nothing

jsonAppliedExpression :: String -> String -> RepoExpression
jsonAppliedExpression applyExpr = expression EvalJson $ Just applyExpr

expression :: NixEvalOutput -> Maybe String -> String -> RepoExpression
expression output applyExpr attr = RepoExpression output applyExpr attr

evaluate :: NixEvaluator -> RepoSource -> RepoExpression -> IO (Either String String)
evaluate evaluator (RepoSource installable) repoExpr =
    evaluateRequest evaluator $
        NixEvalRequest
            { evalImpure = False
            , evalOutput = expressionOutput repoExpr
            , evalApply = expressionApply repoExpr
            , evalTarget = EvalInstallable installable (expressionAttr repoExpr)
            }

evaluateImpure :: NixEvaluator -> String -> IO (Either String String)
evaluateImpure evaluator expr =
    evaluateRequest evaluator $
        NixEvalRequest True EvalJson Nothing $ EvalExpr expr

evaluateRequest :: NixEvaluator -> NixEvalRequest -> IO (Either String String)
evaluateRequest evaluator request = do
    response <- newEmptyTMVarIO
    atomically $ writeTQueue (replWorkerQueue $ workerForRequest evaluator request) (QueuedEval request response)
    atomically $ takeTMVar response

workerForRequest :: NixEvaluator -> NixEvalRequest -> ReplWorker
workerForRequest evaluator request
    | evalImpure request = impureWorker evaluator
    | otherwise = pureWorker evaluator

newWorker :: ReplKind -> IO ReplWorker
newWorker kind = do
    session <- newMVar Nothing
    queue <- newTQueueIO
    let worker = ReplWorker kind session queue
    void $ forkIO $ replWorkerLoop worker
    pure worker

replWorkerLoop :: ReplWorker -> IO ()
replWorkerLoop worker = forever $ do
    QueuedEval request response <- atomically $ readTQueue (replWorkerQueue worker)
    result <-
        runRequestWithSession True worker request
            `catch` \(err :: SomeException) -> pure $ Left $ "nix repl request failed: " ++ show err
    atomically $ putTMVar response result
    releaseIdleSession worker

runRequestWithSession :: Bool -> ReplWorker -> NixEvalRequest -> IO (Either String String)
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
        ReplFailed err -> pure $ Left err
        ReplDied err
            | mayRetry -> runRequestWithSession False worker request
            | otherwise -> pure $ Left err
  where
    openActiveSession = try $ openSession $ replWorkerKind worker

releaseIdleSession :: ReplWorker -> IO ()
releaseIdleSession worker = do
    idle <- atomically $ isEmptyTQueue (replWorkerQueue worker)
    when idle $
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
