{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OutPaths (
    getProjectOutPaths,
    warmProjectOutPaths,
    warmProjectOutPathsForCommit,
    scheduleProjectOutPathsWarm,
    withWriteRepoTransaction,
    lookupCachedStepOutPath,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, readMVar, tryPutMVar, tryReadMVar)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import NixRepl (NixEvalOutput (..), NixEvalRequest (..), NixEvalTarget (..), scheduleNixReplWarmRotation, scheduleNixReplWarmRotationWithResult)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), WriteRepoContext, ensureRepoCommit, runNixEvalJsonInRepo, runNixEvalJsonInRepoBackground, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

-- Types

data ProjectDef = ProjectDef
    { projectDefId :: Int
    , projectDefHidden :: Bool
    , projectDefSteps :: [StepRef]
    }
    deriving (Show, Generic)

instance FromJSON ProjectDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "projectDef"

data StepRef = StepRef
    { stepRefHidden :: Bool
    , stepRefDef :: StepDef
    }
    deriving (Show, Generic)

instance FromJSON StepRef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "stepRef"

newtype StepDef = StepDef
    { stepDefId :: Int
    }
    deriving (Show, Generic)

instance FromJSON StepDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "stepDef"

prefixedFieldOptions :: String -> Options
prefixedFieldOptions prefix =
    defaultOptions
        { fieldLabelModifier = \field ->
            map toLower (fromMaybe field (stripPrefix prefix field))
        }

-- OutPath evaluation cache with singleflight

data OutPathCache = OutPathCache
    { cacheEntries :: Map (Int, Text) (MVar (Either String (Map Int Text)))
    , cacheOrder :: [(Int, Text)]
    }

maxOutPathCacheSize :: Int
maxOutPathCacheSize = 32

{-# NOINLINE outPathCacheRef #-}
outPathCacheRef :: MVar OutPathCache
outPathCacheRef = unsafePerformIO (newMVar (OutPathCache Map.empty []))

pruneCache :: OutPathCache -> OutPathCache
pruneCache cache
    | length (cacheOrder cache) <= maxOutPathCacheSize = cache
    | (oldest : rest) <- cacheOrder cache =
        cache
            { cacheEntries = Map.delete oldest (cacheEntries cache)
            , cacheOrder = rest
            }
    | otherwise = cache

{- | Claim a slot in the outPath cache for a (projectId, commit) key.
Returns (mv, True) when the caller is the producer and MUST eventually
call 'completeProjectOutPaths'; returns (mv, False) when the slot is
already claimed and the caller should 'readMVar' the result.
-}
claimProjectOutPaths :: (Int, Text) -> IO (MVar (Either String (Map Int Text)), Bool)
claimProjectOutPaths key = modifyMVar outPathCacheRef $ \cache ->
    case Map.lookup key (cacheEntries cache) of
        Just mv -> return (cache, (mv, False))
        Nothing -> do
            mv <- newEmptyMVar
            let cache' =
                    pruneCache
                        cache
                            { cacheEntries = Map.insert key mv (cacheEntries cache)
                            , cacheOrder = cacheOrder cache ++ [key]
                            }
            return (cache', (mv, True))

{- | Complete a claimed outPath cache slot. Uses 'tryPutMVar' so the
completing thread never blocks. On failure the slot is evicted from
the cache (identity-checked as today) so waiters can retry.
-}
completeProjectOutPaths :: (Int, Text) -> MVar (Either String (Map Int Text)) -> Either String (Map Int Text) -> IO ()
completeProjectOutPaths key mv result = do
    ok <- tryPutMVar mv result
    when ok $
        case result of
            Left _ -> discardFailedProjectOutPaths key mv
            Right _ -> return ()

{- | Read-only projection over the bounded project cache: look up a step's
store path for a given commit without blocking on in-flight evaluations.
Snaps same-commit MVars, inspects via 'tryReadMVar', skips in-flight /
error slots, and only returns a path when every completed hit agrees.
-}
lookupCachedStepOutPath :: Text -> Int -> IO (Maybe Text)
lookupCachedStepOutPath commit stepId = do
    cache <- readMVar outPathCacheRef
    let matching = [mv | ((_, c), mv) <- Map.toList (cacheEntries cache), c == commit]
    results <- mapM tryReadMVar matching
    let paths = [p | Just (Right m) <- results, Just p <- [Map.lookup stepId m]]
    case paths of
        [] -> return Nothing
        (p : ps)
            | all (== p) ps -> return (Just p)
            | otherwise -> return Nothing

{- | Resolve and cache project outPaths. Concurrent callers for the same
(projectId, commit) key share one evaluation (singleflight).
-}
getProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
getProjectOutPaths pid targetCommit = do
    let key = (pid, targetCommit)
    (mv, isNew) <- claimProjectOutPaths key
    if isNew
        then do
            result <-
                evalProjectOutPaths pid targetCommit
                    `catch` \(err :: SomeException) ->
                        return $ Left $ "Project outPath evaluation crashed: " ++ show err
            completeProjectOutPaths key mv result
            return result
        else readMVar mv

-- Failed evaluations are removed after waking current waiters. A later caller
-- can retry once a transiently missing commit or repository fetch recovers.
discardFailedProjectOutPaths :: (Int, Text) -> MVar (Either String (Map Int Text)) -> IO ()
discardFailedProjectOutPaths key evaluatedMv =
    modifyMVar_ outPathCacheRef $ \cache ->
        case Map.lookup key (cacheEntries cache) of
            Just cachedMv
                | cachedMv == evaluatedMv ->
                    return
                        cache
                            { cacheEntries = Map.delete key (cacheEntries cache)
                            , cacheOrder = filter (/= key) (cacheOrder cache)
                            }
            _ -> return cache

evalProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
evalProjectOutPaths pid targetCommit = do
    commitResult <- runExceptT $ ensureRepoCommit (unpack targetCommit)
    case commitResult of
        Left err -> return $ Left $ "Failed to prepare project commit: " ++ err
        Right () -> do
            repoPath <- userRepoPath
            result <-
                runExceptT $
                    runNixEvalJsonInRepo
                        (ReadRepoContext repoPath (unpack targetCommit))
                        ("#pointy.projectOutPaths." ++ show pid)
            return $ case result of
                Left err -> Left $ "Failed to evaluate #pointy.projectOutPaths." ++ show pid ++ ": " ++ err
                Right output ->
                    case decode (TLE.encodeUtf8 (TL.pack output)) of
                        Nothing -> Left $ "Failed to parse #pointy.projectOutPaths." ++ show pid
                        Just paths -> Right paths

{- | Schedule a warmed REPL rotation for a project's outPaths attribute.
Claims the cache slot first: if another caller already claimed it this is
a no-op.  When we are the producer the warm REPL callback decodes the
JSON result and completes the slot, removing the duplicate active
evaluation that the old implementation forked.
-}
scheduleProjectOutPathsWarm :: Int -> Text -> IO ()
scheduleProjectOutPathsWarm pid commit = do
    let key = (pid, commit)
    (mv, isNew) <- claimProjectOutPaths key
    when isNew $
        ( do
            repoPath <- userRepoPath
            let installable = "git+file://" ++ repoPath ++ "?rev=" ++ unpack commit ++ "&allRefs=true"
                attr = "#pointy.projectOutPaths." ++ show pid
                req = NixEvalRequest False EvalJson Nothing (EvalInstallable installable attr)
                decodeResult (Left err) = Left $ "Failed to evaluate " ++ attr ++ ": " ++ err
                decodeResult (Right output) =
                    case decode (TLE.encodeUtf8 (TL.pack output)) of
                        Nothing -> Left $ "Failed to parse " ++ attr
                        Just paths -> Right paths
            scheduleNixReplWarmRotationWithResult req (\result -> completeProjectOutPaths key mv (decodeResult result))
        )
            `catch` \(err :: SomeException) ->
                completeProjectOutPaths key mv (Left $ "Warm scheduling crashed: " ++ show err)

-- REPL warming (no cold restart — uses warm REPL rotation)

warmProjectOutPaths :: IO ()
warmProjectOutPaths = do
    repoPath <- userRepoPath
    mTargetCommit <- withReadRepoTransaction $ \(ReadRepoContext _ hash) ->
        return $ pack hash
    case mTargetCommit of
        Left err -> putStrLn $ "Project outPath warm skipped: " ++ err
        Right targetCommit -> do
            let targetCommitString = unpack targetCommit
                installable = "git+file://" ++ repoPath ++ "?rev=" ++ targetCommitString ++ "&allRefs=true"
                attr = "#pointy.projectOutPaths"
                req = NixEvalRequest False EvalJson Nothing (EvalInstallable installable attr)
            scheduleNixReplWarmRotation [req]
            result <- runExceptT $ warmProjectOutPathsForCommit (ReadRepoContext repoPath targetCommitString)
            case result of
                Left err -> putStrLn $ "Project outPath warm failed: " ++ err
                Right () -> return ()

scheduleHeadOutPathsWarm :: IO ()
scheduleHeadOutPathsWarm = do
    repoPath <- userRepoPath
    mTargetCommit <- withReadRepoTransaction $ \(ReadRepoContext _ hash) ->
        return $ pack hash
    case mTargetCommit of
        Left err -> putStrLn $ "Project outPath warm skipped: " ++ err
        Right targetCommit -> do
            let targetCommitString = unpack targetCommit
                installable = "git+file://" ++ repoPath ++ "?rev=" ++ targetCommitString ++ "&allRefs=true"
                attr = "#pointy.projectOutPaths"
                req = NixEvalRequest False EvalJson Nothing (EvalInstallable installable attr)
            scheduleNixReplWarmRotation [req]

warmProjectOutPathsForCommit :: ReadRepoContext -> ExceptT String IO ()
warmProjectOutPathsForCommit ctx = do
    _ <- runNixEvalJsonInRepoBackground ctx "#pointy.projectOutPaths"
    return ()

-- Coalesced background warming

{- | At most one background warm runs at a time. Writes that arrive while a
warm is in flight set 'warmPending' so the worker re-reads the latest HEAD
and warms again once the current eval finishes. This collapses bursts of
rapid commits into a single warm of the latest HEAD instead of queuing one
stale warm per commit behind the REPL session lock.
-}
data WarmState = WarmState
    { warmRunning :: Bool
    , warmPending :: Bool
    }

{-# NOINLINE warmStateRef #-}
warmStateRef :: MVar WarmState
warmStateRef = unsafePerformIO (newMVar (WarmState False False))

-- | Mark a warm as pending, forking a worker if none is running.
scheduleWarm :: IO ()
scheduleWarm =
    modifyMVar_ warmStateRef $ \st ->
        if warmRunning st
            then return st{warmPending = True}
            else do
                void $ forkIO warmWorker
                return st{warmRunning = True, warmPending = False}

runWarmSafely :: IO ()
runWarmSafely =
    scheduleHeadOutPathsWarm `catch` handleWarmException

handleWarmException :: SomeException -> IO ()
handleWarmException err =
    putStrLn $ "Project outPath warm crashed: " ++ show err

-- | Run one warm, then loop once more if another write landed during it.
warmWorker :: IO ()
warmWorker = do
    runWarmSafely
    again <- modifyMVar warmStateRef $ \st ->
        if warmPending st
            then return (st{warmRunning = True, warmPending = False}, True)
            else return (st{warmRunning = False, warmPending = False}, False)
    when again warmWorker

-- Write transaction with post-write REPL warming

withWriteRepoTransaction :: (WriteRepoContext -> ExceptT String IO a) -> IO (Either String a)
withWriteRepoTransaction action = do
    result <- withWriteRepoTransactionRaw action
    case result of
        Right _ -> scheduleWarm
        Left _ -> return ()
    return result
