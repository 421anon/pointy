{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OutPaths (
    getProjectOutPaths,
    warmProjectOutPaths,
    warmProjectOutPathsForCommit,
    scheduleProjectOutPathsWarm,
    withWriteRepoTransaction,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar)
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
import NixRepl (NixEvalOutput (..), NixEvalRequest (..), NixEvalTarget (..), scheduleNixReplWarmRotation)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), WriteRepoContext, runNixEvalJsonInRepo, runNixEvalJsonInRepoBackground, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

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

{- | Resolve and cache project outPaths. Concurrent callers for the same
(projectId, commit) key share one evaluation (singleflight).
-}
getProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
getProjectOutPaths pid targetCommit = do
    let key = (pid, targetCommit)
    (mv, isNew) <- modifyMVar outPathCacheRef $ \cache ->
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
    if isNew
        then do
            result <-
                evalProjectOutPaths pid targetCommit
                    `catch` \(err :: SomeException) ->
                        return $ Left $ "Project outPath evaluation crashed: " ++ show err
            putMVar mv result
            return result
        else readMVar mv

evalProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
evalProjectOutPaths pid targetCommit = do
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

{- | Schedule a warmed REPL rotation for a project's outPaths attribute,
and fork a background cache fill so the result is available immediately
when the frontend requests it.
-}
scheduleProjectOutPathsWarm :: Int -> Text -> IO ()
scheduleProjectOutPathsWarm pid commit = do
    repoPath <- userRepoPath
    let installable = "git+file://" ++ repoPath ++ "?rev=" ++ unpack commit ++ "&allRefs=true"
        attr = "#pointy.projectOutPaths." ++ show pid
        req = NixEvalRequest False EvalJson Nothing (EvalInstallable installable attr)
    scheduleNixReplWarmRotation [req]
    void $ forkIO $ do
        _ <- getProjectOutPaths pid commit
        return ()

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
