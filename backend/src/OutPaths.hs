{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Exception (SomeException, catch, handle)
import Control.Monad (forM, forM_, void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Either (isLeft, isRight)
import Data.List (stripPrefix)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), WriteRepoContext, ensureRepoCommit, rewarmRepoJsonExpressions, runNixEvalJsonInRepo, runNixEvalJsonInRepoBackground, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

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

-- The Bool marks the caller responsible for completing a new cache entry.
claimProjectOutPaths :: (Int, Text) -> IO (MVar (Either String (Map Int Text)), Bool)
claimProjectOutPaths key = modifyMVar outPathCacheRef $ \cache ->
    case Map.lookup key (cacheEntries cache) of
        Just mv -> pure (cache, (mv, False))
        Nothing -> do
            mv <- newEmptyMVar
            let cache' =
                    pruneCache
                        cache
                            { cacheEntries = Map.insert key mv (cacheEntries cache)
                            , cacheOrder = cacheOrder cache ++ [key]
                            }
            pure (cache', (mv, True))

completeProjectOutPaths :: (Int, Text) -> MVar (Either String (Map Int Text)) -> Either String (Map Int Text) -> IO ()
completeProjectOutPaths key mv result = do
    completed <- tryPutMVar mv result
    when (completed && isLeft result) $ discardFailedProjectOutPaths key mv

lookupCachedStepOutPath :: Text -> Int -> IO (Maybe Text)
lookupCachedStepOutPath commit stepId = do
    cache <- readMVar outPathCacheRef
    results <- mapM tryReadMVar [mv | ((_, c), mv) <- Map.toList (cacheEntries cache), c == commit]
    let paths = [p | Just (Right m) <- results, Just p <- [Map.lookup stepId m]]
    pure $ case paths of
        [] -> Nothing
        p : ps | all (== p) ps -> Just p
        _ -> Nothing

getProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
getProjectOutPaths pid targetCommit = do
    let key = (pid, targetCommit)
    (mv, isNew) <- claimProjectOutPaths key
    if isNew
        then do
            result <-
                evalProjectOutPaths pid targetCommit
                    `catch` \(err :: SomeException) ->
                        pure $ Left $ "Project outPath evaluation crashed: " ++ show err
            completeProjectOutPaths key mv result
            pure result
        else readMVar mv

-- Failed evaluations are removed after waking current waiters. A later caller
-- can retry once a transiently missing commit or repository fetch recovers.
discardFailedProjectOutPaths :: (Int, Text) -> MVar (Either String (Map Int Text)) -> IO ()
discardFailedProjectOutPaths key evaluatedMv =
    modifyMVar_ outPathCacheRef $ \cache ->
        case Map.lookup key (cacheEntries cache) of
            Just cachedMv
                | cachedMv == evaluatedMv ->
                    pure
                        cache
                            { cacheEntries = Map.delete key (cacheEntries cache)
                            , cacheOrder = filter (/= key) (cacheOrder cache)
                            }
            _ -> pure cache

evalProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
evalProjectOutPaths pid targetCommit = runExceptT $ do
    let attr = projectOutPathAttr pid
    withExceptT ("Failed to prepare project commit: " ++) $
        ensureRepoCommit $
            unpack targetCommit
    repoPath <- liftIO userRepoPath
    output <-
        withExceptT (("Failed to evaluate " ++ attr ++ ": ") ++) $
            runNixEvalJsonInRepo
                (ReadRepoContext repoPath $ unpack targetCommit)
                attr
    maybe (throwError $ "Failed to parse " ++ attr) pure $ decodeJson output

scheduleProjectOutPathsWarm :: Int -> Text -> IO ()
scheduleProjectOutPathsWarm pid commit = do
    let key = (pid, commit)
    (mv, isNew) <- claimProjectOutPaths key
    when isNew $ do
        repoPath <- userRepoPath
        let attr = projectOutPathAttr pid
            ctx = ReadRepoContext repoPath (unpack commit)
            onError (err :: SomeException) =
                completeProjectOutPaths key mv $ Left $ "Warm scheduling crashed: " ++ show err
        void $ forkIO $ handle onError $ do
            result <- runExceptT $ runNixEvalJsonInRepoBackground ctx attr
            completeProjectOutPaths key mv $ decodeOutPathResult pid result

warmProjectOutPaths :: IO ()
warmProjectOutPaths = do
    repoPath <- userRepoPath
    withReadRepoTransaction (pure . pack . readCommitHash) >>= \case
        Left err -> putStrLn $ "Project outPath warm skipped: " ++ err
        Right commit ->
            runExceptT (warmProjectOutPathsForCommit $ ReadRepoContext repoPath $ unpack commit)
                >>= either (putStrLn . ("Project outPath warm failed: " ++)) pure

scheduleHeadOutPathsWarm :: IO ()
scheduleHeadOutPathsWarm = warmProjectOutPaths

warmProjectOutPathsForCommit :: ReadRepoContext -> ExceptT String IO ()
warmProjectOutPathsForCommit ctx = do
    let commit = readCommitHash ctx
        commitText = pack commit
    projectsRaw <- runNixEvalJsonInRepo ctx "#pointy.projects"
    projectDefs <-
        maybe (throwError "Failed to parse #pointy.projects") pure (decodeJson projectsRaw :: Maybe (Map String ProjectDef))
    claims <- liftIO $ forM (map projectDefId $ Map.elems projectDefs) $ \pid -> do
        (mv, isNew) <- claimProjectOutPaths (pid, commitText)
        pure (pid, mv, isNew)

    let newClaims = [(pid, mv) | (pid, mv, True) <- claims]
    forM_ [(pid, mv) | (pid, mv, False) <- claims] $ \(pid, mv) ->
        liftIO (readMVar mv)
            >>= either
                (throwError . (("Project " ++ show pid ++ " outPath evaluation failed: ") ++))
                (const $ pure ())

    case NonEmpty.nonEmpty newClaims of
        Nothing -> pure ()
        Just pending -> do
            results <-
                liftIO $
                    rewarmRepoJsonExpressions ctx $
                        fmap (projectOutPathAttr . fst) pending
            decoded <- liftIO $ forM (zip (NonEmpty.toList pending) results) $ \((pid, mv), result) -> do
                let value = decodeOutPathResult pid result
                completeProjectOutPaths (pid, commitText) mv value
                pure value
            mapM_ (either throwError $ const $ pure ()) decoded

projectOutPathAttr :: Int -> String
projectOutPathAttr pid = "#pointy.projectOutPaths." ++ show pid

decodeOutPathResult :: Int -> Either String String -> Either String (Map Int Text)
decodeOutPathResult pid =
    either (Left . (("Failed to evaluate " ++ attr ++ ": ") ++)) $
        maybe (Left $ "Failed to parse " ++ attr) Right . decodeJson
  where
    attr = projectOutPathAttr pid

decodeJson :: (FromJSON a) => String -> Maybe a
decodeJson = decode . TLE.encodeUtf8 . TL.pack

-- Coalesce commit bursts so only the latest pending HEAD is rewarmed.
data WarmState = WarmState
    { warmRunning :: Bool
    , warmPending :: Bool
    }

{-# NOINLINE warmStateRef #-}
warmStateRef :: MVar WarmState
warmStateRef = unsafePerformIO (newMVar (WarmState False False))

scheduleWarm :: IO ()
scheduleWarm =
    modifyMVar_ warmStateRef $ \st ->
        if warmRunning st
            then pure st{warmPending = True}
            else do
                void $ forkIO warmWorker
                pure st{warmRunning = True, warmPending = False}

runWarmSafely :: IO ()
runWarmSafely =
    scheduleHeadOutPathsWarm `catch` handleWarmException

handleWarmException :: SomeException -> IO ()
handleWarmException err =
    putStrLn $ "Project outPath warm crashed: " ++ show err

warmWorker :: IO ()
warmWorker = do
    runWarmSafely
    again <- modifyMVar warmStateRef $ \st ->
        if warmPending st
            then pure (st{warmRunning = True, warmPending = False}, True)
            else pure (st{warmRunning = False, warmPending = False}, False)
    when again warmWorker

withWriteRepoTransaction :: (WriteRepoContext -> ExceptT String IO a) -> IO (Either String a)
withWriteRepoTransaction action = do
    result <- withWriteRepoTransactionRaw action
    when (isRight result) scheduleWarm
    pure result
