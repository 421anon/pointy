{-# LANGUAGE ScopedTypeVariables #-}

module Handlers.LastBuilt (lastBuiltVersion, stepLastBuiltHandler) where

import BuiltSteps (LastBuiltVersion, builtSteps, firstParentHistory, markSearched, recordBuiltSteps, resolveLastBuilt, unsearchedSteps)
import Bus (broadcastLastBuilt)
import Certificates (storedStepPaths)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, (:>))
import Effects (App, AppM, Eval, Nix)
import ProjectFiles (stepFilePath)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (runGitIn, userRepoPath)

{-# NOINLINE backfillQueue #-}
backfillQueue :: MVar (Maybe (Map Text (Set Int)))
backfillQueue = unsafePerformIO (newMVar Nothing)

backfillDepth :: Int
backfillDepth = 32

backfillSettleMicros :: Int
backfillSettleMicros = 1000000

stepLastBuiltHandler :: Int -> Text -> AppM (Maybe LastBuiltVersion)
stepLastBuiltHandler sid = lift . lastBuiltVersion sid

lastBuiltVersion :: App es => Int -> Text -> Eff es (Maybe LastBuiltVersion)
lastBuiltVersion sid revision = do
    found <- Map.lookup sid <$> resolveLastBuilt revision [sid]
    when (isNothing found) $ liftIO $ do
        unsearched <- unsearchedSteps (Set.singleton sid)
        unless (Set.null unsearched) $ scheduleBackfill revision unsearched
    pure found

scheduleBackfill :: Text -> Set Int -> IO ()
scheduleBackfill revision steps =
    modifyMVar_ backfillQueue $ \queue -> do
        when (isNothing queue) $ void $ forkIO (threadDelay backfillSettleMicros >> backfillWorker)
        pure (Just (Map.insertWith Set.union revision steps (fold queue)))

backfillWorker :: IO ()
backfillWorker = nextBackfill >>= mapM_ (\(revision, steps) -> backfillLogged revision steps >> backfillWorker)

nextBackfill :: IO (Maybe (Text, Set Int))
nextBackfill =
    modifyMVar backfillQueue $ \queue ->
        pure $ case Map.minViewWithKey =<< queue of
            Just (request, queued) -> (Just queued, Just request)
            Nothing -> (Nothing, Nothing)

backfillLogged :: Text -> Set Int -> IO ()
backfillLogged revision steps =
    runAppEffects (backfill revision steps) `catch` \(err :: SomeException) ->
        putStrLn $ "Build history search at " ++ unpack revision ++ " failed: " ++ show err

backfill :: (Eval :> es, Nix :> es, IOE :> es) => Text -> Set Int -> Eff es ()
backfill revision steps = do
    pending <- liftIO (unsearchedSteps steps)
    unless (Set.null pending) $ searchHistory revision pending
    resolved <- resolveLastBuilt revision (Set.toList steps)
    unless (Map.null resolved) $ liftIO (broadcastLastBuilt revision resolved)

searchHistory :: (Eval :> es, Nix :> es, IOE :> es) => Text -> Set Int -> Eff es ()
searchHistory revision pending = do
    history <- liftIO (firstParentHistory revision)
    case history of
        Left err -> liftIO $ putStrLn $ "Build history search at " ++ unpack revision ++ " skipped: " ++ err
        Right commits ->
            searchAncestors (take backfillDepth (drop 1 (map fst commits))) pending
                >>= liftIO . markSearched revision . Set.difference pending

searchAncestors :: (Eval :> es, Nix :> es, IOE :> es) => [Text] -> Set Int -> Eff es (Set Int)
searchAncestors (ancestor : older) pending
    | not (Set.null pending) = do
        present <- liftIO (stepsPresentAt ancestor pending)
        found <- searchAncestor ancestor present
        Set.union found <$> searchAncestors older (Set.difference present found)
searchAncestors _ _ = pure Set.empty

searchAncestor :: (Eval :> es, Nix :> es, IOE :> es) => Text -> Set Int -> Eff es (Set Int)
searchAncestor ancestor present
    | Set.null present = pure Set.empty
    | otherwise = do
        stored <- storedStepPaths ancestor (Set.toList present)
        case stored of
            Left err -> do
                liftIO $ putStrLn $ "Build history search could not evaluate " ++ unpack ancestor ++ ": " ++ err
                pure Set.empty
            Right paths -> do
                built <- builtSteps paths
                liftIO (recordBuiltSteps ancestor built)
                pure (Map.keysSet built)

stepsPresentAt :: Text -> Set Int -> IO (Set Int)
stepsPresentAt commit steps = do
    repoPath <- userRepoPath
    (code, out, _) <- runGitIn repoPath (["ls-tree", "--name-only", "--end-of-options", unpack commit, "--"] ++ Map.keys files)
    pure $ case code of
        ExitSuccess -> Set.fromList (mapMaybe (`Map.lookup` files) (lines out))
        ExitFailure _ -> steps
  where
    files = Map.fromList [(stepFilePath "" sid, sid) | sid <- Set.toList steps]
