module Cache (memoizeStepOutPaths, getOutPathFromCache) where

import Control.Concurrent.STM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data CacheState = CacheState
    { projectPaths :: Map (Int, Text) (Map Int Text) -- (ProjectId, CommitHash) -> (StepId -> OutPath)
    , stepPaths :: Map (Int, Text) Text -- (StepId, CommitHash) -> OutPath (Flattened for fast lookup)
    }

{-# NOINLINE cache #-}
cache :: TVar CacheState
cache = unsafePerformIO $ newTVarIO (CacheState Map.empty Map.empty)

memoizeStepOutPaths :: Int -> Text -> IO (Map Int Text) -> IO (Map Int Text)
memoizeStepOutPaths projectId commitHash action = do
    state <- readTVarIO cache
    case Map.lookup (projectId, commitHash) (projectPaths state) of
        Just result -> return result
        Nothing -> do
            result <- action
            atomically $ modifyTVar' cache $ \s ->
                s
                    { projectPaths = Map.insert (projectId, commitHash) result (projectPaths s)
                    , stepPaths = Map.union (Map.fromList [((sid, commitHash), path) | (sid, path) <- Map.toList result]) (stepPaths s)
                    }
            return result

getOutPathFromCache :: Text -> Int -> IO (Maybe Text)
getOutPathFromCache commitHash stepId = do
    atomically $ Map.lookup (stepId, commitHash) . stepPaths <$> readTVar cache
