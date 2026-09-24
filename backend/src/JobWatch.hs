module JobWatch (
    JobEnded,
    watchJob,
    unwatchJob,
    awaitJobEnded,
    markJobsEnded,
    watchedJobNames,
    isJobWatched,
) where

import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

newtype JobEnded = JobEnded (TVar Bool) deriving (Eq)

data Watch = Watch
    { watchers :: !Int
    , watchEnded :: JobEnded
    }

{-# NOINLINE watches #-}
watches :: TVar (Map.Map String Watch)
watches = unsafePerformIO $ newTVarIO Map.empty

watchJob :: String -> IO JobEnded
watchJob name = atomically $ do
    current <- readTVar watches
    case Map.lookup name current of
        Just watch -> do
            writeTVar watches (Map.insert name watch{watchers = watchers watch + 1} current)
            pure (watchEnded watch)
        Nothing -> do
            ended <- JobEnded <$> newTVar False
            writeTVar watches (Map.insert name (Watch 1 ended) current)
            pure ended

unwatchJob :: String -> JobEnded -> IO ()
unwatchJob name ended = atomically $ modifyTVar' watches (Map.update release name)
  where
    release watch
        | watchEnded watch /= ended = Just watch
        | watchers watch <= 1 = Nothing
        | otherwise = Just watch{watchers = watchers watch - 1}

awaitJobEnded :: JobEnded -> IO ()
awaitJobEnded (JobEnded ended) = atomically $ readTVar ended >>= check

markJobsEnded :: [String] -> IO ()
markJobsEnded names = atomically $ do
    current <- readTVar watches
    let (ended, remaining) = Map.partitionWithKey (\name _ -> Set.member name wanted) current
    mapM_ (signal . watchEnded) ended
    writeTVar watches remaining
  where
    wanted = Set.fromList names
    signal :: JobEnded -> STM ()
    signal (JobEnded flag) = writeTVar flag True

watchedJobNames :: IO [String]
watchedJobNames = Map.keys <$> readTVarIO watches

isJobWatched :: String -> IO Bool
isJobWatched name = Map.member name <$> readTVarIO watches
