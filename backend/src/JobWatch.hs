module JobWatch (
    JobEnded,
    watchJob,
    unwatchJob,
    awaitJobEnded,
    markJobsEnded,
    markJobNameEnded,
    markTerminalJobs,
    watchedJobs,
) where

import Control.Concurrent.STM (STM, TVar, atomically, check, modifyTVar', newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

data JobEnded = JobEnded
    { endedWatchId :: !Int
    , endedFlag :: !(TVar Bool)
    }

data Watch = Watch
    { watchName :: !String
    , watchEnded :: !JobEnded
    , watchJobIds :: !(Set.Set String)
    }

{-# NOINLINE watches #-}
watches :: TVar (Map.Map Int Watch)
watches = unsafePerformIO $ newTVarIO Map.empty

{-# NOINLINE nextWatchId #-}
nextWatchId :: TVar Int
nextWatchId = unsafePerformIO $ newTVarIO 0

{-# NOINLINE terminalHintCache #-}
terminalHintCache :: TVar (Set.Set (String, String))
terminalHintCache = unsafePerformIO $ newTVarIO Set.empty

watchJob :: String -> [String] -> IO JobEnded
watchJob name jobIds = atomically $ do
    next <- readTVar nextWatchId
    writeTVar nextWatchId (next + 1)
    ended <- JobEnded next <$> newTVar False
    known <- readTVar terminalHintCache
    let watchedIds = Set.fromList jobIds
        alreadyTerminal = any (\(jobId, jobName) -> jobName == name && Set.member jobId watchedIds) (Set.toList known)
    when alreadyTerminal (writeTVar (endedFlag ended) True)
    modifyTVar' watches (Map.insert next (Watch name ended watchedIds))
    pure ended

unwatchJob :: String -> JobEnded -> IO ()
unwatchJob _ ended = atomically $ modifyTVar' watches (Map.delete (endedWatchId ended))

awaitJobEnded :: JobEnded -> IO ()
awaitJobEnded ended = atomically $ readTVar (endedFlag ended) >>= check

markJobsEnded :: [Int] -> IO ()
markJobsEnded ids = atomically $ do
    current <- readTVar watches
    let (ended, remaining) = Map.partitionWithKey (\watchId _ -> watchId `elem` ids) current
    mapM_ (signal . watchEnded) ended
    writeTVar watches remaining

markJobNameEnded :: [String] -> IO ()
markJobNameEnded names = atomically $ do
    current <- readTVar watches
    let (ended, remaining) = Map.partitionWithKey (\_ watch -> watchName watch `elem` names) current
    mapM_ (signal . watchEnded) ended
    writeTVar watches remaining

markTerminalJobs :: [(String, String)] -> IO ()
markTerminalJobs hints = atomically $ do
    rememberTerminalHints
    current <- readTVar watches
    let (ended, remaining) = Map.partitionWithKey (\_ watch -> watchIsTerminal watch) current
    mapM_ (signal . watchEnded) ended
    writeTVar watches remaining
  where
    watchIsTerminal watch =
        any
            (\(jobId, name) -> name == watchName watch && Set.member jobId (watchJobIds watch))
            hints
    rememberTerminalHints = do
        known <- readTVar terminalHintCache
        let updated = Set.union known (Set.fromList hints)
            bounded = if Set.size updated > terminalHintLimit then Set.deleteMin updated else updated
        writeTVar terminalHintCache bounded

terminalHintLimit :: Int
terminalHintLimit = 4096

watchedJobs :: IO [(Int, String, Set.Set String)]
watchedJobs = Map.elems . Map.map toEntry <$> readTVarIO watches
  where
    toEntry watch = (endedWatchId (watchEnded watch), watchName watch, watchJobIds watch)

signal :: JobEnded -> STM ()
signal ended = writeTVar (endedFlag ended) True
