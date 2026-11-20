{-# LANGUAGE OverloadedStrings #-}

module JobOutcome (
    JobOutcome (..),
    TerminalState (..),
    jobOutcome,
    outcomeFromSlurmState,
    outcomeMessage,
    recordSubmittedJob,
    recordTerminalStates,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data JobOutcome = OutOfMemory | TimeLimitReached | NodeFailed
    deriving (Eq, Show)

data TerminalState = TerminalState
    { terminalStateJobId :: String
    , terminalStateJobName :: String
    , terminalStateName :: String
    }
    deriving (Eq, Show)

data LatestJob = LatestJob
    { latestJobId :: Integer
    , latestJobState :: Maybe String
    }

outcomeFromSlurmState :: String -> Maybe JobOutcome
outcomeFromSlurmState "OUT_OF_MEMORY" = Just OutOfMemory
outcomeFromSlurmState "TIMEOUT" = Just TimeLimitReached
outcomeFromSlurmState "NODE_FAIL" = Just NodeFailed
outcomeFromSlurmState _ = Nothing

outcomeMessage :: JobOutcome -> Text
outcomeMessage OutOfMemory = "Ran out of memory: the build used more than the step's RAM requirement"
outcomeMessage TimeLimitReached = "Stopped at the cluster's time limit"
outcomeMessage NodeFailed = "The compute node failed during the build"

{-# NOINLINE latestJobs #-}
latestJobs :: TVar (Map.Map String LatestJob)
latestJobs = unsafePerformIO $ newTVarIO Map.empty

recordSubmittedJob :: String -> String -> IO ()
recordSubmittedJob jobName jobId = remember [(jobName, jobId, Nothing)]

recordTerminalStates :: [TerminalState] -> IO ()
recordTerminalStates states =
    remember [(terminalStateJobName state, terminalStateJobId state, Just (terminalStateName state)) | state <- states]

remember :: [(String, String, Maybe String)] -> IO ()
remember jobs =
    atomically $ modifyTVar' latestJobs $ \known ->
        bounded (foldl' insert known jobs)
  where
    insert known (jobName, jobId, state) = case numericJobId jobId of
        Just number -> Map.insertWith later jobName (LatestJob number state) known
        Nothing -> known
    later candidate existing = case compare (latestJobId candidate) (latestJobId existing) of
        GT -> candidate
        LT -> existing
        EQ -> existing {latestJobState = latestJobState candidate <|> latestJobState existing}
    bounded known
        | Map.size known > latestJobLimit =
            Map.fromList (take latestJobLimit (sortOn (Down . latestJobId . snd) (Map.toList known)))
        | otherwise = known

jobOutcome :: String -> IO (Maybe JobOutcome)
jobOutcome jobName = do
    known <- readTVarIO latestJobs
    pure (outcomeFromSlurmState =<< latestJobState =<< Map.lookup jobName known)

numericJobId :: String -> Maybe Integer
numericJobId jobId
    | not (null jobId) && all isDigit jobId = Just (read jobId)
    | otherwise = Nothing

latestJobLimit :: Int
latestJobLimit = 4096
