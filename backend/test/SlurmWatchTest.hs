{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildRunner (JobId (..), SlurmJob (..), isRunningState)
import Control.Monad (unless)
import JobOutcome (JobOutcome (..), TerminalState (..), jobOutcome, recordSubmittedJob, recordTerminalStates)
import JobWatch (awaitJobEnded, markJobsEnded, markTerminalJobs, unwatchJob, watchJob, watchedJobs)
import SlurmWatch (TerminalJob (..), WatchEvent (..), parseWatchEvent, snapshotJobs)
import System.Timeout (timeout)

main :: IO ()
main = do
    assertEqual
        "a snapshot frame carries the job, cluster and terminal transcripts, with or without a terminal state"
        ( Right
            ( WatchSnapshot
                "42|pointy-nix-build-x|comment|RUNNING|0:10|None\n"
                (Just "pointy*|up|node1|idle\n")
                [TerminalJob "41" "pointy-nix-build-x" (Just "OUT_OF_MEMORY"), TerminalJob "40" "pointy-nix-build-w" Nothing]
            )
        )
        ( parseWatchEvent
            "{\"type\":\"snapshot\",\"jobs\":\"42|pointy-nix-build-x|comment|RUNNING|0:10|None\\n\",\"cluster\":\"pointy*|up|node1|idle\\n\",\"terminalJobs\":[{\"id\":\"41\",\"name\":\"pointy-nix-build-x\",\"state\":\"OUT_OF_MEMORY\"},{\"id\":\"40\",\"name\":\"pointy-nix-build-w\"}]}"
        )
    assertEqual
        "a snapshot without terminal jobs is accepted"
        (Right (WatchSnapshot "" Nothing []))
        (parseWatchEvent "{\"type\":\"snapshot\",\"jobs\":\"\"}")
    assertEqual "a heartbeat frame is recognized" (Right WatchHeartbeat) (parseWatchEvent "{\"type\":\"heartbeat\"}")
    assertBool "an unknown frame is rejected" (isLeft (parseWatchEvent "{\"type\":\"other\"}"))
    assertBool "malformed JSON is rejected" (isLeft (parseWatchEvent "not json"))

    let jobs = snapshotJobs "42|pointy-nix-build-x|comment|SAVING|0:10|None\n43|pointy-nix-build-y|(null)|PENDING|0:00|Resources\n"
    assertEqual "snapshot jobs use the existing AllJobs line format" 2 (length jobs)
    assertEqual "a saving state is active" True (any (\job -> slurmJobName job == "pointy-nix-build-x" && isRunningState (slurmJobState job)) jobs)
    assertEqual "job fields survive snapshot parsing" (Just (JobId "43")) (slurmJobId <$> lookupJob "pointy-nix-build-y" jobs)

    first <- watchJob "pointy-nix-build-x" ["41"]
    second <- watchJob "pointy-nix-build-x" ["42"]
    registered <- watchedJobs
    assertEqual "each waiter gets its own watch token" 2 (length registered)
    let firstToken = head [watchId | (watchId, name, _) <- registered, name == "pointy-nix-build-x"]
    markJobsEnded [firstToken]
    firstDone <- timeout 1000000 (awaitJobEnded first)
    assertEqual "marking the registered token wakes its waiter" (Just ()) firstDone
    secondDone <- timeout 100000 (awaitJobEnded second)
    assertEqual "marking one token does not wake a different token" Nothing secondDone
    markTerminalJobs [("42", "pointy-nix-build-x")]
    terminalDone <- timeout 1000000 (awaitJobEnded second)
    assertEqual "a terminal hint ends the waiter bound to that job id" (Just ()) terminalDone

    cached <- watchJob "pointy-nix-build-x" ["42"]
    cachedDone <- timeout 100000 (awaitJobEnded cached)
    assertEqual "a waiter registered after a terminal hint sees it immediately" (Just ()) cachedDone

    third <- watchJob "pointy-nix-build-y" ["43"]
    markTerminalJobs [("44", "pointy-nix-build-y")]
    newerDone <- timeout 100000 (awaitJobEnded third)
    assertEqual "a terminal hint for another job id does not end a newer retry" Nothing newerDone
    unwatchJob "pointy-nix-build-y" third

    recordTerminalStates [TerminalState "51" "pointy-nix-build-oom" "OUT_OF_MEMORY"]
    oomOutcome <- jobOutcome "pointy-nix-build-oom"
    assertEqual "an out-of-memory terminal state is the job's outcome" (Just OutOfMemory) oomOutcome
    recordSubmittedJob "pointy-nix-build-oom" "52"
    recordTerminalStates [TerminalState "51" "pointy-nix-build-oom" "OUT_OF_MEMORY"]
    retriedOutcome <- jobOutcome "pointy-nix-build-oom"
    assertEqual "a resubmitted job hides the previous job's outcome, even when its state is resent" Nothing retriedOutcome
    recordSubmittedJob "pointy-nix-build-late" "61"
    recordTerminalStates [TerminalState "61" "pointy-nix-build-late" "TIMEOUT"]
    recordSubmittedJob "pointy-nix-build-late" "61"
    lateOutcome <- jobOutcome "pointy-nix-build-late"
    assertEqual "a submission recorded after its own terminal state keeps that state" (Just TimeLimitReached) lateOutcome
    recordTerminalStates [TerminalState "71" "pointy-nix-build-failed" "FAILED"]
    failedOutcome <- jobOutcome "pointy-nix-build-failed"
    assertEqual "an ordinary failure has no outcome beyond its build log" Nothing failedOutcome

lookupJob :: String -> [SlurmJob] -> Maybe SlurmJob
lookupJob name = findJob
  where
    findJob [] = Nothing
    findJob (job : rest)
        | slurmJobName job == name = Just job
        | otherwise = findJob rest

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

assertBool :: String -> Bool -> IO ()
assertBool label ok = unless ok (fail label)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
