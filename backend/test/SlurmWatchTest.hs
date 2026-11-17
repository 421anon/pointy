{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildRunner (JobId (..), SlurmJob (..), isRunningState)
import Control.Monad (unless)
import JobWatch (awaitJobEnded, markJobsEnded, markTerminalJobs, unwatchJob, watchJob, watchedJobs)
import SlurmWatch (TerminalJob (..), WatchEvent (..), parseWatchEvent, snapshotJobs)
import System.Timeout (timeout)

main :: IO ()
main = do
    assertEqual
        "a snapshot frame carries the job, cluster and terminal transcripts"
        ( Right
            ( WatchSnapshot
                "42|pointy-nix-build-x|comment|RUNNING|0:10|None\n"
                (Just "pointy*|up|node1|idle\n")
                [TerminalJob "41" "pointy-nix-build-x"]
            )
        )
        ( parseWatchEvent
            "{\"type\":\"snapshot\",\"jobs\":\"42|pointy-nix-build-x|comment|RUNNING|0:10|None\\n\",\"cluster\":\"pointy*|up|node1|idle\\n\",\"terminalJobs\":[{\"id\":\"41\",\"name\":\"pointy-nix-build-x\"}]}"
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
