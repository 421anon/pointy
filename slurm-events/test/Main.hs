{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (intercalate)
import SlurmEvents

main :: IO ()
main = do
    forM_ tests $ \(name, test) -> do
        test
        putStrLn ("ok: " ++ name)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | expected == actual = pure ()
    | otherwise = ioError (userError (label ++ ": expected " ++ show expected ++ ", got " ++ show actual))

assertTrue :: String -> Bool -> IO ()
assertTrue label value = assertEqual label True value

tests :: [(String, IO ())]
tests =
    [ ("account snapshots exclude foreign and native jobs", testAccountIsolation)
    , ("native snapshots exclude cloud jobs", testNativeIsolation)
    , ("publishing marker is visible until terminal", testPublishingMarker)
    , ("terminal event suppresses a completing job", testTerminalSuppression)
    , ("terminal metadata is account-scoped", testTerminalMetadata)
    , ("cancelled-before-start stays terminal without a start", testCompletionBeforeStart)
    , ("requeue clears publishing and terminal state", testRequeue)
    , ("events cannot name another account", testAccountSpoofing)
    , ("repeated events are idempotent", testIdempotence)
    , ("a snapshot started before subscription is not delivered", testSubscriptionRace)
    ]

queueLine :: String -> String -> String -> String -> String -> String -> String -> String
queueLine jobId account name comment state elapsed reason =
    intercalate "|" [jobId, account, name, comment, state, elapsed, reason]

jobEvent :: EventKind -> String -> String -> Event
jobEvent kind jobId name =
    Event
        { eventKind = kind
        , eventAccount = Nothing
        , eventJobId = Just jobId
        , eventJobName = Just name
        , eventState = Nothing
        }

apply :: BrokerState -> Event -> Either String BrokerState
apply broker event = applyEvent 100 event broker

testAccountIsolation :: IO ()
testAccountIsolation = do
    let input =
            unlines
                [ queueLine "101" "mbs" "pc-mbs-own" "own" "RUNNING" "0:05" "None"
                , queueLine "102" "mbs-lab" "pc-mbs-lab-secret" "private" "RUNNING" "0:07" "None"
                , queueLine "103" "mbs" "pointy-nix-build-native" "native" "RUNNING" "0:01" "None"
                ]
        output = filterJobs (WatchAccount "mbs") emptyBrokerState input
    assertEqual
        "only owned rows with stripped names"
        "101|own|own|RUNNING|0:05|None\n"
        output

testNativeIsolation :: IO ()
testNativeIsolation = do
    let input =
            unlines
                [ queueLine "101" "mbs" "pc-mbs-own" "own" "RUNNING" "0:05" "None"
                , queueLine "103" "mbs" "pointy-nix-build-native" "native" "RUNNING" "0:01" "None"
                ]
        output = filterJobs WatchNative emptyBrokerState input
    assertEqual "only native rows" "103|pointy-nix-build-native|native|RUNNING|0:01|None\n" output

testPublishingMarker :: IO ()
testPublishingMarker = do
    let input = queueLine "101" "mbs" "pc-mbs-own" "own" "RUNNING" "0:05" "None"
    broker <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent PublishingEvent "101" "pc-mbs-own"))
    assertEqual
        "publishing state"
        "101|own|own|PUBLISHING|0:05|None\n"
        (filterJobs (WatchAccount "mbs") broker input)

testTerminalSuppression :: IO ()
testTerminalSuppression = do
    let input = queueLine "101" "mbs" "pc-mbs-own" "own" "COMPLETING" "0:05" "None"
    broker <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent TerminalEvent "101" "pc-mbs-own"))
    assertEqual "terminal id suppressed" "" (filterJobs (WatchAccount "mbs") broker input)

testTerminalMetadata :: IO ()
testTerminalMetadata = do
    let ownedEvent = (jobEvent TerminalEvent "101" "pc-mbs-own") {eventAccount = Just "mbs"}
        foreignEvent = (jobEvent TerminalEvent "202" "pc-other-secret") {eventAccount = Just "other"}
    broker <- either (ioError . userError) pure (apply emptyBrokerState ownedEvent)
    foreignTerminal <- either (ioError . userError) pure (apply broker foreignEvent)
    assertEqual
        "owned terminal metadata"
        [("101", "own")]
        (filterTerminalJobs (WatchAccount "mbs") foreignTerminal)
    assertEqual
        "foreign terminal metadata hidden"
        [("202", "secret")]
        (filterTerminalJobs (WatchAccount "other") foreignTerminal)

testCompletionBeforeStart :: IO ()
testCompletionBeforeStart = do
    let input = queueLine "101" "mbs" "pc-mbs-own" "own" "RUNNING" "0:05" "None"
    terminal <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent TerminalEvent "101" "pc-mbs-own"))
    started <- either (ioError . userError) pure (apply terminal (jobEvent StartEvent "101" "pc-mbs-own"))
    assertEqual "missing watched job is terminal" "" (filterJobs (WatchAccount "mbs") started input)
    let publishAgain = apply started (jobEvent PublishingEvent "101" "pc-mbs-own")
    assertEqual "publishing cannot revive a terminal job" (Right started) publishAgain

testRequeue :: IO ()
testRequeue = do
    let input = queueLine "101" "mbs" "pc-mbs-own" "own" "PENDING" "0:00" "None"
        requeue = (jobEvent TerminalEvent "101" "pc-mbs-own") {eventState = Just "REQUEUED"}
    publishing <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent PublishingEvent "101" "pc-mbs-own"))
    requeued <- either (ioError . userError) pure (apply publishing requeue)
    assertEqual "requeued job is active again" "101|own|own|PENDING|0:00|None\n" (filterJobs (WatchAccount "mbs") requeued input)

testAccountSpoofing :: IO ()
testAccountSpoofing = do
    let spoofed = (jobEvent TerminalEvent "101" "pc-mbs-own") {eventAccount = Just "other"}
    assertTrue "foreign account rejected" (isLeft (apply emptyBrokerState spoofed))
    let invalid = jobEvent TerminalEvent "101" "pc-own"
    assertTrue "malformed scoped name rejected" (isLeft (apply emptyBrokerState invalid))

testIdempotence :: IO ()
testIdempotence = do
    let terminal = jobEvent TerminalEvent "101" "pc-mbs-own"
    once <- either (ioError . userError) pure (apply emptyBrokerState terminal)
    twice <- either (ioError . userError) pure (apply once terminal)
    assertEqual "terminal event applied twice" once twice
    let publishing = jobEvent PublishingEvent "101" "pc-mbs-own"
    published <- either (ioError . userError) pure (apply emptyBrokerState publishing)
    publishedAgain <- either (ioError . userError) pure (apply published publishing)
    assertEqual "publishing event applied twice" published publishedAgain

testSubscriptionRace :: IO ()
testSubscriptionRace = do
    let (first, running) = beginSnapshotWindow emptySnapshotWindow
        required = snapshotGeneration running + 1
        during = markSnapshotDirty running
        (second, fresh) = beginSnapshotWindow during
    assertEqual "first generation" 1 first
    assertEqual "subscriber needs a later generation" 2 required
    assertTrue "dirty snapshot is not ready" (not (snapshotReady during required))
    assertEqual "second generation" 2 second
    assertTrue "clean later snapshot is ready" (snapshotReady fresh required)
