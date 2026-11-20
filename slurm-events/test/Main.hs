{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Either (isLeft)
import Data.List (intercalate)
import SlurmBroker (JobScope (..), jobScopeFromCgroup, parseWatchArguments)
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
    , ("saving marker is visible until terminal", testSavingMarker)
    , ("terminal event suppresses a completing job", testTerminalSuppression)
    , ("terminal metadata is account-scoped", testTerminalMetadata)
    , ("cancelled-before-start stays terminal without a start", testCompletionBeforeStart)
    , ("requeue clears saving and terminal state", testRequeue)
    , ("account metadata must match the scope", testAccountSpoofing)
    , ("repeated events are idempotent", testIdempotence)
    , ("a snapshot started before subscription is not delivered", testSubscriptionRace)
    , ("systemd job cgroups yield the Slurm job scope", testSluidScope)
    , ("legacy job cgroups yield the numeric job scope", testLegacyJobScope)
    , ("non-job cgroups have no job scope", testForeignCgroupScope)
    , ("account watches use the configured prefix only", testWatchPrefixConfiguration)
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
                [ queueLine "101" "mbs" "site-mbs-own" "own" "RUNNING" "0:05" "None"
                , queueLine "102" "mbs-lab" "site-mbs-lab-secret" "private" "RUNNING" "0:07" "None"
                , queueLine "103" "mbs" "pointy-nix-build-native" "native" "RUNNING" "0:01" "None"
                ]
        output = filterJobs (WatchScoped "site-" "mbs") emptyBrokerState input
    assertEqual
        "only owned rows with stripped names"
        "101|own|own|RUNNING|0:05|None\n"
        output

testNativeIsolation :: IO ()
testNativeIsolation = do
    let input =
            unlines
                [ queueLine "101" "mbs" "site-mbs-own" "own" "RUNNING" "0:05" "None"
                , queueLine "103" "mbs" "pointy-nix-build-native" "native" "RUNNING" "0:01" "None"
                ]
        output = filterJobs WatchNative emptyBrokerState input
    assertEqual "only native rows" "103|pointy-nix-build-native|native|RUNNING|0:01|None\n" output

testSavingMarker :: IO ()
testSavingMarker = do
    let input = queueLine "101" "mbs" "site-mbs-own" "own" "RUNNING" "0:05" "None"
    broker <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent SavingEvent "101" "site-mbs-own"))
    assertEqual
        "saving state"
        "101|own|own|SAVING|0:05|None\n"
        (filterJobs (WatchScoped "site-" "mbs") broker input)

testTerminalSuppression :: IO ()
testTerminalSuppression = do
    let input = queueLine "101" "mbs" "site-mbs-own" "own" "COMPLETING" "0:05" "None"
    broker <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent TerminalEvent "101" "site-mbs-own"))
    assertEqual "terminal id suppressed" "" (filterJobs (WatchScoped "site-" "mbs") broker input)

testTerminalMetadata :: IO ()
testTerminalMetadata = do
    let ownedEvent = (jobEvent TerminalEvent "101" "site-mbs-own") {eventAccount = Just "mbs", eventState = Just "OUT_OF_MEMORY"}
        foreignEvent = (jobEvent TerminalEvent "202" "site-other-secret") {eventAccount = Just "other", eventState = Just "FAILED\nforged"}
    broker <- either (ioError . userError) pure (apply emptyBrokerState ownedEvent)
    foreignTerminal <- either (ioError . userError) pure (apply broker foreignEvent)
    assertEqual
        "owned terminal metadata carries the job state"
        [("101", "own", Just "OUT_OF_MEMORY")]
        (visibleTerminal (filterTerminalJobs (WatchScoped "site-" "mbs") foreignTerminal))
    assertEqual
        "foreign terminal metadata hidden and a malformed state dropped"
        [("202", "secret", Nothing)]
        (visibleTerminal (filterTerminalJobs (WatchScoped "site-" "other") foreignTerminal))
  where
    visibleTerminal = map (\record -> (terminalJobId record, terminalJobName record, terminalState record))

testCompletionBeforeStart :: IO ()
testCompletionBeforeStart = do
    let input = queueLine "101" "mbs" "site-mbs-own" "own" "RUNNING" "0:05" "None"
    terminal <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent TerminalEvent "101" "site-mbs-own"))
    started <- either (ioError . userError) pure (apply terminal (jobEvent StartEvent "101" "site-mbs-own"))
    assertEqual "missing watched job is terminal" "" (filterJobs (WatchScoped "site-" "mbs") started input)
    let saveAgain = apply started (jobEvent SavingEvent "101" "site-mbs-own")
    assertEqual "saving cannot revive a terminal job" (Right started) saveAgain

testRequeue :: IO ()
testRequeue = do
    let input = queueLine "101" "mbs" "site-mbs-own" "own" "PENDING" "0:00" "None"
        requeue = (jobEvent TerminalEvent "101" "site-mbs-own") {eventState = Just "REQUEUED"}
    saving <- either (ioError . userError) pure (apply emptyBrokerState (jobEvent SavingEvent "101" "site-mbs-own"))
    requeued <- either (ioError . userError) pure (apply saving requeue)
    assertEqual "requeued job is active again" "101|own|own|PENDING|0:00|None\n" (filterJobs (WatchScoped "site-" "mbs") requeued input)

testAccountSpoofing :: IO ()
testAccountSpoofing = do
    let mislabeled = (jobEvent TerminalEvent "101" "site-mbs-own") {eventAccount = Just "trotter-lab"}
    broker <- either (ioError . userError) pure (apply emptyBrokerState mislabeled)
    assertEqual
        "scope requires a matching account"
        []
        (filterTerminalJobs (WatchScoped "site-" "mbs") broker)
    let invalid = (jobEvent TerminalEvent "101" "site-mbs-own") {eventAccount = Just "Bad Name"}
    assertTrue "invalid account rejected" (isLeft (apply emptyBrokerState invalid))

testIdempotence :: IO ()
testIdempotence = do
    let terminal = jobEvent TerminalEvent "101" "site-mbs-own"
    once <- either (ioError . userError) pure (apply emptyBrokerState terminal)
    twice <- either (ioError . userError) pure (apply once terminal)
    assertEqual "terminal event applied twice" once twice
    let saving = jobEvent SavingEvent "101" "site-mbs-own"
    published <- either (ioError . userError) pure (apply emptyBrokerState saving)
    publishedAgain <- either (ioError . userError) pure (apply published saving)
    assertEqual "saving event applied twice" published publishedAgain

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

testSluidScope :: IO ()
testSluidScope =
    assertEqual
        "systemd Slurm job scope"
        (Just (SluidScope "sF61M7MS1MK300"))
        (jobScopeFromCgroup "0::/system.slice/slurmstepd.scope/sF61M7MS1MK300/step_batch/user/task_0\n")

testLegacyJobScope :: IO ()
testLegacyJobScope =
    assertEqual
        "legacy cgroup job directory"
        (Just (JobIdScope "17056"))
        (jobScopeFromCgroup "0::/slurm/uid_1001/job_17056/step_batch\n")

testForeignCgroupScope :: IO ()
testForeignCgroupScope = do
    assertEqual
        "service cgroup"
        Nothing
        (jobScopeFromCgroup "0::/system.slice/nix-daemon.service\n")
    assertEqual
        "unrelated scope-like component"
        Nothing
        (jobScopeFromCgroup "0::/system.slice/sF61M7MS1MK300/step_batch\n")

testWatchPrefixConfiguration :: IO ()
testWatchPrefixConfiguration = do
    assertEqual
        "native watch needs no prefix"
        (Right WatchNative)
        (parseWatchArguments Nothing ["native"])
    assertEqual
        "account watch needs a configured prefix"
        (Left "account watches are not configured")
        (parseWatchArguments Nothing ["account", "mbs"])
    assertEqual
        "account watch uses the configured prefix"
        (Right (WatchScoped "site-" "mbs"))
        (parseWatchArguments (Just "site-") ["account", "mbs"])
