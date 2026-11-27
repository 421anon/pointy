{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BuildLog (LogAccess (..), ResolvedLog (..), buildStepStore, rawStatusesBatched, resolveBuildLog, resolveStatusesBatched)
import BuildRunner (BuildKey (..), buildKeyForOutPath)
import BuildStatus (StepPaths (..), checkStatus, resolveStepStatus)
import Certificates (rawStatusesFor)
import Control.Concurrent.STM (atomically, check, modifyTVar', newTVarIO, readTVar, registerDelay, writeTVar)
import Control.Monad (unless)
import Data.Aeson (Value (..), encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Fixture.Document (FixtureDocument (..))
import Interpreters.Fixture (FixtureJob (..), FixtureState (..), newFixtureState, runFixture)
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects (LogStamp (..), Nix (..))
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName)
import System.IO (hClose, hPutStr, openTempFile)

legacyPath :: Text -> StepPaths
legacyPath path = StepPaths (T.unpack path) (T.unpack path)

main :: IO ()
main = do
    state <- newFixtureState document
    (certified, unbuilt, invalid) <- runFixture state $ do
        certified_ <- checkStatus (legacyPath certifiedCertificate)
        unbuilt_ <- checkStatus (legacyPath unbuiltCertificate)
        invalid_ <- checkStatus (legacyPath "/invalid")
        pure (certified_, unbuilt_, invalid_)
    assertEqual "certified certificate is success" ("success", Nothing) certified
    assertEqual "unbuilt certificate is not-started" ("not-started", Nothing) unbuilt
    assertEqual "unevaluated certificate is not-started" ("not-started", Nothing) invalid

    addJob state (buildKeyForOutPath (T.unpack unbuiltCertificate))
    running <- runFixture state (checkStatus (legacyPath unbuiltCertificate))
    assertEqual "certificate build in flight is running" ("running", Nothing) running

    store <- runFixture state (buildStepStore certificates Map.empty)
    statuses <-
        runFixture state $
            rawStatusesBatched store (\certificate -> certificate == unbuiltCertificate) certificates
    assertEqual
        "batched statuses classify certificates"
        ( Map.fromList
            [ (172, ("success", Nothing))
            , (173, ("running", Nothing))
            , (174, ("not-started", Nothing))
            , (175, ("not-started", Nothing))
            ]
        )
        statuses

    tempDir <- maybe "/tmp" id <$> lookupEnv "TMPDIR"
    (logPath, logHandle) <- openTempFile tempDir "pointy-certificate-status.log"
    hPutStr logHandle recordedLog
    hClose logHandle
    refreshState <- newFixtureState (refreshDocument logPath)
    (refreshStatuses, refreshStore) <- runFixture refreshState (rawStatusesFor (Map.map legacyPath refreshCertificates))
    assertEqual
        "refresh probe reports raw statuses"
        ( Map.fromList
            [ (176, ("not-started", Nothing))
            , (177, ("not-started", Nothing))
            , (178, ("success", Nothing))
            ]
        )
        refreshStatuses
    assertEqual
        "refresh probe leaves the recorded log unresolved"
        (Just ("not-started", Nothing))
        (Map.lookup 176 refreshStatuses)
    batchedResolved <- runFixture refreshState (resolveStatusesBatched refreshStore refreshStatuses)
    assertEqual
        "batched statuses resolve the recorded log"
        ( Map.fromList
            [ (176, ("failure", Just (T.pack recordedLog)))
            , (177, ("not-started", Nothing))
            , (178, ("success", Nothing))
            ]
        )
        batchedResolved
    resolved <-
        runFixture refreshState $
            resolveStepStatus (Just (T.unpack loggedCertificate)) (176, ("not-started", Nothing))
    assertEqual
        "per-step status resolves the recorded log"
        (176, ("failure", Just (T.pack recordedLog)))
        resolved
    mapM_ checkContentAddressedGraph [False, True]
    checkConcurrentLevelProbes
    checkInputLogAccess

checkInputLogAccess :: IO ()
checkInputLogAccess = do
    remote <- resolveRecordingLogs RemoteLogs
    assertEqual
        "remote search fetches the input log through substituters"
        (Just (failedDrv, failedLog), [["log", rootDrv], ["log", failedDrv]])
        remote
    local <- resolveRecordingLogs LocalLogs
    assertEqual
        "local search reads the input log offline without substituters"
        (Just (failedDrv, failedLog), [["--option", "substituters", "", "--offline", "log", failedDrv]])
        local
  where
    rootDrv = "/nix/store/00000000000000000000000000000000-root.drv"
    failedDrv = "/nix/store/11111111111111111111111111111111-failed-input.drv"
    failedLog = "fixture input failed" :: BC.ByteString
    inputsOf drv
        | drv == rootDrv = [failedDrv]
        | otherwise = []
    derivations paths =
        LBS.toStrict . encode $
            Map.fromList
                [ (drv, object ["outputs" .= object ["out" .= object ["hash" .= ("sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" :: Text), "method" .= ("nar" :: Text)]], "inputDrvs" .= object [Key.fromString input .= (["out"] :: [Text]) | input <- inputsOf drv]])
                | drv <- paths
                ]
    resolveRecordingLogs access = do
        logInvocations <- newIORef []
        resolved <-
            runEff $ interpret (\_ -> \case
                RunNixCli ("derivation" : "show" : paths) ->
                    pure (ExitSuccess, derivations paths, "")
                RunNixCli args | (drv : "log" : _) <- reverse args -> do
                    liftIO $ modifyIORef' logInvocations (++ [args])
                    pure $ if drv == failedDrv then (ExitSuccess, failedLog, "") else (ExitFailure 1, "", "")
                RunNixStoreCli ("--realise" : "--dry-run" : paths) ->
                    pure (ExitSuccess, "", BC.pack (unlines (("these " ++ show (length paths) ++ " derivations will be built:") : map ("  " ++) paths)))
                LocalLogStamp drv ->
                    pure (if drv == failedDrv then Just (LogStamp (posixSecondsToUTCTime 0) (fromIntegral (BC.length failedLog))) else Nothing)
                _ -> liftIO (fail "input log access test: unexpected Nix operation")
                )
                (resolveBuildLog access rootDrv)
        recorded <- readIORef logInvocations
        pure (fmap (\result -> (resolvedDrv result, resolvedLog result)) resolved, recorded)

checkConcurrentLevelProbes :: IO ()
checkConcurrentLevelProbes = do
    started <- newTVarIO (0 :: Int)
    expired <- registerDelay 5000000
    stalled <- newTVarIO False
    let resolveGraph :: Eff '[Nix, IOE] (Maybe ResolvedLog) -> IO (Maybe ResolvedLog)
        resolveGraph =
            runEff . interpret (\_ -> \case
                RunNixCli ("derivation" : "show" : paths) ->
                    pure (ExitSuccess, derivations paths, "")
                RunNixCli ["log", drv]
                    | drv == rootDrv ->
                        pure (ExitFailure 1, "", "")
                    | otherwise -> do
                        liftIO $ atomically $ modifyTVar' started (+1)
                        arrived <- liftIO $ atomically $ do
                            probed <- readTVar started
                            outOfTime <- readTVar expired
                            check (probed >= levelSize || outOfTime)
                            pure (probed >= levelSize)
                        liftIO $ unless arrived $ atomically $ writeTVar stalled True
                        pure $ case Map.lookup drv siblingLogs of
                            Just logText -> (ExitSuccess, logText, "")
                            Nothing -> (ExitFailure 1, "", "")
                RunNixStoreCli ("--realise" : "--dry-run" : paths) ->
                    pure (ExitSuccess, "", BC.pack (unlines (("these " ++ show (length paths) ++ " derivations will be built:") : map ("  " ++) paths)))
                _ -> liftIO (fail "concurrent level probe test: unexpected Nix operation")
                )
    resolved <- resolveGraph (resolveBuildLog RemoteLogs rootDrv)
    stalledProbes <- atomically $ readTVar stalled
    assertEqual "search probes a level concurrently" False stalledProbes
    assertEqual
        "the first logged input in level order wins"
        (Just (failedDrv, "fixture sibling two failed", []))
        (fmap (\result -> (resolvedDrv result, resolvedLog result, resolvedVia result)) resolved)
  where
    levelSize = 4 :: Int
    rootDrv = "/nix/store/00000000000000000000000000000000-root.drv"
    siblingDrvs =
        [ "/nix/store/1111111111111111111111111111111" ++ [name] ++ "-sibling-" ++ [name] ++ ".drv"
        | name <- "abcd"
        ]
    failedDrv = siblingDrvs !! 1
    siblingLogs =
        Map.fromList
            [ (siblingDrvs !! 1, "fixture sibling two failed")
            , (siblingDrvs !! 2, "fixture sibling three failed")
            ] :: Map.Map FilePath BC.ByteString
    derivations paths =
        LBS.toStrict . encode $
            Map.fromList
                [ (drv, object ["outputs" .= object ["out" .= object ["hash" .= ("sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" :: Text), "method" .= ("nar" :: Text)]], "inputDrvs" .= object [Key.fromString input .= (["out"] :: [Text]) | input <- inputsOf drv]])
                | drv <- paths
                ]
    inputsOf drv
        | drv == rootDrv = siblingDrvs
        | otherwise = []

checkContentAddressedGraph :: Bool -> IO ()
checkContentAddressedGraph version4 = do
    resolved <- runGraphFixture (resolveBuildLog RemoteLogs rootDrv)
    assertEqual
        ("failure log remains reachable through unknown CA outputs, version4=" ++ show version4)
        (Just (failedDrv, "fixture dependency failed", [caDrv]))
        (fmap (\result -> (resolvedDrv result, resolvedLog result, resolvedVia result)) resolved)
  where
    rootDrv = "/nix/store/00000000000000000000000000000000-root.drv"
    caDrv = "/nix/store/11111111111111111111111111111111-content-addressed.drv"
    failedDrv = "/nix/store/22222222222222222222222222222222-failed-dependency.drv"
    rootOutput = "/nix/store/33333333333333333333333333333333-root"
    unknownOutput = object ["hash" .= ("sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" :: Text), "method" .= ("nar" :: Text)]
    storeName path = if version4 then takeFileName path else path
    inputs drv
        | drv == rootDrv = [caDrv]
        | drv == caDrv = [failedDrv]
        | otherwise = []
    node drv =
        let outputs =
                if drv == rootDrv
                    then object ["out" .= object ["path" .= storeName rootOutput], "floating" .= unknownOutput]
                    else object ["out" .= unknownOutput]
            dependencies = object [Key.fromString (storeName input) .= (["out"] :: [Text]) | input <- inputs drv]
         in if version4
                then object ["outputs" .= outputs, "inputs" .= object ["drvs" .= dependencies]]
                else object ["outputs" .= outputs, "inputDrvs" .= dependencies]
    derivations paths =
        let entries = object [Key.fromString (storeName drv) .= node drv | drv <- paths]
         in LBS.toStrict . encode $
                if version4 then object ["version" .= (4 :: Int), "derivations" .= entries] else entries
    runGraphFixture :: Eff '[Nix, IOE] a -> IO a
    runGraphFixture = runEff . interpret (\_ -> \case
        RunNixCli ("derivation" : "show" : paths) ->
            pure (ExitSuccess, derivations paths, "")
        RunNixCli ["log", drv] ->
            pure $ if drv == failedDrv then (ExitSuccess, "fixture dependency failed", "") else (ExitFailure 1, "", "")
        RunNixStoreCli ("--realise" : "--dry-run" : paths) ->
            pure (ExitSuccess, "", BC.pack (unlines (("these " ++ show (length paths) ++ " derivations will be built:") : map ("  " ++) paths)))
        _ -> liftIO (fail "unexpected Nix operation while resolving the derivation graph"))

certificates :: Map.Map Int Text
certificates =
    Map.fromList
        [ (172, certifiedCertificate)
        , (173, unbuiltCertificate)
        , (174, otherUnbuiltCertificate)
        , (175, "/invalid")
        ]

certifiedCertificate :: Text
certifiedCertificate = "/nix/store/00000000000000000000000000000000-pointy-certificate-172"

unbuiltCertificate :: Text
unbuiltCertificate = "/nix/store/11111111111111111111111111111111-pointy-certificate-173"

otherUnbuiltCertificate :: Text
otherUnbuiltCertificate = "/nix/store/22222222222222222222222222222222-pointy-certificate-174"

loggedCertificate :: Text
loggedCertificate = "/nix/store/33333333333333333333333333333333-pointy-certificate-176"

unloggedCertificate :: Text
unloggedCertificate = "/nix/store/44444444444444444444444444444444-pointy-certificate-177"

loggedCertificateDrv :: FilePath
loggedCertificateDrv = "/nix/store/55555555555555555555555555555555-pointy-certificate-176.drv"

unloggedCertificateDrv :: FilePath
unloggedCertificateDrv = "/nix/store/66666666666666666666666666666666-pointy-certificate-177.drv"

builtCertificate :: Text
builtCertificate = "/nix/store/77777777777777777777777777777777-pointy-certificate-178"

builtCertificateDrv :: FilePath
builtCertificateDrv = "/nix/store/88888888888888888888888888888888-pointy-certificate-178.drv"

refreshCertificates :: Map.Map Int Text
refreshCertificates =
    Map.fromList
        [ (176, loggedCertificate)
        , (177, unloggedCertificate)
        , (178, builtCertificate)
        ]

recordedLog :: String
recordedLog = "error: fixture build failed"

document :: FixtureDocument
document =
    FixtureDocument
        { documentBranch = "main"
        , documentAgent = Nothing
        , documentColdStorage = Nothing
        , documentJson = Map.empty
        , documentRaw = Map.empty
        , documentPresets = Null
        , documentOutPaths = Map.empty
        , documentCertificates = Map.empty
        , documentExtrasOutPaths = Map.empty
        , documentNotices = Map.empty
        , documentReviews = Map.empty
        , documentProjectStepIds = Map.empty
        , documentValidPaths = [T.unpack certifiedCertificate]
        , documentDerivations = Map.empty
        , documentLogs = Map.empty
        , documentReferences = Map.empty
        , documentOutputs = Map.empty
        , documentNarSizes = Map.empty
        , documentPriorOutPaths = Map.empty
        , documentPriorCertificates = Map.empty
        }

refreshDocument :: FilePath -> FixtureDocument
refreshDocument logPath =
    document
        { documentValidPaths = [T.unpack builtCertificate]
        , documentDerivations =
            Map.fromList
                [ (T.unpack loggedCertificate, loggedCertificateDrv)
                , (T.unpack unloggedCertificate, unloggedCertificateDrv)
                , (T.unpack builtCertificate, builtCertificateDrv)
                ]
        , documentLogs = Map.fromList [(loggedCertificateDrv, logPath)]
        }

addJob :: FixtureState -> BuildKey -> IO ()
addJob state (BuildKey name) =
    atomically $
        modifyTVar'
            (fixtureJobs state)
            ( Map.insert
                name
                FixtureJob{jobId = "1", jobName = name, jobComment = "", jobState = "RUNNING"}
            )

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (actual == expected) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
