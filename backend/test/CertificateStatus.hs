{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildLog (buildStepStore, rawStatusesBatched)
import BuildRunner (BuildKey (..), buildKeyForOutPath)
import BuildStatus (checkStatus, resolveStepStatus)
import Certificates (rawStatusesFor)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (unless)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Fixture.Document (FixtureDocument (..))
import Interpreters.Fixture (FixtureJob (..), FixtureState (..), newFixtureState, runFixture)
import System.Environment (lookupEnv)
import System.IO (hClose, hPutStr, openTempFile)

main :: IO ()
main = do
    state <- newFixtureState document
    (certified, unbuilt, invalid) <- runFixture state $ do
        certified_ <- checkStatus (T.unpack certifiedCertificate)
        unbuilt_ <- checkStatus (T.unpack unbuiltCertificate)
        invalid_ <- checkStatus "/invalid"
        pure (certified_, unbuilt_, invalid_)
    assertEqual "certified certificate is success" ("success", Nothing) certified
    assertEqual "unbuilt certificate is not-started" ("not-started", Nothing) unbuilt
    assertEqual "unevaluated certificate is not-started" ("not-started", Nothing) invalid

    addJob state (buildKeyForOutPath (T.unpack unbuiltCertificate))
    running <- runFixture state (checkStatus (T.unpack unbuiltCertificate))
    assertEqual "certificate build in flight is running" ("running", Nothing) running

    store <- runFixture state (buildStepStore certificates)
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
    (refreshStatuses, _) <- runFixture refreshState (rawStatusesFor refreshCertificates)
    assertEqual
        "refresh probe reports raw statuses"
        ( Map.fromList
            [ (176, ("not-started", Nothing))
            , (177, ("not-started", Nothing))
            ]
        )
        refreshStatuses
    assertEqual
        "refresh probe leaves the recorded log unresolved"
        (Just ("not-started", Nothing))
        (Map.lookup 176 refreshStatuses)
    resolved <-
        runFixture refreshState $
            resolveStepStatus (Just (T.unpack loggedCertificate)) (176, ("not-started", Nothing))
    assertEqual
        "per-step status resolves the recorded log"
        (176, ("failure", Just (T.pack recordedLog)))
        resolved

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

refreshCertificates :: Map.Map Int Text
refreshCertificates =
    Map.fromList
        [ (176, loggedCertificate)
        , (177, unloggedCertificate)
        ]

recordedLog :: String
recordedLog = "error: fixture build failed"

document :: FixtureDocument
document =
    FixtureDocument
        { documentBranch = "main"
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
        }

refreshDocument :: FilePath -> FixtureDocument
refreshDocument logPath =
    document
        { documentDerivations =
            Map.fromList
                [ (T.unpack loggedCertificate, loggedCertificateDrv)
                , (T.unpack unloggedCertificate, unloggedCertificateDrv)
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
