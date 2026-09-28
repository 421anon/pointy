{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildLog (buildStepStore, rawStatusesBatched)
import BuildRunner (BuildKey (..), buildKeyForOutPath)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (unless)
import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Fixture.Document (FixtureDocument (..))
import BuildStatus (StepPaths (..), checkStatus, markBuiltOutputs)
import Interpreters.Fixture (FixtureJob (..), FixtureState (..), newFixtureState, runFixture)

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

    marked <- runFixture state (markBuiltOutputs stepPaths unmarkedStatuses)
    assertEqual
        "built outputs mark uncertified steps"
        ( Map.fromList
            [ (176, ("built-not-certified", Nothing))
            , (177, ("certification-failed", Just "msg"))
            , (178, ("not-started", Nothing))
            , (179, ("not-started", Nothing))
            , (180, ("not-started", Nothing))
            , (181, ("not-started", Nothing))
            , (182, ("success", Nothing))
            , (183, ("running", Nothing))
            ]
        )
        marked

certificates :: Map.Map Int Text
certificates =
    Map.fromList
        [ (172, certifiedCertificate)
        , (173, unbuiltCertificate)
        , (174, otherUnbuiltCertificate)
        , (175, "/invalid")
        ]

stepPaths :: Map.Map Int StepPaths
stepPaths =
    Map.fromList
        [ (176, StepPaths (T.unpack otherUnbuiltCertificate) builtOutput)
        , (177, StepPaths (T.unpack otherUnbuiltCertificate) builtOutput)
        , (178, StepPaths (T.unpack otherUnbuiltCertificate) unbuiltOutput)
        , (179, StepPaths builtOutput builtOutput)
        , (180, StepPaths "/invalid" "/invalid")
        , (181, StepPaths "/invalid" builtOutput)
        , (182, StepPaths (T.unpack certifiedCertificate) builtOutput)
        , (183, StepPaths (T.unpack unbuiltCertificate) builtOutput)
        ]

unmarkedStatuses :: Map.Map Int (Text, Maybe Text)
unmarkedStatuses =
    Map.fromList
        [ (176, ("not-started", Nothing))
        , (177, ("failure", Just "msg"))
        , (178, ("not-started", Nothing))
        , (179, ("not-started", Nothing))
        , (180, ("not-started", Nothing))
        , (181, ("not-started", Nothing))
        , (182, ("success", Nothing))
        , (183, ("running", Nothing))
        ]

certifiedCertificate :: Text
certifiedCertificate = "/nix/store/00000000000000000000000000000000-pointy-certificate-172"

unbuiltCertificate :: Text
unbuiltCertificate = "/nix/store/11111111111111111111111111111111-pointy-certificate-173"

otherUnbuiltCertificate :: Text
otherUnbuiltCertificate = "/nix/store/22222222222222222222222222222222-pointy-certificate-174"

builtOutput :: FilePath
builtOutput = "/nix/store/33333333333333333333333333333333-pointy-output-176"

unbuiltOutput :: FilePath
unbuiltOutput = "/nix/store/44444444444444444444444444444444-pointy-output-178"

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
        , documentValidPaths = [T.unpack certifiedCertificate, builtOutput]
        , documentDerivations = Map.empty
        , documentLogs = Map.empty
        , documentReferences = Map.empty
        , documentOutputs = Map.empty
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
