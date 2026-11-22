{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildLog (buildStepStore, rawStatusesBatched, validPaths)
import BuildStatus (StepPaths (..), checkStatus)
import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Aeson (eitherDecode)
import Fixture.Document (FixtureDocument (..))
import Data.Either (isLeft)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Interpreters.Fixture (newFixtureState, runFixture)
import Handlers.Storage (HeadStep (..), pathOwners)
import StorageBus (StorageCapacity (..), StorageRevision (..), parseStorageCapacity, revisionMovable)
import StorageHistory (CachedStep (..), ColdSummary (..), HistoryCommit (..), RevisionSeed (..), dependentsOf, parseHistory, revisionsFromHistory, summarizeCold)

main :: IO ()
main = do
    checkHistoryParsing
    checkRevisions
    checkDependents
    checkCold
    checkRevisionMovable
    checkCapacity
    checkCertificationRequiresOutput
    checkValidityBatchSkipsNonStorePaths
    checkSharedOutputOwners

checkHistoryParsing :: IO ()
checkHistoryParsing = do
    assertEqual
        "commits are newest first and carry their files"
        ( [ ("c" <> T.replicate 39 "c", time 300, ["steps/2.nix"])
          , ("b" <> T.replicate 39 "b", time 200, ["steps/1.nix", "steps/2.nix"])
          , ("a" <> T.replicate 39 "a", time 100, ["steps/1.nix"])
          ]
        )
        (parseHistory sampleLog)

checkRevisions :: IO ()
checkRevisions = do
    assertEqual
        "one row per output path, dated at the commit that first produced it, even when it reappears"
        [ RevisionSeed 1 "First" "derived" (commit 1) (time 1) "/nix/store/a"
        , RevisionSeed 1 "First" "derived" (commit 4) (time 4) "/nix/store/b"
        , RevisionSeed 2 "Extract" "download" (commit 1) (time 1) "/nix/store/x"
        , RevisionSeed 2 "Second" "upload" (commit 2) (time 2) "/nix/store/y"
        ]
        (revisionsFromHistory sampleHistory)
    assertEqual
        "a missing step starts a fresh revision when it comes back with another output"
        [RevisionSeed 1 "N" "upload" (commit 1) (time 1) "/nix/store/a", RevisionSeed 1 "N" "derived" (commit 3) (time 3) "/nix/store/b"]
        (revisionsFromHistory [atCommit 3 [(1, "N", "derived", Just "/nix/store/b")], atCommit 2 [(1, "N", "derived", Nothing)], atCommit 1 [(1, "N", "upload", Just "/nix/store/a")]])

checkDependents :: IO ()
checkDependents = do
    assertEqual "a step's readers are transitive" (Set.fromList [2, 3]) (dependentsOf graph 1)
    assertEqual "a leaf has no readers" Set.empty (dependentsOf graph 3)
    assertEqual "a cycle terminates" (Set.fromList [1, 2]) (dependentsOf (Map.fromList [(1, Set.singleton 2), (2, Set.singleton 1)]) 1)
  where
    graph :: Map Int (Set Int)
    graph = Map.fromList [(1, Set.empty), (2, Set.singleton 1), (3, Set.singleton 2)]

checkCold :: IO ()
checkCold = do
    assertEqual "unknown status keeps the output local and unmovable" (ColdSummary "unknown" 1000 0 0 0 (Just "Cold storage status is unavailable.")) (summarizeCold 1000 Nothing)
    assertEqual "an untracked output is local" (ColdSummary "local" 1000 0 0 0 Nothing) (summarizeCold 1000 (Just (ColdStatus [] Nothing Nothing)))
    assertEqual "an offloading output reports its progress" (ColdSummary "offloading" 100 900 400 900 Nothing) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 (Offloading 400 900)] Nothing Nothing)))
    assertEqual "an onloading output counts its returned bytes as local" (ColdSummary "onloading" 500 500 400 900 Nothing) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 (Onloading 400 900)] Nothing Nothing)))
    assertEqual "a failed onload leaves the file cold and names the error" (ColdSummary "failed" 100 900 0 0 (Just "box down")) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 (OnloadFailed "box down")] Nothing Nothing)))
    assertEqual "an offloaded output is cold and uses no local bytes" (ColdSummary "cold" 0 900 0 0 Nothing) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 Offloaded] Nothing Nothing)))
    assertEqual
        "a failed onload keeps the file's bytes cold beside an offloaded one"
        (ColdSummary "failed" 200 800 0 0 (Just "box down"))
        (summarizeCold 1000 (Just (ColdStatus [file "a.bin" 300 Offloaded, file "b.bin" 500 (OnloadFailed "box down")] Nothing Nothing)))
  where
    file path size state = ColdFile path size state

checkRevisionMovable :: IO ()
checkRevisionMovable = do
    assertEqual "a local revision is movable" True (revisionMovable (revision 1 "Name" 1 10 0 "local" Nothing))
    assertEqual "a failed revision is movable" True (revisionMovable (revision 1 "Name" 1 10 0 "failed" Nothing))
    assertEqual "an offloading revision is not movable" False (revisionMovable (revision 1 "Name" 1 10 0 "offloading" Nothing))
    assertEqual "a cold revision is not movable" False (revisionMovable (revision 1 "Name" 1 0 10 "cold" Nothing))
  where
    revision stepId name seconds local cold state blocked =
        StorageRevision
            { revisionStep = stepId
            , revisionName = name
            , revisionCurrentName = Just name
            , revisionKind = "derived"
            , revisionCommit = commit seconds
            , revisionCommittedAt = time seconds
            , revisionLocal = local
            , revisionCold = cold
            , revisionState = state
            , revisionDone = 0
            , revisionTotal = 0
            , revisionError = Nothing
            , revisionBlocked = blocked
            }

checkCapacity :: IO ()
checkCapacity = do
    assertEqual
        "a quota usage is rejected so Pointy can use step-output bytes"
        True
        (isLeft (parseStorageCapacity (LBS.pack "{\"kind\":\"quota\",\"totalBytes\":1000,\"usedBytes\":400}")))
    assertEqual
        "available bytes derive used bytes and a filesystem defaults to a disk label"
        (Right (StorageCapacity "filesystem" "disk" 1000 250 750))
        (parseStorageCapacity (LBS.pack "{\"totalBytes\":1000,\"availableBytes\":750,\"kind\":\"filesystem\"}"))
    assertEqual
        "a provider label names the capacity"
        (Right (StorageCapacity "quota" "account quota" 1000 0 1000))
        (parseStorageCapacity (LBS.pack "{\"kind\":\"quota\",\"totalBytes\":1000,\"label\":\"account quota\"}"))
    assertEqual
        "available bytes clamp below zero"
        (Right (StorageCapacity "filesystem" "disk" 1000 1000 0))
        (parseStorageCapacity (LBS.pack "{\"kind\":\"filesystem\",\"totalBytes\":1000,\"availableBytes\":-5}"))
    assertEqual "both byte counts are rejected" True (isLeft (parseStorageCapacity (LBS.pack "{\"kind\":\"filesystem\",\"totalBytes\":1000,\"usedBytes\":400,\"availableBytes\":600}")))
    assertEqual
        "a quota may report only its total"
        (Right (StorageCapacity "quota" "quota" 1000 0 1000))
        (parseStorageCapacity (LBS.pack "{\"kind\":\"quota\",\"totalBytes\":1000}"))
    assertEqual "a filesystem still needs a byte count" True (isLeft (parseStorageCapacity (LBS.pack "{\"kind\":\"filesystem\",\"totalBytes\":1000}")))
    assertEqual "a missing kind is rejected" True (isLeft (parseStorageCapacity (LBS.pack "{\"totalBytes\":1000,\"usedBytes\":400}")))
    assertEqual "a non-positive total is rejected" True (isLeft (parseStorageCapacity (LBS.pack "{\"kind\":\"quota\",\"totalBytes\":0,\"usedBytes\":0}")))
    assertEqual "an unknown kind is rejected" True (isLeft (parseStorageCapacity (LBS.pack "{\"kind\":\"other\",\"totalBytes\":1000,\"usedBytes\":400}")))

checkCertificationRequiresOutput :: IO ()
checkCertificationRequiresOutput = do
    document <- either fail pure (eitherDecode "{\"valid\":[\"/nix/store/cert\"]}" :: Either String FixtureDocument)
    state <- newFixtureState document
    single <- runFixture state (checkStatus (StepPaths "/nix/store/cert" "/nix/store/output"))
    assertEqual "a certificate does not make a missing output successful" ("not-started", Nothing) single
    batched <- runFixture state $ do
        store <- buildStepStore (Map.singleton 1 "cert") (Map.singleton 1 "/nix/store/output")
        rawStatusesBatched store (const False) (Map.singleton 1 "cert")
    assertEqual "batched status also checks output presence" (Map.singleton 1 ("not-started", Nothing)) batched

checkValidityBatchSkipsNonStorePaths :: IO ()
checkValidityBatchSkipsNonStorePaths = do
    document <- either fail pure (eitherDecode "{\"valid\":[\"/nix/store/present\"]}" :: Either String FixtureDocument)
    state <- newFixtureState document
    validity <- runFixture state (validPaths ["/nix/store/present", "/invalid"])
    assertEqual "a non-store path does not poison the validity batch" (Just (Set.singleton "/nix/store/present")) validity

checkSharedOutputOwners :: IO ()
checkSharedOutputOwners = do
    let owners = Map.fromList [(1, HeadStep "one" Set.empty "/nix/store/shared" "/nix/store/cert1"), (2, HeadStep "two" Set.empty "/nix/store/other" "/nix/store/cert2"), (3, HeadStep "three" Set.empty "/nix/store/shared" "/nix/store/cert3")]
    assertEqual "all owners of a shared output are protected" [1, 3] (pathOwners owners "/nix/store/shared")

sampleLog :: String
sampleLog =
    unlines
        [ "cccccccccccccccccccccccccccccccccccccccc 300"
        , ""
        , "steps/2.nix"
        , ""
        , "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb 200"
        , ""
        , "steps/1.nix"
        , "steps/2.nix"
        , ""
        , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 100"
        , ""
        , "steps/1.nix"
        ]

sampleHistory :: [HistoryCommit]
sampleHistory =
    [ atCommit 4 [(1, "First", "derived", Just "/nix/store/b"), (2, "Second", "upload", Just "/nix/store/y")]
    , atCommit 3 [(1, "First", "derived", Just "/nix/store/a")]
    , atCommit 2 [(1, "First", "derived", Just "/nix/store/a"), (2, "Second", "upload", Just "/nix/store/y")]
    , atCommit 1 [(1, "First", "derived", Just "/nix/store/a"), (2, "Extract", "download", Just "/nix/store/x")]
    ]

atCommit :: Integer -> [(Int, Text, Text, Maybe Text)] -> HistoryCommit
atCommit seconds steps =
    HistoryCommit
        (commit seconds)
        (time seconds)
        (Map.fromList [(stepId, CachedStep name path kind) | (stepId, name, kind, path) <- steps])

commit :: Integer -> Text
commit seconds = T.pack (take 40 (cycle (show seconds)))

time :: Integer -> UTCTime
time = posixSecondsToUTCTime . fromIntegral

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
