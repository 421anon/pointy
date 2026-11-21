{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import StorageBus (StorageRevision (..), revisionMovable)
import StorageHistory (CachedStep (..), ColdSummary (..), HistoryCommit (..), RevisionSeed (..), dependentsOf, parseHistory, revisionsFromHistory, summarizeCold)

main :: IO ()
main = do
    checkHistoryParsing
    checkRevisions
    checkDependents
    checkCold
    checkRevisionMovable

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
    assertEqual "an untracked output is local" (ColdSummary "local" 1000 0 0 0 Nothing) (summarizeCold 1000 (Just (ColdStatus [])))
    assertEqual "an offloading output reports its progress" (ColdSummary "offloading" 100 900 400 900 Nothing) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 (Offloading 400 900)])))
    assertEqual "an onloading output still counts as cold and reports its progress" (ColdSummary "onloading" 100 900 400 900 Nothing) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 (Onloading 400 900)])))
    assertEqual "a failed onload leaves the file cold and names the error" (ColdSummary "failed" 100 900 0 0 (Just "box down")) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 (OnloadFailed "box down")])))
    assertEqual "an offloaded output is cold and uses no local bytes" (ColdSummary "cold" 0 900 0 0 Nothing) (summarizeCold 1000 (Just (ColdStatus [file "big.bin" 900 Offloaded])))
    assertEqual
        "a failed onload keeps the file's bytes cold beside an offloaded one"
        (ColdSummary "failed" 200 800 0 0 (Just "box down"))
        (summarizeCold 1000 (Just (ColdStatus [file "a.bin" 300 Offloaded, file "b.bin" 500 (OnloadFailed "box down")])))
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
