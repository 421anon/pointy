{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ColdRefusal (applyColdRefusals, clearColdRefusals, clearColdRefusalsForPath, recordColdRefusal)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..), parseColdStatus)
import Handlers.StepInputs (coldInputReaders, runReads)

main :: IO ()
main = do
    assertEqual
        "each provider state"
        (Just (ColdStatus [ColdFile "a.csv" 3 Offloaded, ColdFile "b.png" 10 (Onloading 5 10), ColdFile "c.bam" 7 (OnloadFailed "box unreachable")] Nothing Nothing))
        ( parseColdStatus
            "{\"files\":[{\"path\":\"a.csv\",\"size\":3,\"state\":\"offloaded\"},{\"path\":\"b.png\",\"size\":10,\"state\":\"onloading\",\"done\":5,\"total\":10},{\"path\":\"c.bam\",\"size\":7,\"state\":\"failed\",\"error\":\"box unreachable\"}]}"
        )
    assertEqual "a status without a size is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"a.csv\",\"state\":\"offloaded\"}]}")
    assertEqual "a local or untracked output" (Just (ColdStatus [] Nothing Nothing)) (parseColdStatus "{\"files\":[]}")
    assertEqual
        "a provider reports its cold capacity"
        (Just (ColdStatus [] (Just 5497558138880) (Just 3839679791104)))
        (parseColdStatus "{\"files\":[],\"capacityBytes\":5497558138880,\"availableBytes\":3839679791104}")
    assertEqual
        "an offloading file carries its progress"
        (Just (ColdStatus [ColdFile "a.csv" 3 (Offloading 1 3)] Nothing Nothing))
        (parseColdStatus "{\"files\":[{\"path\":\"a.csv\",\"size\":3,\"state\":\"offloading\",\"done\":1,\"total\":3}]}")
    assertEqual "onloading without progress is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"b.png\",\"state\":\"onloading\"}]}")
    assertEqual "a failure without its error is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"c.bam\",\"state\":\"failed\"}]}")
    assertEqual "an unknown state is not a status" Nothing (parseColdStatus "{\"files\":[{\"path\":\"a.csv\",\"state\":\"frozen\"}]}")
    assertEqual "output that is not JSON" Nothing (parseColdStatus "not json")
    checkColdInputReaders
    checkColdRefusals

checkColdInputReaders :: IO ()
checkColdInputReaders = do
    assertEqual
        "every unbuilt step names its own built inputs with cold files, even behind an unbuilt step that also reads some"
        (Map.fromList [(3, [2]), (4, [1])])
        (coldInputReaders (runReads graph (`elem` [1, 2]) (`elem` [1, 2])) (`elem` [1, 2]))
    assertEqual
        "cold files in an input that is not built do not count, since the run rebuilds it"
        Map.empty
        (coldInputReaders (runReads graph (const False) (const False)) (const True))
    assertEqual
        "a built step reads nothing, even when its inputs hold cold files"
        Map.empty
        (coldInputReaders (runReads graph (const True) (const True)) (const True))
    assertEqual
        "an uncertified step whose output is built reads its own output, not its inputs"
        (Map.fromList [(3, [3]), (4, [1])])
        (coldInputReaders (runReads graph (`elem` [1, 2]) (`elem` [1, 2, 3])) (`elem` [1, 2, 3]))
    assertEqual
        "cold inputs do not block certifying an already built output"
        Map.empty
        (coldInputReaders (runReads graph (`elem` [1, 2]) (`elem` [1, 2, 3])) (`elem` [2]))
  where
    graph = Map.fromList [(1, []), (2, []), (3, [2]), (4, [1, 3])]

checkColdRefusals :: IO ()
checkColdRefusals = do
    recordColdRefusal 7 "refusal-commit" (Set.singleton "/nix/store/out7") "cold files"
    recordColdRefusal 8 "refusal-commit" (Set.fromList ["/nix/store/out8", "/nix/store/out9"]) "cold files"
    retained <- applyColdRefusals "refusal-commit" statuses
    assertEqual
        "a retained refusal replaces an unstarted or failed step but not another status"
        (Map.fromList [(7, ("inputs-cold", Just "cold files")), (8, ("inputs-cold", Just "cold files")), (9, ("success", Nothing))])
        retained
    clearColdRefusalsForPath "/nix/store/out7"
    partiallyCleared <- applyColdRefusals "refusal-commit" statuses
    assertEqual
        "a refusal clears when all of its cold paths are local"
        (Map.fromList [(7, ("not-started", Nothing)), (8, ("inputs-cold", Just "cold files")), (9, ("success", Nothing))])
        partiallyCleared
    clearColdRefusalsForPath "/nix/store/out8"
    stillRefused <- applyColdRefusals "refusal-commit" statuses
    assertEqual
        "a refusal stays while another of its cold paths is still cold"
        (Map.fromList [(7, ("not-started", Nothing)), (8, ("inputs-cold", Just "cold files")), (9, ("success", Nothing))])
        stillRefused
    clearColdRefusalsForPath "/nix/store/out9"
    fullyCleared <- applyColdRefusals "refusal-commit" statuses
    assertEqual "clearing every cold path leaves statuses unchanged" statuses fullyCleared
    recordColdRefusal 7 "refusal-commit" (Set.singleton "/nix/store/out7") "cold files"
    clearColdRefusals "refusal-commit" [7]
    cleared <- applyColdRefusals "refusal-commit" statuses
    assertEqual "clearing a refusal by commit leaves statuses unchanged" statuses cleared
  where
    statuses = Map.fromList [(7, ("not-started", Nothing)), (8, ("failure", Just "old build")), (9, ("success", Nothing))]

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual
    | actual == expected = pure ()
    | otherwise = fail $ label ++ ": expected " ++ show expected ++ ", got " ++ show actual
