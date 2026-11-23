{-# LANGUAGE OverloadedStrings #-}

module StorageHistory (
    RevisionSeed (..),
    ColdSummary (..),
    parseHistory,
    revisionsFromHistory,
    directReadersOf,
    summarizeCold,
) where

import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..))
import Data.Char (isDigit, isHexDigit)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Vector.Unboxed as VU
import StepEvaluations (CachedStep (..), StepEvaluations, commitEvaluation, internedStep)
import Text.Read (readMaybe)

data RevisionSeed = RevisionSeed
    { seedStep :: Int
    , seedName :: Text
    , seedKind :: Text
    , seedCommit :: Text
    , seedCommitTime :: UTCTime
    , seedPath :: Text
    }
    deriving (Eq, Show)

data ColdSummary = ColdSummary
    { coldSummaryState :: Text
    , coldSummaryLocal :: Integer
    , coldSummaryCold :: Integer
    , coldSummaryDone :: Integer
    , coldSummaryTotal :: Integer
    , coldSummaryError :: Maybe Text
    }
    deriving (Eq, Show)

parseHistory :: String -> [(Text, UTCTime, [FilePath])]
parseHistory output = reverse (map ordered (finish (foldl' step (Nothing, []) (lines output))))
  where
    ordered (commit, time, files) = (commit, time, reverse files)
    step (current, commits) line = case parseHeader line of
        Just (commit, time) -> (Just (commit, time, []), maybe commits (: commits) current)
        Nothing
            | null line -> (current, commits)
            | otherwise -> (fmap (\(commit, time, files) -> (commit, time, line : files)) current, commits)
    finish (current, commits) = maybe commits (: commits) current
    parseHeader line = case words line of
        [commit, seconds]
            | length commit == 40 && all isHexDigit commit
            , all isDigit seconds
            , Just value <- (readMaybe seconds :: Maybe Integer) ->
                Just (T.pack commit, posixSecondsToUTCTime (fromIntegral value))
        _ -> Nothing

revisionsFromHistory :: StepEvaluations -> [(Text, UTCTime)] -> [RevisionSeed]
revisionsFromHistory evaluations history = sortOn seedOrder (Map.elems seeds)
  where
    (_, _, seeds) = foldl' addCommit (VU.empty, IntMap.empty, Map.empty) (reverse history)
    addCommit state@(previous, current, found) (commit, time) = case commitEvaluation commit evaluations of
        Nothing -> state
        Just entries ->
            let (current', found') = foldl' (addStep commit time) (current, found) (changedSince previous entries)
             in (entries, current', found')
    addStep commit time (current, found) (stepId32, stepRef) =
        let stepId = fromIntegral stepId32
            cached = internedStep evaluations stepRef
         in case cachedStepPath cached of
                Nothing -> (IntMap.delete stepId current, found)
                Just path
                    | IntMap.lookup stepId current == Just path -> (current, found)
                    | otherwise ->
                        ( IntMap.insert stepId path current
                        , Map.insertWith
                            ( \later earlier ->
                                if T.null (seedName earlier) && not (T.null (seedName later))
                                    then earlier{seedName = seedName later, seedKind = seedKind later}
                                    else earlier
                            )
                            path
                            (RevisionSeed stepId (cachedStepName cached) (cachedStepKind cached) commit time path)
                            found
                        )
    seedOrder seed = (seedStep seed, seedCommitTime seed, seedPath seed)

changedSince :: VU.Vector (Int32, Int32) -> VU.Vector (Int32, Int32) -> [(Int32, Int32)]
changedSince previous entries = go 0 0
  where
    go older newer
        | newer >= VU.length entries = []
        | older >= VU.length previous = VU.toList (VU.drop newer entries)
        | otherwise =
            let before = previous VU.! older
                after = entries VU.! newer
             in case compare (fst before) (fst after) of
                    LT -> go (older + 1) newer
                    GT -> after : go older (newer + 1)
                    EQ
                        | before == after -> go (older + 1) (newer + 1)
                        | otherwise -> after : go (older + 1) (newer + 1)

directReadersOf :: Map Int (Set Int) -> Int -> Set Int
directReadersOf dependencies stepId =
    Map.keysSet (Map.filter (Set.member stepId) dependencies)

summarizeCold :: Integer -> Maybe ColdStatus -> ColdSummary
summarizeCold narSize status = case status of
    Nothing -> ColdSummary "unknown" narSize 0 0 0 (Just "Cold storage status is unavailable.")
    Just (ColdStatus files _ _) ->
        ColdSummary state local coldBytes done total firstError
      where
        offloading = filter isOffloading files
        onloading = filter isOnloading files
        failed = filter isFailed files
        coldBytes = sum (map coldBytesFor files)
        moving = if null offloading then onloading else offloading
        done = sum (map progressDone moving)
        total = sum (map progressTotal moving)
        firstError = case failed of
            file : _ -> Just (failureMessage file)
            [] -> Nothing
        state
            | not (null offloading) = "offloading"
            | not (null onloading) = "onloading"
            | not (null failed) = "failed"
            | coldBytes > 0 = "cold"
            | otherwise = "local"
        local
            | state == "cold" = 0
            | otherwise = max 0 (narSize - coldBytes)

coldBytesFor :: ColdFile -> Integer
coldBytesFor file = case coldFileState file of
    Onloading done _ -> max 0 (coldFileSize file - done)
    _ -> coldFileSize file

isOffloading :: ColdFile -> Bool
isOffloading file = case coldFileState file of
    Offloading _ _ -> True
    _ -> False

isOnloading :: ColdFile -> Bool
isOnloading file = case coldFileState file of
    Onloading _ _ -> True
    _ -> False

isFailed :: ColdFile -> Bool
isFailed file = case coldFileState file of
    OnloadFailed _ -> True
    _ -> False

progressDone :: ColdFile -> Integer
progressDone file = case coldFileState file of
    Offloading done _ -> done
    Onloading done _ -> done
    _ -> 0

progressTotal :: ColdFile -> Integer
progressTotal file = case coldFileState file of
    Offloading _ total -> total
    Onloading _ total -> total
    _ -> 0

failureMessage :: ColdFile -> Text
failureMessage file = case coldFileState file of
    OnloadFailed message -> message
    _ -> ""
