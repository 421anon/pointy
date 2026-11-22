{-# LANGUAGE OverloadedStrings #-}

module StorageHistory (
    CachedStep (..),
    HistoryCommit (..),
    RevisionSeed (..),
    ColdSummary (..),
    parseHistory,
    revisionsFromHistory,
    dependentsOf,
    summarizeCold,
) where

import ColdProvider (ColdFile (..), ColdFileState (..), ColdStatus (..))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:), (.:?), (.=))
import Data.Char (isDigit, isHexDigit)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Text.Read (readMaybe)

data CachedStep = CachedStep
    { cachedStepName :: Text
    , cachedStepPath :: Maybe Text
    , cachedStepKind :: Text
    }
    deriving (Eq, Show)

instance FromJSON CachedStep where
    parseJSON = withObject "cached step" $ \fields ->
        CachedStep
            <$> fields .:? "name" .!= ""
            <*> fields .:? "path"
            <*> fields .: "kind"

instance ToJSON CachedStep where
    toJSON step = object ["name" .= cachedStepName step, "path" .= cachedStepPath step, "kind" .= cachedStepKind step]

data HistoryCommit = HistoryCommit
    { historyCommit :: Text
    , historyTime :: UTCTime
    , historySteps :: Map Int CachedStep
    }
    deriving (Eq, Show)

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

revisionsFromHistory :: [HistoryCommit] -> [RevisionSeed]
revisionsFromHistory history = sortOn seedOrder (Map.elems seeds)
  where
    (_, seeds) = foldl' addCommit (Map.empty, Map.empty) (reverse history)
    addCommit (current, found) commit = foldl' (addStep commit) (current, found) (Map.toList (historySteps commit))
    addStep commit (current, found) (stepId, cached) = case cachedStepPath cached of
        Nothing -> (Map.delete stepId current, found)
        Just path
            | Map.lookup stepId current == Just path -> (current, found)
            | otherwise ->
                ( Map.insert stepId path current
                , Map.insertWith
                    ( \later earlier ->
                        if T.null (seedName earlier) && not (T.null (seedName later))
                            then earlier{seedName = seedName later, seedKind = seedKind later}
                            else earlier
                    )
                    path
                    (RevisionSeed stepId (cachedStepName cached) (cachedStepKind cached) (historyCommit commit) (historyTime commit) path)
                    found
                )
    seedOrder seed = (seedStep seed, seedCommitTime seed, seedPath seed)

dependentsOf :: Map Int (Set Int) -> Int -> Set Int
dependentsOf dependencies stepId = go Set.empty [stepId]
  where
    inverse = Map.fromListWith Set.union [(dependency, Set.singleton step) | (step, stepDependencies) <- Map.toList dependencies, dependency <- Set.toList stepDependencies]
    go seen [] = seen
    go seen (current : rest) =
        let next = maybe [] (filter (`Set.notMember` seen) . Set.toList) (Map.lookup current inverse)
         in go (foldl' (flip Set.insert) seen next) (next ++ rest)

summarizeCold :: Integer -> Maybe ColdStatus -> ColdSummary
summarizeCold narSize status = case status of
    Nothing -> ColdSummary "unknown" narSize 0 0 0 (Just "Cold storage status is unavailable.")
    Just (ColdStatus files _) ->
        ColdSummary state local coldBytes done total firstError
      where
        offloading = filter isOffloading files
        onloading = filter isOnloading files
        failed = filter isFailed files
        coldBytes = sum (map coldFileSize files)
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
