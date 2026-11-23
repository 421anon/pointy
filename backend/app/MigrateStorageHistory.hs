{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Aeson (FromJSON (..), eitherDecodeStrict', withObject, (.!=), (.:), (.:?))
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import StepEvaluations (CachedStep (..), emptyStepEvaluations, recordEvaluation, stepEvaluationsPath, writeStepEvaluations)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

newtype JsonStep = JsonStep CachedStep

instance FromJSON JsonStep where
    parseJSON = withObject "cached step" $ \fields ->
        fmap JsonStep $
            CachedStep
                <$> fields .:? "name" .!= ""
                <*> fields .:? "path"
                <*> fields .: "kind"

newtype JsonEvaluations = JsonEvaluations (Map Text (Map Text JsonStep))

instance FromJSON JsonEvaluations where
    parseJSON = withObject "storage evaluations" $ \fields -> JsonEvaluations <$> fields .: "commits"

main :: IO ()
main = do
    arguments <- getArgs
    case arguments of
        [jsonCache] -> migrate jsonCache
        _ -> die "usage: pointy-migrate-storage-history <evaluations json cache>"

migrate :: FilePath -> IO ()
migrate jsonCache = do
    target <- stepEvaluationsPath
    exists <- doesFileExist target
    when exists $ die (target ++ " already exists; the storage history is already migrated.")
    JsonEvaluations commits <- either (\err -> die ("Cannot decode " ++ jsonCache ++ ": " ++ err)) pure . eitherDecodeStrict' =<< BS.readFile jsonCache
    let (_, records) = foldl' record (emptyStepEvaluations, []) (Map.toList commits)
        record (evaluations, written) (commit, steps) =
            let (updated, added) = recordEvaluation commit (numbered steps) evaluations
             in (updated, reverse added ++ written)
    path <- writeStepEvaluations (reverse records)
    putStrLn ("Wrote " ++ show (Map.size commits) ++ " evaluated commits to " ++ path ++ ".")

numbered :: Map Text JsonStep -> Map Int CachedStep
numbered steps = Map.fromList (mapMaybe (\(key, JsonStep step) -> (\stepId -> (stepId, step)) <$> readMaybe (T.unpack key)) (Map.toList steps))
