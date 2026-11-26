{-# LANGUAGE OverloadedStrings #-}

module ColdRefusal (
    applyColdRefusals,
    clearAllColdRefusals,
    clearColdRefusals,
    clearColdRefusalsForPath,
    recordColdRefusal,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

data ColdRefusal = ColdRefusal
    { refusalCommit :: Text
    , refusalMessage :: Text
    , refusalPaths :: Set Text
    }

{-# NOINLINE coldRefusals #-}
coldRefusals :: MVar (Map.Map Int ColdRefusal)
coldRefusals = unsafePerformIO (newMVar Map.empty)

recordColdRefusal :: Int -> Text -> Set Text -> Text -> IO ()
recordColdRefusal stepId commit paths message =
    modifyMVar_ coldRefusals $ \refusals ->
        pure $ Map.insert stepId (ColdRefusal commit message paths) refusals

clearAllColdRefusals :: IO ()
clearAllColdRefusals = modifyMVar_ coldRefusals (const (pure Map.empty))

clearColdRefusals :: Text -> [Int] -> IO ()
clearColdRefusals commit stepIds =
    modifyMVar_ coldRefusals $ \refusals ->
        pure $ foldr (Map.update (dropAt commit)) refusals stepIds
  where
    dropAt c refusal
        | c == refusalCommit refusal = Nothing
        | otherwise = Just refusal

clearColdRefusalsForPath :: Text -> IO [(Int, Text)]
clearColdRefusalsForPath outPath =
    modifyMVar coldRefusals $ \refusals ->
        let assessed = [(stepId, refusal, withoutPath outPath refusal) | (stepId, refusal) <- Map.toList refusals]
            remaining = Map.fromList [(stepId, kept) | (stepId, _, Just kept) <- assessed]
            cleared = [(stepId, refusalCommit refusal) | (stepId, refusal, Nothing) <- assessed]
         in pure (remaining, cleared)
  where
    withoutPath path refusal =
        let remaining = Set.delete path (refusalPaths refusal)
         in if Set.null remaining then Nothing else Just refusal{refusalPaths = remaining}

applyColdRefusals :: Text -> Map.Map Int (Text, Maybe Text) -> IO (Map.Map Int (Text, Maybe Text))
applyColdRefusals commit statuses = do
    refusals <- readMVar coldRefusals
    pure $ Map.mapWithKey (retained refusals) statuses
  where
    retained refusals stepId entry@(state, _)
        | state `notElem` ["not-started", "failure", "built-not-certified", "certification-failed"] = entry
        | otherwise = case Map.lookup stepId refusals of
            Just refusal
                | refusalCommit refusal == commit -> ("inputs-cold", Just (refusalMessage refusal))
            _ -> entry
