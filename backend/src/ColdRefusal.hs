{-# LANGUAGE OverloadedStrings #-}

module ColdRefusal (
    applyColdRefusals,
    clearColdRefusals,
    recordColdRefusal,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE coldRefusals #-}
coldRefusals :: MVar (Map.Map Int (Text, Text))
coldRefusals = unsafePerformIO (newMVar Map.empty)

recordColdRefusal :: Int -> Text -> Text -> IO ()
recordColdRefusal stepId commit message =
    modifyMVar_ coldRefusals $ \refusals ->
        pure $ Map.insert stepId (commit, message) refusals

clearColdRefusals :: Text -> [Int] -> IO ()
clearColdRefusals commit stepIds =
    modifyMVar_ coldRefusals $ \refusals ->
        pure $ foldr (Map.update (dropAt commit)) refusals stepIds
  where
    dropAt c entry@(c', _)
        | c == c' = Nothing
        | otherwise = Just entry

applyColdRefusals :: Text -> Map.Map Int (Text, Maybe Text) -> IO (Map.Map Int (Text, Maybe Text))
applyColdRefusals commit statuses = do
    refusals <- readMVar coldRefusals
    pure $ Map.mapWithKey (retained refusals) statuses
  where
    retained refusals stepId entry@(state, _)
        | state `notElem` ["not-started", "failure"] = entry
        | otherwise = case Map.lookup stepId refusals of
            Just (refusalCommit, message)
                | refusalCommit == commit -> ("inputs-cold", Just message)
            _ -> entry
