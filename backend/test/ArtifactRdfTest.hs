{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ArtifactRdf (ArtifactRef (..), parseArtifactSegments)
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

main :: IO ()
main = segmentTests

commit :: T.Text
commit = "fc87dfdf0123456789abcdef0123456789abcdef"

segmentTests :: IO ()
segmentTests = do
    assertEqual "a permalink names its project path, step and commit" (Just (ArtifactRef [113, 131, 134] 3294 commit)) (parseArtifactSegments ["113", "131", "134", "3294", commit])
    assertEqual "a permalink outside any folder has an empty project path" (Just (ArtifactRef [] 3294 commit)) (parseArtifactSegments ["3294", commit])
    assertEqual "a branch name is not an immutable commit" Nothing (parseArtifactSegments ["113", "3294", "main"])
    assertEqual "an abbreviated commit is rejected" Nothing (parseArtifactSegments ["3294", T.take 12 commit])
    assertEqual "an uppercase commit is rejected" Nothing (parseArtifactSegments ["3294", T.toUpper commit])
    assertEqual "a non-numeric step id is rejected" Nothing (parseArtifactSegments ["113", "x3294", commit])
    assertEqual "a trailing slash is rejected" Nothing (parseArtifactSegments ["113", "3294", commit, ""])

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expectedValue actual =
    unless (expectedValue == actual) $ do
        TIO.putStrLn (T.pack ("FAIL: " ++ label))
        TIO.putStrLn (T.pack ("expected: " ++ show expectedValue))
        TIO.putStrLn (T.pack ("actual:   " ++ show actual))
        exitFailure
