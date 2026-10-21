{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ArtifactRdf (ArtifactFile (..), ArtifactRef (..), parseArtifactFile, parseArtifactSegments)
import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)

main :: IO ()
main = do
    segmentTests
    fileTests

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

fileTests :: IO ()
fileTests = do
    assertEqual "a file path is encoded like the frontend's permalink query" (Just "out%2Fplot%201%C3%A9.png") (fileQuery <$> parseArtifactFile "out/plot 1é.png")
    assertEqual "query delimiters inside a file name are encoded" (Just "a%26b%3Dc%3F%23d") (fileQuery <$> parseArtifactFile "a&b=c?#d")
    assertEqual "characters encodeURIComponent keeps stay literal" (Just "it's_(1)!~*-.txt") (fileQuery <$> parseArtifactFile "it's_(1)!~*-.txt")
    assertEqual "a parent segment is not a canonical file name" Nothing (parseArtifactFile "out/../secret")
    assertEqual "a leading slash is not a canonical file name" Nothing (parseArtifactFile "/out/plot.png")
    assertEqual "a trailing slash is not a canonical file name" Nothing (parseArtifactFile "out/")

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expectedValue actual =
    unless (expectedValue == actual) $ do
        TIO.putStrLn (T.pack ("FAIL: " ++ label))
        TIO.putStrLn (T.pack ("expected: " ++ show expectedValue))
        TIO.putStrLn (T.pack ("actual:   " ++ show actual))
        exitFailure
