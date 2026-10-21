{-# LANGUAGE OverloadedStrings #-}

module ArtifactRdf (
    ArtifactRef (..),
    parseArtifactSegments,
) where

import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

data ArtifactRef = ArtifactRef
    { refProjectPath :: [Int]
    , refStepId :: Int
    , refCommit :: Text
    }
    deriving (Eq, Show)

parseArtifactSegments :: [Text] -> Maybe ArtifactRef
parseArtifactSegments segments = case reverse segments of
    commit : stepSegment : reversedPath
        | isFullCommit commit -> do
            stepId <- readId stepSegment
            projectPath <- mapM readId (reverse reversedPath)
            pure (ArtifactRef projectPath stepId commit)
    _ -> Nothing
  where
    isFullCommit commit = T.length commit == 40 && T.all (\c -> isHexDigit c && c `notElem` ['A' .. 'F']) commit
    readId segment
        | not (T.null segment) && T.all (`elem` ['0' .. '9']) segment = readMaybe (T.unpack segment)
        | otherwise = Nothing
