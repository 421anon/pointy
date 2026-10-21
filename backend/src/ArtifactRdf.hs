{-# LANGUAGE OverloadedStrings #-}

module ArtifactRdf (
    ArtifactRef (..),
    ArtifactFile (..),
    parseArtifactSegments,
    parseArtifactFile,
) where

import Data.Char (isAlphaNum, isAscii, isControl, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import Network.URI (escapeURIString)
import Text.Read (readMaybe)

data ArtifactRef = ArtifactRef
    { refProjectPath :: [Int]
    , refStepId :: Int
    , refCommit :: Text
    }
    deriving (Eq, Show)

data ArtifactFile = ArtifactFile
    { filePath :: Text
    , fileQuery :: Text
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

parseArtifactFile :: Text -> Maybe ArtifactFile
parseArtifactFile path
    | all isCanonicalSegment (T.splitOn "/" path) = Just (ArtifactFile path (encodeURIComponent path))
    | otherwise = Nothing
  where
    isCanonicalSegment segment = segment `notElem` ["", ".", ".."] && not (T.any isControl segment)

-- Matches the frontend's Url.percentEncode, which is JavaScript's encodeURIComponent: https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
encodeURIComponent :: Text -> Text
encodeURIComponent = T.pack . escapeURIString isUnreserved . T.unpack
  where
    isUnreserved c = isAscii c && (isAlphaNum c || c `elem` ("-_.!~*'()" :: String))
