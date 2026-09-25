{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.Scratch (
    ScratchEntry (..),
    ScratchListing (..),
    ScratchRootResponse (..),
    ScratchWrapRequest (..),
    scratchListHandler,
    scratchRootHandler,
    scratchWrapHandler,
) where

import Control.Exception (IOException, try)
import Control.Monad (forM, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effects (AppM)
import Handlers.StepReview (requireStepUnreviewed)
import IngestJobs (IngestRequest (..), startIngestJob)
import Servant (ServerError (..), err400, err403, err404, err409, errBody, throwError)
import Storage (scratchDirectory)
import System.Directory (canonicalizePath, doesDirectoryExist, getFileSize, listDirectory)
import System.FilePath (addTrailingPathSeparator, isAbsolute, makeRelative, (</>))

data ScratchRootResponse = ScratchRootResponse
    { scratchRootPath :: Maybe FilePath
    }

instance ToJSON ScratchRootResponse where
    toJSON (ScratchRootResponse path) = object ["root" .= path]

data ScratchEntry = ScratchEntry
    { scratchEntryName :: Text
    , scratchEntryDirectory :: Bool
    , scratchEntrySize :: Maybe Integer
    }

instance ToJSON ScratchEntry where
    toJSON entry =
        object
            [ "name" .= scratchEntryName entry
            , "directory" .= scratchEntryDirectory entry
            , "size" .= scratchEntrySize entry
            ]

data ScratchListing = ScratchListing
    { scratchListingPath :: Text
    , scratchListingEntries :: [ScratchEntry]
    }

instance ToJSON ScratchListing where
    toJSON listing = object ["path" .= scratchListingPath listing, "entries" .= scratchListingEntries listing]

newtype ScratchWrapRequest = ScratchWrapRequest
    { scratchWrapPath :: FilePath
    }

instance FromJSON ScratchWrapRequest where
    parseJSON = withObject "ScratchWrapRequest" $ \object_ -> ScratchWrapRequest <$> object_ .: "path"

scratchRootHandler :: AppM ScratchRootResponse
scratchRootHandler = do
    configured <- liftIO scratchDirectory
    case configured of
        Nothing -> pure (ScratchRootResponse Nothing)
        Just path -> ScratchRootResponse . Just <$> liftIO (canonicalise path)

scratchListHandler :: Maybe FilePath -> AppM ScratchListing
scratchListHandler mPath = do
    root <- requireScratchRoot
    let requested = fromMaybe "" mPath
    target <-
        if null requested
            then requireDirectory root
            else resolveScratchPath root requested
    names <- ioOr (listDirectory target) err403{errBody = "Scratch directory is not readable"}
    entries <- liftIO $ forM names (entryFor target)
    pure $
        ScratchListing
            { scratchListingPath = relativeScratchPath root target
            , scratchListingEntries = sortOn (\entry -> (not (scratchEntryDirectory entry), scratchEntryName entry)) entries
            }

scratchWrapHandler :: Int -> ScratchWrapRequest -> AppM Text
scratchWrapHandler stepId request = do
    root <- requireScratchRoot
    let requested = scratchWrapPath request
    when (null requested) $ throwError err400{errBody = "Path must not be empty"}
    target <- resolveScratchPath root requested
    when (target == root) $ throwError err400{errBody = "The scratch root itself cannot be wrapped"}
    requireStepUnreviewed stepId
    let relative = T.unpack (relativeScratchPath root target)
    started <-
        liftIO $
            startIngestJob
                IngestRequest
                    { ingestRequestStepId = stepId
                    , ingestRequestDirectory = target
                    , ingestRequestPath = Just (T.pack relative)
                    , ingestRequestMessage = "Wrap scratch directory " ++ relative ++ " for step " ++ show stepId
                    , ingestRequestStaging = Nothing
                    }
    case started of
        Nothing -> throwError err409{errBody = "An ingest job is already running for this step."}
        Just _ -> pure "Ingest job started"

requireScratchRoot :: AppM FilePath
requireScratchRoot = do
    configured <- liftIO scratchDirectory
    case configured of
        Nothing -> throwError err404{errBody = "Scratch directory is not configured"}
        Just path -> liftIO (canonicalise path)

resolveScratchPath :: FilePath -> FilePath -> AppM FilePath
resolveScratchPath root requested = do
    when (isAbsolute requested) $ throwError err400{errBody = "Path must be relative to the scratch root"}
    canonicalTarget <- ioOr (canonicalizePath (root </> requested)) err400{errBody = "Invalid scratch path"}
    unless (insideRoot root canonicalTarget) $ throwError err400{errBody = "Path escapes the scratch root"}
    requireDirectory canonicalTarget

requireDirectory :: FilePath -> AppM FilePath
requireDirectory path = do
    isDirectory <- liftIO $ doesDirectoryExist path
    unless isDirectory $ throwError err400{errBody = "Not a directory"}
    pure path

insideRoot :: FilePath -> FilePath -> Bool
insideRoot root target = target == root || addTrailingPathSeparator root `isPrefixOf` target

canonicalise :: FilePath -> IO FilePath
canonicalise path = either (const path) id <$> try @IOException (canonicalizePath path)

relativeScratchPath :: FilePath -> FilePath -> Text
relativeScratchPath root target
    | root == target = ""
    | otherwise = T.pack (makeRelative root target)

entryFor :: FilePath -> FilePath -> IO ScratchEntry
entryFor target name = do
    isDirectory <- doesDirectoryExist path
    size <- if isDirectory then pure Nothing else either (const Nothing) Just <$> try @IOException (getFileSize path)
    pure
        ScratchEntry
            { scratchEntryName = T.pack name
            , scratchEntryDirectory = isDirectory
            , scratchEntrySize = size
            }
  where
    path = target </> name

ioOr :: IO a -> ServerError -> AppM a
ioOr action failure = either (const (throwError failure)) pure =<< liftIO (try @IOException action)
