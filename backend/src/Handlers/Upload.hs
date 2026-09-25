{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Upload (uploadHandler) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import Effects (AppM)
import Handlers.StepReview (requireStepUnreviewed)
import IngestJobs (IngestRequest (..), discardDirectory, startIngestJob)
import Servant (err400, err409, err500, errBody, throwError)
import Servant.Multipart (FileData (fdFileName, fdPayload), MultipartData (files), Tmp)
import Storage (uploadStagingRoot)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (createTempDirectory)

uploadHandler :: Int -> MultipartData Tmp -> AppM Text
uploadHandler stepId multipartData = do
    let uploadedFiles = files multipartData
    when (null uploadedFiles) $ throwError err400{errBody = "No files found"}
    requireStepUnreviewed stepId

    root <- liftIO uploadStagingRoot
    staging <- liftIO $ createTempDirectory root ("step-" ++ show stepId ++ "-")
    let storeRefDir = staging </> "store-ref"
    staged <- liftIO $ try @IOException $ do
        createDirectoryIfMissing True storeRefDir
        forM_ uploadedFiles $ \file -> renameFile (fdPayload file) (storeRefDir </> takeFileName (T.unpack (fdFileName file)))
    case staged of
        Left err -> do
            liftIO $ discardDirectory staging
            throwError err500{errBody = TLE.encodeUtf8 (TL.pack ("Failed to stage upload: " ++ show err))}
        Right () -> do
            started <-
                liftIO $
                    startIngestJob
                        IngestRequest
                            { ingestRequestStepId = stepId
                            , ingestRequestDirectory = storeRefDir
                            , ingestRequestPath = Nothing
                            , ingestRequestMessage = "Upload files for step " ++ show stepId
                            , ingestRequestStaging = Just staging
                            }
            case started of
                Nothing -> do
                    liftIO $ discardDirectory staging
                    throwError err409{errBody = "An ingest job is already running for this step."}
                Just _ -> pure "Ingest job started"
