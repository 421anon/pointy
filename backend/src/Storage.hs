module Storage (scratchDirectory, uploadStagingRoot) where

import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.IO.Temp (getCanonicalTemporaryDirectory)

scratchDirectory :: IO (Maybe FilePath)
scratchDirectory = lookupEnv "POINTY_SCRATCH_DIR"

uploadStagingRoot :: IO FilePath
uploadStagingRoot = do
    configured <- lookupEnv "POINTY_UPLOAD_DIR"
    case configured of
        Nothing -> getCanonicalTemporaryDirectory
        Just directory -> do
            createDirectoryIfMissing True directory
            pure directory
