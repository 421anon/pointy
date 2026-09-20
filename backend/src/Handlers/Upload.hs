{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Upload (uploadHandler) where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (bracket)
import Effects (AppM, Eval, addFixed)
import Handlers.StepReview (ensureStepUnreviewed, requireStepUnreviewed)
import OutPaths (withWriteRepoTransaction)
import Servant (err400, err409, errBody, throwError)
import Servant.Multipart (FileData (fdFileName, fdPayload), MultipartData (files), Tmp)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, renameFile)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import UserRepo (WriteRepoContext (..), commitAndPushChanges, runNix, runNixEvalImpureJsonExpr)

import Handlers.Projects (jsonToNix)

uploadHandler :: Int -> MultipartData Tmp -> AppM Text
uploadHandler stepId multipartData = do
    let uploadedFiles = files multipartData
    when (null uploadedFiles) $ throwError err400{errBody = "No files found"}
    requireStepUnreviewed stepId

    hash <- lift $ bracket
        (liftIO $ getCanonicalTemporaryDirectory >>= \dir -> createTempDirectory dir ("upload_" ++ show stepId))
        (\tmpDir -> liftIO (removeDirectoryRecursive tmpDir `catchIOError` \_ -> pure ()))
        $ \tmpDir -> do
            let storeRefDir = tmpDir </> "store-ref"
            liftIO $ createDirectoryIfMissing True storeRefDir

            liftIO $ forM_ uploadedFiles $ \file -> do
                let fileName = T.unpack $ fdFileName file
                    filePath = storeRefDir </> fileName
                    tempFilePath = fdPayload file
                renameFile tempFilePath filePath

            storePathResult <- addFixed storeRefDir
            storePath <- case storePathResult of
                Left err -> error err
                Right out -> return (T.unpack $ T.strip $ T.pack out)

            hashOutput <- runExceptT $ do
                jsonOut <- runNix ["path-info", "--json", storePath]
                case decode (TLE.encodeUtf8 (TL.pack jsonOut)) >>= extractNarHash of
                    Just h -> return h
                    Nothing -> ExceptT $ return $ Left $ "Could not parse narHash from: " ++ jsonOut
            case hashOutput of
                Left err -> error $ "nix path-info failed: " ++ err
                Right h -> return h

    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        ensureStepUnreviewed ctx stepId
        updateStepNixFile ctx stepId hash
        commitAndPushChanges ctx $ "Upload files for step " ++ show stepId
    case result of
        Left err -> throwError err409{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> return $ "Uploaded " <> T.pack (show (length uploadedFiles)) <> " files with hash: " <> hash

extractNarHash :: Value -> Maybe Text
extractNarHash (Object outerObj) = do
    Object innerObj <- listToMaybe (toList outerObj)
    String h <- KM.lookup "narHash" innerObj
    return h
extractNarHash _ = Nothing

updateStepNixFile :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> Text -> ExceptT String (Eff es) ()
updateStepNixFile (WriteRepoContext worktreePath) stepId hash = do
    let nixFilePath = worktreePath </> "steps" </> show stepId ++ ".nix"
        nixExpr = "let orig = import " <> T.pack nixFilePath <> "; in orig // { args = orig.args // { uploaded = (orig.args.uploaded or {}) // { hash = \"" <> hash <> "\"; }; }; }"

    output <- runNixEvalImpureJsonExpr (T.unpack nixExpr)
    nixResult <- liftEither $ jsonToNix (TLE.encodeUtf8 (TL.pack output))
    liftIO $ TIO.writeFile nixFilePath nixResult
