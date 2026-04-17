{-# LANGUAGE OverloadedStrings #-}

module Handlers.Upload (uploadHandler) where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), decode)
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import OutPaths (withWriteRepoTransaction)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, err400, err500, errBody, throwError)
import Servant.Multipart (FileData (fdFileName, fdPayload), MultipartData (files), Tmp)
import System.Directory (createDirectoryIfMissing, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import UserRepo (WriteRepoContext (..), commitAndPushChanges, runNix)

import Handlers.Projects (evaluateJsonToNix)

uploadHandler :: Int -> MultipartData Tmp -> Handler Text
uploadHandler stepId multipartData = do
    let uploadedFiles = files multipartData
    when (null uploadedFiles) $ throwError err400{errBody = "No files found"}

    hash <- liftIO $ withSystemTempDirectory ("upload_" ++ show stepId) $ \tmpDir -> do
        let storeRefDir = tmpDir </> "store-ref"
        createDirectoryIfMissing True storeRefDir

        forM_ uploadedFiles $ \file -> do
            let fileName = T.unpack $ fdFileName file
                filePath = storeRefDir </> fileName
                tempFilePath = fdPayload file
            renameFile tempFilePath filePath

        (exitCode1, storePathOut, stderr1) <- readProcessWithExitCodeL "nix-store" ["--add-fixed", "--recursive", "sha256", storeRefDir] ""
        case exitCode1 of
            ExitFailure _ -> error $ "nix-store --add-fixed failed: " ++ stderr1
            ExitSuccess -> return ()

        let storePath = T.unpack $ T.strip $ T.pack storePathOut
        hashOutput <- runExceptT $ do
            jsonOut <- runNix ["path-info", "--json", storePath]
            case decode (TLE.encodeUtf8 (TL.pack jsonOut)) >>= extractNarHash of
                Just h -> return h
                Nothing -> ExceptT $ return $ Left $ "Could not parse narHash from: " ++ jsonOut
        case hashOutput of
            Left err -> error $ "nix path-info failed: " ++ err
            Right h -> return h

    result <- liftIO $ withWriteRepoTransaction $ \ctx -> do
        updateStepNixFile ctx stepId hash
        commitAndPushChanges ctx $ "Upload files for step " ++ show stepId
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right _ -> return $ "Uploaded " <> T.pack (show (length uploadedFiles)) <> " files with hash: " <> hash

extractNarHash :: Value -> Maybe Text
extractNarHash (Object outerObj) = do
    Object innerObj <- listToMaybe (toList outerObj)
    String h <- KM.lookup "narHash" innerObj
    return h
extractNarHash _ = Nothing

updateStepNixFile :: WriteRepoContext -> Int -> Text -> ExceptT String IO ()
updateStepNixFile (WriteRepoContext worktreePath) stepId hash = ExceptT $ do
    let nixFilePath = worktreePath </> "steps" </> show stepId ++ ".nix"
        nixExpr = "let orig = import " <> T.pack nixFilePath <> "; in orig // { args = orig.args // { uploaded = (orig.args.uploaded or {}) // { hash = \"" <> hash <> "\"; }; }; }"

    runExceptT $ do
        output <- runNix ["eval", "--impure", "--json", "--expr", T.unpack nixExpr]
        nixResult <- ExceptT $ evaluateJsonToNix (T.pack output)
        liftIO $ TIO.writeFile nixFilePath nixResult
