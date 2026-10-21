{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.ArtifactRdf (artifactRdfHandler) where

import ArtifactRdf (ArtifactFile (..), ArtifactRef (..), parseArtifactFile, parseArtifactSegments)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Value (String), eitherDecode, withObject, (.:?))
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM)
import ProjectFiles (valueToNix)
import Servant (Header, Headers, ServerError (..), addHeader, err404, err500)
import System.Environment (lookupEnv)
import UserRepo (commitContext, ensureRepoCommit, runNixEvalJsonApplyInRepo, userRepoPath)

type ArtifactRdfResponse = Headers '[Header "Cache-Control" Text, Header "Vary" Text] Text

data LinkedDataAnswer = LinkedDataAnswer (Maybe Text) (Maybe Text)

instance FromJSON LinkedDataAnswer where
    parseJSON = withObject "Linked data answer" $ \fields ->
        LinkedDataAnswer <$> fields .:? "turtle" <*> fields .:? "missing"

artifactRdfHandler :: [Text] -> Maybe Text -> AppM ArtifactRdfResponse
artifactRdfHandler segments mPath = do
    ref <- maybe (throwError err404{errBody = "Expected /artifact/<project path>/<step id>/<commit>"}) pure (parseArtifactSegments segments)
    mFile <- case mPath of
        Just path | not (T.null path) -> maybe (throwError err404{errBody = "Expected ?path= to name a file inside the output"}) (pure . Just) (parseArtifactFile path)
        _ -> pure Nothing
    resolved <- liftIO $ runExceptT $ do
        ensureRepoCommit (T.unpack (refCommit ref))
        repoPath <- liftIO userRepoPath
        commitContext repoPath (refCommit ref)
    ctx <- either (\err -> throwError err404{errBody = utf8 err}) pure resolved
    evaluated <- lift $ runExceptT $ runNixEvalJsonApplyInRepo ctx (linkedDataExpression ref mFile) "#pointy"
    output <- either (\err -> throwError err500{errBody = utf8 ("Failed to evaluate the linked data: " ++ err)}) pure evaluated
    answer <- either (const (throwError err500{errBody = "Unexpected linked data answer"})) pure $ eitherDecode (LBS.fromStrict (TE.encodeUtf8 (T.pack output)))
    turtle <- case answer of
        LinkedDataAnswer (Just turtle) _ -> pure turtle
        LinkedDataAnswer Nothing (Just "linked-data") ->
            throwError err404{errBody = utf8 ("Commit " ++ T.unpack (refCommit ref) ++ " predates linked data")}
        LinkedDataAnswer Nothing (Just "step") ->
            throwError err404{errBody = utf8 (missingStepMessage ref)}
        LinkedDataAnswer Nothing (Just "file-linked-data") ->
            throwError err404{errBody = utf8 ("Commit " ++ T.unpack (refCommit ref) ++ " predates linked data for individual files")}
        _ -> throwError err500{errBody = "Unexpected linked data answer"}
    base <- liftIO (lookupEnv "POINTY_LINKED_DATA_BASE")
    pure (addHeader "public, max-age=31536000, immutable" (addHeader "Accept" (maybe turtle (`withBase` turtle) base)))

withBase :: String -> Text -> Text
withBase base turtle = "@base <" <> T.dropWhileEnd (== '/') (T.pack base) <> "/> .\n" <> turtle

linkedDataExpression :: ArtifactRef -> Maybe ArtifactFile -> String
linkedDataExpression ref mFile =
    concat
        [ "pointy: let "
        , "root = pointy.root or null; "
        , "node = builtins.foldl' (current: pid: if current == null || !(builtins.isAttrs current) || !(builtins.hasAttr (toString pid) current) then null else current.${toString pid}) root [ "
        , unwords (map show (refProjectPath ref))
        , " ]; "
        , "step = if node == null || !(builtins.isAttrs node) || !(builtins.hasAttr \"steps\" node) || !(builtins.hasAttr \""
        , show (refStepId ref)
        , "\" node.steps) then null else node.steps.\""
        , show (refStepId ref)
        , "\"; "
        , "in if root == null then { missing = \"linked-data\"; } else if step == null then { missing = \"step\"; } else "
        , documentExpression
        ]
  where
    documentExpression = case mFile of
        Nothing -> "{ turtle = step.turtle; }"
        Just file ->
            concat
                [ "if !(step ? fileTurtle) then { missing = \"file-linked-data\"; } else { turtle = step.fileTurtle { path = "
                , T.unpack (valueToNix (String (filePath file)))
                , "; query = \""
                , T.unpack (fileQuery file)
                , "\"; }; }"
                ]

missingStepMessage :: ArtifactRef -> String
missingStepMessage ref =
    "Step "
        ++ show (refStepId ref)
        ++ " is not in "
        ++ folderDescription (refProjectPath ref)
        ++ " at "
        ++ T.unpack (refCommit ref)
  where
    folderDescription [] = "the root"
    folderDescription path = "folder " ++ T.unpack (T.intercalate " › " (map (T.pack . show) path))

utf8 :: String -> LBS.ByteString
utf8 = TLE.encodeUtf8 . TL.pack
