{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Projects (getProjectsHandler, patchProjectHandler, postProjectHandler, deleteProjectHandler, jsonToNix, RawJSON) where

import ApiTypes (DynamicJson (..))
import Control.Monad.Except (ExceptT (..), catchError, liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON)
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Fix (foldFix)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (floatingOrInteger)
import qualified Data.Vector as V
import Network.HTTP.Media ((//))
import OutPaths (withWriteRepoTransaction)
import Servant (Accept (..), Handler, MimeRender (..), MimeUnrender (..), NoContent (..))
import Servant.Server (err400, err500, errBody)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixEvalJsonInRepo, withReadRepoTransaction)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Nix.Expr.Shorthands (attrsE, mkBool, mkFloat, mkIndentedStr, mkInt, mkList, mkNull, mkStr)
import Nix.Expr.Types (Antiquoted (..), NExpr, NExprF (..), NString (..))
import Nix.Pretty (exprFNixDoc, getDoc, simpleExpr)
import Prettyprinter (defaultLayoutOptions, hardline, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)

data RawJSON

instance Accept RawJSON where contentType _ = "application" // "json"
instance MimeRender RawJSON DynamicJson where mimeRender _ = unDynamicJson
instance MimeUnrender RawJSON DynamicJson where mimeUnrender _ = Right . DynamicJson

getProjectsHandler :: Maybe T.Text -> Handler DynamicJson
getProjectsHandler commit = do
    result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) -> do
        let targetCommit = maybe commitHash T.unpack commit
            targetCtx = ReadRepoContext repoPath targetCommit
        output <- runNixEvalJsonInRepo targetCtx "#pointy.projects"
        mtimes <- liftIO $ readRecordMtimes repoPath targetCommit
        case eitherDecode (LB.fromStrict (TE.encodeUtf8 (T.pack output))) of
            Left err -> ExceptT $ return $ Left $ "decoding #pointy.projects failed: " ++ err
            Right value ->
                let annotated = annotateRecordMtimes mtimes value
                 in return $ encode annotated
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

readRecordMtimes :: FilePath -> String -> IO (Map.Map FilePath T.Text)
readRecordMtimes repoPath commit = do
    (code, out, _) <- runGitIn repoPath ["log", commit, "--pretty=tformat:%cI", "--name-only", "--", "steps/", "projects/"]
    return $ case code of
        ExitSuccess -> snd $ foldl' step (T.empty, Map.empty) (lines out)
        ExitFailure _ -> Map.empty
  where
    step (iso, acc) line
        | null line = (iso, acc)
        | '/' `notElem` line = (T.pack line, acc)
        | otherwise = (iso, Map.insertWith (\_ old -> old) line iso acc)

annotateRecordMtimes :: Map.Map FilePath T.Text -> Value -> Value
annotateRecordMtimes mts = onObject (KeyMap.map decorateProject)
  where
    decorateProject = onObject (stamp "projects/" . adjustKey "steps" (onArray (V.map decorateStep)))
    decorateStep = onObject (adjustKey "def" (onObject (stamp "steps/")))
    stamp prefix obj = maybe obj (\iso -> KeyMap.insert "lastModifiedAt" (String iso) obj) (integerId obj >>= \i -> Map.lookup (prefix ++ show i ++ ".nix") mts)
    integerId obj = KeyMap.lookup "id" obj >>= \v -> case fromJSON v :: Result Int of Success i -> Just i; _ -> Nothing
    onObject f v = case v of Object o -> Object (f o); _ -> v
    onArray f v = case v of Array a -> Array (f a); _ -> v
    adjustKey k f m = maybe m (\v -> KeyMap.insert k (f v) m) (KeyMap.lookup k m)

patchProjectHandler :: Int -> DynamicJson -> Handler NoContent
patchProjectHandler projectId (DynamicJson jsonBody) = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx -> do
        _ <- saveProject ctx (Just projectId) jsonBody
        commitAndPushChanges ctx $ "Update project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

deleteProjectHandler :: Int -> Handler NoContent
deleteProjectHandler projectId = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        let outputPath = worktreePath </> "projects" </> show projectId ++ ".nix"
        _ <- liftIO $ readProcessWithExitCode "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
        commitAndPushChanges ctx $ "Delete project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

postProjectHandler :: DynamicJson -> Handler DynamicJson
postProjectHandler (DynamicJson jsonBody) = do
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        projectId <- saveProject ctx Nothing jsonBody
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]
        output <- catchError (TLE.encodeUtf8 . TL.pack <$> runNixEvalJsonInRepo ctx ("#pointy.projects." ++ show projectId)) $ \err -> do
            let outputPath = worktreePath </> "projects" </> show projectId ++ ".nix"
            _ <- liftIO $ readProcessWithExitCode "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
            throwError err
        commitAndPushChanges ctx $ "Create project " ++ show projectId
        return output
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

saveProject :: WriteRepoContext -> Maybe Int -> LB.ByteString -> ExceptT String IO Int
saveProject (WriteRepoContext worktreePath) maybeId jsonBody = do
    nixText <- liftEither $ jsonToNix jsonBody
    let projectsDir = worktreePath </> "projects"
    projectId <- liftIO $ maybe (getNextProjectId projectsDir) return maybeId
    let outputPath = projectsDir </> show projectId ++ ".nix"
    liftIO $ TIO.writeFile outputPath (nixText <> "\n")
    return projectId

getNextProjectId :: FilePath -> IO Int
getNextProjectId projectsDir = do
    exists <- doesDirectoryExist projectsDir
    if not exists
        then return 1
        else do
            files <- listDirectory projectsDir
            let ids = mapMaybe (readMaybe . takeBaseName) files :: [Int]
            return $ if null ids then 1 else maximum ids + 1

jsonToNix :: LB.ByteString -> Either String T.Text
jsonToNix bs = do
    val <- eitherDecode bs
    return $ renderMultilineNix $ jsonValueToNixExpr val

jsonValueToNixExpr :: Value -> NExpr
jsonValueToNixExpr (Object obj) =
    attrsE [(toText key, jsonValueToNixExpr value) | (key, value) <- KeyMap.toAscList obj]
jsonValueToNixExpr (Array arr) = mkList (map jsonValueToNixExpr $ V.toList arr)
jsonValueToNixExpr (String text)
    | T.any (== '\n') text = mkIndentedStr 0 text
    | otherwise = mkStr text
jsonValueToNixExpr (Number number) = either mkFloat mkInt $ floatingOrInteger number
jsonValueToNixExpr (Bool boolean) = mkBool boolean
jsonValueToNixExpr Null = mkNull

renderMultilineNix :: NExpr -> T.Text
renderMultilineNix = renderStrict . layoutPretty defaultLayoutOptions . getDoc . foldFix renderNode
  where
    renderNode (NStr (Indented _ [Plain text])) =
        simpleExpr $ "''" <> hardline <> pretty (T.replace "${" "''${" $ T.replace "'" "''\\'" text) <> "''"
    renderNode node = exprFNixDoc node
