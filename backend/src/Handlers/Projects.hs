{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.Projects (getProjectsHandler, patchProjectHandler, postProjectHandler, deleteProjectHandler, evaluateJsonToNix, evaluateJsonToNixPreservingNewlines, RawJSON) where

import ApiTypes (DynamicJson (..))
import Control.Monad.Except (ExceptT (..), catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Data.Aeson (Result (..), Value (..), eitherDecode, encode, fromJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Fix (Fix (..), foldFix)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
import Nix (nixEvalExpr, withNixContext)
import Nix.Expr.Shorthands (mkIndentedStr, mkStr, mkSym, (@.), (@@))
import Nix.Expr.Types (Antiquoted (..), NExpr, NExprF (..), NString (..))
import Nix.Normal (normalForm)
import Nix.Options (defaultOptions)
import Nix.Pretty (exprFNixDoc, getDoc, prettyNix, simpleExpr, valueToExpr)
import Nix.Standard (runWithBasicEffectsIO)
import NixUtils (sortAttrSet)
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
saveProject (WriteRepoContext worktreePath) maybeId jsonBody = ExceptT $ do
    case TE.decodeUtf8' (LB.toStrict jsonBody) of
        Left utf8Err -> return $ Left $ "Invalid UTF-8 in request body: " ++ show utf8Err
        Right jsonText -> do
            result <- evaluateJsonToNix jsonText
            case result of
                Left err -> return $ Left err
                Right nixText -> do
                    let projectsDir = worktreePath </> "projects"
                    projectId <- maybe (getNextProjectId projectsDir) return maybeId
                    let outputPath = projectsDir </> show projectId ++ ".nix"
                    TIO.writeFile outputPath (nixText <> "\n")
                    return $ Right projectId

getNextProjectId :: FilePath -> IO Int
getNextProjectId projectsDir = do
    exists <- doesDirectoryExist projectsDir
    if not exists
        then return 1
        else do
            files <- listDirectory projectsDir
            let ids = mapMaybe (readMaybe . takeBaseName) files :: [Int]
            return $ if null ids then 1 else maximum ids + 1

evaluateJsonToNix :: T.Text -> IO (Either String T.Text)
evaluateJsonToNix = evaluateJsonToNixWith False

evaluateJsonToNixPreservingNewlines :: T.Text -> IO (Either String T.Text)
evaluateJsonToNixPreservingNewlines = evaluateJsonToNixWith True

evaluateJsonToNixWith :: Bool -> T.Text -> IO (Either String T.Text)
evaluateJsonToNixWith preserveNewlines jsonText = do
    let fullExpr = mkSym "builtins" @. "fromJSON" @@ mkStr jsonText
    let opts = defaultOptions $ posixSecondsToUTCTime 0
    result <- runWithBasicEffectsIO opts $ withNixContext Nothing $ do
        val <- nixEvalExpr Nothing fullExpr
        nf <- normalForm val
        return $ valueToExpr nf
    let sortedResult = sortAttrSet result
        nixText
            | preserveNewlines = renderMultilineNix $ rewriteMultilineStrings sortedResult
            | otherwise = renderStrict $ layoutPretty defaultLayoutOptions $ prettyNix sortedResult
    return $ Right nixText

rewriteMultilineStrings :: NExpr -> NExpr
rewriteMultilineStrings = foldFix rewriteNode
  where
    rewriteNode (NStr (DoubleQuoted [Plain text]))
        | T.any (== '\n') text = mkIndentedStr 0 text
    rewriteNode node = Fix node

renderMultilineNix :: NExpr -> T.Text
renderMultilineNix = renderStrict . layoutPretty defaultLayoutOptions . getDoc . foldFix renderNode
  where
    renderNode (NStr (Indented _ [Plain text])) =
        simpleExpr $ "''" <> hardline <> pretty (escapeIndented text) <> "''"
    renderNode node = exprFNixDoc node

escapeIndented :: T.Text -> T.Text
escapeIndented = preserveCommonIndent . T.replace "${" "''${" . T.replace "''" "'''"

preserveCommonIndent :: T.Text -> T.Text
preserveCommonIndent text
    | not (null contentLines) && all (T.isPrefixOf " ") contentLines = "${\"\"}" <> text
    | otherwise = text
  where
    contentLines = filter (not . T.null) $ T.splitOn "\n" text
