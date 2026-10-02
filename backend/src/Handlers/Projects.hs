{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Projects (getProjectsHandler, patchProjectHandler, batchUpdateProjectsHandler, postProjectHandler, deleteProjectHandler, jsonToNix, rewriteNixFile, rewriteProjectFile, RawJSON, ProjectUpdate (..)) where

import ApiTypes (DynamicJson (..))
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (mapM_)
import Control.Monad.Except (ExceptT, catchError, liftEither, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Options (..), Result (..), Value (..), defaultOptions, eitherDecode, encode, fromJSON, genericParseJSON, toJSON)
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LB
import Data.Fix (foldFix)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (floatingOrInteger)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Effectful (Eff, IOE, (:>))
import Effects (AppM, Eval)
import GHC.Generics (Generic)
import Network.HTTP.Media ((//))
import Certificates (evalProjectDefinition, evalProjectDefinitions, withWriteRepoTransaction)
import ProjectTree (ChildChanges (..), ChildRef (..), ProjectFields, appendChildren, applyChildChanges, newProject)
import Servant (Accept (..), MimeRender (..), MimeUnrender (..), NoContent (..))
import Servant.Server (err400, err500, errBody)
import System.Directory (doesDirectoryExist, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath (takeBaseName, takeExtension, (</>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixEvalImpureJsonExpr, withReadRepoTransaction)

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

getProjectsHandler :: Maybe T.Text -> AppM DynamicJson
getProjectsHandler commit = do
    result <- lift $ withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) -> do
        let targetCommit = maybe commitHash T.unpack commit
        projects <- evalProjectDefinitions (ReadRepoContext repoPath targetCommit)
        mtimes <- liftIO $ readRecordMtimes repoPath targetCommit
        return $ encode (annotateRecordMtimes mtimes projects)
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

{-# NOINLINE mtimeCacheRef #-}
mtimeCacheRef :: MVar [(String, Map.Map FilePath T.Text)]
mtimeCacheRef = unsafePerformIO (newMVar [])

mtimeCacheLimit :: Int
mtimeCacheLimit = 8

readRecordMtimes :: FilePath -> String -> IO (Map.Map FilePath T.Text)
readRecordMtimes repoPath commit = do
    cache <- readMVar mtimeCacheRef
    case lookup commit cache of
        Just mtimes -> return mtimes
        Nothing -> do
            mtimes <- loadRecordMtimes repoPath commit
            modifyMVar_ mtimeCacheRef $ \entries ->
                return $ take mtimeCacheLimit $ (commit, mtimes) : filter ((/= commit) . fst) entries
            return mtimes

loadRecordMtimes :: FilePath -> String -> IO (Map.Map FilePath T.Text)
loadRecordMtimes repoPath commit = do
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
    decorateProject = onObject (stamp "projects/" . adjustKey "children" (onArray (V.map decorateChild)))
    decorateChild = onObject (adjustKey "step" (onObject (adjustKey "def" (onObject (stamp "steps/")))))
    stamp prefix obj = maybe obj (\iso -> KeyMap.insert "lastModifiedAt" (String iso) obj) (integerId obj >>= \i -> Map.lookup (prefix ++ show i ++ ".nix") mts)
    integerId obj = KeyMap.lookup "id" obj >>= \v -> case fromJSON v :: Result Int of Success i -> Just i; _ -> Nothing
    onObject f v = case v of Object o -> Object (f o); _ -> v
    onArray f v = case v of Array a -> Array (f a); _ -> v
    adjustKey k f m = maybe m (\v -> KeyMap.insert k (f v) m) (KeyMap.lookup k m)

patchProjectHandler :: Int -> ProjectFields -> AppM NoContent
patchProjectHandler projectId fields = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        rewriteProjectFile ctx projectId (replaceProjectFields fields)
        commitAndPushChanges ctx $ "Update project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

data ProjectUpdate = ProjectUpdate
    { projectUpdateId :: Int
    , projectUpdateRecord :: ProjectFields
    }
    deriving (Generic, Show)

instance FromJSON ProjectUpdate where
    parseJSON = genericParseJSON $ defaultOptions{fieldLabelModifier = \label -> if label == "projectUpdateRecord" then "record" else "id"}

batchUpdateProjectsHandler :: [ProjectUpdate] -> AppM NoContent
batchUpdateProjectsHandler [] =
    throwError $ err400{errBody = "Empty project update batch"}
batchUpdateProjectsHandler updates = do
    result <- lift $ withWriteRepoTransaction $ \ctx -> do
        mapM_ (\(ProjectUpdate projectId fields) -> rewriteProjectFile ctx projectId (replaceProjectFields fields)) updates
        let plural = if null (tail updates) then "project" else "projects"
        commitAndPushChanges ctx $ "Update " ++ show (length updates) ++ " " ++ plural
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

replaceProjectFields :: ProjectFields -> T.Text
replaceProjectFields fields = "builtins.removeAttrs orig [ \"preset\" \"templates\" ] // " <> valueToNix (toJSON fields)

deleteProjectHandler :: Int -> AppM NoContent
deleteProjectHandler 0 =
    throwError $ err400{errBody = "The root project cannot be deleted."}
deleteProjectHandler projectId = do
    result <- lift $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        _ <- liftIO $ readProcessWithExitCode "git" ["-C", worktreePath, "rm", "-f", projectFilePath worktreePath projectId] ""
        parents <- projectFilesReferencing (worktreePath </> "projects") projectId
        mapM_ (\file -> rewriteNixFile (worktreePath </> "projects" </> file) (applyChildChanges (ChildChanges [] [ProjectChild projectId]))) parents
        commitAndPushChanges ctx $ "Delete project " ++ show projectId
    case result of
        Right _ -> return NoContent
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

projectFilesReferencing :: (Eval :> es, IOE :> es) => FilePath -> Int -> ExceptT String (Eff es) [FilePath]
projectFilesReferencing projectsDir projectId = do
    files <- liftIO $ map projectFileName <$> projectFileIds projectsDir
    output <-
        runNixEvalImpureJsonExpr $
            "builtins.filter (name: builtins.any (c: c ? project && c.project.id == "
                ++ show projectId
                ++ ") ((import ("
                ++ projectsDir
                ++ " + \"/${name}\")).children or [ ])) [ "
                ++ unwords (map show files)
                ++ " ]"
    liftEither $ either (Left . (("Failed to find the projects containing project " ++ show projectId ++ ": ") ++)) Right $ eitherDecode (TLE.encodeUtf8 (TL.pack output))

postProjectHandler :: Maybe Int -> ProjectFields -> AppM DynamicJson
postProjectHandler maybeParentId fields = do
    let parentId = fromMaybe 0 maybeParentId
    result <- lift $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        projectId <- liftIO $ getNextProjectId (worktreePath </> "projects")
        liftIO $ TIO.writeFile (projectFilePath worktreePath projectId) (valueToNix (newProject fields) <> "\n")
        rewriteProjectFile ctx parentId (appendChildren [ProjectChild projectId])
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]
        output <- catchError (TLE.encodeUtf8 . TL.pack <$> evalProjectDefinition ctx projectId) $ \err -> do
            _ <- liftIO $ readProcessWithExitCode "git" ["-C", worktreePath, "rm", "-f", projectFilePath worktreePath projectId] ""
            throwError err
        commitAndPushChanges ctx $ "Create project " ++ show projectId ++ " in project " ++ show parentId
        return output
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

rewriteProjectFile :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> T.Text -> ExceptT String (Eff es) ()
rewriteProjectFile (WriteRepoContext worktreePath) projectId =
    rewriteNixFile (projectFilePath worktreePath projectId)

projectFilePath :: FilePath -> Int -> FilePath
projectFilePath worktreePath projectId = worktreePath </> "projects" </> projectFileName projectId

projectFileName :: Int -> FilePath
projectFileName projectId = show projectId ++ ".nix"

projectFileIds :: FilePath -> IO [Int]
projectFileIds projectsDir = do
    exists <- doesDirectoryExist projectsDir
    if not exists
        then return []
        else mapMaybe projectFileId <$> listDirectory projectsDir
  where
    projectFileId file
        | takeExtension file == ".nix" = readMaybe (takeBaseName file)
        | otherwise = Nothing

getNextProjectId :: FilePath -> IO Int
getNextProjectId projectsDir = do
    ids <- projectFileIds projectsDir
    return $ if null ids then 1 else maximum ids + 1

jsonToNix :: LB.ByteString -> Either String T.Text
jsonToNix bs = valueToNix <$> eitherDecode bs

valueToNix :: Value -> T.Text
valueToNix = renderMultilineNix . jsonValueToNixExpr

rewriteNixFile :: (Eval :> es, IOE :> es) => FilePath -> T.Text -> ExceptT String (Eff es) ()
rewriteNixFile path transformation = do
    output <- runNixEvalImpureJsonExpr $ T.unpack $ "let orig = import " <> T.pack path <> "; in " <> transformation
    nixResult <- liftEither $ jsonToNix (TLE.encodeUtf8 (TL.pack output))
    liftIO $ TIO.writeFile path (nixResult <> "\n")

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
