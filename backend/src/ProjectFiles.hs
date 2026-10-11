{-# LANGUAGE OverloadedStrings #-}

module ProjectFiles (
    jsonToNix,
    valueToNix,
    rewriteNixFile,
    projectFilePath,
    stepFilePath,
    nextProjectId,
    srcFilesPath,
    copyClonedSrcFiles,
    saveStep,
    writeTreePlan,
    RawProjectFile (..),
    loadRawProjectFilesAt,
    loadTreeState,
    applyTreeOpsIn,
    applyTreeOpsInWith,
) where

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT, liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, eitherDecodeStrict', withObject, (.:))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LB
import Data.Fix (foldFix)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Scientific (floatingOrInteger)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Vector as V
import Effectful (Eff, IOE, (:>))
import Effects (Eval)
import Nix.Expr.Shorthands (attrsE, mkBool, mkFloat, mkIndentedStr, mkInt, mkList, mkNull, mkStr)
import Nix.Expr.Types (Antiquoted (..), NExpr, NExprF (..), NString (..))
import Nix.Pretty (exprFNixDoc, getDoc, simpleExpr)
import Prettyprinter (defaultLayoutOptions, hardline, layoutPretty, pretty)
import Prettyprinter.Render.Text (renderStrict)
import ProjectTree (ProjectSource (..), TreeOp, TreePlan (..), TreeState (..), applyTreeOps, renderTreeOpError)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), runNixEvalImpureJsonExpr, runNixEvalJsonApplyInRepo)

jsonToNix :: LB.ByteString -> Either String T.Text
jsonToNix bs = valueToNix <$> eitherDecode bs

valueToNix :: Value -> T.Text
valueToNix = renderMultilineNix . jsonValueToNixExpr

rewriteNixFile :: (Eval :> es, IOE :> es) => FilePath -> T.Text -> ExceptT String (Eff es) ()
rewriteNixFile path transformation = do
    output <- runNixEvalImpureJsonExpr $ T.unpack $ "let orig = import " <> T.pack path <> "; in " <> transformation
    nixResult <- liftEither $ jsonToNix (TLE.encodeUtf8 (TL.pack output))
    liftIO $ TIO.writeFile path (nixResult <> "\n")

projectFilePath :: FilePath -> Int -> FilePath
projectFilePath worktreePath projectId = worktreePath </> "projects" </> projectFileName projectId

projectFileName :: Int -> FilePath
projectFileName projectId = show projectId ++ ".nix"

projectFileIds :: FilePath -> IO [Int]
projectFileIds worktreePath = numberedFileIds (worktreePath </> "projects")

stepFilePath :: FilePath -> Int -> FilePath
stepFilePath worktreePath stepId = worktreePath </> "steps" </> stepFileName stepId

stepFileName :: Int -> FilePath
stepFileName stepId = show stepId ++ ".nix"

stepFileIds :: FilePath -> IO [Int]
stepFileIds worktreePath = numberedFileIds (worktreePath </> "steps")

numberedFileIds :: FilePath -> IO [Int]
numberedFileIds dir = do
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else mapMaybe numberedFileId <$> listDirectory dir
  where
    numberedFileId file
        | takeExtension file == ".nix" = readMaybe (takeBaseName file)
        | otherwise = Nothing

nextProjectId :: FilePath -> IO Int
nextProjectId worktreePath = nextId (worktreePath </> "projects")

nextStepId :: FilePath -> IO Int
nextStepId worktreePath = nextId (worktreePath </> "steps")

nextId :: FilePath -> IO Int
nextId dir = do
    ids <- numberedFileIds dir
    return $ if null ids then 1 else maximum ids + 1

saveStep :: (IOE :> es) => FilePath -> Maybe Int -> LB.ByteString -> ExceptT String (Eff es) Int
saveStep worktreePath maybeId jsonBody = do
    nixText <- liftEither $ jsonToNix jsonBody
    stepId <- liftIO $ maybe (nextStepId worktreePath) return maybeId
    liftIO $ TIO.writeFile (stepFilePath worktreePath stepId) (nixText <> "\n")
    return stepId

writeTreePlan :: FilePath -> TreePlan -> IO ()
writeTreePlan worktreePath plan =
    forM_ (Map.toList (planWrites plan)) $ \(projectId, fields) ->
        TIO.writeFile (projectFilePath worktreePath projectId) (valueToNix (Object fields) <> "\n")

srcFilesPath :: FilePath -> Int -> FilePath
srcFilesPath worktreePath stepId = worktreePath </> "srcFiles" </> show stepId

copyClonedSrcFiles :: FilePath -> Maybe Int -> Int -> IO ()
copyClonedSrcFiles _ Nothing _ = return ()
copyClonedSrcFiles worktreePath (Just sourceId) newStepId = do
    let sourceDir = srcFilesPath worktreePath sourceId
        destDir = srcFilesPath worktreePath newStepId
    sourceExists <- doesDirectoryExist sourceDir
    when sourceExists $ copyDirectoryRecursive sourceDir destDir

copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive sourceDir destDir = do
    createDirectoryIfMissing True destDir
    entries <- listDirectory sourceDir
    forM_ entries $ \entry -> do
        let sourcePath = sourceDir </> entry
            destPath = destDir </> entry
        isDir <- doesDirectoryExist sourcePath
        if isDir
            then copyDirectoryRecursive sourcePath destPath
            else copyFile sourcePath destPath

data RawProjectFile = RawProjectFile
    { rawFileId :: Int
    , rawFilePayload :: Maybe T.Text
    }

instance FromJSON RawProjectFile where
    parseJSON = withObject "RawProjectFile" $ \fields ->
        RawProjectFile <$> fields .: "id" <*> fields .: "payload"

loadTreeState :: (Eval :> es, IOE :> es) => FilePath -> ExceptT String (Eff es) TreeState
loadTreeState worktreePath = do
    projectIds <- liftIO $ projectFileIds worktreePath
    entries <- loadRawProjectEntries worktreePath projectIds
    stepIds <- liftIO $ Set.fromList <$> stepFileIds worktreePath
    return
        TreeState
            { treeProjects = Map.fromList [(rawFileId entry, source entry) | entry <- entries]
            , treeSteps = stepIds
            }
  where
    source entry = case rawFilePayload entry of
        Just payload -> either ProjectUnreadable ProjectReadable (eitherDecodeStrict' (TE.encodeUtf8 payload))
        Nothing -> ProjectUnreadable "the file could not be evaluated"

loadRawProjectEntries :: (Eval :> es, IOE :> es) => FilePath -> [Int] -> ExceptT String (Eff es) [RawProjectFile]
loadRawProjectEntries worktreePath projectIds = do
    batched <- lift $ runExceptT (runNixEvalImpureJsonExpr (rawProjectsExpression (worktreePath </> "projects") projectIds))
    case batched >>= decodeRawProjectFiles of
        Right entries -> pure entries
        Left _ -> mapM (loadRawProjectEntry worktreePath) projectIds

loadRawProjectEntry :: (Eval :> es, IOE :> es) => FilePath -> Int -> ExceptT String (Eff es) RawProjectFile
loadRawProjectEntry worktreePath projectId = do
    result <- lift $ runExceptT (runNixEvalImpureJsonExpr (rawProjectExpression (worktreePath </> "projects") projectId))
    pure $ case result >>= decodeRawProjectFile of
        Right entry -> entry
        Left _ -> RawProjectFile projectId Nothing

decodeRawProjectFiles :: String -> Either String [RawProjectFile]
decodeRawProjectFiles output = eitherDecode (TLE.encodeUtf8 (TL.pack output))

decodeRawProjectFile :: String -> Either String RawProjectFile
decodeRawProjectFile output = eitherDecode (TLE.encodeUtf8 (TL.pack output))

loadRawProjectFilesAt :: (Eval :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) [RawProjectFile]
loadRawProjectFilesAt ctx = do
    output <- runNixEvalJsonApplyInRepo ctx rawProjectsAtExpression ""
    either (throwError . ("Failed to read the project files: " ++)) pure (decodeRawProjectFiles output)

rawProjectsAtExpression :: String
rawProjectsAtExpression =
    "flake: let "
        ++ "dir = flake.outPath + \"/projects\"; "
        ++ "names = if builtins.pathExists dir then builtins.attrNames (builtins.readDir dir) else [ ]; "
        ++ "isProject = name: builtins.match \"[0-9]+\\\\.nix\" name != null; "
        ++ "load = name: let attempt = builtins.tryEval (builtins.toJSON (import (dir + (\"/\" + name)))); "
        ++ "in { id = builtins.fromJSON (builtins.substring 0 (builtins.stringLength name - 4) name); "
        ++ "payload = if attempt.success then attempt.value else null; }; "
        ++ "in map load (builtins.filter isProject names)"

applyTreeOpsIn :: (Eval :> es, IOE :> es) => FilePath -> [TreeOp] -> ExceptT String (Eff es) (TreeState, TreePlan)
applyTreeOpsIn worktreePath = applyTreeOpsInWith worktreePath (\_ _ -> return ())

applyTreeOpsInWith :: (Eval :> es, IOE :> es) => FilePath -> (TreeState -> TreePlan -> ExceptT String (Eff es) ()) -> [TreeOp] -> ExceptT String (Eff es) (TreeState, TreePlan)
applyTreeOpsInWith worktreePath validate ops = do
    state <- loadTreeState worktreePath
    plan <- either (throwError . renderTreeOpError) pure (applyTreeOps state ops)
    validate state plan
    liftIO $ writeTreePlan worktreePath plan
    pure (state, plan)

rawProjectsExpression :: FilePath -> [Int] -> String
rawProjectsExpression projectsDir projectIds =
    "let dir = "
        ++ projectsDir
        ++ "; load = id: let attempt = builtins.tryEval (builtins.toJSON (import (dir + (\"/\" + toString id + \".nix\")))); "
        ++ "in { id = id; payload = if attempt.success then attempt.value else null; }; "
        ++ "in map load [ "
        ++ unwords (map show projectIds)
        ++ " ]"

rawProjectExpression :: FilePath -> Int -> String
rawProjectExpression projectsDir projectId =
    "let dir = "
        ++ projectsDir
        ++ "; id = "
        ++ show projectId
        ++ "; attempt = builtins.tryEval (builtins.toJSON (import (dir + (\"/\" + toString id + \".nix\")))); "
        ++ "in { id = id; payload = if attempt.success then attempt.value else null; }"

jsonValueToNixExpr :: Value -> NExpr
jsonValueToNixExpr (Object obj) =
    attrsE [(Key.toText key, jsonValueToNixExpr value) | (key, value) <- KeyMap.toAscList obj]
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
