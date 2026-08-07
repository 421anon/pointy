{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.FileIndex (FileIndexEntry (..), fileIndexHandler) where

import Control.Monad.Except (ExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, eitherDecode)
import Data.List (isPrefixOf, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import Handlers.Store (resolveCommitHash)
import OutPaths (ProjectDef (..), StepDef (..), StepRef (..), getProjectOutPaths)
import Servant (Handler, ServerError (..), err500)
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import UserRepo (ReadRepoContext (..), runNixEvalJsonInRepo, runNixEvalRawInRepo, userRepoPath)

data FileIndexEntry = FileIndexEntry
    { projectId :: Int
    , stepId :: Int
    , target :: Text
    , path :: [String]
    }
    deriving (Generic, ToJSON)

fileIndexHandler :: Maybe Text -> Handler [FileIndexEntry]
fileIndexHandler mCommit = do
    repoPath <- liftIO userRepoPath
    commit <- resolveCommitHash mCommit
    entries <- liftIO $ runExceptT $ fileIndexAt $ ReadRepoContext repoPath commit
    either (\message -> throwError err500{errBody = TLE.encodeUtf8 $ TL.pack message}) pure entries

fileIndexAt :: ReadRepoContext -> ExceptT String IO [FileIndexEntry]
fileIndexAt ctx = do
    projectsJson <- evalAttr runNixEvalJsonInRepo "#pointy.projects"
    projects <- case eitherDecode (TLE.encodeUtf8 (TL.pack projectsJson)) of
        Left err -> throwError $ "decoding #pointy.projects failed: " ++ err
        Right defs -> pure (defs :: Map String ProjectDef)
    srcFilesBase <- T.strip . T.pack <$> evalAttr runNixEvalRawInRepo "#pointy.srcFiles"
    liftIO $ concat <$> mapM (projectEntries ctx (unpack srcFilesBase)) (Map.elems projects)
  where
    evalAttr eval attr = withExceptT (("Failed to evaluate " ++ attr ++ ": ") ++) (eval ctx attr)

projectEntries :: ReadRepoContext -> FilePath -> ProjectDef -> IO [FileIndexEntry]
projectEntries ctx srcFilesBase project = do
    outPaths <- either outputsUnavailable pure =<< getProjectOutPaths pid (pack (readCommitHash ctx))
    concat
        <$> sequence
            ( [entriesUnder pid sid "output" (unpack outPath) | (sid, outPath) <- Map.toAscList outPaths]
                ++ [entriesUnder pid sid "source" (srcFilesBase </> show sid) | sid <- stepIds]
            )
  where
    pid = projectDefId project
    stepIds = Set.toAscList $ Set.fromList $ map (stepDefId . stepRefDef) $ projectDefSteps project
    outputsUnavailable err = do
        putStrLn $ "File index skipped the outputs of project " ++ show pid ++ ": " ++ err
        pure Map.empty

{- | Symlinked directories are followed as they are when browsing a step, so an
aliased directory is indexed under every alias; one already on the current
branch is a cycle and yields nothing.
-}
entriesUnder :: Int -> Int -> Text -> FilePath -> IO [FileIndexEntry]
entriesUnder pid sid entryTarget = go Set.empty []
  where
    go branch prefix dir = do
        canonical <- canonicalizePath dir
        isDir <- doesDirectoryExist canonical
        isFile <- doesFileExist canonical
        names <-
            if isDir && not (Set.member canonical branch) && "/nix/store/" `isPrefixOf` canonical
                then sort <$> listDirectory dir
                else pure []
        nested <- mapM (\name -> go (Set.insert canonical branch) (name : prefix) (dir </> name)) names
        pure $ [FileIndexEntry pid sid entryTarget (reverse prefix) | isFile] ++ concat nested
