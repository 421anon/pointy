{-# LANGUAGE OverloadedStrings #-}

module Handlers.Steps (patchStepHandler, postStepHandler, noticesHandler) where

import ApiTypes (DynamicJson (..))
import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT (..), catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Handlers.Download (discoverDownloadTemplates, extractDownloadHash, extractDownloadUrl, extractReqType, injectDownloadHash, prefetchFile, validateHttpUrl)
import Handlers.ProjectEntities (assignRecordToProject)
import Handlers.Projects (evaluateJsonToNix)
import Handlers.Statuses (forkBroadcastProjectStatusAtHead, forkBroadcastStatusForStepProjectsAtHead)
import OutPaths (scheduleProjectOutPathsWarm, withWriteRepoTransaction)
import Servant (Handler, NoContent (..), throwError)
import Servant.Server (err400, err500, errBody)
import System.Directory (copyFile, createDirectoryIfMissing, doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, (</>))
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import UserRepo (ReadRepoContext (..), WriteRepoContext (..), commitAndPushChanges, runGitIn, runNixEvalJsonApplyInRepo, runNixEvalJsonInRepo, withReadRepoTransaction)

-----------------------------------------------------------------------------
-- Internal helpers
-----------------------------------------------------------------------------

{- | Validate a URL then prefetch it, returning the Nix store hash.
Errors are thrown as 400s directly in the Handler monad.
-}
prefetchDownloadUrl :: T.Text -> Handler T.Text
prefetchDownloadUrl url = do
    case validateHttpUrl url of
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 $ TL.pack err}
        Right validUrl -> do
            result <- liftIO $ prefetchFile validUrl
            case result of
                Left err -> throwError $ err400{errBody = TLE.encodeUtf8 $ TL.pack err}
                Right hash -> return hash

-----------------------------------------------------------------------------
-- PATCH /api/steps/:id'
-----------------------------------------------------------------------------

patchStepHandler :: Int -> DynamicJson -> Handler NoContent
patchStepHandler stepId (DynamicJson jsonBody) = do
    bodyValue <- case eitherDecode jsonBody of
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 $ TL.pack $ "Invalid JSON in request body: " ++ err}
        Right v -> return v

    -- Read-only phase: discover download templates only.
    templates <- liftIO $ withReadRepoTransaction $ \ctx ->
        discoverDownloadTemplates ctx
    templates' <- case templates of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 $ TL.pack err}
        Right ts -> return ts

    let mReqType = extractReqType bodyValue
        isDownload = maybe False (\t -> Set.member t templates') mReqType

    -- Prefetch phase: only for downloads — evaluate existing step then prefetch.
    (mHash, mExistingVal) <-
        if isDownload
            then do
                case extractDownloadUrl bodyValue of
                    Nothing -> throwError $ err400{errBody = "Download step requires args.url"}
                    Just newUrl -> do
                        -- Evaluate existing step only when we know it is a download.
                        existingResult <- liftIO $ withReadRepoTransaction $ \ctx -> do
                            existingJson <- runNixEvalJsonInRepo ctx ("#pointy.stepDefs." ++ show stepId)
                            case eitherDecode (LBS.fromStrict (TE.encodeUtf8 (T.pack existingJson))) of
                                Left err -> throwError $ "Failed to decode existing step: " ++ err
                                Right v -> return v
                        existingVal <- case existingResult of
                            Right v -> return v
                            Left err -> throwError $ err500{errBody = TLE.encodeUtf8 $ TL.pack err}

                        let mOldUrl = extractDownloadUrl existingVal
                            mOldHash = extractDownloadHash existingVal
                        h <-
                            if Just newUrl == mOldUrl
                                then case mOldHash of
                                    Just h -> return h
                                    Nothing -> prefetchDownloadUrl newUrl
                                else prefetchDownloadUrl newUrl
                        return (Just h, Just existingVal)
            else return (Nothing, Nothing)

    -- Build the final request body, injecting the hash when needed.
    let finalBody = case mHash of
            Just h -> DynamicJson (encode (injectDownloadHash bodyValue h))
            Nothing -> DynamicJson jsonBody

    -- Write transaction.
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        -- Re-discover templates under the write lock; abort if classification changed.
        templatesW <- discoverDownloadTemplates ctx
        let isDownloadW = maybe False (\t -> Set.member t templatesW) mReqType
        when (isDownload /= isDownloadW) $
            throwError "Step kind classification changed; retry"

        -- For download steps: re-read the current step definition and abort
        -- if its URL or hash differs from our preflight read (no network under
        -- the write lock).
        case mExistingVal of
            Just existingVal -> do
                currentJson <- runNixEvalJsonInRepo ctx ("#pointy.stepDefs." ++ show stepId)
                currentVal <- case eitherDecode (LBS.fromStrict (TE.encodeUtf8 (T.pack currentJson))) of
                    Left err -> throwError $ "Failed to decode current step: " ++ err
                    Right v -> return v
                when
                    ( extractDownloadUrl existingVal /= extractDownloadUrl currentVal
                        || extractDownloadHash existingVal /= extractDownloadHash currentVal
                    )
                    $ throwError "Step changed underfoot; retry"
            Nothing -> return ()

        evalRes <- ExceptT $ evaluateJsonToNix (TE.decodeUtf8 (LBS.toStrict (unDynamicJson finalBody)))
        let outputPath = worktreePath </> "steps" </> show stepId ++ ".nix"
        liftIO $ TIO.writeFile outputPath (evalRes <> "\n")

        -- For download steps: evaluate the saved definition to verify the
        -- hash is valid and no mismatch crept in.
        case mHash of
            Just _ -> do
                _ <- runNixEvalJsonInRepo ctx ("#pointy.stepDefs." ++ show stepId)
                return ()
            Nothing -> return ()

        commitAndPushChanges ctx $ "Update step " ++ show stepId
    case result of
        Right _ -> do
            liftIO $ forkBroadcastStatusForStepProjectsAtHead stepId
            return NoContent
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

-----------------------------------------------------------------------------
-- POST /api/steps
-----------------------------------------------------------------------------

postStepHandler :: Maybe Int -> Maybe Int -> DynamicJson -> Handler DynamicJson
postStepHandler maybeProjectId maybeSourceId (DynamicJson jsonBody) = do
    bodyValue <- case eitherDecode jsonBody of
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 $ TL.pack $ "Invalid JSON in request body: " ++ err}
        Right v -> return v

    -- Read-only phase: discover download templates.
    templates <- liftIO $ withReadRepoTransaction $ \ctx ->
        discoverDownloadTemplates ctx
    templates' <- case templates of
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 $ TL.pack $ "Failed to load step config: " ++ err}
        Right ts -> return ts

    let mReqType = extractReqType bodyValue
        isDownload = maybe False (\t -> Set.member t templates') mReqType

    -- Prefetch phase: download the URL before the write transaction.
    mHash <-
        if isDownload
            then do
                case extractDownloadUrl bodyValue of
                    Nothing -> throwError $ err400{errBody = "Download step requires args.url"}
                    Just url -> Just <$> prefetchDownloadUrl url
            else return Nothing

    -- Build the final request body, injecting the hash when needed.
    let finalBody = case mHash of
            Just h -> DynamicJson (encode (injectDownloadHash bodyValue h))
            Nothing -> DynamicJson jsonBody

    -- Write transaction.
    result <- liftIO $ withWriteRepoTransaction $ \ctx@(WriteRepoContext worktreePath) -> do
        stepId <- saveStep ctx Nothing (unDynamicJson finalBody)
        liftIO $ copyClonedSrcFiles worktreePath maybeSourceId stepId
        _ <- liftIO $ runGitIn worktreePath ["add", "--intent-to-add", "-A"]

        -- Re-discover templates under the write lock; abort if classification changed.
        templatesW <- discoverDownloadTemplates ctx
        let isDownloadW = maybe False (\t -> Set.member t templatesW) mReqType
        when (isDownload /= isDownloadW) $
            throwError "Step kind classification changed; retry"
        case maybeProjectId of
            Just projectId -> assignRecordToProject ctx projectId stepId
            Nothing -> return ()
        output <- catchError (TLE.encodeUtf8 . TL.pack <$> runNixEvalJsonInRepo ctx ("#pointy.stepDefs." ++ show stepId)) $ \err -> do
            let outputPath = worktreePath </> "steps" </> show stepId ++ ".nix"
            _ <- liftIO $ readProcessWithExitCode "git" ["-C", worktreePath, "rm", "-f", outputPath] ""
            throwError err
        let cloneNote = maybe "" (\srcId -> " (clone of " ++ show srcId ++ ")") maybeSourceId
        commitAndPushChanges ctx $
            case maybeProjectId of
                Just projectId -> "Create step " ++ show stepId ++ cloneNote ++ " and assign to project " ++ show projectId
                Nothing -> "Create step " ++ show stepId ++ cloneNote

        return output
    case result of
        Right output -> do
            case maybeProjectId of
                Just projectId -> do
                    -- Schedule explicit outPath warming for the affected project/commit
                    -- before status broadcast so the broadcast and any open stream share it.
                    eHead <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext _ hash) -> return (T.pack hash)
                    case eHead of
                        Right headCommit -> liftIO $ scheduleProjectOutPathsWarm projectId headCommit
                        Left _ -> return ()
                    liftIO $ forkBroadcastProjectStatusAtHead projectId
                Nothing -> return ()
            return (DynamicJson output)
        Left err -> throwError $ err400{errBody = TLE.encodeUtf8 (TL.pack err)}

-----------------------------------------------------------------------------
-- GET /api/steps/:id/notices
-----------------------------------------------------------------------------

noticesHandler :: Int -> Maybe T.Text -> Handler DynamicJson
noticesHandler stepId mCommit = do
    result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext repoPath headCommit) -> do
        let targetCommit = maybe headCommit T.unpack mCommit
            ctx = ReadRepoContext repoPath targetCommit
            attr = "#pointy.steps." ++ show stepId
            applyExpr = "s: if s ? meta && s.meta ? pointy && s.meta.pointy ? notices then s.meta.pointy.notices else []"
        TLE.encodeUtf8 . TL.pack <$> runNixEvalJsonApplyInRepo ctx applyExpr attr
    case result of
        Right output -> return (DynamicJson output)
        Left err -> throwError $ err500{errBody = TLE.encodeUtf8 (TL.pack err)}

-----------------------------------------------------------------------------
-- Save / allocate step
-----------------------------------------------------------------------------

saveStep :: WriteRepoContext -> Maybe Int -> LBS.ByteString -> ExceptT String IO Int
saveStep (WriteRepoContext worktreePath) maybeId jsonBody = ExceptT $ do
    case TE.decodeUtf8' (LBS.toStrict jsonBody) of
        Left utf8Err -> return $ Left $ "Invalid UTF-8 in request body: " ++ show utf8Err
        Right jsonText -> do
            result <- evaluateJsonToNix jsonText
            case result of
                Left err -> return $ Left err
                Right nixText -> do
                    let stepsDir = worktreePath </> "steps"
                    stepId <- maybe (getNextStepId stepsDir) return maybeId
                    let outputPath = stepsDir </> show stepId ++ ".nix"
                    TIO.writeFile outputPath (nixText <> "\n")
                    return $ Right stepId

getNextStepId :: FilePath -> IO Int
getNextStepId stepsDir = do
    exists <- doesDirectoryExist stepsDir
    if not exists
        then return 1
        else do
            files <- listDirectory stepsDir
            let ids = mapMaybe (readMaybe . takeBaseName) files :: [Int]
            return $ if null ids then 1 else maximum ids + 1

{- | When a step is cloned, duplicate its source files so the new step starts
with its own editable copy under @srcFiles/<newStepId>@.
-}
copyClonedSrcFiles :: FilePath -> Maybe Int -> Int -> IO ()
copyClonedSrcFiles _ Nothing _ = return ()
copyClonedSrcFiles worktreePath (Just sourceId) newStepId = do
    let sourceDir = worktreePath </> "srcFiles" </> show sourceId
        destDir = worktreePath </> "srcFiles" </> show newStepId
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
