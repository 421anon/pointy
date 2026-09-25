{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module IngestJobs (
    IngestRequest (..),
    discardDirectory,
    startIngestJob,
    updateStepNixFile,
) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, liftEither)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text (Text)
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, (:>))
import Effects (AppEffects, Eval, ingestDirectory)
import Handlers.Projects (jsonToNix)
import Handlers.StepReview (ensureStepUnreviewed)
import Ingest (IngestResult (..), storeRefName)
import qualified IngestBus
import OutPaths (withWriteRepoTransaction)
import System.Directory (removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import UserRepo (WriteRepoContext (..), commitAndPushChanges, runNixEvalImpureJsonExpr)

data IngestRequest = IngestRequest
    { ingestRequestStepId :: Int
    , ingestRequestDirectory :: FilePath
    , ingestRequestPath :: Maybe Text
    , ingestRequestMessage :: String
    , ingestRequestStaging :: Maybe FilePath
    }

startIngestJob :: IngestRequest -> IO (Maybe Int)
startIngestJob request = do
    reserved <- IngestBus.reserveJob (ingestRequestStepId request) (sourceName request) (ingestRequestPath request)
    case reserved of
        Nothing -> pure Nothing
        Just ident -> do
            void $ forkIO (runJob ident request)
            pure (Just ident)

sourceName :: IngestRequest -> Text
sourceName request = maybe "upload" (const "scratch") (ingestRequestPath request)

runJob :: Int -> IngestRequest -> IO ()
runJob ident request = do
    outcome <- try (runAppEffects (ingest ident request))
    case outcome of
        Left (err :: SomeException) -> failJob (show err)
        Right (Left message) -> failJob message
        Right (Right result) -> do
            when (not (ingestReferencesSource result)) (discardStaging request)
            IngestBus.recordSuccess ident (ingestNarHash result)
  where
    failJob message = do
        discardStaging request
        IngestBus.recordFailure ident (T.pack message)

ingest :: Int -> IngestRequest -> Eff AppEffects (Either String IngestResult)
ingest ident request = do
    result <- ingestDirectory (ingestRequestDirectory request) storeRefName (IngestBus.recordProgress ident)
    case result of
        Left message -> pure (Left message)
        Right ingested -> do
            written <- withWriteRepoTransaction $ \context -> do
                ensureStepUnreviewed context (ingestRequestStepId request)
                updateStepNixFile context (ingestRequestStepId request) (ingestNarHash ingested)
                commitAndPushChanges context (ingestRequestMessage request)
            pure $ case written of
                Left message -> Left message
                Right () -> Right ingested

discardStaging :: IngestRequest -> IO ()
discardStaging request = case ingestRequestStaging request of
    Nothing -> pure ()
    Just directory -> discardDirectory directory

discardDirectory :: FilePath -> IO ()
discardDirectory directory = removeDirectoryRecursive directory `catchIOError` \_ -> pure ()

updateStepNixFile :: (Eval :> es, IOE :> es) => WriteRepoContext -> Int -> Text -> ExceptT String (Eff es) ()
updateStepNixFile (WriteRepoContext worktreePath) stepId hash = do
    let nixFilePath = worktreePath </> "steps" </> show stepId ++ ".nix"
        nixExpr = "let orig = import " <> T.pack nixFilePath <> "; in orig // { args = orig.args // { uploaded = (orig.args.uploaded or {}) // { hash = \"" <> hash <> "\"; }; }; }"

    output <- runNixEvalImpureJsonExpr (T.unpack nixExpr)
    nixResult <- liftEither $ jsonToNix (TLE.encodeUtf8 (TL.pack output))
    liftIO $ TIO.writeFile nixFilePath nixResult
