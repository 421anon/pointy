{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OutPaths (
    getProjectCertificates,
    warmProjectCertificates,
    warmProjectCertificatesForCommit,
    scheduleProjectCertificatesWarm,
    withWriteRepoTransaction,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.Either (isRight)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, (:>))
import Effects (Eval)
import GHC.Generics (Generic)
import NixEvaluator (RepoSource, repoSource)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), WriteRepoContext, ensureRepoCommit, rewarmRepoJsonExpressions, runNixEvalJsonInRepo, runNixEvalJsonInRepoBackground, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

data ProjectDef = ProjectDef
    { projectDefId :: Int
    , projectDefHidden :: Bool
    , projectDefSteps :: [StepRef]
    }
    deriving (Show, Generic)

instance FromJSON ProjectDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "projectDef"

data StepRef = StepRef
    { stepRefHidden :: Bool
    , stepRefDef :: StepDef
    }
    deriving (Show, Generic)

instance FromJSON StepRef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "stepRef"

newtype StepDef = StepDef
    { stepDefId :: Int
    }
    deriving (Show, Generic)

instance FromJSON StepDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "stepDef"

prefixedFieldOptions :: String -> Options
prefixedFieldOptions prefix =
    defaultOptions
        { fieldLabelModifier = \field ->
            map toLower (fromMaybe field (stripPrefix prefix field))
        }

getProjectCertificates :: (Eval :> es, IOE :> es) => Int -> Text -> Eff es (Either String (Map Int Text))
getProjectCertificates pid targetCommit = runExceptT $ do
    let attr = projectCertificateAttr pid
    withExceptT ("Failed to prepare project commit: " ++) $
        ExceptT $
            liftIO $
                runExceptT $
                    ensureRepoCommit $
                        unpack targetCommit
    repoPath <- liftIO userRepoPath
    output <-
        withExceptT (("Failed to evaluate " ++ attr ++ ": ") ++) $
            runNixEvalJsonInRepo
                (ReadRepoContext repoPath $ unpack targetCommit)
                attr
    maybe (throwError $ "Failed to parse " ++ attr) pure $ decodeJson output

scheduleProjectCertificatesWarm :: Int -> Text -> IO ()
scheduleProjectCertificatesWarm pid commit = do
    repoPath <- userRepoPath
    let ctx = ReadRepoContext repoPath $ unpack commit
    void $ forkIO $ runAppEffects $ void $ runExceptT $ runNixEvalJsonInRepoBackground ctx $ projectCertificateAttr pid

warmProjectCertificates :: (Eval :> es, IOE :> es) => Eff es ()
warmProjectCertificates = do
    repoPath <- liftIO userRepoPath
    withReadRepoTransaction (pure . pack . readCommitHash) >>= \case
        Left err -> liftIO $ putStrLn $ "Project certificate warm skipped: " ++ err
        Right commit ->
            runExceptT (warmProjectCertificatesForCommit $ ReadRepoContext repoPath $ unpack commit)
                >>= either (liftIO . putStrLn . ("Project certificate warm failed: " ++)) pure

warmProjectCertificatesForCommit :: (Eval :> es) => ReadRepoContext -> ExceptT String (Eff es) ()
warmProjectCertificatesForCommit ctx = do
    attrs <- ExceptT $ revisionProjectExpressions ctx
    results <- ExceptT $ rewarmRepoJsonExpressions (readRepoSource ctx) $ toList attrs
    forM_ results $ \case
        (Nothing, result) ->
            either (throwError . ("Failed to warm #pointy.projects: " ++)) (const $ pure ()) result
        (Just pid, result) ->
            void $ either throwError pure $ decodeCertificateResult pid result

readRepoSource :: ReadRepoContext -> RepoSource
readRepoSource (ReadRepoContext repoPath commitHash) =
    repoSource $ "git+file://" ++ repoPath ++ "?rev=" ++ commitHash ++ "&allRefs=true"

revisionProjectExpressions :: (Eval :> es) => ReadRepoContext -> Eff es (Either String (NonEmpty (Maybe Int, String)))
revisionProjectExpressions ctx = runExceptT $ do
    projectsRaw <- runNixEvalJsonInRepo ctx "#pointy.projects"
    projectDefs <-
        maybe (throwError "Failed to parse #pointy.projects") pure (decodeJson projectsRaw :: Maybe (Map String ProjectDef))
    pure $ (Nothing, "#pointy.projects") :| [(Just pid, projectCertificateAttr pid) | pid <- map projectDefId $ Map.elems projectDefs]

projectCertificateAttr :: Int -> String
projectCertificateAttr pid = "#pointy.projectCertificates." ++ show pid

decodeCertificateResult :: Int -> Either String String -> Either String (Map Int Text)
decodeCertificateResult pid =
    either (Left . (("Failed to evaluate " ++ attr ++ ": ") ++)) $
        maybe (Left $ "Failed to parse " ++ attr) Right . decodeJson
  where
    attr = projectCertificateAttr pid

decodeJson :: (FromJSON a) => String -> Maybe a
decodeJson = decode . TLE.encodeUtf8 . TL.pack

data WarmState = WarmState
    { warmRunning :: Bool
    , warmPending :: Bool
    }

{-# NOINLINE warmStateRef #-}
warmStateRef :: MVar WarmState
warmStateRef = unsafePerformIO (newMVar (WarmState False False))

scheduleWarm :: IO ()
scheduleWarm =
    modifyMVar_ warmStateRef $ \st ->
        if warmRunning st
            then pure st{warmPending = True}
            else do
                void $ forkIO warmWorker
                pure st{warmRunning = True, warmPending = False}

runWarmSafely :: IO ()
runWarmSafely =
    runAppEffects warmProjectCertificates `catch` handleWarmException

handleWarmException :: SomeException -> IO ()
handleWarmException err =
    putStrLn $ "Project certificate warm crashed: " ++ show err

warmWorker :: IO ()
warmWorker = do
    runWarmSafely
    again <- modifyMVar warmStateRef $ \st ->
        if warmPending st
            then pure (st{warmRunning = True, warmPending = False}, True)
            else pure (st{warmRunning = False, warmPending = False}, False)
    when again warmWorker

withWriteRepoTransaction :: (IOE :> es) => (WriteRepoContext -> ExceptT String (Eff es) a) -> Eff es (Either String a)
withWriteRepoTransaction action = do
    result <- withWriteRepoTransactionRaw action
    when (isRight result) $ liftIO scheduleWarm
    pure result
