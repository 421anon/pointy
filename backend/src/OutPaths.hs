{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module OutPaths (
    getProjectCertificates,
    getStepCertificate,
    warmProjectCertificates,
    warmProjectCertificatesForCommit,
    scheduleProjectCertificatesWarm,
    evalProjectDefinitions,
    evalProjectDefinition,
    decodeProjectDefinitions,
    withWriteRepoTransaction,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import BuildLog (validPaths)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch)
import Control.Monad (forM_, join, void, when)
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, eitherDecode, genericParseJSON)
import Data.Char (toLower)
import Data.Either (isRight)
import Data.List (stripPrefix)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import EffectRunner (runAppEffects)
import Effectful (Eff, IOE, (:>))
import Effects (Eval, Nix, rootStorePath)
import GHC.Generics (Generic)
import NixEvaluator (RepoSource, repoSource)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), RepoContext, WriteRepoContext, ensureRepoCommit, rewarmRepoJsonExpressions, runNixEvalJsonApplyInRepo, runNixEvalJsonApplyInRepoBackground, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

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
    let attr = projectAttr pid
    withExceptT ("Failed to prepare project commit: " ++) $
        ExceptT $
            liftIO $
                runExceptT $
                    ensureRepoCommit $
                        unpack targetCommit
    repoPath <- liftIO userRepoPath
    output <-
        withExceptT (("Failed to evaluate " ++ attr ++ ": ") ++) $
            runNixEvalJsonApplyInRepo
                (ReadRepoContext repoPath $ unpack targetCommit)
                certificatesOrLegacyOutPaths
                attr
    maybe (throwError $ "Failed to parse " ++ attr) pure $ decodeJson output

getStepCertificate :: (Eval :> es, IOE :> es) => Int -> Text -> Eff es (Either String (Maybe Text))
getStepCertificate sid targetCommit = runExceptT $ do
    let attr = stepsAttr ++ "." ++ show sid ++ ".certificate"
    withExceptT ("Failed to prepare step commit: " ++) $
        ExceptT $
            liftIO $
                runExceptT $
                    ensureRepoCommit $
                        unpack targetCommit
    repoPath <- liftIO userRepoPath
    output <-
        withExceptT (("Failed to evaluate " ++ attr ++ ": ") ++) $
            runNixEvalJsonApplyInRepo
                (ReadRepoContext repoPath $ unpack targetCommit)
                stepCertificateExpression
                stepsAttr
    maybe (throwError $ "Failed to parse " ++ attr) (pure . join . listToMaybe) (decodeJson output :: Maybe [Maybe Text])
  where
    stepCertificateExpression =
        "steps: map (name: let path = builtins.tryEval (builtins.unsafeDiscardStringContext (toString (steps.${name}.certificate or steps.${name}).outPath)); in if path.success then path.value else null) [ "
            ++ show (show sid)
            ++ " ]"

scheduleProjectCertificatesWarm :: Int -> Text -> IO ()
scheduleProjectCertificatesWarm pid commit = do
    repoPath <- userRepoPath
    let ctx = ReadRepoContext repoPath $ unpack commit
    void $ forkIO $ runAppEffects $ void $ runExceptT $ runNixEvalJsonApplyInRepoBackground ctx certificatesOrLegacyOutPaths $ projectAttr pid

warmProjectCertificates :: (Eval :> es, Nix :> es, IOE :> es) => Eff es ()
warmProjectCertificates = do
    repoPath <- liftIO userRepoPath
    withReadRepoTransaction (pure . pack . readCommitHash) >>= \case
        Left err -> liftIO $ putStrLn $ "Project certificate warm skipped: " ++ err
        Right commit ->
            runExceptT (warmProjectCertificatesForCommit $ ReadRepoContext repoPath $ unpack commit)
                >>= either (liftIO . putStrLn . ("Project certificate warm failed: " ++)) pure

warmProjectCertificatesForCommit :: (Eval :> es, Nix :> es, IOE :> es) => ReadRepoContext -> ExceptT String (Eff es) ()
warmProjectCertificatesForCommit ctx = do
    expressions <- ExceptT $ revisionProjectExpressions ctx
    results <- ExceptT $ rewarmRepoJsonExpressions (readRepoSource ctx) $ toList expressions
    certificates <- Set.unions <$> traverse projectCertificates results
    known <- lift $ validPaths (map unpack $ Set.toList certificates)
    forM_ (maybe [] Set.toList known) (lift . rootStorePath)
  where
    projectCertificates = \case
        (Nothing, result) ->
            either (throwError . (("Failed to warm " ++ projectsAttr ++ ": ") ++)) (const $ pure Set.empty) result
        (Just pid, result) ->
            Set.fromList . Map.elems <$> either throwError pure (decodeCertificateResult pid result)

readRepoSource :: ReadRepoContext -> RepoSource
readRepoSource (ReadRepoContext repoPath commitHash) =
    repoSource $ "git+file://" ++ repoPath ++ "?rev=" ++ commitHash ++ "&allRefs=true"

revisionProjectExpressions :: (Eval :> es) => ReadRepoContext -> Eff es (Either String (NonEmpty (Maybe Int, String, String)))
revisionProjectExpressions ctx = runExceptT $ do
    projectDefs <- ExceptT . pure . decodeProjectDefinitions =<< evalProjectDefinitions ctx
    pure $
        (Nothing, projectDefinitions, projectsAttr)
            :| [(Just pid, certificatesOrLegacyOutPaths, projectAttr pid) | pid <- map projectDefId $ Map.elems projectDefs]

evalProjectDefinitions :: (RepoContext ctx, Eval :> es) => ctx -> ExceptT String (Eff es) String
evalProjectDefinitions ctx = runNixEvalJsonApplyInRepo ctx projectDefinitions projectsAttr

evalProjectDefinition :: (RepoContext ctx, Eval :> es) => ctx -> Int -> ExceptT String (Eff es) String
evalProjectDefinition ctx pid = runNixEvalJsonApplyInRepo ctx projectDefinition (projectAttr pid)

decodeProjectDefinitions :: String -> Either String (Map String ProjectDef)
decodeProjectDefinitions = either (Left . (("Failed to parse " ++ projectsAttr ++ ": ") ++)) Right . eitherDecode . TLE.encodeUtf8 . TL.pack

projectsAttr :: String
projectsAttr = "#pointy.projects"

stepsAttr :: String
stepsAttr = "#pointy.steps"

projectAttr :: Int -> String
projectAttr pid = projectsAttr ++ "." ++ show pid

projectDefinitions :: String
projectDefinitions = "builtins.mapAttrs (_: " ++ projectDefinition ++ ")"

projectDefinition :: String
projectDefinition = "project: builtins.removeAttrs project [ \"outPaths\" \"certificates\" ]"

certificatesOrLegacyOutPaths :: String
certificatesOrLegacyOutPaths = "project: project.certificates or project.outPaths"

decodeCertificateResult :: Int -> Either String String -> Either String (Map Int Text)
decodeCertificateResult pid =
    either (Left . (("Failed to evaluate " ++ attr ++ ": ") ++)) $
        maybe (Left $ "Failed to parse " ++ attr) Right . decodeJson
  where
    attr = projectAttr pid

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
