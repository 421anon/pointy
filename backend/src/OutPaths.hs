{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OutPaths (
    getProjectOutPaths,
    warmProjectOutPaths,
    warmProjectOutPathsForCommit,
    withWriteRepoTransaction,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import NixRepl (restartNixReplSessions)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), WriteRepoContext, runNixEvalJsonInRepo, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

-- Types

data ProjectDef = ProjectDef
    { projectDefId :: Int
    , projectDefHidden :: Bool
    , projectDefSteps :: [StepRef]
    }
    deriving (Show, Generic)

instance FromJSON ProjectDef where
    parseJSON = genericParseJSON $ prefixedFieldOptions "projectDef"

data StepRef = StepRef
    { stepRefDef :: StepDef
    , stepRefHidden :: Bool
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

-- OutPath evaluation

getProjectOutPaths :: Int -> Text -> IO (Either String (Map Int Text))
getProjectOutPaths pid targetCommit = do
    repoPath <- userRepoPath
    result <-
        runExceptT $
            runNixEvalJsonInRepo
                (ReadRepoContext repoPath (unpack targetCommit))
                ("#pointy.projectOutPaths." ++ show pid)
    return $ case result of
        Left err -> Left $ "Failed to evaluate #pointy.projectOutPaths." ++ show pid ++ ": " ++ err
        Right output ->
            case decode (TLE.encodeUtf8 (TL.pack output)) of
                Nothing -> Left $ "Failed to parse #pointy.projectOutPaths." ++ show pid
                Just paths -> Right paths

-- REPL warming

{-# NOINLINE lastWarmedHeadCommitRef #-}
lastWarmedHeadCommitRef :: MVar (Maybe String)
lastWarmedHeadCommitRef = unsafePerformIO (newMVar Nothing)

restartReplIfHeadChanged :: String -> IO ()
restartReplIfHeadChanged targetCommit =
    modifyMVar_ lastWarmedHeadCommitRef $ \previous -> do
        case previous of
            Just oldCommit | oldCommit /= targetCommit -> do
                putStrLn $ "User repo HEAD changed from " ++ oldCommit ++ " to " ++ targetCommit ++ "; restarting nix REPL sessions before warming project out paths."
                restartNixReplSessions
            _ -> return ()
        return (Just targetCommit)

warmProjectOutPaths :: IO ()
warmProjectOutPaths = do
    repoPath <- userRepoPath
    mTargetCommit <- withReadRepoTransaction $ \(ReadRepoContext _ hash) ->
        return $ pack hash
    case mTargetCommit of
        Left err -> putStrLn $ "Project outPath warm skipped: " ++ err
        Right targetCommit -> do
            let targetCommitString = unpack targetCommit
            restartReplIfHeadChanged targetCommitString
            result <- runExceptT $ warmProjectOutPathsForCommit (ReadRepoContext repoPath targetCommitString)
            case result of
                Left err -> putStrLn $ "Project outPath warm failed: " ++ err
                Right () -> return ()

warmProjectOutPathsForCommit :: ReadRepoContext -> ExceptT String IO ()
warmProjectOutPathsForCommit ctx = do
    _ <- runNixEvalJsonInRepo ctx "#pointy.projectOutPaths"
    return ()

-- Write transaction with post-write REPL warming

withWriteRepoTransaction :: (WriteRepoContext -> ExceptT String IO a) -> IO (Either String a)
withWriteRepoTransaction action = do
    result <- withWriteRepoTransactionRaw action
    case result of
        Right _ -> void $ forkIO warmProjectOutPaths
        Left _ -> return ()
    return result
