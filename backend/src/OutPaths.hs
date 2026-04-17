{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OutPaths (
    getProjectOutPaths,
    cacheProjectOutPaths,
    withWriteRepoTransaction,
    ProjectDef (..),
    StepRef (..),
    StepDef (..),
) where

import Cache (memoizeStepOutPaths)
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Aeson (FromJSON (..), Options (fieldLabelModifier), decode, defaultOptions, genericParseJSON)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import UserRepo (ReadRepoContext (..), WriteRepoContext, runNixInRepo, userRepoPath, withReadRepoTransaction, withWriteRepoTransactionRaw)

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

getProjectOutPaths :: Int -> Text -> IO (Map Int Text)
getProjectOutPaths pid targetCommit =
    memoizeStepOutPaths pid targetCommit $ do
        repoPath <- userRepoPath
        result <-
            runExceptT $
                runNixInRepo
                    (ReadRepoContext repoPath (unpack targetCommit))
                    ["eval", "--json"]
                    ("#trotter.projectOutPaths." ++ show pid)
        case result of
            Left _ -> return Map.empty
            Right output ->
                return $
                    fromMaybe Map.empty $
                        decode (TLE.encodeUtf8 (TL.pack output))

-- Cache warming

cacheProjectOutPaths :: IO ()
cacheProjectOutPaths = do
    repoPath <- userRepoPath
    mTargetCommit <- withReadRepoTransaction $ \(ReadRepoContext _ hash) ->
        return $ pack hash
    case mTargetCommit of
        Left _ -> return ()
        Right targetCommit -> do
            result <-
                runExceptT $
                    runNixInRepo
                        (ReadRepoContext repoPath (unpack targetCommit))
                        ["eval", "--json"]
                        "#trotter.projects"
            case result of
                Left _ -> return ()
                Right output ->
                    case decode (TLE.encodeUtf8 (TL.pack output)) :: Maybe (Map String ProjectDef) of
                        Nothing -> return ()
                        Just projects -> do
                            let pids = [projectDefId p | p <- Map.elems projects, not (projectDefHidden p)]
                            void $ mapConcurrently (\pid -> getProjectOutPaths pid targetCommit) pids

-- Write transaction with post-write cache warming

withWriteRepoTransaction :: (WriteRepoContext -> ExceptT String IO a) -> IO (Either String a)
withWriteRepoTransaction action = do
    result <- withWriteRepoTransactionRaw action
    case result of
        Right _ -> void $ forkIO cacheProjectOutPaths
        Left _ -> return ()
    return result
