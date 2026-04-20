{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepLastSuccess (getStepLastSuccessesHandler, StepLastSuccess (..)) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import GHC.Generics (Generic)
import OutPaths (getProjectOutPaths)
import ProcessLimiter (readProcessWithExitCodeL)
import Servant (Handler, err500, errBody, throwError)
import System.Exit (ExitCode (..))
import UserRepo (ReadRepoContext (..), runGitIn, withReadRepoTransaction)

data StepLastSuccess = StepLastSuccess
    { commit :: Text
    , outPath :: Text
    }
    deriving (Generic, ToJSON)

getStepLastSuccessesHandler :: Int -> Int -> Maybe Text -> Handler [StepLastSuccess]
getStepLastSuccessesHandler sid pid mCommit = do
    result <- liftIO $ withReadRepoTransaction $ \(ReadRepoContext repoPath commitHash) -> do
        let startCommit = maybe commitHash unpack mCommit
        liftIO $ findAllSuccesses repoPath pid sid startCommit
    case result of
        Left err -> throwError err500{errBody = TLE.encodeUtf8 (TL.pack err)}
        Right found -> return found

findAllSuccesses :: FilePath -> Int -> Int -> String -> IO [StepLastSuccess]
findAllSuccesses repoPath pid sid startCommit = do
    (ec, out, _) <-
        runGitIn repoPath ["log", "--pretty=%H", startCommit, "--", "steps/*.nix", "templates/*.nix"]
    case ec of
        ExitFailure _ -> return []
        ExitSuccess -> walk (lines out) [] Set.empty
  where
    walk :: [String] -> [StepLastSuccess] -> Set Text -> IO [StepLastSuccess]
    walk [] acc _ = return (reverse acc)
    walk (c : cs) acc seen = do
        outPaths <- getProjectOutPaths pid (pack c)
        case Map.lookup sid outPaths of
            Nothing -> walk cs acc seen
            Just p
                | Set.member p seen -> walk cs acc seen
                | otherwise -> do
                    built <- isBuilt (unpack p)
                    let seen' = Set.insert p seen
                    if built
                        then walk cs (StepLastSuccess (pack c) p : acc) seen'
                        else walk cs acc seen'

isBuilt :: FilePath -> IO Bool
isBuilt p = do
    (ec, _, _) <- readProcessWithExitCodeL "nix" ["path-info", p] ""
    return $ ec == ExitSuccess
