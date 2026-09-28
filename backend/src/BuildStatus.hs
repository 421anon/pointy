{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BuildStatus (
    StepPaths (..),
    checkStatus,
    markBuiltOutputs,
    partitionImmediateStatuses,
    resolveStatuses,
    resolveStepStatus,
) where

import BuildLog (LogAccess (..), ResolvedLog (..), isStorePath, lastMeaningfulLine, lookupDeriver, resolveBuildLog, validPaths)
import BuildRunner (BuildState (..), buildKeyForOutPath, queryState)
import Control.Monad ((<=<))
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Effectful (Eff, IOE, (:>))
import Effects (Nix, Slurm, pathValid)

data StepPaths = StepPaths
    { stepCertificate :: FilePath
    , stepOutput :: FilePath
    }
    deriving (Eq, Show)

instance FromJSON StepPaths where
    parseJSON = withObject "StepPaths" $ \o -> StepPaths <$> o .: "certificate" <*> o .: "output"

checkStatus :: (Nix :> es, Slurm :> es) => FilePath -> Eff es (Text, Maybe Text)
checkStatus certificate = do
    valid <- pathValid certificate
    if valid
        then return ("success", Nothing)
        else do
            state <- queryState $ buildKeyForOutPath certificate
            return $ case state of
                BRunning -> ("running", Nothing)
                BAbsent -> ("not-started", Nothing)
                BSucceeded -> ("success", Nothing)
                BFailed -> ("failure", Nothing)

isImmediateStatus :: (Text, Maybe Text) -> Bool
isImmediateStatus (state, _) = state == "success" || state == "running"

partitionImmediateStatuses :: Map Int (Text, Maybe Text) -> (Map Int (Text, Maybe Text), Map Int (Text, Maybe Text))
partitionImmediateStatuses = Map.partition isImmediateStatus

resolveStepStatus :: (Nix :> es, IOE :> es) => Maybe FilePath -> (Int, (Text, Maybe Text)) -> Eff es (Int, (Text, Maybe Text))
resolveStepStatus _ entry@(_, status_)
    | isImmediateStatus status_ = return entry
resolveStepStatus Nothing entry = return entry
resolveStepStatus (Just certificate) entry@(sid, (state, _))
    | state == "failure" || state == "not-started" = do
        mDrv <- lookupDeriver certificate
        mResolved <- maybe (return Nothing) (resolveBuildLog LocalLogs) mDrv
        return $ case mResolved of
            Just rl -> (sid, ("failure", lastMeaningfulLine (resolvedLog rl)))
            Nothing -> entry
    | otherwise = return entry

markBuiltOutputs :: (Nix :> es) => Map Int StepPaths -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
markBuiltOutputs paths statuses = do
    known <- validPaths (Map.elems candidates)
    let isBuilt output = maybe (pathValid output) (pure . Set.member output) known
    built <- Map.filter id <$> traverse isBuilt candidates
    return $ Map.union (Map.map markBuilt (Map.intersection statuses built)) statuses
  where
    candidates = Map.mapMaybe id (Map.intersectionWith candidateOutput statuses paths)
    candidateOutput (state, _) (StepPaths certificate output)
        | (state == "not-started" || state == "failure") && isStorePath certificate && output /= certificate = Just output
        | otherwise = Nothing
    markBuilt ("not-started", _) = ("built-not-certified", Nothing)
    markBuilt (_, message) = ("certification-failed", message)

resolveStatuses :: (Nix :> es, IOE :> es) => Map Int StepPaths -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatuses paths = markBuiltOutputs paths <=< Map.traverseWithKey (\sid -> fmap snd . resolveStepStatus (stepCertificate <$> Map.lookup sid paths) . (,) sid)
