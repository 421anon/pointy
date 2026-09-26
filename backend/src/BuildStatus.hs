{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BuildStatus (
    checkStatus,
    isImmediateStatus,
    partitionImmediateStatuses,
    resolveStepStatus,
) where

import BuildLog (ResolvedLog (..), lastMeaningfulLine, lookupDeriver, resolveBuildLog)
import BuildRunner (BuildState (..), buildKeyForOutPath, queryState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Effectful (Eff, IOE, (:>))
import Effectful.Exception (catch)
import Effects (Nix, Slurm, pathValid)

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
        mResolved <- maybe (return Nothing) resolveBuildLog mDrv
        return $ case mResolved of
            Just rl -> (sid, ("failure", lastMeaningfulLine (resolvedLog rl)))
            Nothing -> entry
    | otherwise = return entry
