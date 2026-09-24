{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effects (
    AppEffects,
    App,
    AppM,
    Eval (..),
    Nix (..),
    Slurm (..),
    SubmitRequest (..),
    SlurmQuery (..),
    toHandler,
    evalJson,
    evalRaw,
    evalJsonApply,
    evalImpure,
    rewarm,
    runNixCli,
    runNixStoreCli,
    pathValid,
    registerGcRoot,
    addFixed,
    probeMimeType,
    submitJob,
    querySlurm,
    cancelJob,
    clusterAvailability,
) where

import Control.Monad.Except (ExceptT, MonadError, mapExceptT)
import Data.Text (Text)
import System.Exit (ExitCode)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, MonadIO, type (:>))
import Effectful.Dispatch.Dynamic (send)
import NixEvaluator (EvalPriority, RepoSource)
import Servant.Server (ServerError)
import Servant.Server.Internal.Handler (Handler (..))

type AppEffects = '[Eval, Nix, Slurm, IOE]

type App es = (Eval :> es, Nix :> es, Slurm :> es, IOE :> es)

type AppM = ExceptT ServerError (Eff AppEffects)

toHandler :: (forall x. Eff AppEffects x -> IO x) -> AppM a -> Handler a
toHandler runEffects = Handler . mapExceptT runEffects

data Eval :: Effect where
    EvalJson :: RepoSource -> String -> Eval m (Either String String)
    EvalRaw :: RepoSource -> String -> Eval m (Either String String)
    EvalJsonApply :: EvalPriority -> RepoSource -> String -> String -> Eval m (Either String String)
    EvalImpure :: String -> Eval m (Either String String)
    Rewarm :: RepoSource -> [(Maybe Int, String, String)] -> Eval m (Either String [(Maybe Int, Either String String)])

type instance DispatchOf Eval = Dynamic

data Nix :: Effect where
    RunNixCli :: [String] -> Nix m (ExitCode, String, String)
    RunNixStoreCli :: [String] -> Nix m (ExitCode, String, String)
    PathValid :: FilePath -> Nix m Bool
    RegisterGcRoot :: FilePath -> FilePath -> Nix m ()
    AddFixed :: FilePath -> Nix m (Either String String)
    ProbeMimeType :: FilePath -> Nix m (Maybe Text)

type instance DispatchOf Nix = Dynamic

data Slurm :: Effect where
    SubmitJob :: SubmitRequest -> Slurm m (Either String String)
    QuerySlurm :: SlurmQuery -> Slurm m (Either String String)
    CancelJob :: String -> Slurm m ()
    ClusterAvailability :: Slurm m (Either String String)

type instance DispatchOf Slurm = Dynamic

data SubmitRequest = SubmitRequest
    { submitJobName :: String
    , submitComment :: String
    , submitOptions :: [String]
    , submitDependencies :: [String]
    , submitCommand :: [String]
    , submitWait :: Bool
    }

data SlurmQuery
    = JobStatesByName String
    | JobIdsByName String
    | AllJobs

evalJson :: (Eval :> es) => RepoSource -> String -> Eff es (Either String String)
evalJson source attr = send (EvalJson source attr)

evalRaw :: (Eval :> es) => RepoSource -> String -> Eff es (Either String String)
evalRaw source attr = send (EvalRaw source attr)

evalJsonApply :: (Eval :> es) => EvalPriority -> RepoSource -> String -> String -> Eff es (Either String String)
evalJsonApply priority source applyExpr attr = send (EvalJsonApply priority source applyExpr attr)

evalImpure :: (Eval :> es) => String -> Eff es (Either String String)
evalImpure = send . EvalImpure

rewarm :: (Eval :> es) => RepoSource -> [(Maybe Int, String, String)] -> Eff es (Either String [(Maybe Int, Either String String)])
rewarm source attrs = send (Rewarm source attrs)

runNixCli :: (Nix :> es) => [String] -> Eff es (ExitCode, String, String)
runNixCli = send . RunNixCli

runNixStoreCli :: (Nix :> es) => [String] -> Eff es (ExitCode, String, String)
runNixStoreCli = send . RunNixStoreCli

pathValid :: (Nix :> es) => FilePath -> Eff es Bool
pathValid = send . PathValid

registerGcRoot :: (Nix :> es) => FilePath -> FilePath -> Eff es ()
registerGcRoot gcRootPath outPath = send (RegisterGcRoot gcRootPath outPath)

addFixed :: (Nix :> es) => FilePath -> Eff es (Either String String)
addFixed = send . AddFixed

probeMimeType :: (Nix :> es) => FilePath -> Eff es (Maybe Text)
probeMimeType = send . ProbeMimeType

submitJob :: (Slurm :> es) => SubmitRequest -> Eff es (Either String String)
submitJob = send . SubmitJob

querySlurm :: (Slurm :> es) => SlurmQuery -> Eff es (Either String String)
querySlurm = send . QuerySlurm

cancelJob :: (Slurm :> es) => String -> Eff es ()
cancelJob = send . CancelJob

clusterAvailability :: (Slurm :> es) => Eff es (Either String String)
clusterAvailability = send ClusterAvailability
