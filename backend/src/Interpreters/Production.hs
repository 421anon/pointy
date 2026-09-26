{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters.Production (runProduction, submitArgs) where

import BuildRunner (shellCommand)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects (AppEffects, Eval (..), Nix (..), Slurm (..), SlurmQuery (..), SubmitRequest (..))
import Ingest (runIngest)
import Processes (cli)
import NixEvaluator (defaultNixEvaluator, evaluate, evaluateImpure, jsonAppliedExpression, jsonExpression, rawExpression)
import System.Exit (ExitCode (..))


runProduction :: Eff AppEffects a -> IO a
runProduction action = runEff (runSlurmProduction (runNixProduction (runEvalProduction action)))


runEvalProduction :: (IOE :> es) => Eff (Eval : es) a -> Eff es a
runEvalProduction = interpret $ \_ -> \case
    EvalJson source attr -> liftIO $ evaluate defaultNixEvaluator source (jsonExpression attr)
    EvalRaw source attr -> liftIO $ evaluate defaultNixEvaluator source (rawExpression attr)
    EvalJsonApply source applyExpr attr -> liftIO $ evaluate defaultNixEvaluator source (jsonAppliedExpression applyExpr attr)
    EvalImpure expression -> liftIO $ evaluateImpure defaultNixEvaluator expression

runNixProduction :: (IOE :> es) => Eff (Nix : es) a -> Eff es a
runNixProduction = interpret $ \_ -> \case
    RunNixCli args -> liftIO $ cli "nix" args
    RunNixStoreCli args -> liftIO $ cli "nix-store" args
    PathValid path -> do
        (code, _, _) <- liftIO $ cli "nix" ["--offline", "path-info", path]
        pure (code == ExitSuccess)
    RegisterGcRoot gcRootPath outPath -> do
        _ <- liftIO $ cli "nix-store" ["--add-root", gcRootPath, "--realise", outPath]
        pure ()
    IngestDirectory directory name reportProgress -> liftIO $ runIngest directory name reportProgress
    ProbeMimeType path -> do
        (code, stdout, _) <- liftIO $ cli "file" ["-b", "-L", "--mime-type", path]
        pure $ case code of
            ExitSuccess -> Just (T.strip (T.pack stdout))
            ExitFailure _ -> Nothing

runSlurmProduction :: (IOE :> es) => Eff (Slurm : es) a -> Eff es a
runSlurmProduction = interpret $ \_ -> \case
    SubmitJob request -> do
        (code, stdout, stderr) <- liftIO $ cli "sbatch" (submitArgs request)
        pure $ case code of
            ExitSuccess -> Right (takeWhile (/= ';') (takeWhile (/= '\n') stdout))
            ExitFailure _ -> Left ("sbatch failed (exit " ++ show code ++ "): " ++ stderr)
    QuerySlurm query -> do
        (code, stdout, stderr) <- liftIO $ cli "squeue" (queryArgs query)
        pure $ case code of
            ExitSuccess -> Right stdout
            ExitFailure _ -> Left ("squeue failed: " ++ stderr)
    CancelJob key -> do
        _ <- liftIO $ cli "scancel" ["--name=" ++ key]
        pure ()
    ClusterAvailability -> do
        (code, stdout, stderr) <- liftIO $ cli "sinfo" ["-h", "-N", "-o", "%P|%a|%N|%T"]
        pure $ case code of
            ExitSuccess -> Right stdout
            ExitFailure _ -> Left stderr

submitArgs :: SubmitRequest -> [String]
submitArgs request =
    ["--parsable", "--job-name=" ++ submitJobName request, "--comment=" ++ submitComment request, "--output=/dev/null", "--error=/dev/null"]
        ++ ["--wait" | submitWait request]
        ++ dependencyArgs (submitDependencies request)
        ++ submitOptions request
        ++ ["--wrap=" ++ shellCommand (submitCommand request)]

dependencyArgs :: [String] -> [String]
dependencyArgs [] = []
dependencyArgs jobIds = ["--dependency=afterok:" ++ intercalate ":" jobIds, "--kill-on-invalid-dep=yes"]

queryArgs :: SlurmQuery -> [String]
queryArgs = \case
    JobStatesByName name -> ["-h", "-n", name, "-o", "%T"]
    JobIdsByName name -> ["-h", "-n", name, "-o", "%i"]
    AllJobs -> ["-h", "-o", "%i|%j|%k|%T"]

