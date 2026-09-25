{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters.Fixture (
    FixtureState (..),
    FixtureJob (..),
    newFixtureState,
    resetFixture,
    runFixture,


) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects (AppEffects, Eval (..), Nix (..), Slurm (..), SlurmQuery (..), SubmitRequest (..))
import Fixture.Document (FixtureDocument (..), appliedAnswer, derivationAnswer, jsonAnswer, logAnswer, pseudoHash, rawAnswer)
import JobWatch (markJobsEnded)
import Processes (cli)

import System.Directory (doesPathExist)
import System.Exit (ExitCode (..))

data FixtureState = FixtureState
    { fixtureDocument :: FixtureDocument
    , fixtureJobs :: TVar (Map.Map String FixtureJob)
    , fixtureNextJob :: TVar Int
    }

data FixtureJob = FixtureJob
    { jobId :: String
    , jobName :: String
    , jobComment :: String
    , jobState :: String
    }

newFixtureState :: FixtureDocument -> IO FixtureState
newFixtureState document =
    FixtureState document
        <$> newTVarIO Map.empty
        <*> newTVarIO 1

resetFixture :: FixtureState -> IO ()
resetFixture state = do
    names <- atomically $ do
        jobs <- readTVar (fixtureJobs state)
        writeTVar (fixtureJobs state) Map.empty
        writeTVar (fixtureNextJob state) 1
        pure (Map.keys jobs)
    markJobsEnded names

runFixture :: FixtureState -> Eff AppEffects a -> IO a
runFixture state action =
    runEff (runSlurmFixture state (runNixFixture state (runEvalFixture state action)))

runEvalFixture :: (IOE :> es) => FixtureState -> Eff (Eval : es) a -> Eff es a
runEvalFixture state = interpret $ \_ -> \case
    EvalJson _ attr -> pure (jsonAnswer (fixtureDocument state) attr)
    EvalRaw _ attr -> pure (rawAnswer (fixtureDocument state) attr)
    EvalJsonApply _ _ applyExpr attr -> pure (appliedAnswer (fixtureDocument state) applyExpr attr)
    EvalImpure expression -> liftIO $ evalNixJson state expression
    Rewarm _ expressions -> pure $ Right [(key, appliedAnswer (fixtureDocument state) applyExpr attr) | (key, applyExpr, attr) <- expressions]


evalNixJson :: FixtureState -> String -> IO (Either String String)
evalNixJson state expression = do
    (code, stdout, stderr) <- cli "nix" ["--extra-experimental-features", "nix-command", "eval", "--impure", "--json", "--expr", expression]
    pure $ case code of
        ExitSuccess -> Right stdout
        ExitFailure _ -> Left (trim stderr)
  where
    trim = T.unpack . T.strip . T.pack

runNixFixture :: (IOE :> es) => FixtureState -> Eff (Nix : es) a -> Eff es a
runNixFixture state = interpret $ \_ -> \case
    PathValid path -> liftIO $ do
        exists <- doesPathExist path
        pure (exists || path `elem` documentValidPaths (fixtureDocument state))
    RunNixCli args -> liftIO $ do
        answerNix (fixtureDocument state) args
    RunNixStoreCli args -> pure $ case args of
        ["--query", "--references", drv] -> storePathsAnswer "references" (documentReferences (fixtureDocument state)) drv
        ["--query", "--outputs", drv] -> storePathsAnswer "outputs" (documentOutputs (fixtureDocument state)) drv
        _ -> (ExitFailure 1, "", "fixture: unsupported nix-store invocation: " ++ unwords args)
    RegisterGcRoot _ _ -> pure ()
    IngestDirectory _ _ _ -> pure (Left "fixture: the ingest program is not available")
    ProbeMimeType path -> liftIO $ do
        (code, stdout, _) <- cli "file" ["-b", "-L", "--mime-type", path]
        pure $ case code of
            ExitSuccess -> Just (T.strip (T.pack stdout))
            ExitFailure _ -> Nothing

answerNix :: FixtureDocument -> [String] -> IO (ExitCode, String, String)
answerNix document args = case args of
    ["log", drv] -> logFor drv
    ["--offline", "log", drv] -> logFor drv
    ["path-info", "--derivation", path] -> pure $ case derivationAnswer document path of
        Just drv -> (ExitSuccess, drv ++ "\n", "")
        Nothing -> (ExitFailure 1, "", "fixture: no derivation recorded for " ++ path)
    ("--offline" : "path-info" : "--json" : paths) | not (null paths) ->
        pure (ExitSuccess, pathInfoJson document paths, "")
    _ -> pure (ExitFailure 1, "", "fixture: unsupported nix invocation: " ++ unwords args)
  where
    logFor drv = do
        recorded <- logAnswer document drv
        pure $ case recorded of
            Just log -> (ExitSuccess, log, "")
            Nothing -> (ExitFailure 1, "", "fixture: no build log recorded for " ++ drv)

storePathsAnswer :: String -> Map.Map FilePath [FilePath] -> FilePath -> (ExitCode, String, String)
storePathsAnswer label recorded drv = case Map.lookup drv recorded of
    Just paths -> (ExitSuccess, unlines paths, "")
    Nothing -> (ExitFailure 1, "", "fixture: no " ++ label ++ " recorded for " ++ drv)

pathInfoJson :: FixtureDocument -> [FilePath] -> String
pathInfoJson document paths =
    T.unpack . TE.decodeUtf8 . LBS.toStrict . A.encode $
        object [Key.fromText (T.pack path) .= validity path | path <- paths]
  where
    validity path
        | path `elem` documentValidPaths document = object ["narHash" .= pseudoHash path, "valid" .= True]
        | otherwise = A.Null


runSlurmFixture :: (IOE :> es) => FixtureState -> Eff (Slurm : es) a -> Eff es a
runSlurmFixture state = interpret $ \_ -> \case
    SubmitJob request -> liftIO $ do
        generated <- atomically $ do
            next <- readTVar (fixtureNextJob state)
            writeTVar (fixtureNextJob state) (next + 1)
            let jobId_ = show (1000 + next)
            modifyTVar' (fixtureJobs state) $
                Map.insert
                    (submitJobName request)
                    FixtureJob
                        { jobId = jobId_
                        , jobName = submitJobName request
                        , jobComment = submitComment request
                        , jobState = "RUNNING"
                        }
            pure jobId_
        pure (Right generated)
    QuerySlurm query -> liftIO $ do
        jobs <- Map.elems <$> readTVarIO (fixtureJobs state)
        pure $
            Right $
                case query of
                    JobStatesByName name -> unlines [jobState job | job <- jobs, jobName job == name]
                    JobIdsByName name -> unlines [jobId job | job <- jobs, jobName job == name]
                    AllJobs -> unlines [intercalate "|" [jobId job, jobName job, jobComment job, jobState job] | job <- jobs]
    CancelJob key -> liftIO $ do
        atomically $ modifyTVar' (fixtureJobs state) (Map.delete key)
        markJobsEnded [key]
    ClusterAvailability -> pure (Right "pointy*|up|node1|idle\n")
