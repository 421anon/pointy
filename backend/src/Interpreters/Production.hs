{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Interpreters.Production (runProduction, submitArgs) where

import BuildRunner (shellCommand)
import Config (Config (..), EmbeddingConfig (..), JudgeConfig (..), ProviderConfig (..), SearchConfig (..), loadConfig, resolveConfigPath)
import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, IOE, liftIO, runEff, (:>))
import Effectful.Dispatch.Dynamic (interpret)
import Effects (AppEffects, Eval (..), Inference (..), LogStamp (..), Nix (..), Slurm (..), SlurmQuery (..), SubmitRequest (..))
import Ingest (runIngest)
import Network.HTTP.Types.URI (Query, parseQuery)
import Processes (cli, cliBytes)
import NixEvaluator (defaultNixEvaluator, evaluate, evaluateImpure, jsonAppliedExpression, jsonExpression, rawExpression)
import Search.Http (embedRequest, judgeRequest)
import System.Directory (doesFileExist, getFileSize, getModificationTime)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.IO.Unsafe (unsafePerformIO)


runProduction :: Eff AppEffects a -> IO a
runProduction action = runEff (runInferenceProduction (runSlurmProduction (runNixProduction (runEvalProduction action))))

runInferenceProduction :: (IOE :> es) => Eff (Inference : es) a -> Eff es a
runInferenceProduction = interpret $ \_ -> \case
    Embed texts -> do
        request <- liftIO inferenceEmbedRequest
        case request of
            Left err -> pure (Left err)
            Right (embedding, provider) -> liftIO (embedRequest embedding provider texts)
    JudgeCandidates query candidates -> do
        request <- liftIO inferenceJudgeRequest
        case request of
            Left err -> pure (Left err)
            Right (judge, provider) -> liftIO (judgeRequest judge provider query candidates)

inferenceEmbedRequest :: IO (Either String (EmbeddingConfig, ProviderConfig))
inferenceEmbedRequest = do
    config <- resolveConfigPath >>= loadConfig
    pure $ case configSearch config of
        Just search
            | not (T.null (embeddingProvider (searchEmbedding search))) ->
                case Map.lookup (embeddingProvider (searchEmbedding search)) (configProviders config) of
                    Just provider -> Right (searchEmbedding search, provider)
                    Nothing -> Left ("The embedding provider \"" <> T.unpack (embeddingProvider (searchEmbedding search)) <> "\" is not configured.")
        _ -> Left "Semantic search is not configured."

inferenceJudgeRequest :: IO (Either String (JudgeConfig, ProviderConfig))
inferenceJudgeRequest = do
    config <- resolveConfigPath >>= loadConfig
    pure $ case configSearch config >>= searchJudge of
        Just judge -> case Map.lookup (judgeProvider judge) (configProviders config) of
            Just provider -> Right (judge, provider)
            Nothing -> Left ("The judge provider \"" <> T.unpack (judgeProvider judge) <> "\" is not configured.")
        Nothing -> Left "The judge is not configured."


runEvalProduction :: (IOE :> es) => Eff (Eval : es) a -> Eff es a
runEvalProduction = interpret $ \_ -> \case
    EvalJson source attr -> liftIO $ evaluate defaultNixEvaluator source (jsonExpression attr)
    EvalRaw source attr -> liftIO $ evaluate defaultNixEvaluator source (rawExpression attr)
    EvalJsonApply source applyExpr attr -> liftIO $ evaluate defaultNixEvaluator source (jsonAppliedExpression applyExpr attr)
    EvalImpure expression -> liftIO $ evaluateImpure defaultNixEvaluator expression

runNixProduction :: (IOE :> es) => Eff (Nix : es) a -> Eff es a
runNixProduction = interpret $ \_ -> \case
    RunNixCli args -> liftIO $ cliBytes "nix" args
    RunNixStoreCli args -> liftIO $ cliBytes "nix-store" args
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
    LocalLogStamp drv -> liftIO $ productionLogStamp drv

productionLogStamp :: FilePath -> IO (Maybe LogStamp)
productionLogStamp drv = do
    let base = takeFileName drv
        (prefix, rest) = splitAt 2 base
        plain = productionLogDir </> "drvs" </> prefix </> rest
    compressed <- stampOf (plain <> ".bz2")
    case compressed of
        Just stamp -> return (Just stamp)
        Nothing -> stampOf plain

stampOf :: FilePath -> IO (Maybe LogStamp)
stampOf path = do
    outcome <- try @IOException $ do
        exists <- doesFileExist path
        if exists
            then do
                modified <- getModificationTime path
                size <- getFileSize path
                return (Just (LogStamp modified size))
            else return Nothing
    return $ either (const Nothing) id outcome

{-# NOINLINE productionLogDir #-}
productionLogDir :: FilePath
productionLogDir = unsafePerformIO resolveProductionLogDir

resolveProductionLogDir :: IO FilePath
resolveProductionLogDir = do
    remote <- lookupEnv "NIX_REMOTE"
    case remote >>= remoteLogDir of
        Just directory -> return directory
        Nothing -> fromMaybe "/nix/var/log/nix" <$> lookupEnv "NIX_LOG_DIR"

remoteLogDir :: String -> Maybe FilePath
remoteLogDir remote = case break (== '?') remote of
    (_, '?' : query) -> do
        let params = parseQuery (BC.pack query)
        case queryParam "log" params of
            Just directory -> Just directory
            Nothing -> fmap (\root -> root </> "nix" </> "var" </> "log" </> "nix") (queryParam "root" params)
    _ -> Nothing

queryParam :: BS.ByteString -> Query -> Maybe String
queryParam key params = case lookup key params of
    Just (Just value) | not (BS.null value) -> Just (BC.unpack value)
    _ -> Nothing

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
    AllJobs -> ["-h", "-o", "%i|%j|%k|%T|%M|%r"]

