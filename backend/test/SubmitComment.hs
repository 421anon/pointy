{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildRunner (BuildKey, JobComment (..), StepRequirements (..), buildKeyForOutPath, decodeJobComment, encodeJobComment, submitAndWait, submitJob)
import Control.Monad (forM_, unless, void)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects (Slurm (..), SubmitRequest)
import Interpreters.Production (submitArgs)
import System.Environment (setEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

main :: IO ()
main = withSystemTempDirectory "submit-comment" $ \dir -> do
    let configPath = dir </> "config.toml"
    writeFile configPath "[user-repo]\nurl = \"git@example.invalid:repo\"\nkeyfile = \"/dev/null\"\nbranch = \"main\"\n"
    setEnv "POINTY_CONFIG_PATH" configPath
    requests <- newIORef []
    void $ recording requests $ submitJob requirements key [] (encodeJobComment comment) ["true"]
    void $ recording requests $ submitAndWait requirements key (encodeJobComment comment) ["true"]
    submitted <- readIORef requests
    assertEqual "submissions" 2 (length submitted)
    forM_ submitted $ \request ->
        assertEqual "comment kept by sbatch" (Just comment) (decodeJobComment =<< keptComment (submitArgs request))

keptComment :: [String] -> Maybe String
keptComment = listToMaybe . reverse . mapMaybe (stripPrefix "--comment=")

recording :: IORef [SubmitRequest] -> Eff '[Slurm, IOE] a -> IO a
recording requests =
    runEff . interpret (\_ -> \case
        SubmitJob request -> liftIO (modifyIORef' requests (request :)) >> pure (Right "1")
        QuerySlurm _ -> pure (Right "")
        CancelJob _ -> pure ()
        ClusterAvailability -> pure (Right "up\n"))

outPath :: FilePath
outPath = "/nix/store/00000000000000000000000000000000-pointy-certificate-172"

key :: BuildKey
key = buildKeyForOutPath outPath

comment :: JobComment
comment = JobComment "step" 172 "f1c5ec2080eeed56e424178207fd8310081de368" outPath

requirements :: StepRequirements
requirements = StepRequirements{ram = "4G", cpu = 2, ior = "0", iow = "0"}

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (actual == expected) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
