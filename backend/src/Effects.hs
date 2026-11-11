{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effects (
    AppEffects,
    App,
    AppM,
    Eval (..),
    Nix (..),
    LogStamp (..),
    Slurm (..),
    ColdStorage (..),
    Inference (..),
    SubmitRequest (..),
    SlurmQuery (..),
    toHandler,
    evalJson,
    evalRaw,
    evalJsonApply,
    evalImpure,
    runNixCli,
    runNixStoreCli,
    localLogStamp,
    pathValid,
    registerGcRoot,
    rootStorePath,
    rootDerivationForJob,
    unrootDerivationRoot,
    unrootDerivationRootsForJob,
    listDerivationRootJobs,
    uniquePath,
    ingestDirectory,
    probeMimeType,
    submitJob,
    querySlurm,
    cancelJob,
    clusterAvailability,
    coldStorageConfigured,
    coldStorageStatus,
    coldStoragePrefetch,
    embedTexts,
    judgeCandidates,
) where

import Control.Exception (IOException, displayException, try)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector.Unboxed as VU
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getHomeDirectory, listDirectory, removeFile, removePathForcibly)
import System.Exit (ExitCode)
import System.FilePath (takeFileName, (</>))
import System.IO (hClose, openTempFile)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Dynamic (send)
import Ingest (IngestResult)
import NixEvaluator (RepoSource)
import ProviderHttp (JudgeTask)
import Servant.Server (ServerError)
import Servant.Server.Internal.Handler (Handler (..))

type AppEffects = '[Eval, Nix, Slurm, ColdStorage, Inference, IOE]

type App es = (Eval :> es, Nix :> es, Slurm :> es, ColdStorage :> es, Inference :> es, IOE :> es)

type AppM = ExceptT ServerError (Eff AppEffects)

toHandler :: (forall x. Eff AppEffects x -> IO x) -> AppM a -> Handler a
toHandler runEffects = Handler . mapExceptT runEffects

data Eval :: Effect where
    EvalJson :: RepoSource -> String -> Eval m (Either String String)
    EvalRaw :: RepoSource -> String -> Eval m (Either String String)
    EvalJsonApply :: RepoSource -> String -> String -> Eval m (Either String String)
    EvalImpure :: String -> Eval m (Either String String)

type instance DispatchOf Eval = Dynamic

data Nix :: Effect where
    RunNixCli :: [String] -> Nix m (ExitCode, ByteString, ByteString)
    RunNixStoreCli :: [String] -> Nix m (ExitCode, ByteString, ByteString)
    PathValid :: FilePath -> Nix m Bool
    RegisterGcRoot :: FilePath -> FilePath -> Nix m ()
    RegisterDerivationRoot :: FilePath -> FilePath -> Nix m (Either String ())
    IngestDirectory :: FilePath -> String -> (Integer -> Integer -> IO ()) -> Nix m (Either String IngestResult)
    ProbeMimeType :: FilePath -> Nix m (Maybe Text)
    LocalLogStamp :: FilePath -> Nix m (Maybe LogStamp)

type instance DispatchOf Nix = Dynamic

data LogStamp = LogStamp
    { logStampModified :: UTCTime
    , logStampSize :: Integer
    }
    deriving (Eq, Show)

data Slurm :: Effect where
    SubmitJob :: SubmitRequest -> Slurm m (Either String String)
    QuerySlurm :: SlurmQuery -> Slurm m (Either String String)
    CancelJob :: String -> Slurm m ()
    ClusterAvailability :: Slurm m (Either String String)

type instance DispatchOf Slurm = Dynamic

data ColdStorage :: Effect where
    ColdStorageConfigured :: ColdStorage m Bool
    ColdStorageStatus :: Text -> ColdStorage m (Either Text ByteString)
    ColdStoragePrefetch :: Text -> FilePath -> ColdStorage m (Either Text ByteString)

type instance DispatchOf ColdStorage = Dynamic

data Inference :: Effect where
    Embed :: [Text] -> Inference m (Either String [VU.Vector Float])
    JudgeCandidates :: JudgeTask -> Inference m (Either String (Map Int Double))

type instance DispatchOf Inference = Dynamic

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
    | AllAccountingJobs

evalJson :: (Eval :> es) => RepoSource -> String -> Eff es (Either String String)
evalJson source attr = send (EvalJson source attr)

evalRaw :: (Eval :> es) => RepoSource -> String -> Eff es (Either String String)
evalRaw source attr = send (EvalRaw source attr)

evalJsonApply :: (Eval :> es) => RepoSource -> String -> String -> Eff es (Either String String)
evalJsonApply source applyExpr attr = send (EvalJsonApply source applyExpr attr)

evalImpure :: (Eval :> es) => String -> Eff es (Either String String)
evalImpure = send . EvalImpure

runNixCli :: (Nix :> es) => [String] -> Eff es (ExitCode, ByteString, ByteString)
runNixCli = send . RunNixCli

runNixStoreCli :: (Nix :> es) => [String] -> Eff es (ExitCode, ByteString, ByteString)
runNixStoreCli = send . RunNixStoreCli

localLogStamp :: (Nix :> es) => FilePath -> Eff es (Maybe LogStamp)
localLogStamp = send . LocalLogStamp

pathValid :: (Nix :> es) => FilePath -> Eff es Bool
pathValid = send . PathValid

registerGcRoot :: (Nix :> es) => FilePath -> FilePath -> Eff es ()
registerGcRoot gcRootPath outPath = send (RegisterGcRoot gcRootPath outPath)

rootStorePath :: (Nix :> es, IOE :> es) => FilePath -> Eff es ()
rootStorePath path = do
    gcRootPath <- liftIO $ gcRootPathFor path
    registerGcRoot gcRootPath path

rootDerivationForJob :: (Nix :> es, IOE :> es) => String -> FilePath -> Eff es (Either String FilePath)
rootDerivationForJob jobName drvPath = do
    created <- liftIO $ try @IOException $ newDerivationGcRootPath jobName
    case created of
        Left err ->
            return $ Left ("Could not create a GC root for " ++ drvPath ++ ": " ++ displayException err)
        Right gcRootPath -> do
            result <- registerDerivationRoot gcRootPath drvPath
            case result of
                Left err -> do
                    liftIO $ ignoreMissing $ removeFile gcRootPath
                    return (Left err)
                Right () -> return (Right gcRootPath)

unrootDerivationRoot :: (IOE :> es) => FilePath -> Eff es ()
unrootDerivationRoot gcRootPath = liftIO $ ignoreMissing $ removeFile gcRootPath

unrootDerivationRootsForJob :: (IOE :> es) => String -> Eff es ()
unrootDerivationRootsForJob jobName = liftIO $ do
    directory <- (</> jobName) <$> derivationGcRootDirectory
    ignoreMissing $ removePathForcibly directory

listDerivationRootJobs :: IO [String]
listDerivationRootJobs = do
    directory <- derivationGcRootDirectory
    exists <- doesDirectoryExist directory
    if exists then listDirectory directory else return []

registerDerivationRoot :: (Nix :> es) => FilePath -> FilePath -> Eff es (Either String ())
registerDerivationRoot gcRootPath drvPath = send (RegisterDerivationRoot gcRootPath drvPath)

gcRootDir :: IO FilePath
gcRootDir = (</> ".local" </> "state" </> "pointy" </> "gc-roots") <$> getHomeDirectory

gcRootPathFor :: FilePath -> IO FilePath
gcRootPathFor path = do
    directory <- gcRootDir
    createDirectoryIfMissing True directory
    return $ directory </> takeFileName path

derivationGcRootDirectory :: IO FilePath
derivationGcRootDirectory = (</> "derivations") <$> gcRootDir

newDerivationGcRootPath :: String -> IO FilePath
newDerivationGcRootPath jobName = do
    directory <- (</> jobName) <$> derivationGcRootDirectory
    createDirectoryIfMissing True directory
    uniquePath directory "root-"

uniquePath :: FilePath -> String -> IO FilePath
uniquePath directory template = do
    (path, handle) <- openTempFile directory template
    hClose handle
    removeFile path
    return path

ignoreMissing :: IO () -> IO ()
ignoreMissing action = do
    outcome <- try action :: IO (Either IOException ())
    case outcome of
        Left _ -> return ()
        Right () -> return ()

ingestDirectory :: (Nix :> es) => FilePath -> String -> (Integer -> Integer -> IO ()) -> Eff es (Either String IngestResult)
ingestDirectory directory name reportProgress = send (IngestDirectory directory name reportProgress)

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

coldStorageConfigured :: (ColdStorage :> es) => Eff es Bool
coldStorageConfigured = send ColdStorageConfigured

coldStorageStatus :: (ColdStorage :> es) => Text -> Eff es (Either Text ByteString)
coldStorageStatus = send . ColdStorageStatus

coldStoragePrefetch :: (ColdStorage :> es) => Text -> FilePath -> Eff es (Either Text ByteString)
coldStoragePrefetch storePath = send . ColdStoragePrefetch storePath

embedTexts :: (Inference :> es) => [Text] -> Eff es (Either String [VU.Vector Float])
embedTexts = send . Embed

judgeCandidates :: (Inference :> es) => JudgeTask -> Eff es (Either String (Map Int Double))
judgeCandidates task = send (JudgeCandidates task)
