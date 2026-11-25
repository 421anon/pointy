{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import BuildLog (LogAccess (..), ResolvedLog (..), mirrorBuildLog, mirroredLog, resolveBuildLog, storeMirroredLog)
import BuildStatus (resolveStepStatus)
import Control.Monad (unless)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Effectful (Eff, IOE, liftIO, runEff)
import Effectful.Dispatch.Dynamic (interpret)
import Effects (Nix (..))
import System.Directory (doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))

main :: IO ()
main = do
    temporary <- getTemporaryDirectory
    let directory = temporary </> "pointy-build-log-mirror"
    removePathForcibly directory
    setEnv "POINTY_BUILD_LOG_MIRROR" directory
    runNixFixture (mirrorBuildLog certificate)
    stored <- mirroredLog drv
    assertEqual "mirrorBuildLog stores the fetched log under the failing derivation" (Just latestLog) stored
    resolved <- runNixFixture (resolveStepStatus (Just certificate) (4410, ("failure", Nothing)))
    assertEqual "a mirrored log resolves the failure reason" (4410, ("failure", Just reason)) resolved
    storeMirroredLog drv staleLog
    fresh <- runNixFixture (resolveBuildLog RemoteLogs drv)
    assertEqual "a remote log wins over a stale mirror" (Just (ResolvedLog drv latestLog [])) fresh
    removePathForcibly directory
    unsetEnv "POINTY_BUILD_LOG_MIRROR"
    runNixFixture (mirrorBuildLog certificate)
    absent <- doesFileExist (directory </> takeFileName drv)
    assertEqual "without a mirror directory no log is stored" False absent
    unresolved <- runNixFixture (resolveStepStatus (Just certificate) (4410, ("failure", Nothing)))
    assertEqual "without a mirrored log the reason stays unknown" (4410, ("failure", Nothing)) unresolved

runNixFixture :: Eff '[Nix, IOE] a -> IO a
runNixFixture =
    runEff . interpret (\_ -> \case
        RunNixCli ["path-info", "--derivation", path]
            | path == certificate -> pure (ExitSuccess, BC.pack (drv ++ "\n"), BC.empty)
        RunNixCli ["path-info", "--derivation", _] -> pure (ExitFailure 1, BC.empty, "no derivation")
        RunNixCli ["log", target]
            | target == drv -> pure (ExitSuccess, latestLog, BC.empty)
        RunNixCli ["log", _] -> pure (ExitFailure 1, BC.empty, "no log")
        RunNixCli ["--offline", "log", _] -> pure (ExitFailure 1, BC.empty, "no log")
        RunNixCli ["--option", "substituters", "", "log", _] -> pure (ExitFailure 1, BC.empty, "no log")
        RunNixCli ["--option", "substituters", "", "--offline", "log", _] -> pure (ExitFailure 1, BC.empty, "no log")
        RunNixCli ["derivation", "show", target] -> pure (ExitSuccess, derivationShow target, BC.empty)
        RunNixStoreCli ("--realise" : "--dry-run" : paths) ->
            pure
                ( ExitSuccess
                , BC.empty
                , BC.pack (unlines (("these " ++ show (length paths) ++ " derivations will be built:") : map ("  " ++) paths))
                )
        LocalLogStamp _ -> pure Nothing
        _ -> liftIO (fail "build log mirror test: unexpected Nix operation")
    )

derivationShow :: FilePath -> ByteString
derivationShow target =
    LBS.toStrict . encode $
        object
            [ Key.fromString target
                .= object
                    [ "outputs" .= object ["out" .= object ["path" .= output]]
                    , "inputDrvs" .= (object [] :: Value)
                    ]
            ]

certificate :: FilePath
certificate = "/nix/store/00000000000000000000000000000000-pointy-certificate"

drv :: FilePath
drv = "/nix/store/11111111111111111111111111111111-pointy-certificate.drv"

output :: FilePath
output = "/nix/store/22222222222222222222222222222222-pointy-certificate"

latestLog :: ByteString
latestLog = "Running phase: installPhase\nTypeError: '<' not supported between instances of 'int' and 'NoneType'"

staleLog :: ByteString
staleLog = "error: an older attempt failed"

reason :: Text
reason = "TypeError: '<' not supported between instances of 'int' and 'NoneType'"

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
