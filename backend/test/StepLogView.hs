{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildLog (ResolvedLog (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import StepLog (BuildRef (..), BuildStage (..), StepBuild (..), StepLogView (..), stepLogView, stepLogWithoutLog)

main :: IO ()
main = do
    let ownFailure =
            stepLogView 4258 builds (Just outOfMemory) $
                ResolvedLog editstatBuild (stdenvLog [("patchPhase", []), ("configurePhase", ["no configure script, doing nothing"]), ("installPhase", ["loading", "Killed duckdb"])]) []
    assertEqual "own failure names the step build" (StepRef 4258 "editstat" StageBuild) (viewFailed ownFailure)
    assertEqual "own failure shows only the template phases' output" "loading\nKilled duckdb" (viewOutput ownFailure)
    assertEqual "the job outcome is carried as the reason" (Just outOfMemory) (viewReason ownFailure)

    let upstream =
            stepLogView 4258 builds Nothing $
                ResolvedLog alignBuild (stdenvLog [("installPhase", ["STAR failed"])]) [editstatCheck, editstatBuild]
    assertEqual "upstream failure names the upstream step" (StepRef 4246 "align" StageBuild) (viewFailed upstream)
    assertEqual "via omits the requested step" [] (viewVia upstream)

    let fixup =
            stepLogView 4258 builds Nothing $
                ResolvedLog editstatBuild (stdenvLog [("installPhase", ["copied"]), ("fixupPhase", ["patchelf crashed"])]) []
    assertEqual "phases the template does not define are hidden" "copied" (viewOutput fixup)

    let unphased = stepLogView 4258 builds Nothing (ResolvedLog editstatBuild "Simulated failure\n" [])
    assertEqual "a log without phase markers is shown whole" "Simulated failure" (viewOutput unphased)

    let package =
            stepLogView 4258 builds Nothing $
                ResolvedLog extensionBuild (stdenvLog [("patchPhase", []), ("configurePhase", ["cmake ok"]), ("buildPhase", ["cmake error"])]) [editstatBuild]
    assertEqual "unknown builds are packages" (PackageRef "duckdb-ext-0.1.0") (viewFailed package)
    assertEqual "via keeps only builds other than the requested step" [] (viewVia package)
    assertEqual "packages show the output of every phase" "cmake ok\ncmake error" (viewOutput package)

    let noLog = stepLogWithoutLog 4258 builds outOfMemory
    assertEqual "without a log the step's own build is named" (StepRef 4258 "editstat" StageBuild) (viewFailed noLog)
    assertEqual "without a log the reason is still shown" (Just outOfMemory) (viewReason noLog)
    assertEqual "without a log there is no output" "" (viewOutput noLog)
  where
    outOfMemory = "Ran out of memory: the build used more than the step's RAM requirement"
    editstatBuild = "/nix/store/kdsgwrw20dk83wrcrp26w1mjsgk3gr50-duckdb-1.drv"
    editstatCheck = "/nix/store/00000000000000000000000000000001-certificate.drv"
    alignBuild = "/nix/store/00000000000000000000000000000002-alignment-1.drv"
    extensionBuild = "/nix/store/00000000000000000000000000000003-duckdb-ext-0.1.0.drv"
    builds =
        [ StepBuild 4258 "editstat" editstatBuild (Just editstatCheck) ["installPhase"]
        , StepBuild 4246 "align" alignBuild Nothing ["installPhase"]
        ]

stdenvLog :: [(ByteString, [ByteString])] -> ByteString
stdenvLog phases =
    BC.unlines
        [ line
        | (name, output) <- phases
        , line <- ["Running phase: " <> name, "@nix { \"action\": \"setPhase\", \"phase\": \"" <> name <> "\" }"] ++ output
        ]

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)
