{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import BuildLog (ResolvedLog (..))
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import StepLog (BuildRef (..), BuildStage (..), LogSection (..), StepBuild (..), StepLogView (..), stepLogView)

main :: IO ()
main = do
    let ownFailure =
            stepLogView 4258 builds $
                ResolvedLog editstatBuild (stdenvLog [("patchPhase", []), ("configurePhase", ["no configure script, doing nothing"]), ("installPhase", ["Killed duckdb"])]) []
    assertEqual "own failure names the step build" (StepRef 4258 "editstat" StageBuild) (viewFailed ownFailure)
    assertEqual "own failure shows only template phases" [LogSection (Just "installPhase") "Killed duckdb"] (viewSections ownFailure)

    let upstream =
            stepLogView 4258 builds $
                ResolvedLog alignBuild (stdenvLog [("installPhase", ["STAR failed"])]) [editstatCheck, editstatBuild]
    assertEqual "upstream failure names the upstream step" (StepRef 4246 "align" StageBuild) (viewFailed upstream)
    assertEqual "via omits the requested step" [] (viewVia upstream)

    let fixup =
            stepLogView 4258 builds $
                ResolvedLog editstatBuild (stdenvLog [("buildPhase", ["doing nothing"]), ("installPhase", ["copied"]), ("fixupPhase", ["patchelf crashed"])]) []
    assertEqual
        "a failure in a phase the template does not define is still shown"
        [LogSection (Just "installPhase") "copied", LogSection (Just "fixupPhase") "patchelf crashed"]
        (viewSections fixup)

    let package =
            stepLogView 4258 builds $
                ResolvedLog extensionBuild (stdenvLog [("patchPhase", []), ("configurePhase", ["cmake ok"]), ("buildPhase", ["cmake error"])]) [editstatBuild]
    assertEqual "unknown builds are packages" (PackageRef "duckdb-welkerlab-0.1.0") (viewFailed package)
    assertEqual "via keeps only builds other than the requested step" [] (viewVia package)
    assertEqual "packages show every phase with output" [Just "configurePhase", Just "buildPhase"] (map sectionPhase (viewSections package))
  where
    editstatBuild = "/nix/store/kdsgwrw20dk83wrcrp26w1mjsgk3gr50-duckdb-1.drv"
    editstatCheck = "/nix/store/00000000000000000000000000000001-certificate.drv"
    alignBuild = "/nix/store/00000000000000000000000000000002-alignment-1.drv"
    extensionBuild = "/nix/store/00000000000000000000000000000003-duckdb-welkerlab-0.1.0.drv"
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
