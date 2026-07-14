{-# LANGUAGE OverloadedStrings #-}

{- | Resolves the build log relevant to a step.

Status reporting and the log endpoint both consume this. When the step's own
derivation has no recorded log (because a build-time input derivation failed
and the step itself never built — e.g. ShellCheck inside the
@writeShellApplication@ wrapper used by @script.nix@), we walk the input
derivation graph to find the failing input and surface its log instead.
-}
module BuildLog (
    ResolvedLog (..),
    LogSource (..),
    resolveBuildLog,
    lastMeaningfulLine,
) where

import Control.Monad.Except (runExceptT)
import Data.List (isSuffixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack)
import NixUtils (isValidStorePath)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import UserRepo (runNix)

data LogSource
    = StepDrv
    | -- | The drv whose log we returned, and its BFS depth (1 = direct input).
      InputDrv FilePath Int
    deriving (Eq, Show)

data ResolvedLog = ResolvedLog
    { resolvedDrv :: FilePath
    , resolvedLog :: String
    , resolvedSource :: LogSource
    }
    deriving (Eq, Show)

{- | Resolve a build log for any target @nix path-info --derivation@ accepts:
a store output path, a flake installable, or a .drv path.

  1. Resolve the target to its derivation; abort if that fails.
  2. Return the step's own log if recorded.
  3. Otherwise BFS over input derivations and return the first one whose
     declared outputs are all locally invalid AND has a recorded local log
     (i.e. its build was attempted and failed).
  4. If neither, the build was never attempted — return Nothing.
-}
resolveBuildLog :: String -> IO (Maybe ResolvedLog)
resolveBuildLog target = do
    mDrv <- lookupDeriver target
    case mDrv of
        Nothing -> return Nothing
        Just stepDrv -> do
            mOwn <- fetchStepLog stepDrv
            case mOwn of
                Just logText ->
                    return (Just (ResolvedLog stepDrv logText StepDrv))
                Nothing -> do
                    inputs <- inputDrvs stepDrv
                    bfs (Set.singleton stepDrv) [(d, 1) | d <- inputs]

{- | Last non-empty line of a build log, suitable for a one-line failure
summary on the status tile.
-}
lastMeaningfulLine :: String -> Maybe Text
lastMeaningfulLine output =
    case filter (not . null) (lines output) of
        [] -> Nothing
        ls -> Just (pack (last ls))

----------------------------------------------------------------------
-- Internals
----------------------------------------------------------------------

{- | Cap on how deep we search the input derivation graph. Bounded to keep
status checks responsive on large closures (e.g. dream2nix stacks). One hop
handles @writeShellApplication@; deeper handles wrapper-of-wrapper cases.
-}
maxBfsDepth :: Int
maxBfsDepth = 6

bfs :: Set FilePath -> [(FilePath, Int)] -> IO (Maybe ResolvedLog)
bfs _ [] = return Nothing
bfs visited ((drv, depth) : rest)
    | drv `Set.member` visited = bfs visited rest
    | depth > maxBfsDepth = bfs (Set.insert drv visited) rest
    | otherwise = do
        let visited' = Set.insert drv visited
        outputs <- drvOutputs drv
        valid <- anyOutputValid outputs
        if valid
            then bfs visited' rest
            else do
                mLog <- fetchInputLog drv
                case mLog of
                    Just logText ->
                        return $
                            Just (ResolvedLog drv logText (InputDrv drv depth))
                    Nothing -> do
                        inputs <- inputDrvs drv
                        let next =
                                [ (d, depth + 1)
                                | d <- inputs
                                , d `Set.notMember` visited'
                                ]
                        bfs visited' (rest ++ next)

{- | Resolve a target to its derivation path via @nix path-info --derivation@.
Works for store output paths, flake installables, and .drv paths.
-}
lookupDeriver :: String -> IO (Maybe FilePath)
lookupDeriver target = do
    result <- runExceptT $ runNix ["path-info", "--derivation", target]
    return $ case result of
        Right out ->
            case filter (not . null) (lines out) of
                (drv : _) -> Just drv
                _ -> Nothing
        Left _ -> Nothing

{- | Fetch a step's own log. Substitution is allowed because successful builds
might have their log only in a binary cache.
-}
fetchStepLog :: FilePath -> IO (Maybe String)
fetchStepLog drv = do
    result <- runExceptT $ runNix ["log", drv]
    return $ case result of
        Right output | not (null output) -> Just output
        _ -> Nothing

{- | Fetch an input derivation's log offline. We only reach here after
detecting the input's outputs are invalid locally — its log, if any, is the
record of a local failed build. Substituters never carry failure logs, so
skipping them avoids a network stall on the BFS hot path.
-}
fetchInputLog :: FilePath -> IO (Maybe String)
fetchInputLog drv = do
    result <- runExceptT $ runNix ["--offline", "log", drv]
    return $ case result of
        Right output | not (null output) -> Just output
        _ -> Nothing

-- | Output store paths declared by a derivation.
drvOutputs :: FilePath -> IO [FilePath]
drvOutputs drv = do
    (code, out, _) <- readProcessWithExitCode "nix-store" ["--query", "--outputs", drv] ""
    return $ case code of
        ExitSuccess -> filter (not . null) (lines out)
        _ -> []

{- | Direct input derivations of a derivation. References include source paths
too; filter to .drv suffix to get only build-time dependencies.
-}
inputDrvs :: FilePath -> IO [FilePath]
inputDrvs drv = do
    (code, out, _) <- readProcessWithExitCode "nix-store" ["--query", "--references", drv] ""
    return $ case code of
        ExitSuccess -> filter (".drv" `isSuffixOf`) (lines out)
        _ -> []

{- | True iff at least one declared output of the derivation is a valid local
store entry. We use "any" rather than "all" because multi-output drvs
(e.g. nixpkgs's bash, with @out@, @dev@, @man@, @doc@, @info@, @debug@) are
routinely substituted only for the outputs that downstream actually needs;
the others legitimately stay invalid without indicating a failure. Conversely,
a fully failed build leaves no valid outputs at all, and a partial on-disk
artefact left behind by a failed write (the @writeShellApplication@ case)
is unregistered and so reports invalid.

Empty list means "no outputs known" — treated as valid so we don't
spuriously recurse into nodes whose outputs we can't enumerate.
-}
anyOutputValid :: [FilePath] -> IO Bool
anyOutputValid [] = return True
anyOutputValid outputs = go outputs
  where
    go [] = return False
    go (p : ps) = do
        valid <- isValidStorePath p
        if valid then return True else go ps
