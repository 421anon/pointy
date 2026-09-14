{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- | Resolves the build log relevant to a step.

Status reporting and the log endpoint both consume this. When the step's own
derivation has no recorded log (because a build-time input derivation failed
and the step itself never built — e.g. ShellCheck inside the
@writeShellApplication@ wrapper used by @script.nix@), we walk the input
derivation graph to find the failing input and surface its log instead.

'resolveBuildLog' walks one step at a time and shells out per graph node; it
serves the log endpoint and single-step broadcasts. Project-wide status
broadcasts use the batched probes below ('buildStepStore',
'rawStatusesBatched', 'resolveStatusesBatched'), which answer the same
questions with a handful of processes regardless of step count.
-}
module BuildLog (
    ResolvedLog (..),
    LogSource (..),
    resolveBuildLog,
    lastMeaningfulLine,
    StepStore,
    buildStepStore,
    rawStatusesBatched,
    resolveStatusesBatched,
    logDirectoryAvailable,
) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM_, guard, unless)
import Control.Monad.Except (runExceptT)
import Data.Aeson (FromJSON (..), Value, eitherDecode, withObject, (.:), (.:?), (.!=))
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import NixUtils (isValidStorePath)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
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

----------------------------------------------------------------------
-- Batched probes
----------------------------------------------------------------------

{- | A derivation's structure as reported by @nix derivation show@: the store
paths it declares and the derivations it uses at build time.
-}
data DrvNode = DrvNode
    { dnOutputs :: [FilePath]
    , dnInputs :: [FilePath]
    }

instance FromJSON DrvNode where
    parseJSON = withObject "derivation" $ \obj -> do
        outputs <- obj .:? "outputs" .!= (Map.empty :: Map Text (Maybe OutputInfo))
        inputs <- obj .:? "inputDrvs" .!= (Map.empty :: Map FilePath Value)
        return
            DrvNode
                { dnOutputs = [path | Just (OutputInfo path) <- Map.elems outputs]
                , dnInputs = sort (Map.keys inputs)
                }

newtype OutputInfo = OutputInfo FilePath

instance FromJSON OutputInfo where
    parseJSON = withObject "derivation output" $ \obj -> OutputInfo <$> obj .: "path"

{- | What nix would do to realise a set of derivations: build them (outputs
missing and not substitutable) or fetch some of their outputs. A fully valid
derivation, or one whose missing outputs are all substitutable, appears only
in 'spFetch'; one with at least one valid output appears in neither.
-}
data StorePlan = StorePlan
    { spBuild :: Set FilePath
    , spFetch :: Set FilePath
    }
    deriving (Show)

{- | Per-project store snapshot shared by every step of one status broadcast:
step to derivation, nix's build plan for those derivations, and a cache of
derivation structures filled in as the failure walk expands.
-}
data StepStore = StepStore
    { ssDrvOf :: Map Int FilePath
    , ssPlan :: StorePlan
    , ssCache :: IORef (Map FilePath DrvNode)
    }

-- | Store paths are the only values the store-facing probes accept.
isStorePath :: FilePath -> Bool
isStorePath path = "/nix/store/" `isPrefixOf` path

{- | Resolve every step's derivation and build plan with two processes: one
@nix derivation show@ over the step output paths, one build-plan query over
the resulting derivations.
-}
buildStepStore :: Map Int Text -> IO StepStore
buildStepStore outPaths = do
    -- Steps whose evaluation failed resolve to the placeholder @\/invalid@;
    -- they have no derivation and would make @nix derivation show@ fail for the
    -- whole batch, so they never reach it.
    nodes <- queryDerivations (filter isStorePath (map unpack (Map.elems outPaths)))
    let byOutput =
            Map.fromList
                [ (output, drv)
                | (drv, node) <- Map.toList nodes
                , output <- dnOutputs node
                ]
        drvOf = Map.mapMaybe (\outPath -> Map.lookup (unpack outPath) byOutput) outPaths
    cache <- newIORef nodes
    plan <- queryStorePlan (Set.toList (Set.fromList (Map.elems drvOf)))
    return StepStore{ssDrvOf = drvOf, ssPlan = plan, ssCache = cache}

{- | Raw statuses for a whole project. A step is successful when its output is
a valid local store entry; an active job makes it running, anything else
not-started.
-}
rawStatusesBatched :: StepStore -> (Text -> Bool) -> Map Int Text -> IO (Map Int (Text, Maybe Text))
rawStatusesBatched store isRunning outPaths = Map.traverseWithKey classify outPaths
  where
    classify sid outPath = do
        valid <- stepOutputValid sid (unpack outPath)
        return $
            if valid
                then ("success", Nothing)
                else
                    if isRunning outPath
                        then ("running", Nothing)
                        else ("not-started", Nothing)

    stepOutputValid sid outPath
        -- Only store paths can be valid entries; anything else (the @\/invalid@
        -- placeholder, empty strings) is not one, and asking nix about it would
        -- cost a process each.
        | not (isStorePath outPath) = return False
        | otherwise = case Map.lookup sid (ssDrvOf store) of
            Nothing -> probe outPath
            Just drv -> do
                node <- Map.lookup drv <$> readIORef (ssCache store)
                case node of
                    -- A multi-output step can have its own output valid while
                    -- siblings are missing; ask about the step's output directly.
                    Just node_ | length (dnOutputs node_) > 1 -> probe outPath
                    _ ->
                        return $
                            not
                                ( Set.member drv (spBuild (ssPlan store))
                                    || Set.member outPath (spFetch (ssPlan store))
                                )

    probe path = isValidStorePath path `catch` \(_ :: SomeException) -> return False

{- | Resolve failure logs for the pending steps of one project with a
level-batched walk over the derivation graph: one @nix derivation show@ per
BFS level, one log-file stat per visited node, and one @nix log@ per recovered
failure. Semantics match 'resolveBuildLog': a step's own log wins; otherwise
the first input (level order, depth at most 'maxBfsDepth') whose outputs are
all invalid and that has a recorded local log.
-}
resolveStatusesBatched :: StepStore -> Map Int (Text, Maybe Text) -> IO (Map Int (Text, Maybe Text))
resolveStatusesBatched store statuses = do
    logCache <- newIORef Map.empty
    resolved <- newIORef Map.empty
    let seeds =
            [ (sid, drv)
            | (sid, (state, _)) <- Map.toList statuses
            , state == "failure" || state == "not-started"
            , Just drv <- [Map.lookup sid (ssDrvOf store)]
            ]
    -- The step's own log takes precedence, as in 'resolveBuildLog'. Status
    -- resolution reads only locally recorded logs; cache-only logs belong to
    -- successful substitutions, which never reach this path.
    forM_ seeds $ \(sid, drv) -> do
        mOwnLog <- cachedLog logCache drv
        forM_ mOwnLog $ \logText ->
            modifyIORef' resolved (Map.insert sid ("failure", lastMeaningfulLine logText))
    seedNodes <- fetchNodes store (map snd seeds)
    visited <- newIORef (Map.fromList [(sid, Set.singleton drv) | (sid, drv) <- seeds])
    frontierRef <-
        newIORef $
            Map.fromList
                [ (sid, [(input, 1) | input <- dnInputs node])
                | (sid, drv) <- seeds
                , Just node <- [Map.lookup drv seedNodes]
                ]
    let levelLoop = do
            frontier <- readIORef frontierRef
            let levelNodes = Set.fromList [drv | queue <- Map.elems frontier, (drv, _) <- queue]
            unless (Set.null levelNodes) $ do
                level <- fetchNodes store (Set.toList levelNodes)
                nextFrontier <- newIORef Map.empty
                forM_ (Map.toList frontier) $ \(sid, queue) -> do
                    done <- Map.member sid <$> readIORef resolved
                    unless done $ do
                        seen <- Map.findWithDefault Set.empty sid <$> readIORef visited
                        (mLog, seen', expanded) <- walkQueue store logCache level seen queue
                        modifyIORef' visited (Map.insert sid seen')
                        case mLog of
                            Just logText ->
                                modifyIORef' resolved (Map.insert sid ("failure", lastMeaningfulLine logText))
                            Nothing ->
                                unless (null expanded) $
                                    modifyIORef' nextFrontier (Map.insert sid expanded)
                writeIORef frontierRef =<< readIORef nextFrontier
                levelLoop
    levelLoop
    resolvedMap <- readIORef resolved
    return $ Map.union resolvedMap statuses

{- | One step's queue at one BFS level. Returns the log of the first input whose
outputs are all invalid and that carries a recorded local log, plus the visited
set and the queue for the next level.
-}
walkQueue :: StepStore -> IORef (Map FilePath (Maybe String)) -> Map FilePath DrvNode -> Set FilePath -> [(FilePath, Int)] -> IO (Maybe String, Set FilePath, [(FilePath, Int)])
walkQueue store logCache level seen queue = go seen [] queue
  where
    go visited expanded [] = return (Nothing, visited, expanded)
    go visited expanded ((drv, depth) : rest)
        | drv `Set.member` visited = go visited expanded rest
        | depth > maxBfsDepth = go (Set.insert drv visited) expanded rest
        | not (Set.member drv (spBuild (ssPlan store))) =
            go (Set.insert drv visited) expanded rest
        | otherwise = case Map.lookup drv level of
            Nothing -> go (Set.insert drv visited) expanded rest
            Just node -> do
                mLog <- cachedLog logCache drv
                case mLog of
                    Just logText -> do
                        -- The build plan marks drvs that need work; a candidate
                        -- additionally needs every output invalid (partial
                        -- multi-output drvs are pruned), as in the per-step walk.
                        allInvalid <- outputsAllInvalid node
                        if allInvalid
                            then return (Just logText, Set.insert drv visited, expanded)
                            else go (Set.insert drv visited) expanded rest
                    Nothing ->
                        go
                            (Set.insert drv visited)
                            ( expanded
                                ++ [ (input, depth + 1)
                                   | input <- dnInputs node
                                   , depth < maxBfsDepth
                                   , input `Set.notMember` visited
                                   ]
                            )
                            rest

outputsAllInvalid :: DrvNode -> IO Bool
outputsAllInvalid node = case dnOutputs node of
    [] -> return False
    [_] -> return True
    outputs -> and <$> mapM (fmap not . isValidStorePath) outputs

cachedLog :: IORef (Map FilePath (Maybe String)) -> FilePath -> IO (Maybe String)
cachedLog cacheRef drv = do
    cached <- Map.lookup drv <$> readIORef cacheRef
    case cached of
        Just result -> return result
        Nothing -> do
            result <- readLocalLog drv
            modifyIORef' cacheRef (Map.insert drv result)
            return result

{- | Structures for the given derivations; one @nix derivation show@ for
whatever is not cached yet.
-}
fetchNodes :: StepStore -> [FilePath] -> IO (Map FilePath DrvNode)
fetchNodes store paths = do
    cached <- readIORef (ssCache store)
    let wanted = Set.fromList paths
        missing = Set.toList (wanted `Set.difference` Map.keysSet cached)
    fetched <-
        if null missing
            then return Map.empty
            else do
                nodes <- queryDerivations missing
                modifyIORef' (ssCache store) (Map.union nodes)
                return nodes
    return $ Map.union fetched (Map.restrictKeys cached wanted)

{- | Structural lookups for many derivations. One unreadable path makes
@nix derivation show@ fail for the whole batch, so failures halve the batch
until the culprit is isolated.
-}
queryDerivations :: [FilePath] -> IO (Map FilePath DrvNode)
queryDerivations [] = return Map.empty
queryDerivations [path] = do
    result <- runExceptT $ runNix ["derivation", "show", path]
    return $ case result of
        Right output -> fromMaybe Map.empty (decodeDrvNodes output)
        Left _ -> Map.empty
queryDerivations paths = do
    result <- runExceptT $ runNix (["derivation", "show"] ++ paths)
    case result of
        Right output | Just nodes <- decodeDrvNodes output -> return nodes
        _ -> do
            let (left, right) = splitAt (length paths `div` 2) paths
            Map.union <$> queryDerivations left <*> queryDerivations right

decodeDrvNodes :: String -> Maybe (Map FilePath DrvNode)
decodeDrvNodes = either (const Nothing) Just . eitherDecode . TLE.encodeUtf8 . TL.pack

{- | Nix's build plan for a set of derivations. As with the structure lookup, a
batch that fails is halved; a single derivation that still fails is assumed to
need a build, the conservative answer for status display.
-}
queryStorePlan :: [FilePath] -> IO StorePlan
queryStorePlan [] = return (StorePlan Set.empty Set.empty)
queryStorePlan [path] = do
    (code, out, err) <- readProcessWithExitCode "nix-store" ["--realise", "--dry-run", path] ""
    return $ case code of
        ExitSuccess -> parseStorePlan (out ++ err)
        ExitFailure _ -> StorePlan (Set.singleton path) Set.empty
queryStorePlan paths = do
    (code, out, err) <- readProcessWithExitCode "nix-store" (["--realise", "--dry-run"] ++ paths) ""
    case code of
        ExitSuccess -> return (parseStorePlan (out ++ err))
        ExitFailure _ -> do
            let (left, right) = splitAt (length paths `div` 2) paths
            leftPlan <- queryStorePlan left
            rightPlan <- queryStorePlan right
            return
                StorePlan
                    { spBuild = spBuild leftPlan <> spBuild rightPlan
                    , spFetch = spFetch leftPlan <> spFetch rightPlan
                    }

-- | Read the two headings of @nix-store --realise --dry-run@ (which nix prints
-- on stderr): derivations under @... will be built:@, store paths under
-- @... will be fetched:@.
parseStorePlan :: String -> StorePlan
parseStorePlan = go False (StorePlan Set.empty Set.empty) . lines
  where
    go _ plan [] = plan
    go building plan (line : rest)
        | isSuffixOf ":" line = go ("will be built:" `isSuffixOf` line) plan rest
        | Just path <- entryOf line =
            go
                building
                ( if building
                    then plan{spBuild = Set.insert path (spBuild plan)}
                    else plan{spFetch = Set.insert path (spFetch plan)}
                )
                rest
        | otherwise = go building plan rest

    entryOf line = case words line of
        [path] | "/nix/store/" `isPrefixOf` path -> Just path
        _ -> Nothing

-- | True when the local build-log tree is readable, i.e. the batched walk can
-- rely on stat-ing log files.
logDirectoryAvailable :: IO Bool
logDirectoryAvailable = doesDirectoryExist logRoot

-- | Read a locally recorded log, skipping the subprocess when the log file is
-- absent — the common case during status refreshes.
readLocalLog :: FilePath -> IO (Maybe String)
readLocalLog drv = do
    present <- maybe (return False) doesFileExist (logPathFor drv)
    if not present
        then return Nothing
        else do
            result <- runExceptT $ runNix ["--offline", "log", drv]
            return $ case result of
                Right output | not (null output) -> Just output
                _ -> Nothing

logRoot :: FilePath
logRoot = "/nix/var/log/nix/drvs"

-- | @\/nix\/store\/<hash>-<name>.drv@ maps to
-- @\/nix\/var\/log\/nix\/drvs\/<hash prefix>\/<hash remainder>-<name>.drv.bz2@.
logPathFor :: FilePath -> Maybe FilePath
logPathFor drv = do
    let file = takeFileName drv
    guard (".drv" `isSuffixOf` file)
    let stem = take (length file - 4) file
        (hash, rest) = break (== '-') stem
    guard (length hash == 32)
    guard (not (null rest))
    return $ logRoot </> take 2 hash </> (drop 2 hash ++ rest ++ ".drv.bz2")
