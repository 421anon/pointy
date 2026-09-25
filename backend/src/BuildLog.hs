{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

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

import Control.Monad (forM_, guard, unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
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
import Effectful (Eff, IOE, (:>))
import Effects (Nix, pathValid, runNixStoreCli)
import NixStore (rootedPath)
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import UserRepo (runNix)

data LogSource
    = StepDrv
    | InputDrv FilePath Int
    deriving (Eq, Show)

data ResolvedLog = ResolvedLog
    { resolvedDrv :: FilePath
    , resolvedLog :: String
    , resolvedSource :: LogSource
    }
    deriving (Eq, Show)

resolveBuildLog :: (Nix :> es) => String -> Eff es (Maybe ResolvedLog)
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

lastMeaningfulLine :: String -> Maybe Text
lastMeaningfulLine output =
    case filter (not . null) (lines output) of
        [] -> Nothing
        ls -> Just (pack (last ls))

maxBfsDepth :: Int
maxBfsDepth = 6

bfs :: (Nix :> es) => Set FilePath -> [(FilePath, Int)] -> Eff es (Maybe ResolvedLog)
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

lookupDeriver :: (Nix :> es) => String -> Eff es (Maybe FilePath)
lookupDeriver target = do
    result <- runExceptT $ runNix ["path-info", "--derivation", target]
    return $ case result of
        Right out ->
            case filter (not . null) (lines out) of
                (drv : _) -> Just drv
                _ -> Nothing
        Left _ -> Nothing

fetchStepLog :: (Nix :> es) => FilePath -> Eff es (Maybe String)
fetchStepLog drv = do
    result <- runExceptT $ runNix ["log", drv]
    return $ case result of
        Right output | not (null output) -> Just output
        _ -> Nothing

fetchInputLog :: (Nix :> es) => FilePath -> Eff es (Maybe String)
fetchInputLog drv = do
    result <- runExceptT $ runNix ["--offline", "log", drv]
    return $ case result of
        Right output | not (null output) -> Just output
        _ -> Nothing

drvOutputs :: (Nix :> es) => FilePath -> Eff es [FilePath]
drvOutputs drv = do
    (code, out, _) <- runNixStoreCli ["--query", "--outputs", drv]
    return $ case code of
        ExitSuccess -> filter (not . null) (lines out)
        _ -> []

inputDrvs :: (Nix :> es) => FilePath -> Eff es [FilePath]
inputDrvs drv = do
    (code, out, _) <- runNixStoreCli ["--query", "--references", drv]
    return $ case code of
        ExitSuccess -> filter (".drv" `isSuffixOf`) (lines out)
        _ -> []

anyOutputValid :: (Nix :> es) => [FilePath] -> Eff es Bool
anyOutputValid [] = return True
anyOutputValid outputs = go outputs
  where
    go [] = return False
    go (p : ps) = do
        valid <- pathValid p
        if valid then return True else go ps

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

data StorePlan = StorePlan
    { spBuild :: Set FilePath
    , spFetch :: Set FilePath
    }
    deriving (Show)

data StepStore = StepStore
    { ssDrvOf :: Map Int FilePath
    , ssPlan :: StorePlan
    , ssCache :: IORef (Map FilePath DrvNode)
    }

isStorePath :: FilePath -> Bool
isStorePath path = "/nix/store/" `isPrefixOf` path

buildStepStore :: (Nix :> es, IOE :> es) => Map Int Text -> Eff es StepStore
buildStepStore certificates = do
    nodes <- queryDerivations (filter isStorePath (map unpack (Map.elems certificates)))
    let byOutput =
            Map.fromList
                [ (output, drv)
                | (drv, node) <- Map.toList nodes
                , output <- dnOutputs node
                ]
        drvOf = Map.mapMaybe (\certificate -> Map.lookup (unpack certificate) byOutput) certificates
    cache <- liftIO $ newIORef nodes
    plan <- queryStorePlan (Set.toList (Set.fromList (Map.elems drvOf)))
    return StepStore{ssDrvOf = drvOf, ssPlan = plan, ssCache = cache}

rawStatusesBatched :: (Nix :> es, IOE :> es) => StepStore -> (Text -> Bool) -> Map Int Text -> Eff es (Map Int (Text, Maybe Text))
rawStatusesBatched store isRunning certificates = Map.traverseWithKey classify certificates
  where
    classify sid certificate = do
        certified <- certificateValid sid (unpack certificate)
        return $
            if certified
                then ("success", Nothing)
                else
                    if isRunning certificate
                        then ("running", Nothing)
                        else ("not-started", Nothing)

    certificateValid sid certificate
        | not (isStorePath certificate) = return False
        | otherwise = case Map.lookup sid (ssDrvOf store) of
            Nothing -> probe certificate
            Just drv -> do
                node <- Map.lookup drv <$> liftIO (readIORef (ssCache store))
                case node of
                    Just node_ | length (dnOutputs node_) > 1 -> probe certificate
                    _ ->
                        return $
                            not
                                ( Set.member drv (spBuild (ssPlan store))
                                    || Set.member certificate (spFetch (ssPlan store))
                                )

    probe path = pathValid path

resolveStatusesBatched :: (Nix :> es, IOE :> es) => StepStore -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesBatched store statuses = do
    logCache <- liftIO $ newIORef Map.empty
    resolved <- liftIO $ newIORef Map.empty
    let seeds =
            [ (sid, drv)
            | (sid, (state, _)) <- Map.toList statuses
            , state == "failure" || state == "not-started"
            , Just drv <- [Map.lookup sid (ssDrvOf store)]
            ]
    forM_ seeds $ \(sid, drv) -> do
        mOwnLog <- cachedLog logCache drv
        forM_ mOwnLog $ \logText ->
            liftIO $ modifyIORef' resolved (Map.insert sid ("failure", lastMeaningfulLine logText))
    seedNodes <- fetchNodes store (map snd seeds)
    visited <- liftIO $ newIORef (Map.fromList [(sid, Set.singleton drv) | (sid, drv) <- seeds])
    frontierRef <-
        liftIO $
            newIORef $
                Map.fromList
                    [ (sid, [(input, 1) | input <- dnInputs node])
                    | (sid, drv) <- seeds
                    , Just node <- [Map.lookup drv seedNodes]
                    ]
    let levelLoop = do
            frontier <- liftIO $ readIORef frontierRef
            let levelNodes = Set.fromList [drv | queue <- Map.elems frontier, (drv, _) <- queue]
            unless (Set.null levelNodes) $ do
                level <- fetchNodes store (Set.toList levelNodes)
                nextFrontier <- liftIO $ newIORef Map.empty
                forM_ (Map.toList frontier) $ \(sid, queue) -> do
                    done <- Map.member sid <$> liftIO (readIORef resolved)
                    unless done $ do
                        seen <- Map.findWithDefault Set.empty sid <$> liftIO (readIORef visited)
                        (mLog, seen', expanded) <- walkQueue store logCache level seen queue
                        liftIO $ modifyIORef' visited (Map.insert sid seen')
                        case mLog of
                            Just logText ->
                                liftIO $ modifyIORef' resolved (Map.insert sid ("failure", lastMeaningfulLine logText))
                            Nothing ->
                                unless (null expanded) $
                                    liftIO $ modifyIORef' nextFrontier (Map.insert sid expanded)
                liftIO $ writeIORef frontierRef =<< readIORef nextFrontier
                levelLoop
    levelLoop
    resolvedMap <- liftIO $ readIORef resolved
    return $ Map.union resolvedMap statuses

walkQueue :: (Nix :> es, IOE :> es) => StepStore -> IORef (Map FilePath (Maybe String)) -> Map FilePath DrvNode -> Set FilePath -> [(FilePath, Int)] -> Eff es (Maybe String, Set FilePath, [(FilePath, Int)])
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

outputsAllInvalid :: (Nix :> es) => DrvNode -> Eff es Bool
outputsAllInvalid node = case dnOutputs node of
    [] -> return False
    [_] -> return True
    outputs -> and <$> mapM (fmap not . pathValid) outputs

cachedLog :: (Nix :> es, IOE :> es) => IORef (Map FilePath (Maybe String)) -> FilePath -> Eff es (Maybe String)
cachedLog cacheRef drv = do
    cached <- Map.lookup drv <$> liftIO (readIORef cacheRef)
    case cached of
        Just result -> return result
        Nothing -> do
            result <- readLocalLog drv
            liftIO $ modifyIORef' cacheRef (Map.insert drv result)
            return result

fetchNodes :: (Nix :> es, IOE :> es) => StepStore -> [FilePath] -> Eff es (Map FilePath DrvNode)
fetchNodes store paths = do
    cached <- liftIO $ readIORef (ssCache store)
    let wanted = Set.fromList paths
        missing = Set.toList (wanted `Set.difference` Map.keysSet cached)
    fetched <-
        if null missing
            then return Map.empty
            else do
                nodes <- queryDerivations missing
                liftIO $ modifyIORef' (ssCache store) (Map.union nodes)
                return nodes
    return $ Map.union fetched (Map.restrictKeys cached wanted)

queryDerivations :: (Nix :> es) => [FilePath] -> Eff es (Map FilePath DrvNode)
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

queryStorePlan :: (Nix :> es) => [FilePath] -> Eff es StorePlan
queryStorePlan [] = return (StorePlan Set.empty Set.empty)
queryStorePlan [path] = do
    (code, out, err) <- runNixStoreCli ["--realise", "--dry-run", path]
    return $ case code of
        ExitSuccess -> parseStorePlan (out ++ err)
        ExitFailure _ -> StorePlan (Set.singleton path) Set.empty
queryStorePlan paths = do
    (code, out, err) <- runNixStoreCli (["--realise", "--dry-run"] ++ paths)
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

logDirectoryAvailable :: IO Bool
logDirectoryAvailable = doesDirectoryExist logRoot

readLocalLog :: (Nix :> es, IOE :> es) => FilePath -> Eff es (Maybe String)
readLocalLog drv = do
    present <- liftIO $ maybe (return False) doesFileExist (logPathFor drv)
    if not present
        then return Nothing
        else do
            result <- runExceptT $ runNix ["--offline", "log", drv]
            return $ case result of
                Right output | not (null output) -> Just output
                _ -> Nothing

logRoot :: FilePath
logRoot = rootedPath "/nix/var/log/nix/drvs"

logPathFor :: FilePath -> Maybe FilePath
logPathFor drv = do
    let file = takeFileName drv
    guard (".drv" `isSuffixOf` file)
    let stem = take (length file - 4) file
        (hash, rest) = break (== '-') stem
    guard (length hash == 32)
    guard (not (null rest))
    return $ logRoot </> take 2 hash </> (drop 2 hash ++ rest ++ ".drv.bz2")
