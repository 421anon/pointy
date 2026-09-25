{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BuildLog (
    ResolvedLog (..),
    LogSource (..),
    resolveBuildLog,
    validPaths,
    lookupDeriver,
    lastMeaningfulLine,
    StepStore,
    buildStepStore,
    rawStatusesBatched,
    resolveStatusesBatched,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, guard, unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Value (Null), eitherDecode, withObject, (.:), (.:?), (.!=))
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift), (:>), withEffToIO)
import Effects (Nix, pathValid, runNixStoreCli)
import System.Exit (ExitCode (..))
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

resolveBuildLog :: (Nix :> es, IOE :> es) => FilePath -> Eff es (Maybe ResolvedLog)
resolveBuildLog stepDrv = do
    logCache <- liftIO $ newIORef Map.empty
    nodeCache <- liftIO $ newIORef Map.empty
    mOwn <- cachedLog fetchStepLog logCache stepDrv
    case mOwn of
        Just logText ->
            return (Just (ResolvedLog stepDrv logText StepDrv))
        Nothing -> do
            nodes <- fetchNodes nodeCache [stepDrv]
            bfs nodeCache logCache (Set.singleton stepDrv) [(input, 1) | input <- maybe [] dnInputs (Map.lookup stepDrv nodes)]
  where
    bfs _ _ _ [] = return Nothing
    bfs nodeCache logCache visited level = do
        plan <- queryStorePlan (map fst level)
        (found, unbuilt) <- logOfFirstUnbuilt logCache plan level
        case found of
            Just resolved ->
                return (Just resolved)
            Nothing -> do
                nodes <- fetchNodes nodeCache (map fst unbuilt)
                bfs nodeCache logCache seen $
                    dedupe seen
                        [ (input, depth + 1)
                        | (drv, depth) <- unbuilt
                        , depth < maxBfsDepth
                        , Just node <- [Map.lookup drv nodes]
                        , input <- dnInputs node
                        ]
      where
        seen = visited <> Set.fromList (map fst level)

    logOfFirstUnbuilt logCache plan = go []
      where
        go unbuilt [] = return (Nothing, reverse unbuilt)
        go unbuilt ((drv, depth) : rest)
            | drv `Set.member` spBuild plan = do
                mLog <- cachedLog fetchInputLog logCache drv
                case mLog of
                    Just logText ->
                        return $
                            ( Just (ResolvedLog drv logText (InputDrv drv depth))
                            , []
                            )
                    Nothing -> go ((drv, depth) : unbuilt) rest
            | otherwise = go unbuilt rest

dedupe :: Set FilePath -> [(FilePath, Int)] -> [(FilePath, Int)]
dedupe _ [] = []
dedupe excluded ((drv, depth) : rest)
    | drv `Set.member` excluded = dedupe excluded rest
    | otherwise = (drv, depth) : dedupe (Set.insert drv excluded) rest

lastMeaningfulLine :: String -> Maybe Text
lastMeaningfulLine output =
    case filter (not . null) (lines output) of
        [] -> Nothing
        ls -> Just (pack (last ls))

maxBfsDepth :: Int
maxBfsDepth = 6

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

validPaths :: (Nix :> es) => [FilePath] -> Eff es (Maybe (Set FilePath))
validPaths [] = return (Just Set.empty)
validPaths paths = do
    result <- runExceptT $ runNix (["path-info", "--offline", "--json"] ++ paths)
    return $ case result of
        Right output -> parsedValidity output
        Left _ -> Nothing
  where
    parsedValidity output = do
        validity <- decodeValidity output
        guard (Set.fromList (map pack paths) `Set.isSubsetOf` Map.keysSet validity)
        return $ Set.fromList (map unpack (Map.keys (Map.filter (/= Null) validity)))

decodeValidity :: String -> Maybe (Map Text Value)
decodeValidity = either (const Nothing) Just . eitherDecode . TLE.encodeUtf8 . TL.pack

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
rawStatusesBatched store isRunning certificates = do
    nodes <- liftIO $ readIORef (ssCache store)
    let probed = Map.filterWithKey (\sid certificate -> needsProbe nodes sid (unpack certificate)) certificates
    known <- validPaths (map unpack (Map.elems probed))
    let probe certificate = case known of
            Just valid -> return (Set.member certificate valid)
            Nothing -> pathValid certificate
    Map.traverseWithKey (classify nodes probe) certificates
  where
    classify nodes probe sid certificate = do
        certified <- certificateValid nodes probe sid (unpack certificate)
        return $
            if certified
                then ("success", Nothing)
                else
                    if isRunning certificate
                        then ("running", Nothing)
                        else ("not-started", Nothing)

    needsProbe nodes sid certificate
        | not (isStorePath certificate) = False
        | otherwise = case Map.lookup sid (ssDrvOf store) of
            Nothing -> True
            Just drv -> maybe False ((> 1) . length . dnOutputs) (Map.lookup drv nodes)

    certificateValid nodes probe sid certificate
        | not (isStorePath certificate) = return False
        | otherwise = case Map.lookup sid (ssDrvOf store) of
            Nothing -> probe certificate
            Just drv -> case Map.lookup drv nodes of
                Just node | length (dnOutputs node) > 1 -> probe certificate
                _ ->
                    return $
                        not
                            ( Set.member drv (spBuild (ssPlan store))
                                || Set.member certificate (spFetch (ssPlan store))
                            )

resolveStatusesBatched :: (Nix :> es, IOE :> es) => StepStore -> Map Int (Text, Maybe Text) -> Eff es (Map Int (Text, Maybe Text))
resolveStatusesBatched store statuses = do
    logCache <- liftIO $ newIORef Map.empty
    let seeds =
            [ (sid, drv)
            | (sid, (state, _)) <- Map.toList statuses
            , state == "failure" || state == "not-started"
            , Just drv <- [Map.lookup sid (ssDrvOf store)]
            ]
    ownLogs <-
        withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
            mapConcurrently (unlift . seedFailureLog logCache) seeds
    resolved <- liftIO $ newIORef (Map.fromList (catMaybes ownLogs))
    seedNodes <- fetchNodes (ssCache store) (map snd seeds)
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
                level <- fetchNodes (ssCache store) (Set.toList levelNodes)
                done <- liftIO $ readIORef resolved
                seen <- liftIO $ readIORef visited
                walked <-
                    withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
                        mapConcurrently
                            ( \(sid, queue) ->
                                fmap ((,) sid) (unlift (walkQueue store logCache level (Map.findWithDefault Set.empty sid seen) queue))
                            )
                            [ (sid, queue)
                            | (sid, queue) <- Map.toList frontier
                            , not (Map.member sid done)
                            ]
                nextFrontier <- liftIO $ newIORef Map.empty
                forM_ walked $ \(sid, (mLog, seen', expanded)) -> do
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
  where
    seedFailureLog logCache (sid, drv) = do
        mLog <- cachedLog fetchInputLog logCache drv
        return $ fmap (\logText -> (sid, ("failure", lastMeaningfulLine logText))) mLog

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
                mLog <- cachedLog fetchInputLog logCache drv
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

cachedLog :: (Nix :> es, IOE :> es) => (FilePath -> Eff es (Maybe String)) -> IORef (Map FilePath (Maybe String)) -> FilePath -> Eff es (Maybe String)
cachedLog fetch cacheRef drv = do
    cached <- Map.lookup drv <$> liftIO (readIORef cacheRef)
    case cached of
        Just result -> return result
        Nothing -> do
            result <- fetch drv
            liftIO $ atomicModifyIORef' cacheRef (\entries -> (Map.insert drv result entries, ()))
            return result

fetchNodes :: (Nix :> es, IOE :> es) => IORef (Map FilePath DrvNode) -> [FilePath] -> Eff es (Map FilePath DrvNode)
fetchNodes cacheRef paths = do
    cached <- liftIO $ readIORef cacheRef
    let wanted = Set.fromList paths
        missing = Set.toList (wanted `Set.difference` Map.keysSet cached)
    fetched <-
        if null missing
            then return Map.empty
            else do
                nodes <- queryDerivations missing
                liftIO $ modifyIORef' cacheRef (Map.union nodes)
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

