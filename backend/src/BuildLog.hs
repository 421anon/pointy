{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BuildLog (
    ResolvedLog (..),
    LogSource (..),
    LogAccess (..),
    resolveBuildLog,
    validPaths,
    isStorePath,
    lookupDeriver,
    lastMeaningfulLine,
    StepStore,
    buildStepStore,
    rawStatusesBatched,
    resolveStatusesBatched,
) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent.QSem (QSem, newQSem, signalQSem, waitQSem)
import Control.Monad (forM_, guard, unless)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Value (Null), eitherDecodeStrict, withObject, (.:), (.:?), (.!=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf, sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text.Encoding as TE
import Effectful (Eff, IOE, Limit (Unlimited), Persistence (Persistent), UnliftStrategy (ConcUnlift), (:>), withEffToIO)
import Effectful.Exception (bracket_)
import Effects (LogStamp (..), Nix, localLogStamp, pathValid, runNixStoreCli)
import System.Exit (ExitCode (..))
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (runNix)

data LogSource
    = StepDrv
    | InputDrv FilePath Int
    deriving (Eq, Show)

data LogAccess
    = LocalLogs
    | RemoteLogs
    deriving (Eq, Show)

data ResolvedLog = ResolvedLog
    { resolvedDrv :: FilePath
    , resolvedLog :: ByteString
    , resolvedSource :: LogSource
    }
    deriving (Eq, Show)

resolveBuildLog :: (Nix :> es, IOE :> es) => LogAccess -> FilePath -> Eff es (Maybe ResolvedLog)
resolveBuildLog access stepDrv = do
    logCache <- liftIO $ newIORef Map.empty
    nodeCache <- liftIO $ newIORef Map.empty
    mOwn <- cachedLog (fetchStepLog access) logCache stepDrv
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
                mLog <- cachedLog (fetchInputLog access) logCache drv
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

lastMeaningfulLine :: ByteString -> Maybe Text
lastMeaningfulLine = fmap TE.decodeUtf8Lenient . lastNonEmptyLine

lastNonEmptyLine :: ByteString -> Maybe ByteString
lastNonEmptyLine output = findLine (stripTrailingNewline output)
  where
    stripTrailingNewline s = case BS.unsnoc s of
        Just (rest, 10) -> rest
        _ -> s
    findLine s
        | BS.null s = Nothing
        | otherwise = case BS.elemIndexEnd 10 s of
            Nothing -> Just s
            Just index ->
                let line = BS.drop (index + 1) s
                 in if BS.null line then findLine (BS.take index s) else Just line

maxBfsDepth :: Int
maxBfsDepth = 6

lookupDeriver :: (Nix :> es) => String -> Eff es (Maybe FilePath)
lookupDeriver target = do
    result <- runExceptT $ runNix ["path-info", "--derivation", target]
    return $ case result of
        Right out ->
            case filter (not . BC.null) (BC.lines out) of
                (drv : _) -> Just (BC.unpack drv)
                _ -> Nothing
        Left _ -> Nothing

logArgs :: LogAccess -> [String] -> [String]
logArgs LocalLogs args = ["--option", "substituters", ""] ++ args
logArgs RemoteLogs args = args

{-# NOINLINE logSemaphore #-}
logSemaphore :: QSem
logSemaphore = unsafePerformIO (newQSem 16)

runLog :: (Nix :> es, IOE :> es) => LogAccess -> [String] -> Eff es (Either String ByteString)
runLog access args = bracket_ (liftIO (waitQSem logSemaphore)) (liftIO (signalQSem logSemaphore)) (runExceptT (runNix (logArgs access args)))

nonEmptyLog :: Either String ByteString -> Maybe ByteString
nonEmptyLog (Right output) | not (BS.null output) = Just output
nonEmptyLog _ = Nothing

logAvailable :: (Nix :> es) => LogAccess -> FilePath -> Eff es Bool
logAvailable LocalLogs drv = isJust <$> localLogStamp drv
logAvailable RemoteLogs _ = return True

fetchStepLog :: (Nix :> es, IOE :> es) => LogAccess -> FilePath -> Eff es (Maybe ByteString)
fetchStepLog access drv = do
    available <- logAvailable access drv
    if available then nonEmptyLog <$> runLog access ["log", drv] else return Nothing

fetchInputLog :: (Nix :> es, IOE :> es) => LogAccess -> FilePath -> Eff es (Maybe ByteString)
fetchInputLog access drv = do
    available <- logAvailable access drv
    if available then nonEmptyLog <$> runLog access ["--offline", "log", drv] else return Nothing

{-# NOINLINE localLogLineCache #-}
localLogLineCache :: IORef (Map FilePath (LogStamp, Maybe (Maybe Text)))
localLogLineCache = unsafePerformIO (newIORef Map.empty)

localLogLine :: (Nix :> es, IOE :> es) => FilePath -> Eff es (Maybe (Maybe Text))
localLogLine drv = do
    mStamp <- localLogStamp drv
    case mStamp of
        Nothing -> return Nothing
        Just stamp -> do
            cached <- Map.lookup drv <$> liftIO (readIORef localLogLineCache)
            case cached of
                Just (cachedStamp, value) | cachedStamp == stamp -> return value
                _ -> do
                    result <- runLog LocalLogs ["--offline", "log", drv]
                    case result of
                        Right logBytes -> do
                            let value = lastMeaningfulLine <$> nonEmptyLog (Right logBytes)
                            liftIO $ atomicModifyIORef' localLogLineCache (\entries -> (Map.insert drv (stamp, value) entries, ()))
                            return value
                        Left _ -> return Nothing

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

decodeValidity :: ByteString -> Maybe (Map Text Value)
decodeValidity = either (const Nothing) Just . eitherDecodeStrict

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
    let seeds =
            [ (sid, drv)
            | (sid, (state, _)) <- Map.toList statuses
            , state == "failure" || state == "not-started"
            , Just drv <- [Map.lookup sid (ssDrvOf store)]
            ]
    ownLogs <-
        withEffToIO (ConcUnlift Persistent Unlimited) $ \unlift ->
            mapConcurrently (unlift . seedFailureLog) seeds
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
                                fmap ((,) sid) (unlift (walkQueue store level (Map.findWithDefault Set.empty sid seen) queue))
                            )
                            [ (sid, queue)
                            | (sid, queue) <- Map.toList frontier
                            , not (Map.member sid done)
                            ]
                nextFrontier <- liftIO $ newIORef Map.empty
                forM_ walked $ \(sid, (mLine, seen', expanded)) -> do
                    liftIO $ modifyIORef' visited (Map.insert sid seen')
                    case mLine of
                        Just line ->
                            liftIO $ modifyIORef' resolved (Map.insert sid ("failure", line))
                        Nothing ->
                            unless (null expanded) $
                                liftIO $ modifyIORef' nextFrontier (Map.insert sid expanded)
                liftIO $ writeIORef frontierRef =<< readIORef nextFrontier
                levelLoop
    levelLoop
    resolvedMap <- liftIO $ readIORef resolved
    return $ Map.union resolvedMap statuses
  where
    seedFailureLog (sid, drv) = do
        mLine <- localLogLine drv
        return $ fmap (\line -> (sid, ("failure", line))) mLine

walkQueue :: (Nix :> es, IOE :> es) => StepStore -> Map FilePath DrvNode -> Set FilePath -> [(FilePath, Int)] -> Eff es (Maybe (Maybe Text), Set FilePath, [(FilePath, Int)])
walkQueue store level seen queue = go seen [] queue
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
                mLine <- localLogLine drv
                case mLine of
                    Just line -> do
                        allInvalid <- outputsAllInvalid node
                        if allInvalid
                            then return (Just line, Set.insert drv visited, expanded)
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

cachedLog :: (Nix :> es, IOE :> es) => (FilePath -> Eff es (Maybe ByteString)) -> IORef (Map FilePath (Maybe ByteString)) -> FilePath -> Eff es (Maybe ByteString)
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

decodeDrvNodes :: ByteString -> Maybe (Map FilePath DrvNode)
decodeDrvNodes = either (const Nothing) Just . eitherDecodeStrict

queryStorePlan :: (Nix :> es) => [FilePath] -> Eff es StorePlan
queryStorePlan [] = return (StorePlan Set.empty Set.empty)
queryStorePlan [path] = do
    (code, out, err) <- runNixStoreCli ["--realise", "--dry-run", path]
    return $ case code of
        ExitSuccess -> parseStorePlan (out <> err)
        ExitFailure _ -> StorePlan (Set.singleton path) Set.empty
queryStorePlan paths = do
    (code, out, err) <- runNixStoreCli (["--realise", "--dry-run"] ++ paths)
    case code of
        ExitSuccess -> return (parseStorePlan (out <> err))
        ExitFailure _ -> do
            let (left, right) = splitAt (length paths `div` 2) paths
            leftPlan <- queryStorePlan left
            rightPlan <- queryStorePlan right
            return
                StorePlan
                    { spBuild = spBuild leftPlan <> spBuild rightPlan
                    , spFetch = spFetch leftPlan <> spFetch rightPlan
                    }

parseStorePlan :: ByteString -> StorePlan
parseStorePlan = go False (StorePlan Set.empty Set.empty) . BC.lines
  where
    go _ plan [] = plan
    go building plan (line : rest)
        | BC.isSuffixOf ":" line = go ("will be built:" `BC.isSuffixOf` line) plan rest
        | Just path <- entryOf line =
            go
                building
                ( if building
                    then plan{spBuild = Set.insert path (spBuild plan)}
                    else plan{spFetch = Set.insert path (spFetch plan)}
                )
                rest
        | otherwise = go building plan rest

    entryOf line = case BC.words line of
        [path] | "/nix/store/" `BC.isPrefixOf` path -> Just (BC.unpack path)
        _ -> Nothing

