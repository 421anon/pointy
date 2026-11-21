{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Handlers.StepInputs (
    ColdInputSource (..),
    ColdInputsView (..),
    coldInputReaders,
    coldInputsHandler,
    coldInputsOnloadHandler,
) where

import BuildLog (validPaths)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Monad (unless)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Effects (AppM, coldStorageConfigured, coldStorageOnload)
import Handlers.Cold (ColdFile (..), ColdStatus (..), coldStatusFor, mapColdProvider, onloadable)
import Handlers.RunStep (StepTarget (..), getDependencyGraph, resolveStepTargets)
import Handlers.StepReview (stepOutPaths)
import Servant (ServerError (..), err500, err502, err503)
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), commitContextEff, withReadRepoTransaction)

data ReturnedFile = ReturnedFile
    { returnedFilePath :: Text
    , returnedFileSize :: Integer
    }
    deriving (Show, Eq)

instance ToJSON ReturnedFile where
    toJSON file = object ["path" .= returnedFilePath file, "size" .= returnedFileSize file]

data ColdInputSource = ColdInputSource
    { coldInputSourceStep :: Int
    , coldInputSourceFiles :: [ColdFile]
    , coldInputSourceBack :: [ReturnedFile]
    }

instance ToJSON ColdInputSource where
    toJSON source =
        object
            [ "step" .= coldInputSourceStep source
            , "files" .= coldInputSourceFiles source
            , "back" .= coldInputSourceBack source
            ]

data ColdInputsView = ColdInputsView
    { coldInputsBlocked :: [Int]
    , coldInputsSources :: [ColdInputSource]
    }

instance ToJSON ColdInputsView where
    toJSON view =
        object
            [ "blocked" .= coldInputsBlocked view
            , "sources" .= coldInputsSources view
            ]

data RunInputs = RunInputs
    { runInputsGraph :: Map Int [Int]
    , runInputsBuilt :: Set Int
    , runInputsOutPaths :: Map Int Text
    }

coldInputsHandler :: Int -> Maybe Text -> AppM ColdInputsView
coldInputsHandler eid commit = coldInputsView =<< runInputsOf eid commit

{-# NOINLINE copyBackRequests #-}
copyBackRequests :: MVar (Map Text (Map Text Integer))
copyBackRequests = unsafePerformIO (newMVar Map.empty)

recordCopyBackRequests :: [(Text, ColdFile)] -> IO ()
recordCopyBackRequests requested =
    modifyMVar_ copyBackRequests $ \known ->
        pure $ Map.unionWith Map.union (Map.fromListWith Map.union [(outPath, Map.singleton (coldFilePath file) (coldFileSize file)) | (outPath, file) <- requested]) known

returnedFiles :: Map Text Integer -> [ColdFile] -> [ReturnedFile]
returnedFiles requested cold =
    [ReturnedFile path size | (path, size) <- Map.toList (Map.withoutKeys requested (Set.fromList (map coldFilePath cold)))]

coldInputsOnloadHandler :: Int -> Maybe Text -> AppM ColdInputsView
coldInputsOnloadHandler eid commit = do
    inputs <- runInputsOf eid commit
    view <- coldInputsView inputs
    let cold =
            [ (step, outPath, file)
            | ColdInputSource step files _ <- coldInputsSources view
            , Just outPath <- [Map.lookup step (runInputsOutPaths inputs)]
            , file <- files
            ]
        requests = [(step, outPath, coldFilePath file) | (step, outPath, file) <- cold, onloadable file]
    liftIO $ recordCopyBackRequests [(outPath, file) | (_, outPath, file) <- cold]
    outcomes <- lift $ mapColdProvider (\(_, outPath, rel) -> coldStorageOnload outPath (T.unpack rel)) requests
    let failures =
            [ "step " <> T.pack (show step) <> ": " <> rel <> ": " <> message
            | ((step, _, rel), Left message) <- zip requests outcomes
            ]
    unless (null failures) $
        failWith err502 ("Could not start copying back: " ++ T.unpack (T.intercalate "; " failures))
    coldInputsView inputs

runInputsOf :: Int -> Maybe Text -> AppM RunInputs
runInputsOf eid commit = do
    configured <- lift coldStorageConfigured
    if not configured
        then pure (RunInputs Map.empty Set.empty Map.empty)
        else do
            context <- orFail err500 =<< lift (withReadRepoTransaction (\head_ -> maybe (pure head_) (commitContextEff (readRepoPath head_)) commit))
            graph <- orFail err500 =<< lift (runExceptT (getDependencyGraph (pure False) context eid))
            targets <- lift (resolveStepTargets context (Map.keys graph))
            case [(sid, err) | (sid, Left err) <- Map.toList targets] of
                (sid, err) : _ -> failWith err500 ("Step " ++ show sid ++ " has no build target: " ++ err)
                [] -> do
                    unless (Map.member eid targets) $
                        failWith err500 ("Step " ++ show eid ++ " has no build target at " ++ readCommitHash context ++ ".")
                    let certificates = Map.fromList [(sid, stepTargetPath target) | (sid, Right target) <- Map.toList targets]
                    valid <- maybe (failWith err500 "Cannot tell which upstream steps are already built.") pure =<< lift (validPaths (Set.toList (Set.fromList (Map.elems certificates))))
                    let built = Map.keysSet (Map.filter (`Set.member` valid) certificates)
                        builtInputs = nubOrd [dep | (sid, deps) <- Map.toList graph, Set.notMember sid built, dep <- deps, Set.member dep built]
                    evaluated <- orFail err500 =<< lift (runExceptT (stepOutPaths context builtInputs))
                    let (unresolved, outPaths) = Map.mapEither id evaluated
                    case Map.keys unresolved of
                        step : _ -> failWith err503 ("The output path of step " ++ show step ++ " could not be evaluated. Try again.")
                        [] -> pure (RunInputs graph built (Map.map T.pack outPaths))

coldInputsView :: RunInputs -> AppM ColdInputsView
coldInputsView inputs = do
    statuses <- lift $ mapColdProvider (\(step, outPath) -> (step,) <$> coldStatusFor outPath) (Map.toList (runInputsOutPaths inputs))
    liftIO $ recordCopyBackRequests [(outPath, file) | (step, Just status) <- statuses, Just outPath <- [Map.lookup step (runInputsOutPaths inputs)], file <- coldStatusFiles status, not (onloadable file)]
    requested <- liftIO $ readMVar copyBackRequests
    case [step | (step, Nothing) <- statuses] of
        step : _ -> failWith err503 ("Cold storage status is unavailable for step " ++ show step ++ ". Try again.")
        [] -> do
            let files = Map.fromList [(step, coldStatusFiles status) | (step, Just status) <- statuses]
                filesOf step = Map.findWithDefault [] step files
                backOf step = returnedFiles (maybe Map.empty (\outPath -> Map.findWithDefault Map.empty outPath requested) (Map.lookup step (runInputsOutPaths inputs))) (filesOf step)
                built = (`Set.member` runInputsBuilt inputs)
                readers = coldInputReaders (runInputsGraph inputs) built (not . null . filesOf)
                involved = coldInputReaders (runInputsGraph inputs) built (\step -> not (null (filesOf step)) || not (null (backOf step)))
                sources = nubOrd (concat (Map.elems involved))
            pure (ColdInputsView (Map.keys readers) [ColdInputSource step (filesOf step) (backOf step) | step <- sources])

coldInputReaders :: Map Int [Int] -> (Int -> Bool) -> (Int -> Bool) -> Map Int [Int]
coldInputReaders graph built holdsColdFiles = Map.mapMaybeWithKey coldInputsOfStep graph
  where
    coldInputsOfStep sid deps
        | built sid = Nothing
        | otherwise = case filter (\dep -> built dep && holdsColdFiles dep) deps of
            [] -> Nothing
            coldDeps -> Just coldDeps

orFail :: ServerError -> Either String a -> AppM a
orFail err = either (failWith err) pure

failWith :: ServerError -> String -> AppM a
failWith err message = throwError err{errBody = TLE.encodeUtf8 (TL.pack message)}
