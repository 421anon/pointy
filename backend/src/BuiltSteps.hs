{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BuiltSteps (
    LastBuiltVersion (..),
    recordBuiltSteps,
    recordBuiltStatuses,
    markSearched,
    unsearchedSteps,
    firstParentHistory,
    builtSteps,
    resolveLastBuilt,
) where

import BuildLog (validPaths)
import BuildStatus (StepPaths (..))
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, catch, handle)
import Control.Monad (filterM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeFileStrict, encode, object, withObject, (.!=), (.:?), (.=))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effectful (Eff, IOE, (:>))
import Effects (Nix, pathValid)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, listDirectory, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath ((<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import UserRepo (runGitIn, userRepoPath)

data LastBuiltVersion = LastBuiltVersion
    { lastBuiltRevision :: Text
    , lastBuiltCommittedAt :: UTCTime
    }
    deriving (Eq, Show)

instance ToJSON LastBuiltVersion where
    toJSON (LastBuiltVersion revision committedAt) = object ["revision" .= revision, "committedAt" .= committedAt]

data CommitRecord = CommitRecord
    { recordBuilt :: Map Int StepPaths
    , recordSearched :: Set Int
    }
    deriving (Eq)

instance ToJSON CommitRecord where
    toJSON (CommitRecord built searched) = object ["built" .= built, "searched" .= searched]

instance FromJSON CommitRecord where
    parseJSON = withObject "CommitRecord" $ \fields ->
        CommitRecord <$> fields .:? "built" .!= Map.empty <*> fields .:? "searched" .!= Set.empty

{-# NOINLINE recordsRef #-}
recordsRef :: MVar (Maybe (Map Text CommitRecord))
recordsRef = unsafePerformIO (newMVar Nothing)

{-# NOINLINE historyCacheRef #-}
historyCacheRef :: MVar [(Text, [(Text, UTCTime)])]
historyCacheRef = unsafePerformIO (newMVar [])

historyCacheLimit :: Int
historyCacheLimit = 8

builtStatusNames :: [Text]
builtStatusNames = ["success", "built-not-certified", "certification-failed"]

recordBuiltSteps :: Text -> Map Int StepPaths -> IO ()
recordBuiltSteps commit paths =
    updateRecord commit $ \record -> record{recordBuilt = Map.union (recordBuilt record) paths}

recordBuiltStatuses :: Text -> Map Int StepPaths -> Map Int (Text, Maybe Text) -> IO ()
recordBuiltStatuses commit paths =
    recordBuiltSteps commit . Map.restrictKeys paths . Map.keysSet . Map.filter ((`elem` builtStatusNames) . fst)

markSearched :: Text -> Set Int -> IO ()
markSearched commit steps =
    updateRecord commit $ \record -> record{recordSearched = recordSearched record <> steps}

unsearchedSteps :: Set Int -> IO (Set Int)
unsearchedSteps steps = Set.difference steps . foldMap explored <$> readRecords
  where
    explored record = Map.keysSet (recordBuilt record) <> recordSearched record

firstParentHistory :: Text -> IO (Either String [(Text, UTCTime)])
firstParentHistory revision =
    maybe (readHistory revision) (pure . Right) . lookup revision =<< readMVar historyCacheRef

resolveLastBuilt :: (Nix :> es, IOE :> es) => Text -> [Int] -> Eff es (Map Int LastBuiltVersion)
resolveLastBuilt revision steps = do
    records <- liftIO readRecords
    history <- liftIO (firstParentHistory revision)
    case history of
        Left err -> do
            liftIO $ putStrLn $ "Last built versions at " ++ unpack revision ++ " are unavailable: " ++ err
            pure Map.empty
        Right entries -> nearestBuilt (Map.fromList [(sid, recordedBuilds records sid (drop 1 entries)) | sid <- steps])

recordedBuilds :: Map Text CommitRecord -> Int -> [(Text, UTCTime)] -> [(LastBuiltVersion, StepPaths)]
recordedBuilds records sid ancestors =
    [ (LastBuiltVersion commit committedAt, paths)
    | (commit, committedAt) <- ancestors
    , Just paths <- [Map.lookup sid . recordBuilt =<< Map.lookup commit records]
    ]

nearestBuilt :: (Nix :> es) => Map Int [(LastBuiltVersion, StepPaths)] -> Eff es (Map Int LastBuiltVersion)
nearestBuilt candidates = do
    valid <- validAmong (foldMap (foldMap (storePaths . snd)) candidates)
    pure (Map.mapMaybe (fmap fst . find (isBuiltWith valid . snd)) candidates)

builtSteps :: (Nix :> es) => Map Int StepPaths -> Eff es (Map Int StepPaths)
builtSteps paths = flip Map.filter paths . isBuiltWith <$> validAmong (foldMap storePaths paths)

validAmong :: (Nix :> es) => Set FilePath -> Eff es (Set FilePath)
validAmong paths = validPaths (Set.toList paths) >>= maybe (Set.fromList <$> filterM pathValid (Set.toList paths)) pure

isBuiltWith :: Set FilePath -> StepPaths -> Bool
isBuiltWith valid = not . Set.disjoint valid . storePaths

storePaths :: StepPaths -> Set FilePath
storePaths (StepPaths certificate output) = Set.fromList [certificate, output]

readHistory :: Text -> IO (Either String [(Text, UTCTime)])
readHistory revision = do
    repoPath <- userRepoPath
    (code, out, err) <- runGitIn repoPath ["log", "--first-parent", "--format=%H %ct", "--end-of-options", unpack revision, "--"]
    case code of
        ExitFailure _ -> pure (Left ("git log " ++ unpack revision ++ " failed: " ++ err))
        ExitSuccess -> do
            let history = mapMaybe historyEntry (lines out)
                resolvedRevision = fst <$> listToMaybe history
            when (resolvedRevision == Just revision) $
                modifyMVar_ historyCacheRef (pure . take historyCacheLimit . ((revision, history) :) . filter ((/= revision) . fst))
            pure (Right history)

historyEntry :: String -> Maybe (Text, UTCTime)
historyEntry line = case words line of
    [commit, seconds] -> (,) (pack commit) . posixSecondsToUTCTime . fromInteger <$> readMaybe seconds
    _ -> Nothing

updateRecord :: Text -> (CommitRecord -> CommitRecord) -> IO ()
updateRecord commit update =
    when (isCommitHash commit) $
        handle (\(err :: IOException) -> putStrLn ("Built steps at " ++ unpack commit ++ " were not recorded: " ++ show err)) $
            withRecords $ \records -> do
                let recorded = Map.findWithDefault emptyRecord commit records
                    updated = update recorded
                if updated == recorded
                    then pure (records, ())
                    else (Map.insert commit updated records, ()) <$ storeRecord commit updated

withRecords :: (Map Text CommitRecord -> IO (Map Text CommitRecord, a)) -> IO a
withRecords action = modifyMVar recordsRef $ \cached -> first Just <$> (action =<< maybe loadRecords pure cached)

readRecords :: IO (Map Text CommitRecord)
readRecords = withRecords (\records -> pure (records, records))

loadRecords :: IO (Map Text CommitRecord)
loadRecords = do
    directory <- builtStepsDirectory
    names <- listDirectory directory `catch` \(_ :: IOException) -> pure []
    Map.mapMaybe id <$> sequence (Map.fromList [(commit, readRecord (recordPath directory commit)) | commit <- mapMaybe (T.stripSuffix ".json" . pack) names])

readRecord :: FilePath -> IO (Maybe CommitRecord)
readRecord path = decodeFileStrict path `catch` \(_ :: IOException) -> pure Nothing

storeRecord :: Text -> CommitRecord -> IO ()
storeRecord commit record = do
    directory <- builtStepsDirectory
    createDirectoryIfMissing True directory
    LBS.writeFile (recordPath directory commit <.> "tmp") (encode record)
    renameFile (recordPath directory commit <.> "tmp") (recordPath directory commit)

recordPath :: FilePath -> Text -> FilePath
recordPath directory commit = directory </> unpack commit <.> "json"

builtStepsDirectory :: IO FilePath
builtStepsDirectory = do
    home <- getHomeDirectory
    pure $ home </> ".local" </> "state" </> "pointy" </> "built-steps"

isCommitHash :: Text -> Bool
isCommitHash commit = not (T.null commit) && T.all isHexDigit commit

emptyRecord :: CommitRecord
emptyRecord = CommitRecord Map.empty Set.empty
