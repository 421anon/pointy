{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Session (
    AgentSession (..),
    AgentSessionSummary (..),
    PreparedApply (..),
    applyConflictsPending,
    AgentTurn (..),
    TurnImage (..),
    AreaCapture (..),
    StoredAreaCapture (..),
    CapturedArtifact (..),
    DocumentSize (..),
    SelectionRect (..),
    turnIsUnfinished,
    latestUnfinishedTurn,
    inferTurnExitCode,
    turnLogHasFinalizationFailure,
    sessionCompacting,
    acquireSessionCompaction,
    releaseSessionCompaction,
    agentSessionsRoot,
    sessionDir,
    sessionMetadataPath,
    turnsDir,
    turnMetadataPath,
    turnLogFilePath,
    turnImageFilePath,
    newTurnId,
    newSessionId,
    normalizeSessionName,
    freshSessionLayout,
    loadSession,
    saveSession,
    loadSessionById,
    listSessions,
    saveTurn,
    loadTurn,
    listTurns,
    listTurnsWithLogs,
    findTurn,
    forgetSessionTurns,
    touchSession,
) where

import Agent.TurnLog (readTurnLogFile)
import Agent.TurnSignal (signalTurnLog)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Monad (filterM, unless)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, object, withObject, (.!=), (.:), (.:?), (.=))
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory, renameFile)
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.IO (hClose, openBinaryTempFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessID)

{-# NOINLINE turnIndex #-}
turnIndex :: MVar (Map FilePath (Map Text AgentTurn))
turnIndex = unsafePerformIO (newMVar Map.empty)

{-# NOINLINE compactingSessions #-}
compactingSessions :: TVar (Set.Set Text)
compactingSessions = unsafePerformIO (newTVarIO Set.empty)

sessionCompacting :: Text -> IO Bool
sessionCompacting sid =
    atomically $ Set.member sid <$> readTVar compactingSessions

acquireSessionCompaction :: Text -> IO Bool
acquireSessionCompaction sid =
    atomically $ do
        active <- readTVar compactingSessions
        if Set.member sid active
            then return False
            else do
                modifyTVar' compactingSessions (Set.insert sid)
                return True

releaseSessionCompaction :: Text -> IO ()
releaseSessionCompaction sid =
    atomically $ modifyTVar' compactingSessions (Set.delete sid)

data PreparedApply = PreparedApply
    { targetHead :: Text
    , agentHead :: Text
    , candidateHead :: Text
    , candidateWorktree :: FilePath
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

applyConflictsPending :: PreparedApply -> Bool
applyConflictsPending = T.null . candidateHead

data AgentSession = AgentSession
    { sessionId :: Text
    , sessionName :: Maybe Text
    , targetBranch :: Text
    , agentBranch :: Text
    , baseCommit :: Text
    , worktreePath :: FilePath
    , status :: Text
    , preparedApply :: Maybe PreparedApply
    , activeTurnId :: Maybe Text
    , lastError :: Maybe Text
    , createdAt :: UTCTime
    , updatedAt :: UTCTime
    , agentCurrentProjectId :: Maybe Int
    , autoApply :: Bool
    , autoApplyClient :: Maybe Text
    }
    deriving (Show, Eq, Generic, ToJSON)

data AgentSessionSummary = AgentSessionSummary
    { session :: AgentSession
    , title :: Text
    , turnCount :: Int
    , hasCommits :: Bool
    }
    deriving (Show, Eq, Generic, ToJSON)

instance FromJSON AgentSession where
    parseJSON = withObject "AgentSession" $ \obj ->
        AgentSession
            <$> obj .: "sessionId"
            <*> obj .:? "sessionName" .!= Nothing
            <*> obj .: "targetBranch"
            <*> obj .: "agentBranch"
            <*> obj .: "baseCommit"
            <*> obj .: "worktreePath"
            <*> obj .: "status"
            <*> obj .:? "preparedApply"
            <*> obj .:? "activeTurnId"
            <*> obj .:? "lastError"
            <*> obj .: "createdAt"
            <*> obj .: "updatedAt"
            <*> obj .:? "agentCurrentProjectId"
            <*> obj .:? "autoApply" .!= True
            <*> obj .:? "autoApplyClient"

data AgentTurn = AgentTurn
    { turnId :: Text
    , turnSessionId :: Text
    , turnPrompt :: Text
    , turnImages :: [Text]
    , turnAreaCaptures :: [StoredAreaCapture]
    , turnAutomatic :: Bool
    , turnStatus :: Text
    , turnExitCode :: Maybe Int
    , turnStartedAt :: UTCTime
    , turnFinishedAt :: Maybe UTCTime
    , turnLogPath :: FilePath
    , turnLog :: Text
    }
    deriving (Show, Eq, Generic)

instance ToJSON AgentTurn where
    toJSON turn =
        object
            [ "turnId" .= turnId turn
            , "turnSessionId" .= turnSessionId turn
            , "turnPrompt" .= turnPrompt turn
            , "turnImages" .= turnImages turn
            , "turnAreaCaptures" .= turnAreaCaptures turn
            , "turnAutomatic" .= turnAutomatic turn
            , "turnStatus" .= turnStatus turn
            , "turnExitCode" .= turnExitCode turn
            , "turnStartedAt" .= turnStartedAt turn
            , "turnFinishedAt" .= turnFinishedAt turn
            , "turnLogPath" .= turnLogPath turn
            , "turnLog" .= turnLog turn
            ]

instance FromJSON AgentTurn where
    parseJSON = withObject "AgentTurn" $ \obj ->
        AgentTurn
            <$> obj .: "turnId"
            <*> obj .: "turnSessionId"
            <*> obj .:? "turnPrompt" .!= ""
            <*> obj .:? "turnImages" .!= []
            <*> obj .:? "turnAreaCaptures" .!= []
            <*> obj .:? "turnAutomatic" .!= False
            <*> obj .: "turnStatus"
            <*> obj .:? "turnExitCode"
            <*> obj .: "turnStartedAt"
            <*> obj .:? "turnFinishedAt"
            <*> obj .: "turnLogPath"
            <*> obj .:? "turnLog" .!= ""

data TurnImage = TurnImage
    { turnImageMimeType :: Text
    , turnImageBase64 :: Text
    , turnImageBytes :: BS.ByteString
    }
    deriving (Show, Eq)

instance FromJSON TurnImage where
    parseJSON = withObject "TurnImage" $ \obj -> do
        mimeType <- obj .: "mimeType"
        unless (mimeType `elem` turnImageMimeTypes) $
            fail ("unsupported image type " <> T.unpack mimeType)
        encoded <- obj .: "data"
        either fail (pure . TurnImage mimeType encoded) (convertFromBase Base64 (TE.encodeUtf8 encoded))

turnImageMimeTypes :: [Text]
turnImageMimeTypes = ["image/png", "image/jpeg", "image/gif", "image/webp"]

data DocumentSize = DocumentSize
    { documentWidth :: Int
    , documentHeight :: Int
    }
    deriving (Show, Eq)

instance ToJSON DocumentSize where
    toJSON size = object ["width" .= documentWidth size, "height" .= documentHeight size]

instance FromJSON DocumentSize where
    parseJSON = withObject "DocumentSize" $ \obj -> DocumentSize <$> obj .: "width" <*> obj .: "height"

data SelectionRect = SelectionRect
    { selectionX :: Int
    , selectionY :: Int
    , selectionWidth :: Int
    , selectionHeight :: Int
    }
    deriving (Show, Eq)

instance ToJSON SelectionRect where
    toJSON rect = object ["x" .= selectionX rect, "y" .= selectionY rect, "width" .= selectionWidth rect, "height" .= selectionHeight rect]

instance FromJSON SelectionRect where
    parseJSON = withObject "SelectionRect" $ \obj ->
        SelectionRect <$> obj .: "x" <*> obj .: "y" <*> obj .: "width" <*> obj .: "height"

data CapturedArtifact = CapturedArtifact
    { capturedProjectPath :: [Int]
    , capturedStepId :: Int
    , capturedCommit :: Text
    , capturedPath :: [Text]
    }
    deriving (Show, Eq)

instance ToJSON CapturedArtifact where
    toJSON artifact =
        object
            [ "projectPath" .= capturedProjectPath artifact
            , "stepId" .= capturedStepId artifact
            , "commit" .= capturedCommit artifact
            , "path" .= capturedPath artifact
            ]

instance FromJSON CapturedArtifact where
    parseJSON = withObject "CapturedArtifact" $ \obj ->
        CapturedArtifact <$> obj .: "projectPath" <*> obj .: "stepId" <*> obj .: "commit" <*> obj .: "path"

data AreaCapture = AreaCapture
    { areaCaptureOverview :: TurnImage
    , areaCaptureSelection :: TurnImage
    , areaCaptureDocumentSize :: DocumentSize
    , areaCaptureSelectionRect :: SelectionRect
    , areaCaptureArtifact :: CapturedArtifact
    }
    deriving (Show, Eq)

instance FromJSON AreaCapture where
    parseJSON = withObject "AreaCapture" $ \obj ->
        AreaCapture <$> obj .: "overview" <*> obj .: "selection" <*> obj .: "documentSize" <*> obj .: "selectionRect" <*> obj .: "artifact"

data StoredAreaCapture = StoredAreaCapture
    { storedOverviewImage :: Int
    , storedSelectionImage :: Int
    , storedDocumentSize :: DocumentSize
    , storedSelectionRect :: SelectionRect
    , storedArtifact :: Maybe CapturedArtifact
    }
    deriving (Show, Eq)

instance ToJSON StoredAreaCapture where
    toJSON stored =
        object
            [ "overviewImage" .= storedOverviewImage stored
            , "selectionImage" .= storedSelectionImage stored
            , "documentSize" .= storedDocumentSize stored
            , "selectionRect" .= storedSelectionRect stored
            , "artifact" .= storedArtifact stored
            ]

instance FromJSON StoredAreaCapture where
    parseJSON = withObject "StoredAreaCapture" $ \obj ->
        StoredAreaCapture <$> obj .: "overviewImage" <*> obj .: "selectionImage" <*> obj .: "documentSize" <*> obj .: "selectionRect" <*> obj .:? "artifact"

turnIsUnfinished :: AgentTurn -> Bool
turnIsUnfinished turn =
    turnStatus turn == "running"
        || turnExitCode turn == Nothing
        || turnFinishedAt turn == Nothing

latestUnfinishedTurn :: [AgentTurn] -> Maybe AgentTurn
latestUnfinishedTurn =
    listToMaybe . reverse . filter turnIsUnfinished

inferTurnExitCode :: Text -> Maybe Int
inferTurnExitCode logText = go (reverse (T.lines logText))
  where
    prefix = "[system] Agent turn finished with exit code "

    go [] = Nothing
    go (line : rest) =
        case T.stripPrefix prefix line of
            Just codeText ->
                case reads (T.unpack codeText) of
                    [(code, "")] -> Just code
                    _ -> go rest
            Nothing -> go rest

turnLogHasFinalizationFailure :: Text -> Bool
turnLogHasFinalizationFailure =
    any (T.isPrefixOf "[system] Turn finalization error:") . T.lines

agentSessionsRoot :: IO FilePath
agentSessionsRoot = do
    home <- getHomeDirectory
    return $ home </> "agent-sessions"

sessionDir :: Text -> IO FilePath
sessionDir sid = do
    root <- agentSessionsRoot
    return $ root </> T.unpack sid

sessionMetadataPath :: Text -> IO FilePath
sessionMetadataPath sid = do
    dir <- sessionDir sid
    return $ dir </> "session.json"

turnsDir :: Text -> IO FilePath
turnsDir sid = do
    dir <- sessionDir sid
    return $ dir </> "turns"

turnMetadataPath :: Text -> Text -> IO FilePath
turnMetadataPath sid tid = do
    dir <- turnsDir sid
    return $ dir </> (T.unpack tid ++ ".json")

turnLogFilePath :: Text -> Text -> IO FilePath
turnLogFilePath sid tid = do
    dir <- turnsDir sid
    return $ dir </> (T.unpack tid ++ ".log")

turnImageFilePath :: Text -> Text -> Int -> IO FilePath
turnImageFilePath sid tid index = do
    dir <- turnsDir sid
    return $ dir </> (T.unpack tid ++ "-image-" ++ show index)

newSessionId :: IO Text
newSessionId = do
    stamp <- floor . (* 1000000) <$> getPOSIXTime :: IO Integer
    pid <- getProcessID
    return $ T.pack (show stamp ++ "-" ++ show pid)

newTurnId :: IO Text
newTurnId = do
    stamp <- floor . (* 1000000) <$> getPOSIXTime :: IO Integer
    return $ T.pack ("turn-" ++ show stamp)

normalizeSessionName :: Text -> Maybe Text
normalizeSessionName rawName =
    let normalized = T.unwords (T.words rawName)
        capped = T.take sessionNameMaxLength normalized
     in if T.null capped
            then Nothing
            else Just capped

sessionNameMaxLength :: Int
sessionNameMaxLength = 80

freshSessionLayout :: Text -> IO (FilePath, FilePath, FilePath)
freshSessionLayout sid = do
    dir <- sessionDir sid
    let worktree = dir </> "worktree"
        home = dir </> "home"
    createDirectoryIfMissing True (dir </> "turns")
    createDirectoryIfMissing True home
    return (dir, worktree, home)

loadSession :: FilePath -> IO (Either String AgentSession)
loadSession path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left "session_not_found"
        else eitherDecodeStrict <$> BS.readFile path

loadSessionById :: Text -> IO (Either String AgentSession)
loadSessionById sid = sessionMetadataPath sid >>= loadSession

writeFileAtomic :: FilePath -> LBS.ByteString -> IO ()
writeFileAtomic path bytes = do
    let dir = takeDirectory path
    createDirectoryIfMissing True dir
    (tmp, handle) <- openBinaryTempFile dir (takeFileName path ++ ".tmp")
    LBS.hPut handle bytes
    hClose handle
    renameFile tmp path

saveSession :: AgentSession -> IO ()
saveSession session_ = do
    path <- sessionMetadataPath (sessionId session_)
    writeFileAtomic path (encode (persistableSession session_))

persistableSession :: AgentSession -> AgentSession
persistableSession session_ =
    session_
        { status =
            if status session_ == "running"
                then "open"
                else status session_
        , activeTurnId = Nothing
        }

listSessions :: IO [AgentSession]
listSessions = do
    root <- agentSessionsRoot
    exists <- doesDirectoryExist root
    if not exists
        then return []
        else do
            names <- listDirectory root
            dirs <- filterM (doesDirectoryExist . (root </>)) names
            parsed <- mapM (loadSession . (</> "session.json") . (root </>)) dirs
            return $ catMaybes $ map eitherToMaybe parsed

saveTurn :: AgentTurn -> IO ()
saveTurn turn = do
    path <- turnMetadataPath (turnSessionId turn) (turnId turn)
    let stored = turn{turnLog = ""}
    modifyMVar_ turnIndex $ \index -> do
        writeFileAtomic path (encode stored)
        return $ Map.adjust (Map.insert (turnId turn) stored) (takeDirectory path) index
    signalTurnLog (turnLogPath turn)

loadTurn :: FilePath -> IO (Either String AgentTurn)
loadTurn path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "turn metadata not found: " ++ path
        else eitherDecodeStrict <$> BS.readFile path

hydrateTurnLog :: AgentTurn -> IO AgentTurn
hydrateTurnLog turn = do
    logText <- readTurnLog turn
    return turn{turnLog = logText}

readTurnLog :: AgentTurn -> IO Text
readTurnLog turn = fromMaybe (turnLog turn) <$> readTurnLogFile (turnLogPath turn)

listTurns :: Text -> IO [AgentTurn]
listTurns sid = do
    dir <- turnsDir sid
    Map.elems <$> modifyMVar turnIndex (loadSessionTurnIndex dir)

loadSessionTurnIndex :: FilePath -> Map FilePath (Map Text AgentTurn) -> IO (Map FilePath (Map Text AgentTurn), Map Text AgentTurn)
loadSessionTurnIndex dir index =
    case Map.lookup dir index of
        Just turns -> return (index, turns)
        Nothing -> do
            exists <- doesDirectoryExist dir
            if not exists
                then return (index, Map.empty)
                else do
                    names <- listDirectory dir
                    let jsonFiles = filter (T.isSuffixOf ".json" . T.pack) names
                    parsed <- mapM (loadTurn . (dir </>)) jsonFiles
                    let turns = Map.fromList [(turnId turn, turn) | turn <- catMaybes (map eitherToMaybe parsed)]
                    return (Map.insert dir turns index, turns)

listTurnsWithLogs :: Text -> IO [AgentTurn]
listTurnsWithLogs sid = listTurns sid >>= mapM hydrateTurnLog

findTurn :: Text -> IO (Maybe AgentTurn)
findTurn tid
    | not (isSafeTurnId tid) = return Nothing
    | otherwise = do
        root <- agentSessionsRoot
        exists <- doesDirectoryExist root
        if not exists
            then return Nothing
            else do
                names <- listDirectory root
                lookupTurnFile root names
  where
    lookupTurnFile _ [] = return Nothing
    lookupTurnFile rootDir (name : rest) = do
        let path = rootDir </> name </> "turns" </> (T.unpack tid ++ ".json")
        exists <- doesFileExist path
        if exists
            then do
                loaded <- loadTurn path
                return $ case loaded of
                    Right turn | turnId turn == tid -> Just turn
                    _ -> Nothing
            else lookupTurnFile rootDir rest

forgetSessionTurns :: Text -> IO ()
forgetSessionTurns sid = do
    dir <- turnsDir sid
    modifyMVar_ turnIndex (return . Map.delete dir)

isSafeTurnId :: Text -> Bool
isSafeTurnId tid =
    not (T.null tid) && T.all (\c -> isAscii c && (isAlphaNum c || c == '-')) tid

touchSession :: AgentSession -> IO AgentSession
touchSession session_ = do
    now <- getCurrentTime
    return session_{updatedAt = now}

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _) = Nothing
