{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Session (
    AgentSession (..),
    AgentSessionSummary (..),
    PreparedApply (..),
    applyConflictsPending,
    AgentTurn (..),
    turnIsUnfinished,
    latestUnfinishedTurn,
    inferTurnExitCode,
    turnLogHasFinalizationFailure,
    agentSessionsRoot,
    sessionDir,
    sessionMetadataPath,
    turnsDir,
    turnMetadataPath,
    turnLogFilePath,
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

import Agent.TurnSignal (signalTurnLog)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isAlphaNum, isAscii)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

data AgentTurn = AgentTurn
    { turnId :: Text
    , turnSessionId :: Text
    , turnPrompt :: Text
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
            <*> obj .: "turnStatus"
            <*> obj .:? "turnExitCode"
            <*> obj .: "turnStartedAt"
            <*> obj .:? "turnFinishedAt"
            <*> obj .: "turnLogPath"
            <*> obj .:? "turnLog" .!= ""

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
readTurnLog turn = do
    exists <- doesFileExist (turnLogPath turn)
    if exists
        then TIO.readFile (turnLogPath turn)
        else return (turnLog turn)

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
