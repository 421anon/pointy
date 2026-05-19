{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Session (
    AgentSession (..),
    PreparedApply (..),
    AgentTurn (..),
    agentSessionsRoot,
    sessionDir,
    sessionMetadataPath,
    turnsDir,
    turnMetadataPath,
    turnLogFilePath,
    newSessionId,
    freshSessionLayout,
    loadSession,
    saveSession,
    loadSessionById,
    listSessions,
    saveTurn,
    loadTurn,
    listTurns,
    findTurn,
    touchSession,
) where

import Control.Monad (filterM)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Process (getProcessID)


data PreparedApply = PreparedApply
    { targetHead :: Text
    , agentHead :: Text
    , candidateHead :: Text
    , candidateWorktree :: FilePath
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


data AgentSession = AgentSession
    { sessionId :: Text
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
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


data AgentTurn = AgentTurn
    { turnId :: Text
    , turnSessionId :: Text
    , turnStatus :: Text
    , turnExitCode :: Maybe Int
    , turnStartedAt :: UTCTime
    , turnFinishedAt :: Maybe UTCTime
    , turnLogPath :: FilePath
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


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
        then return $ Left $ "session metadata not found: " ++ path
        else eitherDecode <$> LBS.readFile path


loadSessionById :: Text -> IO (Either String AgentSession)
loadSessionById sid = sessionMetadataPath sid >>= loadSession


saveSession :: AgentSession -> IO ()
saveSession session = do
    path <- sessionMetadataPath (sessionId session)
    createDirectoryIfMissing True (takeDirectory path)
    LBS.writeFile path (encode session)


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
    createDirectoryIfMissing True (takeDirectory path)
    LBS.writeFile path (encode turn)


loadTurn :: FilePath -> IO (Either String AgentTurn)
loadTurn path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "turn metadata not found: " ++ path
        else eitherDecode <$> LBS.readFile path


listTurns :: Text -> IO [AgentTurn]
listTurns sid = do
    dir <- turnsDir sid
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            names <- listDirectory dir
            let jsonFiles = filter (T.isSuffixOf ".json" . T.pack) names
            parsed <- mapM (loadTurn . (dir </>)) jsonFiles
            return $ catMaybes $ map eitherToMaybe parsed


findTurn :: Text -> IO (Maybe AgentTurn)
findTurn tid = do
    sessions <- listSessions
    turns <- concat <$> mapM (listTurns . sessionId) sessions
    return $ findByTurnId tid turns


touchSession :: AgentSession -> IO AgentSession
touchSession session = do
    now <- getCurrentTime
    return session{updatedAt = now}


eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _) = Nothing


findByTurnId :: Text -> [AgentTurn] -> Maybe AgentTurn
findByTurnId _ [] = Nothing
findByTurnId tid (turn : rest)
    | turnId turn == tid = Just turn
    | otherwise = findByTurnId tid rest


