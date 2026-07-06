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
    touchSession,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, throwIO, try)
import Control.Monad (filterM)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict', encode, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (isInfixOf)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getHomeDirectory, listDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (ioeGetErrorString, isAlreadyInUseError)
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
        then return $ Left $ "session metadata not found: " ++ path
        else eitherDecodeStrict' <$> BS.readFile path

loadSessionById :: Text -> IO (Either String AgentSession)
loadSessionById sid = sessionMetadataPath sid >>= loadSession

saveSession :: AgentSession -> IO ()
saveSession session = do
    path <- sessionMetadataPath (sessionId session)
    createDirectoryIfMissing True (takeDirectory path)
    writeMetadataFile path (encode session)

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
    writeMetadataFile path (encode turn{turnLog = ""})

writeMetadataFile :: FilePath -> LBS.ByteString -> IO ()
writeMetadataFile path bytes = go 0
  where
    maxRetries = 10 :: Int
    go attempt = do
        result <- try (LBS.writeFile path bytes) :: IO (Either IOException ())
        case result of
            Right () -> return ()
            Left err
                | attempt < maxRetries && isTransientFileLock err -> do
                    threadDelay (50000 * (attempt + 1))
                    go (attempt + 1)
                | otherwise -> throwIO err

isTransientFileLock :: IOException -> Bool
isTransientFileLock err =
    isAlreadyInUseError err
        || "resource busy" `isInfixOf` ioeGetErrorString err
        || "file is locked" `isInfixOf` ioeGetErrorString err

loadTurn :: FilePath -> IO (Either String AgentTurn)
loadTurn path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "turn metadata not found: " ++ path
        else eitherDecodeStrict' <$> BS.readFile path

loadTurnWithLog :: FilePath -> IO (Either String AgentTurn)
loadTurnWithLog path = do
    loaded <- loadTurn path
    case loaded of
        Left err -> return (Left err)
        Right turn -> Right <$> hydrateTurnLog turn

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
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            names <- listDirectory dir
            let jsonFiles = filter (T.isSuffixOf ".json" . T.pack) names
            parsed <- mapM (loadTurn . (dir </>)) jsonFiles
            return $ catMaybes $ map eitherToMaybe parsed

listTurnsWithLogs :: Text -> IO [AgentTurn]
listTurnsWithLogs sid = do
    dir <- turnsDir sid
    exists <- doesDirectoryExist dir
    if not exists
        then return []
        else do
            names <- listDirectory dir
            let jsonFiles = filter (T.isSuffixOf ".json" . T.pack) names
            parsed <- mapM (loadTurnWithLog . (dir </>)) jsonFiles
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
