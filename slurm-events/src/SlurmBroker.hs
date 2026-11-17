{-# LANGUAGE OverloadedStrings #-}

module SlurmBroker
    ( BrokerConfig (..)
    , Snapshot (..)
    , defaultSnapshotQuery
    , notifyBroker
    , parseWatchArguments
    , runBrokerServer
    , runWatchClient
    ) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, takeMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (IOException, SomeException, bracket, catch, displayException, finally, try)
import Control.Monad (forever, forM, forM_, unless, void, when)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types ((.!=))
import Data.Char (isDigit)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import SlurmEvents
import System.Directory (createDirectoryIfMissing, doesPathExist, removeFile, renameFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Files (setFileMode)
import System.Posix.User (getUserEntryForName, userID)
import System.Process.Typed (proc, readProcess)
import System.Timeout (timeout)

data BrokerConfig = BrokerConfig
    { brokerSocketPath :: FilePath
    , brokerHeartbeatMicros :: Int
    , brokerTrustedUsers :: [String]
    , brokerAccountWatchUsers :: [String]
    , brokerStateFile :: Maybe FilePath
    , brokerQuery :: IO (Either String Snapshot)
    }

data Snapshot = Snapshot
    { snapshotJobs :: String
    , snapshotCluster :: String
    }
    deriving (Eq, Show)

data BrokerRequest = BrokerRequest
    { requestCommand :: Text
    , requestArguments :: [Text]
    , requestEvent :: Maybe Text
    , requestAccount :: Maybe Text
    , requestJob :: Maybe Text
    , requestName :: Maybe Text
    , requestState :: Maybe Text
    }

instance FromJSON BrokerRequest where
    parseJSON = withObject "BrokerRequest" $ \value ->
        BrokerRequest
            <$> value .: "command"
            <*> value .:? "arguments" .!= []
            <*> value .:? "event"
            <*> value .:? "account"
            <*> value .:? "job"
            <*> value .:? "name"
            <*> value .:? "state"

data BrokerFrame
    = FrameSnapshot String String [(String, String)]
    | FrameHeartbeat

data Client = Client
    { clientId :: Int
    , clientFilter :: WatchFilter
    , clientQueue :: TBQueue BrokerFrame
    , clientThread :: ThreadId
    , clientRequired :: Int
    }

data BrokerCommand
    = RegisterClient WatchFilter (TBQueue BrokerFrame) (MVar (Int, Int)) ThreadId
    | UnregisterClient Int
    | NotifyEvent Event

data Runtime = Runtime
    { rtConfig :: BrokerConfig
    , rtClients :: TVar (Map Int Client)
    , rtState :: TVar BrokerState
    , rtWindow :: TVar SnapshotWindow
    , rtRequests :: TChan ()
    , rtInput :: TChan BrokerCommand
    , rtNextClient :: TVar Int
    , rtTrustedUids :: [Integer]
    , rtAccountWatchUids :: [Integer]
    , rtPersistLock :: MVar ()
    }

data PeerInfo = PeerInfo
    { peerUid :: Maybe Integer
    , peerPid :: Maybe Int
    }
    deriving (Eq, Show)

data PersistedTerminal = PersistedTerminal
    { persistedJobId :: String
    , persistedJobName :: String
    , persistedAccount :: Maybe String
    , persistedAt :: Int
    }
    deriving (Eq, Show)

instance ToJSON PersistedTerminal where
    toJSON record =
        object
            [ "id" .= persistedJobId record
            , "name" .= persistedJobName record
            , "account" .= persistedAccount record
            , "at" .= persistedAt record
            ]

instance FromJSON PersistedTerminal where
    parseJSON = withObject "PersistedTerminal" $ \value ->
        PersistedTerminal
            <$> value .: "id"
            <*> value .: "name"
            <*> value .:? "account"
            <*> value .: "at"

newRuntime :: BrokerConfig -> IO Runtime
newRuntime config = do
    clients <- newTVarIO Map.empty
    state <- newTVarIO =<< loadTerminalState (brokerStateFile config)
    window <- newTVarIO emptySnapshotWindow
    requests <- newTChanIO
    input <- newTChanIO
    nextClient <- newTVarIO 1
    persistLock <- newMVar ()
    trustedUids <- mapMaybeM lookupUid (brokerTrustedUsers config)
    accountWatchUids <- mapMaybeM lookupUid (brokerAccountWatchUsers config)
    pure
        Runtime
            { rtConfig = config
            , rtClients = clients
            , rtState = state
            , rtWindow = window
            , rtRequests = requests
            , rtInput = input
            , rtNextClient = nextClient
            , rtTrustedUids = trustedUids
            , rtAccountWatchUids = accountWatchUids
            , rtPersistLock = persistLock
            }
  where
    lookupUid name = do
        entry <- try (fromIntegral . userID <$> getUserEntryForName name) :: IO (Either IOException Integer)
        pure $ either (const Nothing) Just entry
    mapMaybeM function = fmap catMaybes . mapM function

runBrokerServer :: BrokerConfig -> IO ()
runBrokerServer config = withSocketsDo $ do
    let path = brokerSocketPath config
    createDirectoryIfMissing True (takeDirectory path)
    removeStale path
    bracket (socket AF_UNIX Stream defaultProtocol) close $ \listener -> do
        bind listener (SockAddrUnix path)
        listen listener 128
        setFileMode path 0o660
        runtime <- newRuntime config
        void $ forkIO (commandLoop runtime)
        void $ forkIO (snapshotWorker runtime)
        void $ forkIO (heartbeatLoop runtime)
        acceptLoop runtime listener
            `finally` ignoreIO (removeFile path)

acceptLoop :: Runtime -> Socket -> IO ()
acceptLoop runtime listener =
    forever $ do
        (connection, _) <- accept listener
        void $
            forkIO $
                handleConnection runtime connection
                    `finally` close connection

handleConnection :: Runtime -> Socket -> IO ()
handleConnection runtime connection = do
    peer <- peerInfo connection
    requestBytes <- recvRequest connection
    case requestBytes of
        Left _ -> pure ()
        Right payload ->
            case eitherDecodeStrict' payload of
                Left _ -> pure ()
                Right request ->
                    case requestCommand request of
                        "watch" ->
                            do
                                jobScope <- peerJobScope (peerPid peer)
                                case parseWatchArguments (map T.unpack (requestArguments request)) of
                                    Left _ -> pure ()
                                    Right filter ->
                                        if permittedWatch runtime peer jobScope filter
                                            then do
                                                queue <- newTBQueueIO 64
                                                reply <- newEmptyMVar
                                                thread <- myThreadId
                                                atomically $
                                                    writeTChan
                                                        (rtInput runtime)
                                                        (RegisterClient filter queue reply thread)
                                                (registeredId, _) <- takeMVar reply
                                                writeFrames connection queue
                                                    `finally` atomically
                                                        (writeTChan (rtInput runtime) (UnregisterClient registeredId))
                                            else pure ()
                        "notify" ->
                            case brokerEvent request of
                                Left problem ->
                                    hPutStrLn stderr ("pointy-cloud-slurm-broker: " <> problem)
                                Right event ->
                                    do
                                        jobScope <- peerJobScope (peerPid peer)
                                        if permittedNotify runtime peer jobScope event
                                            then do
                                                atomically $ writeTChan (rtInput runtime) (NotifyEvent event)
                                                ignoreIO (NSB.sendAll connection "ok\n")
                                            else
                                                hPutStrLn stderr $
                                                    "pointy-cloud-slurm-broker: rejected "
                                                        <> eventKindName (eventKind event)
                                                        <> " notification for job "
                                                        <> show (eventJobId event)
                                                        <> " from uid "
                                                        <> show (peerUid peer)
                                                        <> " in job cgroup "
                                                        <> show jobScope
                        _ -> pure ()

peerInfo :: Socket -> IO PeerInfo
peerInfo connection = do
    credential <- getPeerCredential connection
    pure $ case credential of
        (processId, userId, _) ->
            PeerInfo
                { peerUid = fromIntegral <$> userId
                , peerPid = fromIntegral <$> processId
                }

peerJobScope :: Maybe Int -> IO (Maybe String)
peerJobScope Nothing = pure Nothing
peerJobScope (Just processId) = do
    contents <-
        try (BS.readFile ("/proc/" <> show processId <> "/cgroup"))
            :: IO (Either IOException BS.ByteString)
    pure $ case contents of
        Left _ -> Nothing
        Right value ->
            lastMaybe
                ( mapMaybe scopedJob
                    (concatMap (splitOn '/') (lines (BSC.unpack value)))
                )
  where
    scopedJob part =
        case stripPrefix "job_" part of
            Just rest ->
                case span isDigit rest of
                    (identifier, trailing)
                        | not (null identifier)
                        , null trailing || trailing == ".scope" ->
                            Just identifier
                    _ -> Nothing
            Nothing -> Nothing

permittedWatch :: Runtime -> PeerInfo -> Maybe String -> WatchFilter -> Bool
permittedWatch _ _ (Just _) _ = False
permittedWatch _ _ Nothing WatchNative = True
permittedWatch runtime peer Nothing (WatchAccount _) =
    trusted || maybe False (`elem` rtAccountWatchUids runtime) (peerUid peer)
  where
    trusted = maybe False (`elem` rtTrustedUids runtime) (peerUid peer)

permittedNotify :: Runtime -> PeerInfo -> Maybe String -> Event -> Bool
permittedNotify runtime peer jobScope event =
    case jobScope of
        Nothing ->
            if trusted
                then True
                else eventKind event == SubmitEvent
        Just scopeJob ->
            eventKind event `elem` [StartEvent, PublishingEvent]
                && eventJobId event == Just scopeJob
  where
    trusted = maybe False (`elem` rtTrustedUids runtime) (peerUid peer)

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe values = Just (last values)

splitOn :: Char -> String -> [String]
splitOn separator value = case break (== separator) value of
    (part, []) -> [part]
    (part, _ : rest) -> part : splitOn separator rest

recvRequest :: Socket -> IO (Either String BS.ByteString)
recvRequest connection = go BS.empty
  where
    go accumulated
        | BS.length accumulated > maximumRequestBytes = pure (Left "request is too large")
        | otherwise = do
            chunk <- NSB.recv connection 4096
            if BS.null chunk
                then pure (Right accumulated)
                else case BSC.elemIndex '\n' chunk of
                    Just index -> pure (Right (accumulated <> BS.take index chunk))
                    Nothing -> go (accumulated <> chunk)

maximumRequestBytes :: Int
maximumRequestBytes = 1024 * 1024

writeFrames :: Socket -> TBQueue BrokerFrame -> IO ()
writeFrames connection queue =
    ignoreIO $
        forever $ do
            frame <- atomically (readTBQueue queue)
            NSB.sendAll connection (frameBytes frame)

frameBytes :: BrokerFrame -> BS.ByteString
frameBytes frame =
    BL.toStrict (encode value) <> "\n"
  where
    value = case frame of
        FrameSnapshot jobs cluster terminalJobs ->
            object
                [ "type" .= ("snapshot" :: Text)
                , "jobs" .= jobs
                , "cluster" .= cluster
                , "terminalJobs"
                    .= [ object ["id" .= identifier, "name" .= name]
                       | (identifier, name) <- terminalJobs
                       ]
                ]
        FrameHeartbeat ->
            object ["type" .= ("heartbeat" :: Text)]

tryWriteTBQueue :: TBQueue a -> a -> STM Bool
tryWriteTBQueue queue value = (True <$ writeTBQueue queue value) `orElse` pure False

heartbeatLoop :: Runtime -> IO ()
heartbeatLoop runtime = forever $ do
    threadDelay (max 1 (brokerHeartbeatMicros (rtConfig runtime)))
    clients <- readTVarIO (rtClients runtime)
    forM_ (Map.elems clients) $ \client -> do
        delivered <- atomically $ tryWriteTBQueue (clientQueue client) FrameHeartbeat
        unless delivered $ dropClient runtime (clientId client)

snapshotWorker :: Runtime -> IO ()
snapshotWorker runtime = forever $ do
    atomically $ readTChan (rtRequests runtime)
    snapshotLoop runtime

snapshotLoop :: Runtime -> IO ()
snapshotLoop runtime = loop 0
  where
    loop attempts = do
        clients <- readTVarIO (rtClients runtime)
        unless (Map.null clients) $ do
            generation <- atomically $ do
                window <- readTVar (rtWindow runtime)
                let (next, updated) = beginSnapshotWindow window
                writeTVar (rtWindow runtime) updated
                pure next
            outcome <- try (brokerQuery (rtConfig runtime)) :: IO (Either SomeException (Either String Snapshot))
            case outcome of
                Left exception -> do
                    hPutStrLn stderr ("pointy-cloud-slurm-broker: snapshot failed: " <> displayException exception)
                    closeAll runtime
                Right (Left problem) -> do
                    hPutStrLn stderr ("pointy-cloud-slurm-broker: snapshot failed: " <> problem)
                    closeAll runtime
                Right (Right snapshot) -> do
                    window <- readTVarIO (rtWindow runtime)
                    if snapshotDirty window
                        then
                            if attempts < maximumSnapshotAttempts
                                then loop (attempts + 1)
                                else do
                                    _ <- publishSnapshot runtime generation snapshot
                                    requestSnapshot runtime
                        else do
                            published <- publishSnapshot runtime generation snapshot
                            if published
                                then do
                                    window' <- readTVarIO (rtWindow runtime)
                                    when (snapshotDirty window') (loop (attempts + 1))
                                else loop (attempts + 1)
    maximumSnapshotAttempts = 8

publishSnapshot :: Runtime -> Int -> Snapshot -> IO Bool
publishSnapshot runtime generation snapshot = do
    (state, window) <-
        atomically $ do
            currentState <- readTVar (rtState runtime)
            currentWindow <- readTVar (rtWindow runtime)
            pure (currentState, currentWindow)
    if snapshotDirty window
        then pure False
        else do
            clients <- readTVarIO (rtClients runtime)
            dropped <-
                catMaybes
                    <$> forM (Map.elems clients) (\client ->
                            if clientRequired client <= generation
                                then do
                                    let jobs = filterJobs (clientFilter client) state (snapshotJobs snapshot)
                                        terminalJobs = filterTerminalJobs (clientFilter client) state
                                        frame = FrameSnapshot jobs (snapshotCluster snapshot) terminalJobs
                                    accepted <- atomically $ tryWriteTBQueue (clientQueue client) frame
                                    pure (if accepted then Nothing else Just (clientId client))
                                else pure Nothing
                        )
            forM_ dropped (dropClient runtime)
            pure True

dropClient :: Runtime -> Int -> IO ()
dropClient runtime identifier = do
    client <- atomically $ do
        clients <- readTVar (rtClients runtime)
        case Map.lookup identifier clients of
            Nothing -> pure Nothing
            Just found -> do
                writeTVar (rtClients runtime) (Map.delete identifier clients)
                pure (Just found)
    forM_ client (killThread . clientThread)

closeAll :: Runtime -> IO ()
closeAll runtime = do
    clients <- atomically $ do
        current <- readTVar (rtClients runtime)
        writeTVar (rtClients runtime) Map.empty
        pure current
    forM_ (Map.elems clients) (killThread . clientThread)

requestSnapshot :: Runtime -> IO ()
requestSnapshot runtime = do
    clients <- readTVarIO (rtClients runtime)
    unless (Map.null clients) $
        atomically $ writeTChan (rtRequests runtime) ()

commandLoop :: Runtime -> IO ()
commandLoop runtime = forever $ do
    command <- atomically $ readTChan (rtInput runtime)
    case command of
        RegisterClient filter queue reply thread -> do
            (identifier, required) <- atomically $ do
                identifier <- readTVar (rtNextClient runtime)
                writeTVar (rtNextClient runtime) (identifier + 1)
                window <- readTVar (rtWindow runtime)
                writeTVar (rtWindow runtime) (markSnapshotDirty window)
                let client =
                        Client
                            { clientId = identifier
                            , clientFilter = filter
                            , clientQueue = queue
                            , clientThread = thread
                            , clientRequired = snapshotGeneration window + 1
                            }
                modifyTVar' (rtClients runtime) (Map.insert identifier client)
                pure (identifier, clientRequired client)
            putMVar reply (identifier, required)
            requestSnapshot runtime
        UnregisterClient identifier ->
            atomically $ modifyTVar' (rtClients runtime) (Map.delete identifier)
        NotifyEvent event -> do
            timestamp <- round <$> getPOSIXTime
            accepted <- atomically $ do
                state <- readTVar (rtState runtime)
                case applyEvent timestamp event state of
                    Left _ -> pure False
                    Right updated -> do
                        writeTVar (rtState runtime) updated
                        window <- readTVar (rtWindow runtime)
                        writeTVar (rtWindow runtime) (markSnapshotDirty window)
                        pure True
            when accepted $ do
                when (eventKind event == TerminalEvent) (persistTerminalState runtime)
                if eventKind event == ResetEvent
                    then closeAll runtime
                    else requestSnapshot runtime

loadTerminalState :: Maybe FilePath -> IO BrokerState
loadTerminalState Nothing = pure emptyBrokerState
loadTerminalState (Just path) = do
    exists <- doesPathExist path
    if not exists
        then pure emptyBrokerState
        else do
            contents <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
            case contents of
                Left _ -> pure emptyBrokerState
                Right value ->
                    case (eitherDecodeStrict' value :: Either String [PersistedTerminal]) of
                        Left _ -> pure emptyBrokerState
                        Right records -> do
                            now <- round <$> getPOSIXTime
                            pure $
                                pruneBrokerState now $
                                    BrokerState
                                        { brokerTerminal =
                                            Map.fromList
                                                [ ( (persistedJobId record, persistedJobName record)
                                                  , TerminalRecord
                                                        (persistedJobId record)
                                                        (persistedJobName record)
                                                        (persistedAccount record)
                                                        (persistedAt record)
                                                  )
                                                | record <- records
                                                ]
                                        , brokerPublishing = Map.empty
                                        }

persistTerminalState :: Runtime -> IO ()
persistTerminalState runtime =
    case brokerStateFile (rtConfig runtime) of
        Nothing -> pure ()
        Just path ->
            withMVar (rtPersistLock runtime) $ \_ -> do
                records <- Map.elems . brokerTerminal <$> readTVarIO (rtState runtime)
                let temporary = path <> ".tmp"
                outcome <-
                    try (do
                            createDirectoryIfMissing True (takeDirectory path)
                            BS.writeFile temporary (BL.toStrict (encode (map persisted records)))
                            renameFile temporary path
                        ) :: IO (Either IOException ())
                case outcome of
                    Left exception ->
                        hPutStrLn stderr ("pointy-cloud-slurm-broker: could not persist terminal state: " <> displayException (exception :: IOException))
                    Right () -> pure ()
  where
    persisted record =
        PersistedTerminal
            { persistedJobId = terminalJobId record
            , persistedJobName = terminalJobName record
            , persistedAccount = terminalAccount record
            , persistedAt = terminalAt record
            }

brokerEvent :: BrokerRequest -> Either String Event
brokerEvent request = do
    name <- maybe (Left "event is required") Right (requestEvent request)
    kind <- maybe (Left ("unknown event: " <> T.unpack name)) Right (parseEventKind (T.unpack name))
    Right
        Event
            { eventKind = kind
            , eventAccount = T.unpack <$> requestAccount request
            , eventJobId = T.unpack <$> requestJob request
            , eventJobName = T.unpack <$> requestName request
            , eventState = T.unpack <$> requestState request
            }

parseWatchArguments :: [String] -> Either String WatchFilter
parseWatchArguments ["native"] = Right WatchNative
parseWatchArguments ["account", account]
    | validAccountName account = Right (WatchAccount account)
parseWatchArguments _ = Left "watch arguments must be native or account ACCOUNT"

runWatchClient :: FilePath -> [String] -> IO ()
runWatchClient path arguments = do
    _ <- either (ioError . userError) pure (parseWatchArguments arguments)
    bracket (connectBroker path) close $ \connection -> do
        NSB.sendAll connection (watchRequest arguments)
        copyFrames connection
  where
    copyFrames connection = do
        chunk <- NSB.recv connection 65536
        unless (BS.null chunk) $ do
            BS.hPut stdout chunk
            hFlush stdout
            copyFrames connection

watchRequest :: [String] -> BS.ByteString
watchRequest arguments =
    BL.toStrict
        ( encode
            ( object
                [ "command" .= ("watch" :: Text)
                , "arguments" .= map T.pack arguments
                ]
            )
        )
        <> "\n"

notifyBroker :: FilePath -> Event -> IO ()
notifyBroker path event =
    void $ timeout 2000000 $ ignoreIO $
        bracket (connectBroker path) close $ \connection -> do
            NSB.sendAll connection (BL.toStrict (encode payload) <> "\n")
            void $ NSB.recv connection 3
  where
    payload =
        object
            [ "command" .= ("notify" :: Text)
            , "event" .= eventKindName (eventKind event)
            , "account" .= eventAccount event
            , "job" .= eventJobId event
            , "name" .= eventJobName event
            , "state" .= eventState event
            ]

connectBroker :: FilePath -> IO Socket
connectBroker path = do
    connection <- socket AF_UNIX Stream defaultProtocol
    connect connection (SockAddrUnix path)
    pure connection

removeStale :: FilePath -> IO ()
removeStale path =
    ignoreIO $ do
        exists <- doesPathExist path
        when exists (removeFile path)

ignoreIO :: IO () -> IO ()
ignoreIO action = action `catch` handler
  where
    handler :: IOException -> IO ()
    handler _ = pure ()

defaultSnapshotQuery :: IO (Either String Snapshot)
defaultSnapshotQuery = do
    jobs <- queryCommand "squeue" ["-h", "-o", "%i|%a|%j|%k|%T|%M|%r"]
    case jobs of
        Left failure -> pure (Left failure)
        Right jobsOutput -> do
            cluster <- queryCommand "sinfo" ["-h", "-N", "-o", "%P|%a|%N|%T"]
            pure (Snapshot jobsOutput <$> cluster)

queryCommand :: String -> [String] -> IO (Either String String)
queryCommand program arguments = do
    outcome <-
        try (timeout commandTimeoutMicros (readProcess (proc program arguments)))
            :: IO (Either IOException (Maybe (ExitCode, BL.ByteString, BL.ByteString)))
    pure $ case outcome of
        Left exception -> Left (program <> ": " <> displayException exception)
        Right Nothing -> Left (program <> ": timed out")
        Right (Just (ExitSuccess, output, _)) -> Right (decodeOutput output)
        Right (Just (ExitFailure status, _, errors)) ->
            Left (program <> " exited " <> show status <> ": " <> decodeOutput errors)
  where
    commandTimeoutMicros = 5 * 1000000

decodeOutput :: BL.ByteString -> String
decodeOutput =
    T.unpack . decodeUtf8With lenientDecode . BL.toStrict
