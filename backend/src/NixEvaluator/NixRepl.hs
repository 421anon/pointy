{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixEvaluator.NixRepl (
    NixEvalOutput (..),
    NixEvalRequest (..),
    NixEvalTarget (..),
    ReplKind (..),
    ReplOutcome (..),
    ReplSession,
    openSession,
    runRequest,
    readSessionMemoryBytes,
    closeSession,
    readLines,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (SomeException, catch, evaluate, try)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecodeStrict')
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (isAlphaNum)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO (BufferMode (..), Handle, hClose, hFlush, hIsClosed, hPutStrLn, hSetBuffering)
import System.IO.Error (isEOFError)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (CreatePipe), createProcess, getPid, proc, terminateProcess, waitForProcess)
import Text.Read (readMaybe)

data NixEvalOutput = EvalJson | EvalRaw
    deriving (Eq, Ord, Show)

data NixEvalTarget
    = EvalInstallable
        { evalInstallable :: String
        , evalAttr :: String
        }
    | EvalExpr String
    deriving (Eq, Show)

data NixEvalRequest = NixEvalRequest
    { evalImpure :: Bool
    , evalOutput :: NixEvalOutput
    , evalApply :: Maybe String
    , evalTarget :: NixEvalTarget
    }
    deriving (Eq, Show)

data ReplKind = PureRepl | ImpureRepl
    deriving (Eq, Show)

replSystem :: String
replSystem = "x86_64-linux"

data ReplSession = ReplSession
    { replName :: String
    , replPid :: Maybe Int
    , replInput :: Handle
    , replEvents :: TQueue ReplEvent
    , replCounter :: MVar Int
    , replFlakeVars :: MVar [(String, String)]
    , replClose :: IO ()
    }

data ReplStream = ReplStdout | ReplStderr
    deriving (Eq, Show)

data ReplEvent
    = ReplLine ReplStream ByteString
    | ReplClosed ReplStream String
    deriving (Eq, Show)

data ReplOutcome
    = ReplSucceeded ByteString
    | ReplFailed String
    | ReplDied String
    deriving (Eq, Show)

openSession :: ReplKind -> IO ReplSession
openSession kind = do
    events <- newTQueueIO
    let args =
            ["repl", "--extra-experimental-features", "nix-command flakes"]
                ++ (if kind == PureRepl then ["--option", "pure-eval", "true"] else [])
                ++ ["--impure" | kind == ImpureRepl]
        cp =
            (proc "nix" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    (Just stdinH, Just stdoutH, Just stderrH, ph) <- createProcess cp
    pid <- fmap fromIntegral <$> getPid ph
    hSetBuffering stdinH LineBuffering
    void $ forkIO $ readLoop ReplStdout stdoutH events
    void $ forkIO $ readLoop ReplStderr stderrH events
    counter <- newMVar 0
    flakeVars <- newMVar []
    let session =
            ReplSession
                { replName = show kind ++ " nix repl"
                , replPid = pid
                , replInput = stdinH
                , replEvents = events
                , replCounter = counter
                , replFlakeVars = flakeVars
                , replClose = closeLocalSession stdinH stdoutH stderrH ph
                }
    initializeSession session `catch` \(err :: SomeException) -> do
        closeSession session
        fail $ "nix repl initialization failed for " ++ replName session ++ ": " ++ show err
    pure session

initializeSession :: ReplSession -> IO ()
initializeSession session = do
    marker <- nextMarker session "ready"
    sendCommands session [syncCommand marker]
    result <- collectUntilMarkers session marker
    case result of
        ReplDied err -> fail err
        _ -> pure ()

runRequest :: ReplSession -> NixEvalRequest -> IO ReplOutcome
runRequest session req = do
    eExpr <- renderRequestExpression session req
    outcome <- case eExpr of
        Left outcome -> pure outcome
        Right expr -> do
            begin <- nextMarker session "begin"
            end <- nextMarker session "end"
            sendCommands
                session
                [ ":p " ++ nixString begin
                , ":p builtins.toJSON (" ++ expr ++ ")"
                , syncCommand end
                ]
            collectUntilMarkers session end >>= \case
                ReplSucceeded _ -> pure $ ReplDied "internal protocol error: collectUntilMarkers returned success before parsing"
                ReplDied err -> pure $ ReplDied err
                ReplFailed raw -> parseReplOutput begin req raw
    pure outcome

readSessionMemoryBytes :: ReplSession -> IO (Maybe Integer)
readSessionMemoryBytes session =
    maybe (pure Nothing) readStatus $ replPid session
  where
    readStatus pid = do
        status <- try $ do
            contents <- readFile $ "/proc/" ++ show pid ++ "/status"
            evaluate (length contents) >> pure contents
        pure $ either (const Nothing) parseVmRss (status :: Either SomeException String)

    parseVmRss status =
        listToMaybe
            [ value * 1024
            | line <- lines status
            , Just raw <- [stripPrefix "VmRSS:" line]
            , rawValue : _ <- [words raw]
            , Just value <- [readMaybe rawValue]
            ]

collectUntilMarkers :: ReplSession -> String -> IO ReplOutcome
collectUntilMarkers session marker = go False False []
  where
    go True True acc = pure $ ReplFailed (formatEvents $ reverse acc)
    go sawStdout sawStderr acc = do
        event <- atomically $ readTQueue (replEvents session)
        case event of
            ReplLine ReplStdout line
                | line == utf8 marker -> go True sawStderr acc
            ReplLine ReplStderr line
                | line == utf8 ("trace: " ++ marker) -> go sawStdout True acc
            ReplClosed stream err -> pure $ ReplDied $ replName session ++ " closed " ++ show stream ++ " before marker " ++ marker ++ formatClosed err
            _ -> go sawStdout sawStderr (event : acc)

parseReplOutput :: String -> NixEvalRequest -> String -> IO ReplOutcome
parseReplOutput begin req raw = do
    let output = lines raw
        (preBegin, atBegin) = break (== begin) output
    case atBegin of
        [] -> pure $ ReplDied $ "nix repl response did not include begin marker " ++ begin ++ ":\n" ++ raw
        (_ : body) ->
            pure $
                if hasNixError preBegin
                    then ReplFailed $ stripTrailingNewlines $ unlines preBegin
                    else parseBody req body
  where
    keepOutput line = not (all (`elem` (" \t\r" :: String)) line)
    parseBody request body = case [(line, value) | line <- body, keepOutput line, Right value <- [eitherDecodeStrict' (utf8 line)]] of
        [(line, value)] -> renderOutput request value line
        [] -> ReplFailed $ stripTrailingNewlines $ unlines body
        _ -> ReplFailed $ "nix repl returned multiple JSON values:\n" ++ stripTrailingNewlines (unlines body)
    utf8 = TE.encodeUtf8 . T.pack

utf8 :: String -> ByteString
utf8 = TE.encodeUtf8 . T.pack

renderOutput :: NixEvalRequest -> Value -> String -> ReplOutcome
renderOutput req value line = case evalOutput req of
    EvalJson -> ReplSucceeded (utf8 (line ++ "\n"))
    EvalRaw -> case value of
        String txt -> ReplSucceeded (TE.encodeUtf8 txt)
        _ -> ReplFailed $ "nix repl --raw emulation expected a string result, got JSON: " ++ line
  where
    utf8 = TE.encodeUtf8 . T.pack

renderRequestExpression :: ReplSession -> NixEvalRequest -> IO (Either ReplOutcome String)
renderRequestExpression session req =
    fmap apply <$> case evalTarget req of
        EvalExpr expr -> pure $ Right $ "(" ++ expr ++ ")"
        EvalInstallable installable attr ->
            fmap (`renderFlakeInstallableExpression` attr) <$> ensureFlakeBinding session installable
  where
    apply targetExpr =
        maybe targetExpr (\applyExpr -> "(" ++ applyExpr ++ ") (" ++ targetExpr ++ ")") $ evalApply req

ensureFlakeBinding :: ReplSession -> String -> IO (Either ReplOutcome String)
ensureFlakeBinding session installable =
    modifyMVar (replFlakeVars session) $ \bindings ->
        case lookup installable bindings of
            Just varName -> pure (bindings, Right varName)
            Nothing -> do
                let varName = "pointyFlake" ++ show (length bindings + 1)
                marker <- nextMarker session "flake"
                sendCommands
                    session
                    [ varName ++ " = builtins.getFlake " ++ nixString installable
                    , ":p builtins.seq " ++ varName ++ " null"
                    , syncCommand marker
                    ]
                outcome <- collectUntilMarkers session marker
                case outcome of
                    ReplSucceeded _ -> pure (bindings, Left $ ReplDied "internal protocol error while binding flake")
                    ReplDied err -> pure (bindings, Left $ ReplDied err)
                    ReplFailed raw ->
                        if hasNixError (lines raw)
                            then pure (bindings, Left $ ReplFailed $ stripTrailingNewlines raw)
                            else pure ((installable, varName) : bindings, Right varName)

renderFlakeInstallableExpression :: String -> String -> String
renderFlakeInstallableExpression flakeVar attr =
    "let "
        ++ "flake = "
        ++ flakeVar
        ++ "; "
        ++ "attrPath = "
        ++ renderNixStringList segments
        ++ "; "
        ++ "resolve = set: builtins.foldl' (acc: name: if acc ? value && builtins.isAttrs acc.value && builtins.hasAttr name acc.value then { value = acc.value.${name}; } else { }) { value = set; } attrPath; "
        ++ "top = resolve flake; "
        ++ "system = "
        ++ nixString replSystem
        ++ "; "
        ++ "packages = if flake ? packages && flake.packages ? ${system} then resolve flake.packages.${system} else { }; "
        ++ "legacyPackages = if flake ? legacyPackages && flake.legacyPackages ? ${system} then resolve flake.legacyPackages.${system} else { }; "
        ++ "in "
        ++ "if top ? value then top.value "
        ++ "else if packages ? value then packages.value "
        ++ "else if legacyPackages ? value then legacyPackages.value "
        ++ "else builtins.throw "
        ++ nixString ("flake output attribute " ++ attr ++ " not found")
  where
    segments = attrSegments attr

attrSegments :: String -> [String]
attrSegments attr = filter (not . null) $ splitOn '.' withoutHash
  where
    withoutHash = case attr of
        '#' : rest -> rest
        _ -> attr

renderNixStringList :: [String] -> String
renderNixStringList xs = "[ " ++ unwords (map nixString xs) ++ " ]"

nextMarker :: ReplSession -> String -> IO String
nextMarker session label = do
    n <- modifyMVar (replCounter session) $ \current -> do
        let next = current + 1
        pure (next, next)
    pure $ "__pointy_nix_repl_" ++ sanitize label ++ "_" ++ show n ++ "__"

syncCommand :: String -> String
syncCommand marker = ":p builtins.trace " ++ nixString marker ++ " " ++ nixString marker

sendCommands :: ReplSession -> [String] -> IO ()
sendCommands session commands = do
    mapM_ (hPutStrLn (replInput session)) commands
    hFlush (replInput session)

readLoop :: ReplStream -> Handle -> TQueue ReplEvent -> IO ()
readLoop stream handle events = do
    outcome <- try (readLines handle (\line -> atomically (writeTQueue events (ReplLine stream line))))
    case outcome of
        Right () -> atomically $ writeTQueue events (ReplClosed stream "")
        Left err
            | isEOFError err -> atomically $ writeTQueue events (ReplClosed stream "")
            | otherwise -> atomically $ writeTQueue events (ReplClosed stream (show err))

readLines :: Handle -> (ByteString -> IO ()) -> IO ()
readLines handle emit = go BS.empty
  where
    go leftover = do
        next <- nextLine handle leftover
        case next of
            Nothing -> pure ()
            Just (line, rest) -> emit line >> go rest

nextLine :: Handle -> ByteString -> IO (Maybe (ByteString, ByteString))
nextLine handle leftover = do
    let (before, rest) = BS.break (== 10) leftover
    case BS.uncons rest of
        Just (_, after) -> pure (Just (before, after))
        Nothing -> do
            chunk <- BS.hGet handle 65536
            if BS.null chunk
                then pure $ if BS.null leftover then Nothing else Just (leftover, BS.empty)
                else nextLine handle (leftover <> chunk)

closeSession :: ReplSession -> IO ()
closeSession session = replClose session `catch` \(_ :: SomeException) -> pure ()

closeLocalSession :: Handle -> Handle -> Handle -> ProcessHandle -> IO ()
closeLocalSession stdinH stdoutH stderrH ph = do
    hCloseIfOpen stdinH
    hCloseIfOpen stdoutH
    hCloseIfOpen stderrH
    terminateProcess ph `catch` \(_ :: SomeException) -> pure ()
    void (waitForProcess ph) `catch` \(_ :: SomeException) -> pure ()

hCloseIfOpen :: Handle -> IO ()
hCloseIfOpen handle = do
    closed <- hIsClosed handle
    if closed then pure () else hClose handle

formatClosed :: String -> String
formatClosed "" = ""
formatClosed err = ": " ++ err

formatEvents :: [ReplEvent] -> String
formatEvents = unlines . map formatEvent
  where
    formatEvent (ReplLine ReplStdout line) = T.unpack (decodeLineBytes line)
    formatEvent (ReplLine ReplStderr line) = "stderr: " ++ T.unpack (decodeLineBytes line)
    formatEvent (ReplClosed stream err) = show stream ++ " closed" ++ formatClosed err

decodeLineBytes :: ByteString -> T.Text
decodeLineBytes = TE.decodeUtf8With (\_ _ -> Just '\xFFFD')

hasNixError :: [String] -> Bool
hasNixError = any ("error:" `isInfixOf`)

nixString :: String -> String
nixString s = '"' : concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape '$' = "\\$"
    escape c = [c]

sanitize :: String -> String
sanitize = map $ \c -> if isAlphaNum c then c else '_'

splitOn :: Char -> String -> [String]
splitOn sep = go []
  where
    go acc [] = [reverse acc]
    go acc (c : cs)
        | c == sep = reverse acc : go [] cs
        | otherwise = go (c : acc) cs

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (_ : _) [] = Nothing
stripPrefix (x : xs) (y : ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing

stripTrailingNewlines :: String -> String
stripTrailingNewlines = reverse . dropWhile (== '\n') . reverse
