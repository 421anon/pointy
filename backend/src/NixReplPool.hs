{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NixReplPool (
    NixEvalOutput (..),
    NixEvalRequest (..),
    NixEvalTarget (..),
    runNixEval,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Exception (SomeException, catch, try)
import Control.Monad (void)
import Data.Aeson (Value (..), eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Char (isAlphaNum, isSpace)
import Data.List (isInfixOf)
import qualified Data.Text as T
import System.IO (BufferMode (..), Handle, hClose, hFlush, hGetLine, hIsClosed, hPutStrLn, hSetBuffering)
import System.IO.Error (isEOFError)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (CreatePipe), createProcess, proc, terminateProcess, waitForProcess)

-- | The subset of nix eval output modes used by the backend.
data NixEvalOutput = EvalJson | EvalRaw
    deriving (Eq, Show)

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

data ReplSession = ReplSession
    { replName :: String
    , replInput :: Handle
    , replEvents :: TQueue ReplEvent
    , replCounter :: MVar Int
    , replFlakeVars :: MVar [(String, String)]
    , replClose :: IO ()
    }

data ReplStream = ReplStdout | ReplStderr
    deriving (Eq, Show)

data ReplEvent
    = ReplLine ReplStream String
    | ReplClosed ReplStream String
    deriving (Eq, Show)

data ReplOutcome
    = ReplSucceeded String
    | ReplFailed String
    | ReplDied String
    deriving (Eq, Show)

{-# NOINLINE pureSessionRef #-}
pureSessionRef :: MVar (Maybe ReplSession)
pureSessionRef = unsafePerformIO (newMVar Nothing)

{-# NOINLINE impureSessionRef #-}
impureSessionRef :: MVar (Maybe ReplSession)
impureSessionRef = unsafePerformIO (newMVar Nothing)

runNixEval :: NixEvalRequest -> IO (Either String String)
runNixEval req = do
    let kind = if evalImpure req then ImpureRepl else PureRepl
        ref = case kind of
            PureRepl -> pureSessionRef
            ImpureRepl -> impureSessionRef
    runWithSession True kind ref req

runWithSession :: Bool -> ReplKind -> MVar (Maybe ReplSession) -> NixEvalRequest -> IO (Either String String)
runWithSession mayRetry kind ref req = do
    outcome <- modifyMVar ref $ \mSession -> do
        eSession <- case mSession of
            Just session -> return $ Right session
            Nothing -> try $ openSession kind
        case eSession of
            Left (err :: SomeException) -> return (Nothing, ReplDied $ "failed to start " ++ show kind ++ " nix repl: " ++ show err)
            Right session -> do
                outcome <- runRequest session req `catch` \(err :: SomeException) -> return (ReplDied $ replName session ++ " failed: " ++ show err)
                case outcome of
                    ReplDied _ -> closeSession session >> return (Nothing, outcome)
                    _ -> return (Just session, outcome)
    case outcome of
        ReplSucceeded output -> return $ Right output
        ReplFailed err -> return $ Left err
        ReplDied err
            | mayRetry -> runWithSession False kind ref req
            | otherwise -> return $ Left err

openSession :: ReplKind -> IO ReplSession
openSession kind = do
    events <- newTQueueIO
    let args = ["repl", "--extra-experimental-features", "nix-command flakes"] ++ ["--impure" | kind == ImpureRepl]
        cp =
            (proc "nix" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    (Just stdinH, Just stdoutH, Just stderrH, ph) <- createProcess cp
    hSetBuffering stdinH LineBuffering
    void $ forkIO $ readLoop ReplStdout stdoutH events
    void $ forkIO $ readLoop ReplStderr stderrH events
    counter <- newMVar 0
    flakeVars <- newMVar []
    let session =
            ReplSession
                { replName = show kind ++ " nix repl"
                , replInput = stdinH
                , replEvents = events
                , replCounter = counter
                , replFlakeVars = flakeVars
                , replClose = closeLocalSession stdinH stdoutH stderrH ph
                }
    initializeSession session `catch` \(err :: SomeException) -> do
        closeSession session
        fail $ "nix repl initialization failed for " ++ replName session ++ ": " ++ show err
    return session

initializeSession :: ReplSession -> IO ()
initializeSession session = do
    marker <- nextMarker session "ready"
    sendCommands session [":p " ++ nixString marker]
    result <- collectUntilMarker session marker
    case result of
        ReplDied err -> fail err
        _ -> return ()

runRequest :: ReplSession -> NixEvalRequest -> IO ReplOutcome
runRequest session req = do
    eExpr <- renderRequestExpression session req
    case eExpr of
        Left outcome -> return outcome
        Right expr -> do
            begin <- nextMarker session "begin"
            end <- nextMarker session "end"
            sendCommands
                session
                [ ":p " ++ nixString begin
                , ":p builtins.toJSON (" ++ expr ++ ")"
                , ":p " ++ nixString end
                ]
            collectUntilMarker session end >>= \case
                ReplSucceeded _ -> return $ ReplDied "internal protocol error: collectUntilMarker returned success before parsing"
                ReplDied err -> return $ ReplDied err
                ReplFailed raw -> parseReplOutput begin req raw

collectUntilMarker :: ReplSession -> String -> IO ReplOutcome
collectUntilMarker session marker = go []
  where
    go acc = do
        event <- atomically $ readTQueue (replEvents session)
        case event of
            ReplLine ReplStdout line | line == marker -> return $ ReplFailed (formatEvents $ reverse acc)
            ReplClosed ReplStdout err -> return $ ReplDied $ replName session ++ " closed stdout before marker " ++ marker ++ formatClosed err
            _ -> go (event : acc)

parseReplOutput :: String -> NixEvalRequest -> String -> IO ReplOutcome
parseReplOutput begin req raw = do
    let events = parseFormattedEvents raw
        (preBegin, atBegin) = break isBegin events
    case atBegin of
        [] -> return $ ReplDied $ "nix repl response did not include begin marker " ++ begin ++ ":\n" ++ raw
        (_ : body) ->
            if hasNixError preBegin
                then return $ ReplFailed $ stripTrailingNewlines $ formatEvents preBegin
                else return $ parseBody req body
  where
    isBegin (ReplLine ReplStdout line) = line == begin
    isBegin _ = False

parseBody :: NixEvalRequest -> [ReplEvent] -> ReplOutcome
parseBody req body =
    case validJsonLines of
        [(line, value)] -> renderOutput value line
        [] -> ReplFailed $ stripTrailingNewlines $ formatEvents body
        _ -> ReplFailed $ "nix repl returned multiple JSON values:\n" ++ stripTrailingNewlines (formatEvents body)
  where
    stdoutLines = [line | ReplLine ReplStdout line <- body, not (all isSpace line)]
    validJsonLines = [(line, value) | line <- stdoutLines, Right value <- [eitherDecode (LBS8.pack line)]]

    renderOutput value line = case evalOutput req of
        EvalJson -> ReplSucceeded (line ++ "\n")
        EvalRaw -> case value of
            String txt -> ReplSucceeded (T.unpack txt)
            _ -> ReplFailed $ "nix repl --raw emulation expected a string result, got JSON: " ++ line

renderRequestExpression :: ReplSession -> NixEvalRequest -> IO (Either ReplOutcome String)
renderRequestExpression session req = do
    eTargetExpr <- case evalTarget req of
        EvalExpr expr -> return $ Right ("(" ++ expr ++ ")")
        EvalInstallable installable attr -> do
            eFlakeVar <- ensureFlakeBinding session installable
            return $ fmap (`renderFlakeInstallableExpression` attr) eFlakeVar
    return $ case eTargetExpr of
        Left outcome -> Left outcome
        Right targetExpr ->
            Right $ case evalApply req of
                Nothing -> targetExpr
                Just applyExpr -> "(" ++ applyExpr ++ ") (" ++ targetExpr ++ ")"

ensureFlakeBinding :: ReplSession -> String -> IO (Either ReplOutcome String)
ensureFlakeBinding session installable =
    modifyMVar (replFlakeVars session) $ \bindings ->
        case lookup installable bindings of
            Just varName -> return (bindings, Right varName)
            Nothing -> do
                let varName = "pointyFlake" ++ show (length bindings + 1)
                marker <- nextMarker session "flake"
                sendCommands
                    session
                    [ varName ++ " = builtins.getFlake " ++ nixString installable
                    , ":p " ++ nixString marker
                    ]
                collectUntilMarker session marker >>= \case
                    ReplSucceeded _ -> return (bindings, Left $ ReplDied "internal protocol error while binding flake")
                    ReplDied err -> return (bindings, Left $ ReplDied err)
                    ReplFailed raw ->
                        if hasNixError (parseFormattedEvents raw)
                            then return (bindings, Left $ ReplFailed $ stripTrailingNewlines raw)
                            else return ((installable, varName) : bindings, Right varName)

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
        ++ "system = builtins.currentSystem; "
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
        return (next, next)
    return $ "__pointy_nix_repl_" ++ sanitize label ++ "_" ++ show n ++ "__"

sendCommands :: ReplSession -> [String] -> IO ()
sendCommands session commands = do
    mapM_ (hPutStrLn (replInput session)) commands
    hFlush (replInput session)

readLoop :: ReplStream -> Handle -> TQueue ReplEvent -> IO ()
readLoop stream handle events = loop
  where
    loop = do
        eLine <- try (hGetLine handle) :: IO (Either IOError String)
        case eLine of
            Right line -> do
                atomically $ writeTQueue events (ReplLine stream line)
                loop
            Left err
                | isEOFError err -> atomically $ writeTQueue events (ReplClosed stream "")
                | otherwise -> atomically $ writeTQueue events (ReplClosed stream (show err))

closeSession :: ReplSession -> IO ()
closeSession session = replClose session `catch` \(_ :: SomeException) -> return ()

closeLocalSession :: Handle -> Handle -> Handle -> ProcessHandle -> IO ()
closeLocalSession stdinH stdoutH stderrH ph = do
    hCloseIfOpen stdinH
    hCloseIfOpen stdoutH
    hCloseIfOpen stderrH
    terminateProcess ph `catch` \(_ :: SomeException) -> return ()
    void (waitForProcess ph) `catch` \(_ :: SomeException) -> return ()

hCloseIfOpen :: Handle -> IO ()
hCloseIfOpen handle = do
    closed <- hIsClosed handle
    if closed then return () else hClose handle

formatClosed :: String -> String
formatClosed "" = ""
formatClosed err = ": " ++ err

formatEvents :: [ReplEvent] -> String
formatEvents = unlines . map formatEvent
  where
    formatEvent (ReplLine ReplStdout line) = line
    formatEvent (ReplLine ReplStderr line) = "stderr: " ++ line
    formatEvent (ReplClosed stream err) = show stream ++ " closed" ++ formatClosed err

parseFormattedEvents :: String -> [ReplEvent]
parseFormattedEvents = map parseLine . lines
  where
    parseLine line = case stripPrefix "stderr: " line of
        Just stderrLine -> ReplLine ReplStderr stderrLine
        Nothing -> ReplLine ReplStdout line

hasNixError :: [ReplEvent] -> Bool
hasNixError = any hasErrorLine
  where
    hasErrorLine (ReplLine _ line) = "error:" `isInfixOf` line
    hasErrorLine _ = False

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
