{-# LANGUAGE OverloadedStrings #-}

module SlurmEvents
    ( BrokerState (..)
    , Event (..)
    , EventKind (..)
    , PublishingRecord (..)
    , SnapshotWindow (..)
    , TerminalRecord (..)
    , WatchFilter (..)
    , applyEvent
    , beginSnapshotWindow
    , emptyBrokerState
    , emptySnapshotWindow
    , eventKindName
    , filterJobs
    , filterTerminalJobs
    , markSnapshotDirty
    , parseEventKind
    , pruneBrokerState
    , snapshotReady
    , validAccountName
    ) where

import Data.Char (isAsciiLower, isControl, isDigit)
import Data.List (intercalate, isPrefixOf, sortOn, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

data WatchFilter = WatchNative | WatchAccount String
    deriving (Eq, Show)

data EventKind = SubmitEvent | StartEvent | PublishingEvent | TerminalEvent | NodeEvent | ResetEvent
    deriving (Eq, Show)

data Event = Event
    { eventKind :: EventKind
    , eventAccount :: Maybe String
    , eventJobId :: Maybe String
    , eventJobName :: Maybe String
    , eventState :: Maybe String
    }
    deriving (Eq, Show)

data TerminalRecord = TerminalRecord
    { terminalJobId :: String
    , terminalJobName :: String
    , terminalAccount :: Maybe String
    , terminalAt :: Int
    }
    deriving (Eq, Show)

data PublishingRecord = PublishingRecord
    { publishingJobId :: String
    , publishingJobName :: String
    , publishingAccount :: Maybe String
    , publishingAt :: Int
    }
    deriving (Eq, Show)

data BrokerState = BrokerState
    { brokerTerminal :: Map (String, String) TerminalRecord
    , brokerPublishing :: Map (String, String) PublishingRecord
    }
    deriving (Eq, Show)

data SnapshotWindow = SnapshotWindow
    { snapshotGeneration :: Int
    , snapshotDirty :: Bool
    }
    deriving (Eq, Show)

emptyBrokerState :: BrokerState
emptyBrokerState = BrokerState Map.empty Map.empty

emptySnapshotWindow :: SnapshotWindow
emptySnapshotWindow = SnapshotWindow 0 False

beginSnapshotWindow :: SnapshotWindow -> (Int, SnapshotWindow)
beginSnapshotWindow window =
    let generation = snapshotGeneration window + 1
     in (generation, SnapshotWindow generation False)

markSnapshotDirty :: SnapshotWindow -> SnapshotWindow
markSnapshotDirty window = window {snapshotDirty = True}

snapshotReady :: SnapshotWindow -> Int -> Bool
snapshotReady window required =
    not (snapshotDirty window) && snapshotGeneration window >= required

eventKindName :: EventKind -> String
eventKindName SubmitEvent = "submit"
eventKindName StartEvent = "start"
eventKindName PublishingEvent = "publishing"
eventKindName TerminalEvent = "terminal"
eventKindName NodeEvent = "node"
eventKindName ResetEvent = "reset"

parseEventKind :: String -> Maybe EventKind
parseEventKind "submit" = Just SubmitEvent
parseEventKind "start" = Just StartEvent
parseEventKind "publishing" = Just PublishingEvent
parseEventKind "terminal" = Just TerminalEvent
parseEventKind "node" = Just NodeEvent
parseEventKind "reset" = Just ResetEvent
parseEventKind _ = Nothing

validAccountName :: String -> Bool
validAccountName account =
    case account of
        [] -> False
        first : rest ->
            validEndpoint first
                && length account <= 32
                && validRest rest
  where
    validEndpoint character = isAsciiLower character || isDigit character
    validCharacter character = validEndpoint character || character == '-'
    validRest [] = True
    validRest [character] = validEndpoint character
    validRest (character : remaining) = validCharacter character && validRest remaining

applyEvent :: Int -> Event -> BrokerState -> Either String BrokerState
applyEvent now event broker =
    case eventKind event of
        SubmitEvent -> Right broker
        StartEvent -> Right broker
        NodeEvent -> Right broker
        ResetEvent -> Right broker {brokerPublishing = Map.empty}
        PublishingEvent -> publishJob now event broker
        TerminalEvent -> finishJob now event broker

publishJob :: Int -> Event -> BrokerState -> Either String BrokerState
publishJob now event broker = do
    (jobId, jobName, account) <- eventIdentity event
    let key = (jobId, jobName)
    if Map.member key (brokerTerminal broker)
        then Right broker
        else
            if Map.member key (brokerPublishing broker)
                then Right broker
                else
                    Right $
                        pruneBrokerState now $
                            broker
                                { brokerPublishing =
                                    Map.insert
                                        key
                                        (PublishingRecord jobId jobName account now)
                                        (brokerPublishing broker)
                                }

finishJob :: Int -> Event -> BrokerState -> Either String BrokerState
finishJob now event broker = do
    (jobId, jobName, account) <- eventIdentity event
    let key = (jobId, jobName)
    if requeued
        then
            Right $
                broker
                    { brokerTerminal = Map.delete key (brokerTerminal broker)
                    , brokerPublishing = Map.delete key (brokerPublishing broker)
                    }
        else case Map.lookup key (brokerTerminal broker) of
            Just existing
                | terminalJobName existing == jobName -> Right broker
            _ ->
                Right $
                    pruneBrokerState now $
                        broker
                            { brokerTerminal =
                                Map.insert
                                    key
                                    (TerminalRecord jobId jobName account now)
                                    (brokerTerminal broker)
                            , brokerPublishing = Map.delete key (brokerPublishing broker)
                            }
  where
    requeued =
        maybe False ((`elem` ["REQUEUED", "REQUEUE", "REQUEUE_HOLD"]) . map toUpperAscii) (eventState event)

toUpperAscii :: Char -> Char
toUpperAscii character
    | character >= 'a' && character <= 'z' =
        toEnum (fromEnum character - fromEnum 'a' + fromEnum 'A')
    | otherwise = character

eventIdentity :: Event -> Either String (String, String, Maybe String)
eventIdentity event = do
    jobId <- maybe (Left "job id is required") Right (eventJobId event)
    jobName <- maybe (Left "job name is required") Right (eventJobName event)
    if not (validJobId jobId)
        then Left "job id must be numeric"
        else
            if not (validJobName jobName)
                then Left "job name is invalid"
                else do
                    account <- eventAccountFor jobName (eventAccount event)
                    Right (jobId, jobName, account)

eventAccountFor :: String -> Maybe String -> Either String (Maybe String)
eventAccountFor jobName supplied =
    case scopedAccount jobName of
        Just derived ->
            case supplied of
                Nothing -> Right (Just derived)
                Just account
                    | account == derived -> Right (Just derived)
                    | otherwise -> Left "account does not match job name"
        Nothing
            | "pc-" `isPrefixOf` jobName -> Left "job name has a malformed account prefix"
            | otherwise ->
                case supplied of
                    Nothing -> Right Nothing
                    Just account
                        | validAccountName account -> Right (Just account)
                        | otherwise -> Left "invalid account"

scopedAccount :: String -> Maybe String
scopedAccount name =
    case stripPrefix "pc-" name of
        Nothing -> Nothing
        Just rest ->
            case break (== '-') rest of
                (account, '-' : remainder)
                    | validAccountName account
                    , not (null remainder) ->
                        Just account
                _ -> Nothing

validJobId :: String -> Bool
validJobId value =
    case break (== '_') value of
        (base, []) -> not (null base) && all isDigit base
        (base, '_' : array) ->
            not (null base)
                && all isDigit base
                && not (null array)
                && all isDigit array
        _ -> False

validJobName :: String -> Bool
validJobName name =
    not (null name)
        && length name <= 512
        && all validCharacter name
  where
    validCharacter character =
        not (isControl character)
            && character `notElem` ("|\n\r\0" :: String)

pruneBrokerState :: Int -> BrokerState -> BrokerState
pruneBrokerState now broker =
    broker
        { brokerTerminal =
            takeNewest maxRecords terminalAt (Map.filter freshTerminal (brokerTerminal broker))
        , brokerPublishing =
            takeNewest maxRecords publishingAt (Map.filter freshPublishing (brokerPublishing broker))
        }
  where
    freshTerminal record = now - terminalAt record <= terminalLifetime
    freshPublishing record = now - publishingAt record <= publishingLifetime
    maxRecords = 4096
    terminalLifetime = 3600
    publishingLifetime = 86400

takeNewest :: Ord key => Int -> (value -> Int) -> Map key value -> Map key value
takeNewest count recordTime values =
    Map.fromList $
        take count $
            sortOn (negate . recordTime . snd) (Map.toList values)

filterJobs :: WatchFilter -> BrokerState -> String -> String
filterJobs watch broker input =
    unlines (mapMaybe renderLine (lines input))
  where
    renderLine line = do
        row <- parseQueueRow line
        visible <- applyState row
        case watch of
            WatchNative
                | "pointy-nix-build-" `isPrefixOf` queueJobName visible ->
                    pure (renderQueueRow visible (queueJobName visible))
                | otherwise -> Nothing
            WatchAccount account
                | queueAccount visible == account
                , Just remainder <- stripPrefix ("pc-" <> account <> "-") (queueJobName visible)
                , not (null remainder) ->
                    pure (renderQueueRow visible remainder)
                | otherwise -> Nothing
    applyState row =
        let key = (queueJobId row, queueJobName row)
         in if Map.member key (brokerTerminal broker)
                then Nothing
                else
                    case Map.lookup key (brokerPublishing broker) of
                        Just _ -> Just row {queueState = "PUBLISHING"}
                        Nothing -> Just row

filterTerminalJobs :: WatchFilter -> BrokerState -> [(String, String)]
filterTerminalJobs watch broker =
    mapMaybe render (Map.elems (brokerTerminal broker))
  where
    render record =
        case watch of
            WatchNative
                | "pointy-nix-build-" `isPrefixOf` terminalJobName record ->
                    Just (terminalJobId record, terminalJobName record)
                | otherwise -> Nothing
            WatchAccount account
                | Just recordAccount <- terminalAccount record
                , recordAccount == account
                , Just remainder <-
                    stripPrefix ("pc-" <> account <> "-") (terminalJobName record)
                , not (null remainder) ->
                    Just (terminalJobId record, remainder)
                | otherwise -> Nothing

data QueueRow = QueueRow
    { queueJobId :: String
    , queueAccount :: String
    , queueJobName :: String
    , queueComment :: String
    , queueState :: String
    , queueElapsed :: String
    , queueReason :: String
    }
    deriving (Eq, Show)

parseQueueRow :: String -> Maybe QueueRow
parseQueueRow line =
    case splitOn '|' line of
        jobId : account : jobName : rest
            | length rest >= 3 ->
                case splitAt (length rest - 3) rest of
                    (commentParts, [state, elapsed, reason]) ->
                        Just
                            QueueRow
                                { queueJobId = jobId
                                , queueAccount = trim account
                                , queueJobName = jobName
                                , queueComment = intercalate "|" commentParts
                                , queueState = state
                                , queueElapsed = elapsed
                                , queueReason = reason
                                }
                    _ -> Nothing
        _ -> Nothing

renderQueueRow :: QueueRow -> String -> String
renderQueueRow row displayName =
    concat
        [ queueJobId row
        , "|"
        , displayName
        , "|"
        , queueComment row
        , "|"
        , queueState row
        , "|"
        , queueElapsed row
        , "|"
        , queueReason row
        ]

trim :: String -> String
trim = dropWhileEndSpace . dropWhile (== ' ')
  where
    dropWhileEndSpace = reverse . dropWhile (== ' ') . reverse

splitOn :: Char -> String -> [String]
splitOn separator value = case break (== separator) value of
    (part, []) -> [part]
    (part, _ : rest) -> part : splitOn separator rest
