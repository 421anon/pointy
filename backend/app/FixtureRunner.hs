{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Fixture.Document (FixtureAgent (..), FixtureAgentEdit (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (isAbsolute, splitDirectories, takeDirectory, (</>))
import System.IO (BufferMode (..), hFlush, hIsEOF, hSetBuffering, stdin, stdout)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    plan <- loadPlan arguments
    if "-p" `elem` arguments || "--print" `elem` arguments
        then putStrLn (T.unpack (fromMaybe "Fixture chat" (agentTitle =<< plan)))
        else runRpc plan

loadPlan :: [String] -> IO (Maybe FixtureAgent)
loadPlan arguments = case planPath arguments of
    Nothing -> pure Nothing
    Just path -> do
        exists <- doesFileExist path
        if not exists
            then pure Nothing
            else do
                decoded <- A.eitherDecode <$> LBS.readFile path
                case decoded of
                    Left err -> error ("fixture agent plan: " ++ err)
                    Right plan -> pure (Just plan)
  where
    planPath ("--plan" : path : _) = Just path
    planPath (_ : rest) = planPath rest
    planPath [] = Nothing

runRpc :: Maybe FixtureAgent -> IO ()
runRpc plan = do
    worktree <- fromMaybe "." <$> lookupEnv "POINTY_AGENT_WORKTREE"
    hSetBuffering stdin LineBuffering
    loop worktree False
  where
    loop worktree handledInitial = do
        eof <- hIsEOF stdin
        unless eof $ do
            line <- TIO.hGetLine stdin
            case A.decodeStrict (TE.encodeUtf8 line) :: Maybe A.Value of
                Nothing -> loop worktree handledInitial
                Just event -> handle worktree handledInitial event >>= loop worktree
    handle worktree handledInitial event = case textField "type" event of
        Just "prompt" -> do
            let initial = textField "id" event == Just "initial"
            if initial && not handledInitial
                then do
                    outcome <-
                        if null (agentEditsOf plan)
                            then pure (Right ())
                            else applyScriptedEdits worktree (agentEditsOf plan)
                    case outcome of
                        Left err -> do
                            emitObject (assistantEnd ("Fixture agent could not make its scripted edits: " <> err) "error" err)
                            hFlush stdout
                            exitWith (ExitFailure 1)
                        Right () -> do
                            emitObject (assistantEnd (replyOf plan) "endTurn" "")
                            emitObject (object ["type" .= ("agent_end" :: T.Text)])
                            pure True
                else do
                    emitObject (assistantEnd (replyOf plan) "endTurn" "")
                    emitObject (object ["type" .= ("agent_end" :: T.Text)])
                    pure True
        Just "get_session_stats" -> do
            emitObject $
                object
                    [ "type" .= ("response" :: T.Text)
                    , "command" .= ("get_session_stats" :: T.Text)
                    , "success" .= True
                    , "data" .= object ["contextUsage" .= object ["tokens" .= (12000 :: Int), "contextWindow" .= (128000 :: Int)]]
                    ]
            pure handledInitial
        Just "get_state" -> do
            emitObject $
                object
                    [ "type" .= ("response" :: T.Text)
                    , "command" .= ("get_state" :: T.Text)
                    , "success" .= True
                    , "data" .= object ["isStreaming" .= False, "isCompacting" .= False, "pendingMessageCount" .= (0 :: Int)]
                    ]
            pure handledInitial
        Just "abort" -> exitWith (ExitFailure 130)
        _ -> pure handledInitial

agentEditsOf :: Maybe FixtureAgent -> [FixtureAgentEdit]
agentEditsOf = maybe [] agentEdits

replyOf :: Maybe FixtureAgent -> T.Text
replyOf = fromMaybe "" . (agentReply =<<)

assistantEnd :: T.Text -> T.Text -> T.Text -> A.Value
assistantEnd reply stopReason errorMessage =
    object
        [ "type" .= ("message_end" :: T.Text)
        , "message"
            .= object
                [ "role" .= ("assistant" :: T.Text)
                , "content" .= [object ["type" .= ("text" :: T.Text), "text" .= reply]]
                , "stopReason" .= stopReason
                , "errorMessage" .= errorMessage
                ]
        ]

applyScriptedEdits :: FilePath -> [FixtureAgentEdit] -> IO (Either T.Text ())
applyScriptedEdits worktree = go (0 :: Int)
  where
    go _ [] = pure (Right ())
    go index (edit : rest) = do
        let callId = "fixture-edit-" <> T.pack (show index)
        emitObject $
            object
                [ "type" .= ("tool_execution_start" :: T.Text)
                , "toolCallId" .= callId
                , "toolName" .= ("edit" :: T.Text)
                , "args" .= object ["path" .= editPath edit]
                ]
        result <- try (applyEdit worktree edit)
        emitObject $
            object
                [ "type" .= ("tool_execution_end" :: T.Text)
                , "toolCallId" .= callId
                ]
        case result of
            Left (err :: SomeException) -> pure (Left (T.pack (displayException err)))
            Right () -> go (index + 1) rest

applyEdit :: FilePath -> FixtureAgentEdit -> IO ()
applyEdit worktree edit = do
    unless (safeRelativePath path) $
        ioError (userError ("unsafe fixture edit path: " ++ path))
    let full = worktree </> path
    case editContent edit of
        Just content -> do
            createDirectoryIfMissing True (takeDirectory full)
            TIO.writeFile full content
        Nothing -> case (editFind edit, editReplace edit) of
            (Just find, Just replace)
                | not (T.null find) -> do
                    content <- TIO.readFile full
                    case replaceFirst find replace content of
                        Nothing -> ioError (userError ("fixture edit pattern not found in " ++ path ++ ": " ++ T.unpack find))
                        Just updated -> TIO.writeFile full updated
            _ -> ioError (userError ("fixture edit for " ++ path ++ " needs content or find and replace"))
  where
    path = editPath edit

replaceFirst :: T.Text -> T.Text -> T.Text -> Maybe T.Text
replaceFirst find replace content =
    let (before, rest) = T.breakOn find content
     in if T.null rest
            then Nothing
            else Just (before <> replace <> T.drop (T.length find) rest)

safeRelativePath :: FilePath -> Bool
safeRelativePath path =
    not (isAbsolute path)
        && not (null segments)
        && all (\segment -> segment /= ".." && segment /= "." && not (null segment)) segments
  where
    segments = splitDirectories path

textField :: T.Text -> A.Value -> Maybe T.Text
textField name (A.Object object_) = case KeyMap.lookup (Key.fromText name) object_ of
    Just (A.String value) -> Just value
    _ -> Nothing
textField _ _ = Nothing

emitObject :: A.Value -> IO ()
emitObject value = LBS.hPut stdout (A.encode value <> "\n") >> hFlush stdout
