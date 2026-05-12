{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Capture and cache @nix eval@ errors for step drv paths. Errors are
-- parsed from Nix's @--log-format internal-json@ stream; the cache is
-- keyed on @(commit, stepId)@ and never invalidated, since commits are
-- immutable. A per-key empty 'MVar' acts as the single-flight latch.
module EvalError (
    EvalResult,
    getCachedEvalError,
    cachedEvalStepDrv,
    kickEvalStepDrv,
    cleanEvalError,
    shortEvalError,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, tryReadMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (void, when)
import Control.Monad.Except (runExceptT)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafePerformIO)
import UserRepo (ReadRepoContext (..), runNixInRepo)

type EvalResult = Either Text Text

{-# NOINLINE evalCache #-}
evalCache :: TVar (Map.Map (Text, Int) (MVar EvalResult))
evalCache = unsafePerformIO (newTVarIO Map.empty)

getCachedEvalError :: Text -> Int -> IO (Maybe EvalResult)
getCachedEvalError commit sid = do
    m <- readTVarIO evalCache
    maybe (pure Nothing) tryReadMVar (Map.lookup (commit, sid) m)

cachedEvalStepDrv :: ReadRepoContext -> Int -> IO EvalResult
cachedEvalStepDrv ctx sid = do
    (mv, owns) <- claim (T.pack (readCommitHash ctx)) sid
    if owns then fillEval ctx sid mv else readMVar mv

kickEvalStepDrv :: ReadRepoContext -> Int -> (EvalResult -> IO ()) -> IO ()
kickEvalStepDrv ctx sid onDone = do
    (mv, owns) <- claim (T.pack (readCommitHash ctx)) sid
    when owns . void . forkIO $ fillEval ctx sid mv >>= onDone

-- | Atomically reserve the latch for @(commit, sid)@. The 'Bool' is
-- 'True' iff the caller now owns the latch and must fill it.
claim :: Text -> Int -> IO (MVar EvalResult, Bool)
claim commit sid = do
    fresh <- newEmptyMVar
    atomically $ do
        m <- readTVar evalCache
        case Map.lookup (commit, sid) m of
            Just existing -> pure (existing, False)
            Nothing -> do
                modifyTVar' evalCache (Map.insert (commit, sid) fresh)
                pure (fresh, True)

fillEval :: ReadRepoContext -> Int -> MVar EvalResult -> IO EvalResult
fillEval ctx sid mv = do
    attempt <- try (runEval ctx sid) :: IO (Either SomeException EvalResult)
    let r = either (Left . T.pack . ("internal eval error: " <>) . show) id attempt
    putMVar mv r
    pure r

runEval :: ReadRepoContext -> Int -> IO EvalResult
runEval ctx sid =
    bimap T.pack (T.strip . T.pack)
        <$> runExceptT
            ( runNixInRepo
                ctx
                ["eval", "--raw", "--show-trace", "--log-format", "internal-json"]
                ("#pointy.steps." ++ show sid ++ ".drvPath")
            )
  where
    bimap f g = either (Left . f) (Right . g)

-- | The deepest @raw_msg@ from the error stream, suffixed with
-- @\<repo-relative path\>:\<line\>@ when the envelope carries a location.
shortEvalError :: Text -> Maybe Text
shortEvalError t = do
    ev <- lastError t
    let bare = T.strip (stripAnsi (eeRawMsg ev))
    pure (annotateLocation ev bare)

-- | Full trace with ANSI removed, dedented, and pruned of frames that
-- point at third-party Nix code (nixpkgs, dream2nix, the module
-- system). The user-repo store path is inferred from the envelope
-- @file@ and rewritten to a repo-relative path.
cleanEvalError :: Text -> Text
cleanEvalError t = case lastError t of
    Nothing -> t
    Just ev ->
        let m = stripAnsi (eeMsg ev)
         in case inferUserPrefix ev of
                Nothing -> dedent m
                Just up ->
                    let blocks = T.splitOn "\n\n" m
                        kept = filter (keepBlock up) blocks
                        joined = T.intercalate "\n\n" (map (T.replace up "") kept)
                     in dedent joined

data ErrorEvent = ErrorEvent
    { eeFile :: Maybe Text
    , eeLine :: Maybe Int
    , eeRawMsg :: Text
    , eeMsg :: Text
    }

lastError :: Text -> Maybe ErrorEvent
lastError = lastMay . mapMaybe parseLine . T.lines
  where
    parseLine line = do
        payload <- T.stripPrefix "@nix " line
        v <- A.decodeStrict (TE.encodeUtf8 payload)
        A.parseMaybe pickError v
    pickError = A.withObject "msg" $ \o -> do
        action :: Text <- o A..: "action"
        level :: Int <- o A..: "level"
        if action == "msg" && level == 0
            then
                ErrorEvent
                    <$> o A..:? "file"
                    <*> o A..:? "line"
                    <*> o A..: "raw_msg"
                    <*> o A..: "msg"
            else fail ""
    lastMay [] = Nothing
    lastMay xs = Just (last xs)

-- | Store-path prefix Nix copied the user repo to. Whatever store path
-- the envelope @file@ lives under is treated as user-repo; any other
-- prefix is third-party.
inferUserPrefix :: ErrorEvent -> Maybe Text
inferUserPrefix ev = eeFile ev >>= storePathPrefix

storePathPrefix :: Text -> Maybe Text
storePathPrefix p = do
    rest <- T.stripPrefix "/nix/store/" p
    let (hash, after) = T.breakOn "/" rest
    if T.null hash || not ("/" `T.isPrefixOf` after)
        then Nothing
        else Just ("/nix/store/" <> hash <> "/")

annotateLocation :: ErrorEvent -> Text -> Text
annotateLocation ev bare = case (eeFile ev >>= toRepoRelative, eeLine ev) of
    (Just rel, Just ln) -> bare <> " (" <> rel <> ":" <> T.pack (show ln) <> ")"
    _ -> bare

toRepoRelative :: Text -> Maybe Text
toRepoRelative fileLoc = do
    rest <- T.stripPrefix "/nix/store/" fileLoc
    let (_, after) = T.breakOn "/" rest
    rest2 <- T.stripPrefix "/" after
    pure (T.takeWhile (/= ':') rest2)

keepBlock :: Text -> Text -> Bool
keepBlock userPrefix block
    | "(stack trace truncated" `T.isInfixOf` block = False
    | "duplicate frames omitted)" `T.isInfixOf` block = False
    | otherwise =
        let paths = findStorePaths block
            hasUser = any (userPrefix `T.isPrefixOf`) paths
            hasOtherStore = any (not . (userPrefix `T.isPrefixOf`)) paths
            hasBuiltinPath = "at <" `T.isInfixOf` block
         in if hasUser
                then True
                else
                    if hasOtherStore || hasBuiltinPath
                        then False
                        else not (isModuleSystemNoise block)

findStorePaths :: Text -> [Text]
findStorePaths = go
  where
    go t = case T.breakOn "/nix/store/" t of
        (_, rest) | T.null rest -> []
        (_, rest) ->
            let path = T.takeWhile pathChar rest
             in path : go (T.drop (T.length path) rest)
    pathChar c = c /= '\n' && c /= ' ' && c /= '\'' && c /= '`' && c /= '"' && c /= ':'

-- | A single-line frame containing only @while evaluating the option@
-- or @while evaluating definitions from@. With its source-bearing
-- companion frames already filtered out, it carries no useful info.
isModuleSystemNoise :: Text -> Bool
isModuleSystemNoise block =
    let s = T.strip block
     in not ("\n" `T.isInfixOf` s)
            && ( "while evaluating the option" `T.isInfixOf` s
                    || "while evaluating definitions from" `T.isInfixOf` s
               )

-- | Drop the 7-space indent Nix uses for trace frames. Source-snippet
-- lines (11+ spaces) keep their relative alignment.
dedent :: Text -> Text
dedent t = T.unlines (map dropIndent (T.lines t))
  where
    dropIndent l = fromMaybe l (T.stripPrefix "       " l)

stripAnsi :: Text -> Text
stripAnsi t = case T.breakOn "\ESC[" t of
    (before, rest) | T.null rest -> before
    (before, rest) -> before <> stripAnsi (T.drop 1 (snd (T.breakOn "m" (T.drop 2 rest))))
