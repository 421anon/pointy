{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Search.Store (
    SearchStore,
    SearchHit (..),
    newSearchStore,
    storeSearch,
) where

import Config (EmbeddingConfig (..), JudgeConfig (..), SearchConfig (..))
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Binary.Get (getByteString, getWord32le, runGetOrFail)
import qualified Data.Binary.Get as BinaryGet
import Data.Binary.Put (putByteString, putWord32le, runPut)
import qualified Data.Binary.Put as BinaryPut
import Data.ByteArray (convert)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as VU
import Effectful (Eff, IOE, liftIO, (:>))
import Effectful.Exception (bracket_)
import Effects (Eval, Inference, embedTexts, judgeCandidates)
import Search.Document (StepDocument (..), buildDocuments)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.FilePath ((</>))
import UserRepo (ReadRepoContext (..), withReadRepoTransaction)

data SearchHit = SearchHit
    { hitStepId :: Int
    , hitSimilarity :: Double
    , hitRelevance :: Maybe Double
    }
    deriving (Show)

data IndexKey = IndexKey
    { keyCommit :: Text
    , keyNamespace :: Text
    }
    deriving (Eq)

data IndexState = IndexState
    { indexKey :: IndexKey
    , indexDocuments :: Map Int StepDocument
    , indexVectors :: Map Text (VU.Vector Float)
    }

data SearchStore = SearchStore
    { storeState :: IORef (Maybe IndexState)
    , storeLock :: MVar ()
    }

newSearchStore :: IO SearchStore
newSearchStore = SearchStore <$> newIORef Nothing <*> newMVar ()

storeSearch :: (Eval :> es, Inference :> es, IOE :> es) => SearchStore -> SearchConfig -> Maybe Text -> Text -> [Int] -> Eff es [SearchHit]
storeSearch store searchConfig commit query exclude = do
    index <- ensureIndex store searchConfig commit
    case index of
        Nothing -> pure []
        Just indexed -> do
            embedding <- embedTexts [renderTemplate "{query}" (embeddingQueryTemplate (searchEmbedding searchConfig)) query]
            case embedding of
                Right [queryVector] -> judgeRank searchConfig indexed query queryVector (Set.fromList exclude)
                Right vectors -> warn ("The embedding provider returned " <> show (length vectors) <> " vectors for one query.") >> pure []
                Left err -> warn ("Failed to embed the search query: " <> err) >> pure []

judgeRank :: (Inference :> es, IOE :> es) => SearchConfig -> IndexState -> Text -> VU.Vector Float -> Set.Set Int -> Eff es [SearchHit]
judgeRank searchConfig indexed query queryVector exclude = do
    let limit = take (max 1 (searchLimit searchConfig))
        similarityRanked =
            take (max 1 (searchJudgeCandidates searchConfig)) $
                sortOn
                    (Down . snd)
                    [ (stepId, similarity)
                    | (stepId, document) <- Map.toList (indexDocuments indexed)
                    , Set.notMember stepId exclude
                    , Just vector <- [Map.lookup (documentHash document) (indexVectors indexed)]
                    , VU.length vector == VU.length queryVector
                    , let similarity = realToFrac (VU.sum (VU.zipWith (*) queryVector vector))
                    , similarity >= embeddingMinSimilarity (searchEmbedding searchConfig)
                    ]
        similarityHits = [SearchHit stepId similarity Nothing | (stepId, similarity) <- similarityRanked]
    case searchJudge searchConfig of
        Nothing -> pure (limit similarityHits)
        Just _ | null similarityRanked -> pure []
        Just judge -> do
            let candidates =
                    [ (stepId, documentText document)
                    | (stepId, _) <- similarityRanked
                    , Just document <- [Map.lookup stepId (indexDocuments indexed)]
                    ]
            relevance <- judgeCandidates query candidates
            case relevance of
                Left err -> warn ("Failed to judge the step search candidates: " <> err) >> pure (limit similarityHits)
                Right scores ->
                    pure $
                        limit $
                            sortOn
                                (Down . fromMaybe 0 . hitRelevance)
                                [ SearchHit stepId similarity (Just score)
                                | (stepId, similarity) <- similarityRanked
                                , Just score <- [Map.lookup stepId scores]
                                , score >= judgeMinRelevance judge
                                ]

ensureIndex :: (Eval :> es, Inference :> es, IOE :> es) => SearchStore -> SearchConfig -> Maybe Text -> Eff es (Maybe IndexState)
ensureIndex store searchConfig commit = do
    resolved <- resolveCommit commit
    case resolved of
        Left err -> warn ("Failed to resolve the step search commit: " <> err) >> pure Nothing
        Right target -> do
            let key = IndexKey target (vectorNamespace (searchEmbedding searchConfig))
                current = do
                    cached <- liftIO (readIORef (storeState store))
                    pure (cached >>= \state -> if indexKey state == key then Just state else Nothing)
            cached <- current
            case cached of
                Just state -> pure (Just state)
                Nothing ->
                    bracket_ (liftIO (takeMVar (storeLock store))) (liftIO (putMVar (storeLock store) ())) $ do
                        rechecked <- current
                        maybe (buildIndex store searchConfig key) (pure . Just) rechecked

resolveCommit :: (IOE :> es) => Maybe Text -> Eff es (Either String Text)
resolveCommit (Just commit) = pure (Right commit)
resolveCommit Nothing = withReadRepoTransaction (pure . T.pack . readCommitHash)

buildIndex :: (Eval :> es, Inference :> es, IOE :> es) => SearchStore -> SearchConfig -> IndexKey -> Eff es (Maybe IndexState)
buildIndex store searchConfig key = do
    documents <- withReadRepoTransaction $ \ctx -> buildDocuments ctx{readCommitHash = T.unpack (keyCommit key)}
    case documents of
        Left err -> warn ("Failed to build the step search index: " <> err) >> pure Nothing
        Right steps -> do
            let embedding = searchEmbedding searchConfig
                namespace = keyNamespace key
                texts = Map.fromList [(documentHash document, renderTemplate "{document}" (embeddingDocumentTemplate embedding) (documentText document)) | document <- steps]
            stored <- liftIO (loadVectors namespace)
            let known = Map.restrictKeys stored (Map.keysSet texts)
                missing = Map.toList (Map.withoutKeys texts (Map.keysSet known))
            when (null missing && Map.size known /= Map.size stored) $
                liftIO (saveVectors namespace known)
            embedded <- embedBatches namespace known (chunks (max 1 (embeddingBatchSize embedding)) missing)
            case embedded of
                Left err -> warn ("Failed to embed the step documents: " <> err) >> pure Nothing
                Right vectors -> do
                    let state =
                            IndexState
                                { indexKey = key
                                , indexDocuments = Map.fromList [(documentStepId document, document) | document <- steps]
                                , indexVectors = vectors
                                }
                    liftIO (writeIORef (storeState store) (Just state))
                    pure (Just state)

embedBatches :: (Inference :> es, IOE :> es) => Text -> Map Text (VU.Vector Float) -> [[(Text, Text)]] -> Eff es (Either String (Map Text (VU.Vector Float)))
embedBatches _ known [] = pure (Right known)
embedBatches namespace known (batch : rest) = do
    response <- embedTexts (map snd batch)
    case response of
        Left err -> pure (Left err)
        Right vectors
            | length vectors /= length batch ->
                pure (Left ("The embedding provider returned " <> show (length vectors) <> " vectors for " <> show (length batch) <> " texts."))
            | otherwise -> do
                let known' = Map.union known (Map.fromList (zip (map fst batch) vectors))
                liftIO (saveVectors namespace known')
                embedBatches namespace known' rest

warn :: (IOE :> es) => String -> Eff es ()
warn message = liftIO (putStrLn ("Warning: " <> message))

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks size items = let (batch, rest) = splitAt size items in batch : chunks size rest

renderTemplate :: Text -> Text -> Text -> Text
renderTemplate placeholder template value = T.replace placeholder value template

vectorNamespace :: EmbeddingConfig -> Text
vectorNamespace embedding =
    hexDigest
        ( hash
            (TE.encodeUtf8 (embeddingModel embedding <> "|" <> T.pack (show (embeddingDimensions embedding)) <> "|" <> embeddingDocumentTemplate embedding)) ::
            Digest SHA256
        )

cacheVersion :: Int
cacheVersion = 2

vectorCachePath :: Text -> IO FilePath
vectorCachePath namespace = do
    home <- getHomeDirectory
    let dir = home </> ".local" </> "state" </> "pointy" </> "embeddings"
    createDirectoryIfMissing True dir
    pure (dir </> T.unpack namespace <> ".bin")

loadVectors :: Text -> IO (Map Text (VU.Vector Float))
loadVectors namespace = do
    path <- vectorCachePath namespace
    exists <- doesFileExist path
    if not exists
        then pure Map.empty
        else decodeVectors . BL.fromStrict <$> BS.readFile path

decodeVectors :: BL.ByteString -> Map Text (VU.Vector Float)
decodeVectors bytes = case runGetOrFail getHeader bytes of
    Left _ -> Map.empty
    Right (rest, _, version)
        | fromIntegral version /= cacheVersion -> Map.empty
        | otherwise -> decodeEntries rest Map.empty
  where
    getHeader = do
        version <- getWord32le
        _ <- getWord32le
        _ <- getWord32le
        pure version
    decodeEntries remaining acc
        | BL.null remaining = acc
        | otherwise = case runGetOrFail getEntry remaining of
            Left _ -> acc
            Right (rest, _, (documentKey, vector)) -> decodeEntries rest (Map.insert documentKey vector acc)
    getEntry = do
        keyLength <- getWord32le
        keyBytes <- getByteString (fromIntegral keyLength)
        dimensions <- getWord32le
        values <- mapM (const BinaryGet.getFloatle) [1 .. dimensions]
        pure (TE.decodeUtf8 keyBytes, VU.fromList values)

encodeVectors :: Map Text (VU.Vector Float) -> BS.ByteString
encodeVectors vectors =
    BL.toStrict $
        runPut $ do
            putWord32le (fromIntegral cacheVersion)
            putWord32le 0
            putWord32le (fromIntegral (Map.size vectors))
            mapM_ putEntry (Map.toList vectors)
  where
    putEntry (documentKey, vector) = do
        let keyBytes = TE.encodeUtf8 documentKey
        putWord32le (fromIntegral (BS.length keyBytes))
        putByteString keyBytes
        putWord32le (fromIntegral (VU.length vector))
        mapM_ BinaryPut.putFloatle (VU.toList vector)

saveVectors :: Text -> Map Text (VU.Vector Float) -> IO ()
saveVectors namespace vectors = do
    path <- vectorCachePath namespace
    let staging = path <> ".tmp"
    BS.writeFile staging (encodeVectors vectors)
    renameFile staging path

hexDigest :: Digest a -> Text
hexDigest = TE.decodeUtf8 . Base16.encode . convert
