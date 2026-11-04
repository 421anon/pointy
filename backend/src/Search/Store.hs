{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Search.Store (
    SearchStore,
    SearchDocument (..),
    SearchHit (..),
    Corpus (..),
    CorpusSnapshot (..),
    newSearchStore,
    storeSearch,
    semanticSearchEnabled,
) where

import Config (Config (..), EmbeddingConfig (..), JudgeConfig (..), SearchConfig (..), loadConfig, resolveConfigPath)
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
import Effects (Inference, embedTexts, judgeCandidates)
import ProviderHttp (JudgeTask (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory, renameFile)
import System.FilePath ((</>))

data SearchDocument key = SearchDocument
    { documentKey :: key
    , documentJudgeId :: Int
    , documentText :: Text
    }

data SearchHit key = SearchHit
    { hitKey :: key
    , hitSimilarity :: Double
    , hitRelevance :: Maybe Double
    }
    deriving (Show)

data CorpusSnapshot es key = CorpusSnapshot
    { snapshotVersion :: Text
    , snapshotDocuments :: Eff es (Either String [SearchDocument key])
    }

data Corpus es key = Corpus
    { corpusNoun :: Text
    , corpusVectorDirectory :: FilePath
    , corpusQueryTemplate :: EmbeddingConfig -> Text
    , corpusJudgeInstructions :: JudgeConfig -> Text
    , corpusSnapshot :: Eff es (Either String (CorpusSnapshot es key))
    }

data IndexKey = IndexKey
    { keyVersion :: Text
    , keyNamespace :: Text
    }
    deriving (Eq)

data IndexedDocument = IndexedDocument
    { indexedJudgeId :: Int
    , indexedText :: Text
    , indexedHash :: Text
    }

data IndexState key = IndexState
    { indexKey :: IndexKey
    , indexDocuments :: Map key IndexedDocument
    , indexVectors :: Map Text (VU.Vector Float)
    }

data SearchStore key = SearchStore
    { storeState :: IORef (Maybe (IndexState key))
    , storeLock :: MVar ()
    }

newSearchStore :: IO (SearchStore key)
newSearchStore = SearchStore <$> newIORef Nothing <*> newMVar ()

semanticSearchEnabled :: SearchConfig -> Bool
semanticSearchEnabled searchConfig = not (T.null (embeddingProvider (searchEmbedding searchConfig)))

storeSearch :: (Ord key, Inference :> es, IOE :> es) => SearchStore key -> Corpus es key -> Text -> [key] -> Eff es [SearchHit key]
storeSearch store corpus query exclude = do
    config <- liftIO (resolveConfigPath >>= loadConfig)
    case configSearch config of
        Just searchConfig
            | semanticSearchEnabled searchConfig
            , not (T.null (T.strip query)) ->
                searchIndex store corpus searchConfig query (Set.fromList exclude)
        _ -> pure []

searchIndex :: (Ord key, Inference :> es, IOE :> es) => SearchStore key -> Corpus es key -> SearchConfig -> Text -> Set.Set key -> Eff es [SearchHit key]
searchIndex store corpus searchConfig query exclude = do
    index <- ensureIndex store corpus searchConfig
    case index of
        Nothing -> pure []
        Just indexed -> do
            embedding <- embedTexts [renderTemplate "{query}" (corpusQueryTemplate corpus (searchEmbedding searchConfig)) query]
            case embedding of
                Right [queryVector] -> judgeRank corpus searchConfig indexed query queryVector exclude
                Right vectors -> warn ("The embedding provider returned " <> show (length vectors) <> " vectors for one query.") >> pure []
                Left err -> warn ("Failed to embed the " <> noun corpus <> " search query: " <> err) >> pure []

judgeRank :: (Ord key, Inference :> es, IOE :> es) => Corpus es key -> SearchConfig -> IndexState key -> Text -> VU.Vector Float -> Set.Set key -> Eff es [SearchHit key]
judgeRank corpus searchConfig indexed query queryVector exclude = do
    let limit = take (max 1 (searchLimit searchConfig))
        similarityRanked =
            take (max 1 (searchJudgeCandidates searchConfig)) $
                sortOn
                    (Down . snd)
                    [ ((key, document), similarity)
                    | (key, document) <- Map.toList (indexDocuments indexed)
                    , Set.notMember key exclude
                    , Just vector <- [Map.lookup (indexedHash document) (indexVectors indexed)]
                    , VU.length vector == VU.length queryVector
                    , let similarity = realToFrac (VU.sum (VU.zipWith (*) queryVector vector))
                    , similarity >= embeddingMinSimilarity (searchEmbedding searchConfig)
                    ]
        similarityHits = [SearchHit key similarity Nothing | ((key, _), similarity) <- similarityRanked]
    case searchJudge searchConfig of
        Nothing -> pure (limit similarityHits)
        Just _ | null similarityRanked -> pure []
        Just judge -> do
            relevance <-
                judgeCandidates
                    JudgeTask
                        { judgeTaskSubject = corpusNoun corpus <> "s"
                        , judgeTaskInstructions = corpusJudgeInstructions corpus judge
                        , judgeTaskQuery = query
                        , judgeTaskCandidates = [(indexedJudgeId document, indexedText document) | ((_, document), _) <- similarityRanked]
                        }
            case relevance of
                Left err -> warn ("Failed to judge the " <> noun corpus <> " search candidates: " <> err) >> pure (limit similarityHits)
                Right scores ->
                    pure $
                        limit $
                            sortOn
                                (Down . fromMaybe 0 . hitRelevance)
                                [ SearchHit key similarity (Just score)
                                | ((key, document), similarity) <- similarityRanked
                                , Just score <- [Map.lookup (indexedJudgeId document) scores]
                                , score >= judgeMinRelevance judge
                                ]

ensureIndex :: (Ord key, Inference :> es, IOE :> es) => SearchStore key -> Corpus es key -> SearchConfig -> Eff es (Maybe (IndexState key))
ensureIndex store corpus searchConfig = do
    snapshot <- corpusSnapshot corpus
    case snapshot of
        Left err -> warn ("Failed to read the " <> noun corpus <> " search corpus: " <> err) >> pure Nothing
        Right current -> do
            let key = IndexKey (snapshotVersion current) (vectorNamespace (searchEmbedding searchConfig))
                cachedIndex = do
                    cached <- liftIO (readIORef (storeState store))
                    pure (cached >>= \state -> if indexKey state == key then Just state else Nothing)
            cached <- cachedIndex
            case cached of
                Just state -> pure (Just state)
                Nothing ->
                    bracket_ (liftIO (takeMVar (storeLock store))) (liftIO (putMVar (storeLock store) ())) $ do
                        rechecked <- cachedIndex
                        maybe (buildIndex store corpus searchConfig key current) (pure . Just) rechecked

buildIndex :: (Ord key, Inference :> es, IOE :> es) => SearchStore key -> Corpus es key -> SearchConfig -> IndexKey -> CorpusSnapshot es key -> Eff es (Maybe (IndexState key))
buildIndex store corpus searchConfig key snapshot = do
    built <- snapshotDocuments snapshot
    case built of
        Left err -> warn ("Failed to build the " <> noun corpus <> " search index: " <> err) >> pure Nothing
        Right documents -> do
            let embedding = searchEmbedding searchConfig
                indexed = Map.fromList [(documentKey document, indexDocument document) | document <- documents]
                texts = Map.fromList [(indexedHash document, renderTemplate "{document}" (embeddingDocumentTemplate embedding) (indexedText document)) | document <- Map.elems indexed]
            cachePath <- liftIO (vectorCachePath (corpusVectorDirectory corpus) (keyNamespace key))
            stored <- liftIO (loadVectors cachePath)
            let known = Map.restrictKeys stored (Map.keysSet texts)
                missing = Map.toList (Map.withoutKeys texts (Map.keysSet known))
            when (null missing && Map.size known /= Map.size stored) $
                liftIO (saveVectors cachePath known)
            embedded <- embedBatches cachePath known (chunks (max 1 (embeddingBatchSize embedding)) missing)
            case embedded of
                Left err -> warn ("Failed to embed the " <> noun corpus <> " documents: " <> err) >> pure Nothing
                Right vectors -> do
                    let state = IndexState{indexKey = key, indexDocuments = indexed, indexVectors = vectors}
                    liftIO (writeIORef (storeState store) (Just state))
                    pure (Just state)

indexDocument :: SearchDocument key -> IndexedDocument
indexDocument document =
    IndexedDocument
        { indexedJudgeId = documentJudgeId document
        , indexedText = documentText document
        , indexedHash = hexDigest (hash (TE.encodeUtf8 (documentText document)) :: Digest SHA256)
        }

embedBatches :: (Inference :> es, IOE :> es) => FilePath -> Map Text (VU.Vector Float) -> [[(Text, Text)]] -> Eff es (Either String (Map Text (VU.Vector Float)))
embedBatches _ known [] = pure (Right known)
embedBatches cachePath known (batch : rest) = do
    response <- embedTexts (map snd batch)
    case response of
        Left err -> pure (Left err)
        Right vectors
            | length vectors /= length batch ->
                pure (Left ("The embedding provider returned " <> show (length vectors) <> " vectors for " <> show (length batch) <> " texts."))
            | otherwise -> do
                let known' = Map.union known (Map.fromList (zip (map fst batch) vectors))
                liftIO (saveVectors cachePath known')
                embedBatches cachePath known' rest

noun :: Corpus es key -> String
noun = T.unpack . corpusNoun

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

vectorCachePath :: FilePath -> Text -> IO FilePath
vectorCachePath directory namespace = do
    home <- getHomeDirectory
    let dir = home </> ".local" </> "state" </> "pointy" </> directory
    createDirectoryIfMissing True dir
    pure (dir </> T.unpack namespace <> ".bin")

loadVectors :: FilePath -> IO (Map Text (VU.Vector Float))
loadVectors path = do
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
            Right (rest, _, (vectorKey, vector)) -> decodeEntries rest (Map.insert vectorKey vector acc)
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
    putEntry (vectorKey, vector) = do
        let keyBytes = TE.encodeUtf8 vectorKey
        putWord32le (fromIntegral (BS.length keyBytes))
        putByteString keyBytes
        putWord32le (fromIntegral (VU.length vector))
        mapM_ BinaryPut.putFloatle (VU.toList vector)

saveVectors :: FilePath -> Map Text (VU.Vector Float) -> IO ()
saveVectors path vectors = do
    let staging = path <> ".tmp"
    BS.writeFile staging (encodeVectors vectors)
    renameFile staging path

hexDigest :: Digest a -> Text
hexDigest = TE.decodeUtf8 . Base16.encode . convert
