{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.StepSearch (StepSearchRequest (..), StepSearchResponse (..), StepSearchInfo (..), stepSearchHandler, stepSearchInfoHandler) where

import Config (Config (..), EmbeddingConfig (..), SearchConfig (..), loadConfig, resolveConfigPath)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Effects (AppM)
import Search.Store (SearchHit (..), SearchStore, newSearchStore, storeSearch)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE searchStoreRef #-}
searchStoreRef :: SearchStore
searchStoreRef = unsafePerformIO newSearchStore

data StepSearchRequest = StepSearchRequest
    { requestQuery :: Text
    , requestExclude :: [Int]
    }
    deriving (Show)

instance FromJSON StepSearchRequest where
    parseJSON = withObject "StepSearchRequest" $ \fields ->
        StepSearchRequest <$> fields .: "query" <*> (fromMaybe [] <$> fields .:? "exclude")

data StepSearchResult = StepSearchResult
    { resultStepId :: Int
    , resultSimilarity :: Double
    , resultRelevance :: Maybe Double
    }
    deriving (Show)

instance ToJSON StepSearchResult where
    toJSON result =
        object
            [ "step_id" .= resultStepId result
            , "similarity" .= resultSimilarity result
            , "relevance" .= resultRelevance result
            ]

newtype StepSearchResponse = StepSearchResponse {responseResults :: [StepSearchResult]}
    deriving (Show)

instance ToJSON StepSearchResponse where
    toJSON response = object ["results" .= responseResults response]

data StepSearchInfo = StepSearchInfo
    { infoSemantic :: Bool
    , infoJudge :: Bool
    }
    deriving (Show)

instance ToJSON StepSearchInfo where
    toJSON info = object ["semantic" .= infoSemantic info, "judge" .= infoJudge info]

stepSearchInfoHandler :: AppM StepSearchInfo
stepSearchInfoHandler = do
    config <- liftIO readConfig
    pure $ case configSearch config of
        Nothing -> StepSearchInfo False False
        Just search ->
            StepSearchInfo
                { infoSemantic = semanticEnabled search
                , infoJudge = maybe False (const True) (searchJudge search)
                }

stepSearchHandler :: Maybe Text -> StepSearchRequest -> AppM StepSearchResponse
stepSearchHandler commit request = do
    config <- liftIO readConfig
    case configSearch config of
        Just search
            | semanticEnabled search
            , not (T.null (T.strip (requestQuery request))) -> do
                hits <- lift (storeSearch searchStoreRef search commit (requestQuery request) (requestExclude request))
                pure (StepSearchResponse (map toResult hits))
        _ -> pure (StepSearchResponse [])
  where
    toResult hit = StepSearchResult (hitStepId hit) (hitSimilarity hit) (hitRelevance hit)

semanticEnabled :: SearchConfig -> Bool
semanticEnabled search = not (T.null (embeddingProvider (searchEmbedding search)))

readConfig :: IO Config
readConfig = resolveConfigPath >>= loadConfig
