{-# LANGUAGE OverloadedStrings #-}

module Handlers.ChatSearch (ChatSearchRequest (..), ChatSearchResponse (..), chatSearchHandler) where

import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Effects (AppM)
import Search.ChatCorpus (chatCorpus)
import Search.Store (SearchHit (..), SearchStore, newSearchStore, storeSearch)
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE chatSearchStore #-}
chatSearchStore :: SearchStore Text
chatSearchStore = unsafePerformIO newSearchStore

data ChatSearchRequest = ChatSearchRequest
    { requestQuery :: Text
    , requestExclude :: [Text]
    }
    deriving (Show)

instance FromJSON ChatSearchRequest where
    parseJSON = withObject "ChatSearchRequest" $ \fields ->
        ChatSearchRequest <$> fields .: "query" <*> (fromMaybe [] <$> fields .:? "exclude")

data ChatSearchResult = ChatSearchResult
    { resultSessionId :: Text
    , resultSimilarity :: Double
    , resultRelevance :: Maybe Double
    }
    deriving (Show)

instance ToJSON ChatSearchResult where
    toJSON result =
        object
            [ "session_id" .= resultSessionId result
            , "similarity" .= resultSimilarity result
            , "relevance" .= resultRelevance result
            ]

newtype ChatSearchResponse = ChatSearchResponse {responseResults :: [ChatSearchResult]}
    deriving (Show)

instance ToJSON ChatSearchResponse where
    toJSON response = object ["results" .= responseResults response]

chatSearchHandler :: ChatSearchRequest -> AppM ChatSearchResponse
chatSearchHandler request = do
    hits <- lift (storeSearch chatSearchStore chatCorpus (requestQuery request) (requestExclude request))
    pure (ChatSearchResponse (map toResult hits))
  where
    toResult hit = ChatSearchResult (hitKey hit) (hitSimilarity hit) (hitRelevance hit)
