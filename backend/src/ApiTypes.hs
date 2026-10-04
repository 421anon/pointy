{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiTypes (DynamicJson (..), RawJSON) where

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Media ((//))
import Servant (Accept (..), MimeRender (..), MimeUnrender (..))

newtype DynamicJson = DynamicJson {unDynamicJson :: LBS.ByteString}

data RawJSON

instance Accept RawJSON where contentType _ = "application" // "json"
instance MimeRender RawJSON DynamicJson where mimeRender _ = unDynamicJson
instance MimeUnrender RawJSON DynamicJson where mimeUnrender _ = Right . DynamicJson
