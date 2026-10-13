{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ApiTypes (DynamicJson (..), RawJSON, Turtle) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Media ((//), (/:))
import Servant (Accept (..), MimeRender (..), MimeUnrender (..))

newtype DynamicJson = DynamicJson {unDynamicJson :: LBS.ByteString}

data RawJSON

instance Accept RawJSON where contentType _ = "application" // "json"
instance MimeRender RawJSON DynamicJson where mimeRender _ = unDynamicJson
instance MimeUnrender RawJSON DynamicJson where mimeUnrender _ = Right . DynamicJson

data Turtle

instance Accept Turtle where contentType _ = "text" // "turtle" /: ("charset", "utf-8")
instance MimeRender Turtle Text where mimeRender _ = LBS.fromStrict . TE.encodeUtf8
