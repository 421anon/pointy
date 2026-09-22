module ApiTypes (DynamicJson (..)) where

import qualified Data.ByteString.Lazy as LBS

newtype DynamicJson = DynamicJson {unDynamicJson :: LBS.ByteString}
