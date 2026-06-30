-- | Shared HTTP wire types.
module ApiTypes (DynamicJson (..)) where

import qualified Data.ByteString.Lazy as LBS

-- | JSON bytes whose concrete schema is defined in the user repository.
newtype DynamicJson = DynamicJson {unDynamicJson :: LBS.ByteString}
