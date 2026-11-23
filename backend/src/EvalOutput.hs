module EvalOutput (
    evalJsonDecode,
    evalOutputText,
) where

import Data.Aeson (FromJSON, eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

evalJsonDecode :: FromJSON a => ByteString -> Either String a
evalJsonDecode = eitherDecodeStrict'

evalOutputText :: ByteString -> Text
evalOutputText = TE.decodeUtf8With (\_ _ -> Just '\xFFFD')
