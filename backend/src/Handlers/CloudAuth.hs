{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.CloudAuth (CloudLoginForm, CloudLoginResponse, cloudAuthHandler, cloudLoginHandler) where

import Control.Monad.IO.Class (liftIO)
import Crypto.Error (maybeCryptoError)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Types (fromASN1)
import Data.Aeson (FromJSON (..), decodeStrict, withObject, (.:))
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust)
import Data.PEM (pemContent, pemParseBS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.X509 (PubKey (..))
import Servant (Handler, Header, Headers, NoContent (..), addHeader, err401, throwError)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookie, sameSiteLax)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data CloudAuth = CloudAuth
    { cloudAccount :: Text
    , cloudPublicKey :: Ed25519.PublicKey
    }

data Claims = Claims
    { claimsAudience :: Text
    , claimsExpires :: Integer
    }

instance FromJSON Claims where
    parseJSON = withObject "Claims" $ \obj ->
        Claims
            <$> obj .: "aud"
            <*> obj .: "exp"

newtype CloudLoginForm = CloudLoginForm Text

instance FromForm CloudLoginForm where
    fromForm form = CloudLoginForm <$> parseUnique "token" form

type CloudLoginResponse = Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent

{-# NOINLINE cloudAuth #-}
cloudAuth :: Maybe CloudAuth
cloudAuth = unsafePerformIO $ do
    account <- lookupEnv "POINTY_CLOUD_ACCOUNT"
    keyFile <- lookupEnv "POINTY_CLOUD_PUBLIC_KEY"
    case (account, keyFile) of
        (Just name, Just path) -> Just . CloudAuth (T.pack name) <$> readPublicKey path
        _ -> pure Nothing

readPublicKey :: FilePath -> IO Ed25519.PublicKey
readPublicKey path = do
    pem <- BS.readFile path
    case pemParseBS pem of
        Right [block]
            | Right asn1 <- decodeASN1' DER (pemContent block)
            , Right (PubKeyEd25519 key, []) <- fromASN1 asn1 ->
                pure key
        _ -> fail $ path ++ ": expected a single Ed25519 public key"

sessionCookieName :: ByteString
sessionCookieName = "pointy-session"

cloudAuthHandler :: Maybe Text -> Handler NoContent
cloudAuthHandler cookies = do
    now <- liftIO currentSeconds
    if isJust (validToken now =<< lookup sessionCookieName . parseCookies . encodeUtf8 =<< cookies)
        then pure NoContent
        else throwError err401

cloudLoginHandler :: CloudLoginForm -> Handler CloudLoginResponse
cloudLoginHandler (CloudLoginForm token) = do
    now <- liftIO currentSeconds
    case validToken now (encodeUtf8 token) of
        Just expires -> pure $ addHeader "/" $ addHeader (sessionCookie token (expires - now)) NoContent
        Nothing -> throwError err401

sessionCookie :: Text -> Integer -> Text
sessionCookie token seconds =
    decodeUtf8 . BL.toStrict . toLazyByteString . renderSetCookie $
        defaultSetCookie
            { setCookieName = sessionCookieName
            , setCookieValue = encodeUtf8 token
            , setCookiePath = Just "/"
            , setCookieMaxAge = Just (fromInteger seconds)
            , setCookieHttpOnly = True
            , setCookieSecure = True
            , setCookieSameSite = Just sameSiteLax
            }

currentSeconds :: IO Integer
currentSeconds = floor <$> getPOSIXTime

validToken :: Integer -> ByteString -> Maybe Integer
validToken now token = do
    auth <- cloudAuth
    [header, payload, signature] <- pure (BC.split '.' token)
    signatureBytes <- either (const Nothing) Just (convertFromBase Base64URLUnpadded signature :: Either String ByteString)
    parsedSignature <- maybeCryptoError (Ed25519.signature signatureBytes)
    claimsBytes <- either (const Nothing) Just (convertFromBase Base64URLUnpadded payload :: Either String ByteString)
    claims <- decodeStrict claimsBytes
    if Ed25519.verify (cloudPublicKey auth) (header <> "." <> payload) parsedSignature
        && claimsAudience claims == cloudAccount auth
        && claimsExpires claims > now
        then Just (claimsExpires claims)
        else Nothing
