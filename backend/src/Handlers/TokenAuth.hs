{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers.TokenAuth (TokenLoginForm, SessionRedirect, TokenSession, tokenAuthHandler, tokenLoginHandler, tokenLogoutHandler, tokenSessionHandler) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Crypto.Error (maybeCryptoError)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import Data.ASN1.BinaryEncoding (DER (..))
import Data.ASN1.Encoding (decodeASN1')
import Data.ASN1.Types (fromASN1)
import Data.Aeson (FromJSON (..), ToJSON (..), decodeStrict, object, withObject, (.:), (.:?), (.=))
import Data.ByteArray.Encoding (Base (..), convertFromBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isJust, isNothing)
import Data.PEM (pemContent, pemParseBS)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.X509 (PubKey (..))
import Servant (Handler, Header, Headers, NoContent (..), addHeader, err401, err404, throwError)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)
import Web.Cookie (SetCookie (..), defaultSetCookie, parseCookies, renderSetCookie, sameSiteLax)
import Web.FormUrlEncoded (FromForm (..), parseUnique)

data TokenAuth = TokenAuth
    { tokenAudience :: Text
    , tokenPublicKey :: Ed25519.PublicKey
    }

data Claims = Claims
    { claimsAudience :: Text
    , claimsUser :: Maybe Text
    , claimsExpires :: Integer
    }

instance FromJSON Claims where
    parseJSON = withObject "Claims" $ \obj ->
        Claims
            <$> obj .: "aud"
            <*> ((<|>) <$> obj .:? "name" <*> obj .:? "sub")
            <*> obj .: "exp"

data TokenSession = TokenSession
    { sessionAccount :: Text
    , sessionUser :: Maybe Text
    }

instance ToJSON TokenSession where
    toJSON session = object ["account" .= sessionAccount session, "user" .= sessionUser session]

newtype TokenLoginForm = TokenLoginForm Text

instance FromForm TokenLoginForm where
    fromForm form = TokenLoginForm <$> parseUnique "token" form

type SessionRedirect = Headers '[Header "Location" Text, Header "Set-Cookie" Text] NoContent

{-# NOINLINE tokenAuth #-}
tokenAuth :: Maybe TokenAuth
tokenAuth = unsafePerformIO $ do
    audience <- lookupEnv "POINTY_TOKEN_AUDIENCE"
    keyFile <- lookupEnv "POINTY_TOKEN_PUBLIC_KEY"
    case (audience, keyFile) of
        (Just name, Just path) -> Just . TokenAuth (T.pack name) <$> readPublicKey path
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

tokenAuthHandler :: Maybe Text -> Handler NoContent
tokenAuthHandler cookies = do
    now <- liftIO currentSeconds
    if isJust (cookieClaims now cookies)
        then pure NoContent
        else throwError err401

tokenSessionHandler :: Maybe Text -> Handler TokenSession
tokenSessionHandler cookies = do
    when (isNothing tokenAuth) $ throwError err404
    now <- liftIO currentSeconds
    case cookieClaims now cookies of
        Just claims -> pure (TokenSession (claimsAudience claims) (claimsUser claims))
        Nothing -> throwError err401

tokenLoginHandler :: TokenLoginForm -> Handler SessionRedirect
tokenLoginHandler (TokenLoginForm token) = do
    now <- liftIO currentSeconds
    case validToken now (encodeUtf8 token) of
        Just claims -> pure $ addHeader "/" $ addHeader (sessionCookie token (claimsExpires claims - now)) NoContent
        Nothing -> throwError err401

tokenLogoutHandler :: Handler SessionRedirect
tokenLogoutHandler = do
    when (isNothing tokenAuth) $ throwError err404
    pure $ addHeader "/" $ addHeader (sessionCookie "" 0) NoContent

cookieClaims :: Integer -> Maybe Text -> Maybe Claims
cookieClaims now cookies = validToken now =<< lookup sessionCookieName . parseCookies . encodeUtf8 =<< cookies

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

validToken :: Integer -> ByteString -> Maybe Claims
validToken now token = do
    auth <- tokenAuth
    [header, payload, signature] <- pure (BC.split '.' token)
    signatureBytes <- either (const Nothing) Just (convertFromBase Base64URLUnpadded signature :: Either String ByteString)
    parsedSignature <- maybeCryptoError (Ed25519.signature signatureBytes)
    claimsBytes <- either (const Nothing) Just (convertFromBase Base64URLUnpadded payload :: Either String ByteString)
    claims <- decodeStrict claimsBytes
    if Ed25519.verify (tokenPublicKey auth) (header <> "." <> payload) parsedSignature
        && claimsAudience claims == tokenAudience auth
        && claimsExpires claims > now
        then Just claims
        else Nothing
