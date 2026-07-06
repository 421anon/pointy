{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | OpenAPI 3 specification, generated from 'Api.API'.

The instances here teach @servant-openapi3@ how to document the handful of
real Servant combinators that carry no schema by default.
-}
module Docs.OpenApi (pointyOpenApi) where

import Agent.Git (AgentGitState, AgentSessionView, AgentUsage)
import Agent.Session (AgentSession, AgentTurn, PreparedApply)
import Api (API)
import ApiTypes (DynamicJson)
import Control.Lens (ALens', cloneLens, imap, (%~), (&), (.~), (?~), _Just)
import Data.Aeson (object)
import qualified Data.ByteString as BS
import Data.OpenApi hiding (Header)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.Exts (fromList)
import GHC.TypeLits (KnownSymbol)
import Handlers.Agent (ConfirmApplyRequest, RenameSessionRequest, SessionRequest, TurnRequest)
import Handlers.Autocomplete (AutocompleteRequest)
import Handlers.SrcFiles (UserRepoInfo)
import Handlers.Store (DirEntry)
import Network.HTTP.Media ((//))
import Servant
import Servant.Multipart (MultipartData, MultipartForm', Tmp)
import Servant.OpenApi (HasOpenApi (..))
import Servant.Types.SourceT (SourceT)

-- | Free-form JSON whose schema is defined in the user repository.
instance ToSchema DynamicJson where
    declareNamedSchema _ = pure (NamedSchema (Just "DynamicJson") (mempty & example ?~ object []))

instance {-# OVERLAPPING #-} ToSchema (SourceT IO BS.ByteString) where
    declareNamedSchema _ =
        pure . NamedSchema (Just "StreamingBody") $
            mempty
                & type_ ?~ OpenApiString
                & format ?~ "binary"
                & description ?~ "Streaming response body."

instance {-# OVERLAPPING #-} (Typeable hs) => ToSchema (Headers hs (SourceT IO BS.ByteString)) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (SourceT IO BS.ByteString))

-- | @Raw@ has no HTTP method, but this API only uses it for GET behind @CaptureAll@.
instance {-# OVERLAPPING #-} forall sym a. (KnownSymbol sym, ToParamSchema a) => HasOpenApi (CaptureAll sym a :> Raw) where
    toOpenApi _ = toOpenApi (Proxy :: Proxy (Capture sym a :> Get '[OctetStream] FileDownload))

instance forall sub. (HasOpenApi sub) => HasOpenApi (MultipartForm' '[] Tmp (MultipartData Tmp) :> sub) where
    toOpenApi _ =
        toOpenApi (Proxy :: Proxy sub)
            & allOperations . requestBody ?~ Inline multipartRequestBody

data FileDownload

instance ToSchema FileDownload where
    declareNamedSchema _ =
        pure . NamedSchema (Just "FileDownload") $
            mempty
                & type_ ?~ OpenApiString
                & format ?~ "binary"

multipartRequestBody :: RequestBody
multipartRequestBody =
    mempty
        & content
            .~ fromList
                [
                    ( "multipart" // "form-data"
                    , mempty & schema ?~ Inline uploadFormSchema
                    )
                ]

uploadFormSchema :: Schema
uploadFormSchema =
    mempty
        & type_ ?~ OpenApiObject
        & properties .~ [("files", Inline filesField)]
        & required .~ ["files"]
  where
    filesField =
        mempty
            & type_ ?~ OpenApiArray
            & items ?~ OpenApiItemsObject (Inline binaryFile)
    binaryFile = mempty & type_ ?~ OpenApiString & format ?~ "binary"

-- Agent request bodies are hand-written because the Haskell field names
-- differ from the wire JSON keys.

stringField :: Referenced Schema
stringField = Inline (mempty & type_ ?~ OpenApiString)

objectSchema :: Text -> [(Text, Referenced Schema)] -> NamedSchema
objectSchema typeName fields =
    NamedSchema (Just typeName) $
        mempty
            & type_ ?~ OpenApiObject
            & properties .~ fromList fields
            & required .~ map fst fields

instance ToSchema TurnRequest where
    declareNamedSchema _ =
        pure $ objectSchema "TurnRequest" [("sessionId", stringField), ("prompt", stringField)]

instance ToSchema SessionRequest where
    declareNamedSchema _ =
        pure $ objectSchema "SessionRequest" [("sessionId", stringField)]

instance ToSchema RenameSessionRequest where
    declareNamedSchema _ =
        pure $ objectSchema "RenameSessionRequest" [("sessionId", stringField), ("name", stringField)]

instance ToSchema ConfirmApplyRequest where
    declareNamedSchema _ =
        pure $
            objectSchema
                "ConfirmApplyRequest"
                [("sessionId", stringField), ("targetHead", stringField), ("candidateHead", stringField)]

instance ToSchema DirEntry
instance ToSchema UserRepoInfo
instance ToSchema AutocompleteRequest
instance ToSchema PreparedApply
instance ToSchema AgentSession
instance ToSchema AgentTurn
instance ToSchema AgentGitState
instance ToSchema AgentSessionView
instance ToSchema AgentUsage

-- | The complete OpenAPI specification.
pointyOpenApi :: OpenApi
pointyOpenApi =
    withPathTags $
        withPathSummaries $
            toOpenApi (Proxy :: Proxy API)
                & info . title .~ "Pointy Backend API"
                & info . version .~ "1.0.0"
                & info . description ?~ "HTTP API served by the Pointy backend. All routes are mounted under the `/backend` prefix by the reverse proxy."
                & servers .~ ["/backend"]

methodLenses :: [ALens' PathItem (Maybe Operation)]
methodLenses = [get, put, post, delete, options, head_, patch, trace]

-- | Group operations by their first path segment so Sourcey renders API sections.
withPathTags :: OpenApi -> OpenApi
withPathTags = paths %~ imap tagOperations
  where
    tagOperations path item = foldr setTags item methodLenses
      where
        section = firstPathSegment path
        setTags :: ALens' PathItem (Maybe Operation) -> PathItem -> PathItem
        setTags methodLens =
            cloneLens methodLens . _Just %~ \operation ->
                operation{_operationTags = fromList [section]}

firstPathSegment :: FilePath -> Text
firstPathSegment path =
    case takeWhile (/= '/') (dropWhile (== '/') path) of
        "" -> "root"
        segment -> pack segment

withPathSummaries :: OpenApi -> OpenApi
withPathSummaries = paths %~ imap nameOperations
  where
    nameOperations path item = foldr setSummary item methodLenses
      where
        setSummary :: ALens' PathItem (Maybe Operation) -> PathItem -> PathItem
        setSummary methodLens = cloneLens methodLens . _Just . summary ?~ pack path
