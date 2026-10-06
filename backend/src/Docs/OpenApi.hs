{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Docs.OpenApi (pointyOpenApi) where

import Agent.Git (AgentGitState, AgentSessionView, AgentUsage)
import Agent.Session (AgentSession, AgentSessionSummary, AgentTurn, PreparedApply)
import Api (API)
import ApiTypes (DynamicJson)
import Control.Lens (ALens', cloneLens, imap, (%~), (&), (.~), (?~), _Just)
import Data.Aeson (object)
import qualified Data.ByteString as BS
import Data.OpenApi hiding (Header)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import GHC.Exts (fromList, toList)
import GHC.TypeLits (KnownSymbol)
import Handlers.Agent (ApplyRequest, AutoApplyRequest, RenameSessionRequest, SessionRequest, TurnRequest)
import ProjectTree (ChildRef, ProjectFields, TreeOp)
import Handlers.Autocomplete (AutocompleteRequest)
import Handlers.Scratch (ScratchEntry, ScratchListing, ScratchRootResponse, ScratchWrapRequest)
import Handlers.SrcFiles (UserRepoInfo)
import Handlers.StepReview (ReviewRequest, StepReviewReport)
import Handlers.Store (ByteOffset, DirEntry, FileChunk, LineOffset)
import Network.HTTP.Media ((//))
import Servant
import Servant.Multipart (MultipartData, MultipartForm', Tmp)
import Servant.OpenApi (HasOpenApi (..))
import Servant.Types.SourceT (SourceT)

instance ToSchema DynamicJson where
    declareNamedSchema _ = pure (NamedSchema (Just "DynamicJson") (mempty & example ?~ object []))

instance ToSchema ProjectFields where
    declareNamedSchema _ =
        pure . NamedSchema (Just "ProjectFields") $
            mempty
                & type_ ?~ OpenApiObject
                & required .~ ["name"]
                & properties .~ fromList [("name", stringField), ("preset", stringField), ("templates", arrayOf stringField)]
                & description ?~ "A project's name and exactly one of preset or templates."

instance ToSchema ChildRef where
    declareNamedSchema _ =
        pure . NamedSchema (Just "ChildRef") $
            mempty
                & oneOf ?~ [childSchema "step" [("id", integerField)], childSchema "project" [("id", integerField)]]
                & description ?~ "A step or project child of a project."

instance ToSchema TreeOp where
    declareNamedSchema _ = do
        fieldsSchema <- declareSchemaRef (Proxy :: Proxy ProjectFields)
        refSchema <- declareSchemaRef (Proxy :: Proxy ChildRef)
        pure . NamedSchema (Just "TreeOp") $
            mempty
                & type_ ?~ OpenApiObject
                & required .~ ["op"]
                & properties
                    .~ fromList
                        [ ("op", Inline (mempty & type_ ?~ OpenApiString & description ?~ "One of update, link, unlink, order, hide, delete."))
                        , ("project", integerField)
                        , ("parent", integerField)
                        , ("child", refSchema)
                        , ("children", arrayOf refSchema)
                        , ("hidden", Inline (mempty & type_ ?~ OpenApiBoolean))
                        , ("fields", fieldsSchema)
                        ]
                & description ?~ "One project tree operation: update {project,fields}; link or unlink {parent,child}; order {parent,children}; hide {parent,child,hidden}; delete {child}."

instance {-# OVERLAPPING #-} ToSchema (SourceT IO BS.ByteString) where
    declareNamedSchema _ =
        pure . NamedSchema (Just "StreamingBody") $
            mempty
                & type_ ?~ OpenApiString
                & format ?~ "binary"
                & description ?~ "Streaming response body."

instance {-# OVERLAPPING #-} (Typeable hs) => ToSchema (Headers hs (SourceT IO BS.ByteString)) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy (SourceT IO BS.ByteString))

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


stringField :: Referenced Schema
stringField = Inline (mempty & type_ ?~ OpenApiString)

nullableField :: OpenApiType -> Referenced Schema
nullableField openApiType = Inline (mempty & type_ ?~ openApiType & nullable ?~ True)

integerField :: Referenced Schema
integerField = Inline (mempty & type_ ?~ OpenApiInteger)

booleanField :: Referenced Schema
booleanField = Inline (mempty & type_ ?~ OpenApiBoolean)

arrayOf :: Referenced Schema -> Referenced Schema
arrayOf itemSchema = Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject itemSchema)

childSchema :: Text -> [(Text, Referenced Schema)] -> Referenced Schema
childSchema kind fields =
    Inline $
        mempty
            & type_ ?~ OpenApiObject
            & required .~ [kind]
            & properties .~ fromList [(kind, Inline (mempty & type_ ?~ OpenApiObject & required .~ ["id"] & properties .~ fromList fields))]

objectSchema :: Text -> [(Text, Referenced Schema)] -> NamedSchema
objectSchema typeName fields =
    NamedSchema (Just typeName) $
        mempty
            & type_ ?~ OpenApiObject
            & properties .~ fromList fields
            & required .~ map fst fields

instance ToSchema TurnRequest where
    declareNamedSchema _ =
        pure $
            objectSchema
                "TurnRequest"
                [ ("sessionId", stringField)
                , ("prompt", stringField)
                , ("currentProjectId", nullableField OpenApiInteger)
                , ("autoApply", booleanField)
                , ("clientId", stringField)
                ]
                & schema . required .~ ["sessionId", "prompt"]

instance ToSchema SessionRequest where
    declareNamedSchema _ =
        pure $ objectSchema "SessionRequest" [("sessionId", stringField)]

instance ToSchema RenameSessionRequest where
    declareNamedSchema _ =
        pure $ objectSchema "RenameSessionRequest" [("sessionId", stringField), ("name", stringField)]

instance ToSchema ApplyRequest where
    declareNamedSchema _ =
        pure $
            objectSchema "ApplyRequest" [("sessionId", stringField), ("autoApply", booleanField), ("clientId", stringField)]
                & schema . required .~ ["sessionId", "autoApply"]

instance ToSchema AutoApplyRequest where
    declareNamedSchema _ =
        pure $ objectSchema "AutoApplyRequest" [("clientId", stringField), ("autoApply", booleanField)]

instance ToSchema LineOffset where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int)

instance ToSchema ByteOffset where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int)

instance ToSchema DirEntry
instance ToSchema FileChunk
instance ToSchema ScratchRootResponse where
    declareNamedSchema _ =
        pure $ objectSchema "ScratchRootResponse" [("root", nullableField OpenApiString)]

instance ToSchema ScratchEntry where
    declareNamedSchema _ =
        pure $
            objectSchema
                "ScratchEntry"
                [ ("name", stringField)
                , ("directory", Inline (mempty & type_ ?~ OpenApiBoolean))
                , ("size", nullableField OpenApiInteger)
                ]

instance ToSchema ScratchListing where
    declareNamedSchema _ = do
        entrySchema <- declareSchemaRef (Proxy :: Proxy ScratchEntry)
        pure $
            objectSchema
                "ScratchListing"
                [ ("path", stringField)
                , ("entries", Inline (mempty & type_ ?~ OpenApiArray & items ?~ OpenApiItemsObject entrySchema))
                ]

instance ToSchema ScratchWrapRequest where
    declareNamedSchema _ = pure $ objectSchema "ScratchWrapRequest" [("path", stringField)]

instance ToSchema UserRepoInfo
instance ToSchema AutocompleteRequest
instance ToSchema PreparedApply
instance ToSchema AgentSession
instance ToSchema AgentTurn
instance ToSchema AgentGitState
instance ToSchema AgentSessionSummary
instance ToSchema AgentSessionView
instance ToSchema AgentUsage

instance ToSchema ReviewRequest where
    declareNamedSchema _ =
        pure $ objectSchema "ReviewRequest" [("reviewedBy", stringField), ("reviewComments", stringField)]

instance ToSchema StepReviewReport where
    declareNamedSchema _ =
        pure $
            objectSchema
                "StepReviewReport"
                [("reviewedRevision", stringField), ("reviewedBy", stringField), ("reviewComments", stringField), ("reviewedStatus", stringField), ("reviewedStatusError", stringField), ("comparison", stringField), ("comparisonDetail", stringField)]

pointyOpenApi :: OpenApi
pointyOpenApi =
    withPathTags $
        withPathSummaries $
            withoutAgentApi $
                toOpenApi (Proxy :: Proxy API)
                & info . title .~ "Pointy Backend API"
                & info . version .~ "1.0.0"
                & info . description ?~ "HTTP API served by the Pointy backend. All routes are mounted under the `/backend` prefix by the reverse proxy."
                & servers .~ ["/backend"]

withoutAgentApi :: OpenApi -> OpenApi
withoutAgentApi = paths %~ fromList . filter ((/= "agent") . firstPathSegment . fst) . toList

methodLenses :: [ALens' PathItem (Maybe Operation)]
methodLenses = [get, put, post, delete, options, head_, patch, trace]

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
