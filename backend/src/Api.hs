{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | The backend HTTP API, served by @Main@ and documented by @Docs.OpenApi@.
module Api (API) where

import Agent.Git (AgentApplyView, AgentSessionView, AgentUsage)
import Agent.Session (AgentTurn)
import ApiTypes (DynamicJson)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Handlers.Agent (ConfirmApplyRequest, RenameSessionRequest, SessionRequest, TurnRequest)
import Handlers.Autocomplete (AutocompleteRequest)
import Handlers.FileIndex (FileIndexEntry)
import Handlers.Projects (RawJSON)
import Handlers.SrcFiles (UserRepoInfo)
import Handlers.StatusStream (EventStream)
import Handlers.Store (DirEntry, FileChunk)
import Servant
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Servant.Types.SourceT (SourceT)

-- | Required integer @id@ query parameter, shared by most resource routes.
type ReqId = QueryParam' '[Required, Strict] "id" Int

type ReqProjectId = QueryParam' '[Required, Strict] "project_id" Int

type ReqEntityId = QueryParam' '[Required, Strict] "entity_id" Int

type GetCommitHash =
    "commit-hash"
        :> Description "Returns the commit hash the user repository is currently checked out at."
        :> Get '[PlainText] Text

type GetUserRepoInfo =
    "user-repo-info"
        :> Description "Returns the configured user repository URL and branch."
        :> Get '[JSON] UserRepoInfo

type ListStepFiles =
    "step-files"
        :> Description "Lists files in a step's output directory."
        :> ReqId
        :> QueryParam "commit" Text
        :> QueryParam "path" FilePath
        :> Get '[JSON] [DirEntry]

type ListSrcFiles =
    "src-files"
        :> Description "Lists source files available to a step."
        :> ReqId
        :> QueryParam "path" FilePath
        :> Get '[JSON] [DirEntry]

type UpdateSrcFile =
    "src-files"
        :> Description "Updates a source file and commits it to the user repository."
        :> ReqId
        :> QueryParam' '[Required] "path" FilePath
        :> ReqBody '[PlainText] Text
        :> Put '[JSON] NoContent

type CreateSrcFile =
    "src-files"
        :> Description "Creates a source file and commits it to the user repository."
        :> ReqId
        :> QueryParam' '[Required] "path" FilePath
        :> ReqBody '[PlainText] Text
        :> Post '[JSON] NoContent

type DeleteSrcFile =
    "src-files"
        :> Description "Deletes a source file and commits it to the user repository."
        :> ReqId
        :> QueryParam' '[Required] "path" FilePath
        :> Delete '[JSON] NoContent

type GetFileIndex =
    "file-index"
        :> Description "Returns the global index of filenames across all project steps."
        :> QueryParam "commit" Text
        :> Get '[JSON] [FileIndexEntry]

type DeleteProject =
    "projects"
        :> Description "Deletes a project record."
        :> ReqId
        :> Delete '[JSON] NoContent

type AssignRecord =
    "project-entities"
        :> Description "Assigns a record to a project."
        :> ReqProjectId
        :> ReqEntityId
        :> Post '[JSON] NoContent

type BatchAssignRecords =
    "project-entities"
        :> "batch"
        :> Description "Assigns multiple records to a project in one request."
        :> ReqProjectId
        :> ReqBody '[JSON] [Int]
        :> Post '[JSON] NoContent

type UnassignRecord =
    "project-entities"
        :> Description "Removes a record assignment from a project."
        :> ReqProjectId
        :> ReqEntityId
        :> Delete '[JSON] NoContent

type Autocomplete =
    "autocomplete"
        :> Description "Returns autocomplete suggestions for a field, evaluated against the user repository."
        :> QueryParam "commit" Text
        :> ReqBody '[JSON] AutocompleteRequest
        :> Post '[JSON] [Text]

type RunStep =
    "run-step"
        :> Description "Triggers a build of a step."
        :> ReqId
        :> QueryParam "commit" Text
        :> Post '[PlainText] NoContent

type StopStep =
    "stop-step"
        :> Description "Stops a running step build."
        :> ReqId
        :> QueryParam "commit" Text
        :> Post '[PlainText] NoContent

type StepLog =
    "step-log"
        :> Description "Returns the build log of a step."
        :> ReqId
        :> QueryParam "commit" Text
        :> Get '[PlainText] Text

type CreateAgentSession =
    "agent"
        :> "session"
        :> Description "Creates a new agent session."
        :> Post '[JSON] AgentSessionView

type ListAgentSessions =
    "agent"
        :> "sessions"
        :> Description "Lists all agent sessions."
        :> Get '[JSON] [AgentSessionView]

type GetAgentSession =
    "agent"
        :> "session"
        :> Description "Returns a single agent session by id."
        :> Capture "id" Text
        :> Get '[JSON] AgentSessionView

type AgentTurnEndpoint =
    "agent"
        :> "turn"
        :> Description "Starts an agent turn in an existing session."
        :> ReqBody '[JSON] TurnRequest
        :> Post '[JSON] AgentTurn

type StopTurn =
    "agent"
        :> "stop"
        :> Description "Stops the agent turn running in a session."
        :> ReqBody '[JSON] SessionRequest
        :> Post '[JSON] AgentSessionView

type PrepareApply =
    "agent"
        :> "prepare-apply"
        :> Description "Prepares an agent session's changes for review before applying."
        :> ReqBody '[JSON] SessionRequest
        :> Post '[JSON] AgentSessionView

type ConfirmApply =
    "agent"
        :> "confirm-apply"
        :> Description "Applies a prepared agent session's changes to the target branch."
        :> ReqBody '[JSON] ConfirmApplyRequest
        :> Post '[JSON] AgentApplyView

type DiscardSession =
    "agent"
        :> "discard"
        :> Description "Discards an agent session's uncommitted changes."
        :> ReqBody '[JSON] SessionRequest
        :> Post '[JSON] AgentSessionView

type ArchiveSession =
    "agent"
        :> "archive"
        :> Description "Archives an agent session."
        :> ReqBody '[JSON] SessionRequest
        :> Post '[JSON] AgentSessionView

type RenameSession =
    "agent"
        :> "rename"
        :> Description "Renames an agent session."
        :> ReqBody '[JSON] RenameSessionRequest
        :> Post '[JSON] AgentSessionView

type DeleteSession =
    "agent"
        :> "delete"
        :> Description "Permanently deletes an agent session."
        :> ReqBody '[JSON] SessionRequest
        :> Post '[JSON] NoContent

type AgentUsageEndpoint =
    "agent"
        :> "usage"
        :> Description "Returns aggregate counts of agent sessions by state."
        :> Get '[JSON] AgentUsage

type DownloadStepFile =
    "step-files"
        :> "download"
        :> Description "Downloads a single file from a step's output directory."
        :> ReqId
        :> QueryParam "commit" Text
        :> QueryParam' '[Required] "path" FilePath
        :> StreamGet NoFraming OctetStream (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (SourceT IO BS.ByteString))

type StepFileSeek =
    "step-files"
        :> "seek"
        :> Description "Returns a bounded file chunk. Specify exactly one of line or offset and a nonzero signed byte count: positive bytes read forward from the anchor; negative bytes read backward and end at the anchor."
        :> ReqId
        :> QueryParam "commit" Text
        :> QueryParam' '[Required] "path" FilePath
        :> QueryParam "line" Int
        :> QueryParam "offset" Int
        :> QueryParam' '[Required] "bytes" Int
        :> Get '[JSON] FileChunk
type RawStepFile =
    "step-files"
        :> "raw"
        :> Description "Serves the raw bytes of a file from a step's output directory."
        :> ReqId
        :> QueryParam "commit" Text
        :> CaptureAll "segments" String
        :> Raw

type BundleStepFile =
    "step-files"
        :> "bundle"
        :> Description "Serves a raw file within an immutable step-output bundle."
        :> Capture "step-id" Int
        :> Capture "commit" Text
        :> CaptureAll "segments" String
        :> Raw

type StepFileExtras =
    "step-files"
        :> "extras"
        :> Description "Returns a step's JSON extras payload."
        :> ReqId
        :> QueryParam "commit" Text
        :> QueryParam "path" FilePath
        :> Get '[RawJSON] DynamicJson

type DownloadSrcFile =
    "src-files"
        :> "download"
        :> Description "Downloads a single source file."
        :> ReqId
        :> QueryParam' '[Required] "path" FilePath
        :> StreamGet NoFraming OctetStream (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (SourceT IO BS.ByteString))

type SrcFileSeek =
    "src-files"
        :> "seek"
        :> Description "Returns a bounded source-file chunk. Specify exactly one of line or offset and a nonzero signed byte count: positive bytes read forward from the anchor; negative bytes read backward and end at the anchor."
        :> ReqId
        :> QueryParam' '[Required] "path" FilePath
        :> QueryParam "line" Int
        :> QueryParam "offset" Int
        :> QueryParam' '[Required] "bytes" Int
        :> Get '[JSON] FileChunk

type GetProjects =
    "projects"
        :> Description "Returns all project records, optionally at a specific user-repo commit."
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type CreateProject =
    "projects"
        :> Description "Creates a project record in the user repository."
        :> ReqBody '[RawJSON] DynamicJson
        :> Post '[RawJSON] DynamicJson

type UpdateProject =
    "projects"
        :> Description "Updates an existing project record."
        :> ReqId
        :> ReqBody '[RawJSON] DynamicJson
        :> Patch '[JSON] NoContent

type StepStatusStream =
    "step-status-stream"
        :> Description "Streams snapshot and heartbeat SSE events for a project's steps. Emits status-error and closes when status evaluation fails."
        :> ReqProjectId
        :> QueryParam "commit" Text
        :> StreamGet NoFraming EventStream (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceT IO BS.ByteString))

type GetStepConfig =
    "step-config"
        :> Description "Returns the step configuration defined in the user repository."
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type GetPresets =
    "presets"
        :> Description "Returns the field presets defined in the user repository."
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type UpdateStep =
    "step"
        :> Description "Updates an existing step record."
        :> ReqId
        :> ReqBody '[RawJSON] DynamicJson
        :> Patch '[JSON] NoContent

type CreateStep =
    "step"
        :> Description "Creates a step, optionally seeded from a source step."
        :> QueryParam "project_id" Int
        :> QueryParam "source_id" Int
        :> ReqBody '[RawJSON] DynamicJson
        :> Post '[RawJSON] DynamicJson

type GetNotices =
    "notices"
        :> Description "Returns evaluation notices (warnings and errors) for a step."
        :> ReqId
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type Upload =
    "upload"
        :> Description "Uploads files into a step's source directory."
        :> ReqId
        :> MultipartForm Tmp (MultipartData Tmp)
        :> Post '[PlainText] Text

type ClusterStatusStream =
    "cluster-status-stream"
        :> Description "Streams the current SLURM cluster availability and subsequent status changes."
        :> StreamGet NoFraming EventStream (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceT IO BS.ByteString))

type AgentTurnStream =
    "agent"
        :> "turn"
        :> Capture "id" Text
        :> "stream"
        :> Description "Streams output of an agent turn as server-sent events."
        :> StreamGet NoFraming EventStream (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceT IO BS.ByteString))

-- | The full served API, in handler order (matches @Main.server@).
type API =
    GetCommitHash
        :<|> GetUserRepoInfo
        :<|> ListStepFiles
        :<|> DownloadStepFile
        :<|> StepFileSeek
        :<|> RawStepFile
        :<|> BundleStepFile
        :<|> StepFileExtras
        :<|> ListSrcFiles
        :<|> DownloadSrcFile
        :<|> SrcFileSeek
        :<|> UpdateSrcFile
        :<|> CreateSrcFile
        :<|> DeleteSrcFile
        :<|> GetFileIndex
        :<|> GetProjects
        :<|> CreateProject
        :<|> UpdateProject
        :<|> DeleteProject
        :<|> AssignRecord
        :<|> BatchAssignRecords
        :<|> UnassignRecord
        :<|> StepStatusStream
        :<|> GetStepConfig
        :<|> GetPresets
        :<|> Autocomplete
        :<|> UpdateStep
        :<|> CreateStep
        :<|> GetNotices
        :<|> RunStep
        :<|> StopStep
        :<|> StepLog
        :<|> Upload
        :<|> ClusterStatusStream
        :<|> CreateAgentSession
        :<|> ListAgentSessions
        :<|> GetAgentSession
        :<|> AgentTurnEndpoint
        :<|> StopTurn
        :<|> AgentTurnStream
        :<|> PrepareApply
        :<|> ConfirmApply
        :<|> DiscardSession
        :<|> ArchiveSession
        :<|> RenameSession
        :<|> DeleteSession
        :<|> AgentUsageEndpoint
