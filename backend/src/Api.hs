{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (API) where

import Agent.Git (AgentSessionView, AgentUsage)
import Agent.Session (AgentSessionSummary, AgentTurn)
import ApiTypes (DynamicJson, RawJSON)
import qualified Data.ByteString as BS
import Data.Map (Map)
import Data.Text (Text)
import Handlers.Agent (ApplyRequest, AutoApplyRequest, RenameSessionRequest, SessionRequest, TurnRequest)
import Handlers.Autocomplete (AutocompleteRequest)
import ProjectTree (ProjectFields, TreeOp)
import Handlers.Scratch (ScratchListing, ScratchRootResponse, ScratchWrapRequest)
import Handlers.SrcFiles (UserRepoInfo)
import Handlers.StatusStream (EventStream)
import Handlers.StepReview (ReviewRequest, StepReviewReport)
import Handlers.Store (DirEntry, FileChunk)
import Servant
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Servant.Types.SourceT (SourceT)

type ReqId = QueryParam' '[Required, Strict] "id" Int

type ReqProjectId = QueryParam' '[Required, Strict] "project_id" Int

type AcceptedText = Verb 'POST 202 '[PlainText] Text

type SseStream =
    StreamGet NoFraming EventStream (Headers '[Header "Cache-Control" Text, Header "X-Accel-Buffering" Text] (SourceT IO BS.ByteString))

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
        :> Description "Lists source files available to a step, optionally at a specific user-repo commit."
        :> ReqId
        :> QueryParam "commit" Text
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

type JobEnded =
    "job-ended"
        :> Description "Called by the Slurm job-completion hook when a job ends; wakes the build watchers waiting on that job name."
        :> QueryParam' '[Required, Strict] "name" String
        :> Post '[PlainText] NoContent

type CreateAgentSession =
    "agent"
        :> "session"
        :> Description "Creates a new agent session."
        :> Post '[JSON] AgentSessionView

type ListAgentSessions =
    "agent"
        :> "sessions"
        :> Description "Lists all agent sessions."
        :> Get '[JSON] [AgentSessionSummary]

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

type SteerTurn =
    "agent"
        :> "steer"
        :> Description "Sends steering input to the agent turn running in a session."
        :> ReqBody '[JSON] TurnRequest
        :> PostNoContent

type ApplyChanges =
    "agent"
        :> "apply"
        :> Description "Applies the agent's changes that are not applied yet. When the apply is refused, a turn starts that fixes the changes."
        :> ReqBody '[JSON] ApplyRequest
        :> Post '[JSON] AgentSessionView

type SetAutoApply =
    "agent"
        :> "auto-apply"
        :> Description "Sets whether the changes of the running agent turns that this client started are applied when they end."
        :> ReqBody '[JSON] AutoApplyRequest
        :> PostNoContent

type DiscardSession =
    "agent"
        :> "discard"
        :> Description "Discards the agent's changes that are not applied yet."
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
        :> QueryParam "commit" Text
        :> QueryParam' '[Required] "path" FilePath
        :> StreamGet NoFraming OctetStream (Headers '[Header "Content-Disposition" Text, Header "Content-Length" Integer] (SourceT IO BS.ByteString))

type SrcFileSeek =
    "src-files"
        :> "seek"
        :> Description "Returns a bounded source-file chunk. Specify exactly one of line or offset and a nonzero signed byte count: positive bytes read forward from the anchor; negative bytes read backward and end at the anchor."
        :> ReqId
        :> QueryParam "commit" Text
        :> QueryParam' '[Required] "path" FilePath
        :> QueryParam "line" Int
        :> QueryParam "offset" Int
        :> QueryParam' '[Required] "bytes" Int
        :> Get '[JSON] FileChunk

type RawSrcFile =
    "src-files"
        :> "raw"
        :> Description "Serves the raw bytes of a source file (inline, no download disposition) for preview rendering."
        :> ReqId
        :> QueryParam "commit" Text
        :> QueryParam' '[Required] "path" FilePath
        :> Raw

type GetProjects =
    "projects"
        :> Description "Returns every project keyed by id, including the root project 0, optionally at a specific user-repo commit. Each project lists its steps and subprojects in children; revisions written in the former steps format are converted to this shape."
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type GetUnfiled =
    "unfiled"
        :> Description "Returns steps not linked from any project, with evaluated definitions, optionally at a specific user-repo commit. Unreadable raw project links fail the request. Step definitions carry lastModifiedAt and createdAt."
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type GetProjectRollup =
    "project-rollup"
        :> Description "Returns, for every direct child project of a project in its effective order, the number of steps reachable below it and the counts of their statuses, recursively and counting each step once per child project, optionally at a specific user-repo commit."
        :> ReqProjectId
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type CreateProject =
    "projects"
        :> Description "Creates a project from its name and preset or templates, and appends it to the children of the parent project (the root project 0 by default) in one commit. Returns the evaluated project."
        :> QueryParam "parent_id" Int
        :> ReqBody '[JSON] ProjectFields
        :> Post '[RawJSON] DynamicJson

type UpdateProject =
    "projects"
        :> Description "Replaces a project's name and preset or templates; its children are left unchanged."
        :> ReqId
        :> ReqBody '[JSON] ProjectFields
        :> Patch '[JSON] NoContent

type BatchProjectOps =
    "projects"
        :> "batch"
        :> Description "Applies an ordered list of project tree operations (update, link, unlink, order, hide, delete) validated as a whole and written in one commit. Refuses to delete the root project 0, reviewed steps, or steps that remaining steps depend on. Validation failures return 409 with a human message; an empty list returns 400."
        :> ReqBody '[JSON] [TreeOp]
        :> Post '[JSON] NoContent

type StepStatusStream =
    "step-status-stream"
        :> Description "Streams snapshot and heartbeat SSE events for every project's steps."
        :> SseStream

type ProjectStatus =
    "project-status"
        :> Description "Re-evaluates a project's step statuses and broadcasts them on the step status stream."
        :> ReqProjectId
        :> QueryParam "commit" Text
        :> Post '[PlainText] NoContent

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

type GetProjectReview =
    "project-review"
        :> Description "Reports how every step in a project at one revision compares with its reviewed revision, together with that reviewed revision."
        :> ReqProjectId
        :> QueryParam "commit" Text
        :> Get '[JSON] (Map String StepReviewReport)

type ReviewStep =
    "step-review"
        :> Description "Records the viewed revision as a step's reviewed revision, together with who reviewed it and their comments, when there is no review or its output is unchanged. Returns whether the viewed output differs from the reviewed one."
        :> ReqId
        :> QueryParam "commit" Text
        :> ReqBody '[JSON] ReviewRequest
        :> Post '[JSON] Bool

type RemoveReview =
    "step-review"
        :> Description "Removes a step's review."
        :> ReqId
        :> Delete '[JSON] NoContent

type ReviewDiff =
    "step-review-diff"
        :> Description "Serves the diffoscope comparison of a step's reviewed output and the viewed revision's output (HEAD by default)."
        :> ReqId
        :> QueryParam "commit" Text
        :> Raw

type GetNotices =
    "notices"
        :> Description "Returns evaluation notices (warnings and errors) for a step."
        :> ReqId
        :> QueryParam "commit" Text
        :> Get '[RawJSON] DynamicJson

type Upload =
    "upload"
        :> Description "Stages files uploaded into a step's source directory and starts an ingest job."
        :> ReqId
        :> MultipartForm Tmp (MultipartData Tmp)
        :> AcceptedText

type GetScratchRoot =
    "scratch"
        :> Description "Returns the configured scratch directory root, or null when scratch is not configured."
        :> Get '[JSON] ScratchRootResponse

type ListScratch =
    "scratch"
        :> "list"
        :> Description "Lists the entries of a directory inside the scratch root, directories first."
        :> QueryParam "path" FilePath
        :> Get '[JSON] ScratchListing

type WrapScratch =
    "scratch"
        :> "wrap"
        :> Description "Starts an ingest job that wraps a directory inside the scratch root into a step."
        :> ReqId
        :> ReqBody '[JSON] ScratchWrapRequest
        :> AcceptedText

type IngestStream =
    "ingest-stream"
        :> Description "Streams ingest job snapshots and progress updates as server-sent events."
        :> SseStream

type ClusterStatusStream =
    "cluster-status-stream"
        :> Description "Streams the current SLURM cluster availability, the reason for any degradation, and subsequent status changes."
        :> SseStream

type AgentTurnStream =
    "agent"
        :> "turn"
        :> Capture "id" Text
        :> "stream"
        :> Description "Streams output of an agent turn as server-sent events."
        :> SseStream

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
        :<|> RawSrcFile
        :<|> UpdateSrcFile
        :<|> CreateSrcFile
        :<|> DeleteSrcFile
        :<|> GetProjects
        :<|> GetUnfiled
        :<|> GetProjectRollup
        :<|> CreateProject
        :<|> UpdateProject
        :<|> BatchProjectOps
        :<|> StepStatusStream
        :<|> ProjectStatus
        :<|> GetStepConfig
        :<|> GetPresets
        :<|> Autocomplete
        :<|> UpdateStep
        :<|> CreateStep
        :<|> GetProjectReview
        :<|> ReviewStep
        :<|> RemoveReview
        :<|> ReviewDiff
        :<|> GetNotices
        :<|> RunStep
        :<|> StopStep
        :<|> StepLog
        :<|> JobEnded
        :<|> Upload
        :<|> GetScratchRoot
        :<|> ListScratch
        :<|> WrapScratch
        :<|> IngestStream
        :<|> ClusterStatusStream
        :<|> CreateAgentSession
        :<|> ListAgentSessions
        :<|> GetAgentSession
        :<|> AgentTurnEndpoint
        :<|> StopTurn
        :<|> SteerTurn
        :<|> AgentTurnStream
        :<|> ApplyChanges
        :<|> SetAutoApply
        :<|> DiscardSession
        :<|> ArchiveSession
        :<|> RenameSession
        :<|> DeleteSession
        :<|> AgentUsageEndpoint
