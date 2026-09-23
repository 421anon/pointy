module Model.Core exposing (..)

import Api.ApiData as ApiData exposing (ApiData(..))
import Array
import Browser.Navigation
import Components.Select exposing (SelectState, initSelectState)
import Csv.Parser
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import DnDList
import Flow exposing (Flow)
import Grid
import Json.Decode as Decode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Shadow exposing (Presets, StepArgValue, StepConfig, StepType)
import Route exposing (Route)
import Set exposing (Set)
import String.Extra
import Time
import Toast exposing (Toast)


type Status
    = StatusNotStarted
    | StatusRunning
    | StatusSuccess
    | StatusFailure (Maybe String)


type ClusterStatus
    = ClusterAvailable
    | ClusterDegraded
    | ClusterUnavailable
    | ClusterUnknown


type AddMode
    = AddNew
    | AddFromOtherProject


type TemplateSource
    = FromPreset String
    | CustomTemplates (List String)


type alias BaseRecord a =
    { a
        | id : Maybe Int
        , clientId : Maybe Int
        , hidden : Bool
        , sortKey : Maybe Int
        , name : String
        , isUpdating : Bool
        , lastModifiedAt : Maybe Time.Posix
    }


type alias StepRunState =
    { commit : String
    , status : ApiData Status
    , directoryView : DirectoryFolder
    }


type ReviewComparison
    = SameOutPath
    | SameContent
    | DifferentContent
    | ViewedOutputUnbuilt
    | ReviewedOutputUnbuilt


type alias Review =
    { revision : String
    , reviewedBy : String
    , comments : String
    , comparison : ApiData ReviewComparison
    }


type alias ReviewReport =
    { review : Maybe Review
    , reviewedStatus : Maybe Status
    }


type alias ReviewDraft =
    { stepId : Int
    , reviewedBy : String
    , comments : String
    }


type alias SrcFileDraft =
    { name : String
    , content : String
    }


type alias StepRecord =
    BaseRecord
        { type_ : String
        , note : String
        , runState : ApiData StepRunState
        , review : Maybe Review
        , args : Dict String StepArgValue
        , srcFiles : DirectoryFolder
        , srcFileDraft : Maybe SrcFileDraft
        , srcFileWriting : Bool
        }


type NoticeSeverity
    = Info


type alias Notice =
    { field : Maybe String
    , severity : NoticeSeverity
    , message : String
    }


type alias ProjectRecord =
    BaseRecord
        { tables : Dict String (Table StepRecord)
        , templateSource : TemplateSource
        , orphanedSteps : List StepRecord
        , validationErrors : List String
        , hideOrphans : Bool
        , presetSelect : SelectState
        , templatesSelect : SelectState
        }


type alias Table a =
    { records : ApiData (List a)
    , isOpen : Bool
    , showHiddenRecords : Bool
    , edited : Maybe a
    , drafts : Dict Int a
    , newDraft : Maybe a
    , addMode : AddMode
    , nameEditOnly : Bool
    , dnd : DnDList.Model
    , dndAffected : List Int
    , selectExistingSteps : SelectState
    , argSelectStates : Dict String SelectState
    , isUpdating : Bool
    }


type TableTag
    = TagProjects
    | TagSteps String StepType


type alias ModalConfirmConfig =
    { id : String
    , title : String
    , subtitle : Maybe String
    , bodyLines : List String
    , onConfirm : Flow Model ()
    }


initModalConfirmConfig : ModalConfirmConfig
initModalConfirmConfig =
    { id = "modal-confirm"
    , title = ""
    , subtitle = Nothing
    , bodyLines = []
    , onConfirm = Flow.pure ()
    }


type alias UploadProgress =
    { sent : Int
    , size : Int
    }


type alias UserRepoInfo =
    { url : String
    , branch : String
    }


type alias AgentPreparedApply =
    { targetHead : String
    , agentHead : String
    , candidateHead : String
    , candidateWorktree : String
    }





type alias SessionTimestamp =
    { posix : Time.Posix
    , nanos : Int
    }


sessionTimestampAtLeast : SessionTimestamp -> SessionTimestamp -> Bool
sessionTimestampAtLeast a b =
    Time.posixToMillis a.posix
        > Time.posixToMillis b.posix
        || (Time.posixToMillis a.posix == Time.posixToMillis b.posix && a.nanos >= b.nanos)


type alias AgentSession =
    { sessionId : String
    , sessionName : Maybe String
    , targetBranch : String
    , agentBranch : String
    , baseCommit : String
    , worktreePath : String
    , status : String
    , preparedApply : Maybe AgentPreparedApply
    , activeTurnId : Maybe String
    , lastError : Maybe String
    , updatedAt : SessionTimestamp
    }


type alias AgentGitState =
    { headCommit : String
    , commitLog : String
    , branchDiff : String
    , hasAgentCommits : Bool
    }


type alias AgentTurn =
    { turnId : String
    , turnSessionId : String
    , turnPrompt : String
    , turnStatus : String
    , turnExitCode : Maybe Int
    , turnLogPath : String
    , turnLog : String
    }


type alias AgentSessionView =
    { session : AgentSession
    , gitState : AgentGitState
    , turns : List AgentTurn
    }


type alias AgentSessionSummary =
    { session : AgentSession
    , title : String
    , turnCount : Int
    , hasCommits : Bool
    }


summaryFromView : AgentSessionView -> AgentSessionSummary
summaryFromView view =
    { session = view.session
    , title = chatTitle view.session view.turns
    , turnCount = List.length view.turns
    , hasCommits = view.gitState.hasAgentCommits
    }


chatTitle : AgentSession -> List AgentTurn -> String
chatTitle session turns_ =
    session.sessionName
        |> Maybe.andThen normalizeChatName
        |> Maybe.orElse (firstPrompt turns_)
        |> Maybe.withDefault ""


firstPrompt : List AgentTurn -> Maybe String
firstPrompt =
    List.filterMap (.turnPrompt >> normalizeChatName) >> List.head


displayName : AgentSessionSummary -> String
displayName =
    .title >> normalizeChatName >> Maybe.withDefault "New chat"


isSession : String -> AgentSessionSummary -> Bool
isSession sessionId =
    .session >> .sessionId >> (==) sessionId


sessionSummary : String -> AgentState -> Maybe AgentSessionSummary
sessionSummary sessionId =
    .sessions >> ApiData.toMaybe >> Maybe.andThen (List.find (isSession sessionId))


normalizeChatName : String -> Maybe String
normalizeChatName =
    String.words >> String.join " " >> String.left chatNameMaxLength >> String.Extra.nonEmpty


chatNameMaxLength : Int
chatNameMaxLength =
    80


type alias AgentApplyView =
    { sessionView : AgentSessionView
    , invalidatedProjectIds : List Int
    , invalidatedStepIds : List Int
    }


type ChatTurnStatus
    = ChatPending
    | ChatDone
    | ChatStopped
    | ChatFailed String


type alias ChatTurn =
    { turnId : String
    , prompt : String
    , assistant : String
    , status : ChatTurnStatus
    }


type ChatChangesetState
    = ChatChangesetProposed
    | ChatChangesetNeedsReview String
    | ChatChangesetApplied
    | ChatChangesetDiscarded


type alias ChatChangeset =
    { state : ChatChangesetState
    , description : String
    , diff : String
    }


type ChatEntry
    = ChatTurnEntry ChatTurn
    | ChatChangesetEntry ChatChangeset


type ChangesetOperationKind
    = ApplyingChangeset
    | DiscardingChangeset


type alias ChangesetOperation =
    { sessionId : String
    , kind : ChangesetOperationKind
    }


type alias AgentSessionNameEdit =
    { sessionId : String
    , value : String
    , saving : Bool
    }


type AgentRequest
    = CreatingAgentSession
    | SendingAgentPrompt String
    | SteeringAgentTurn String
    | ArchivingAgentSession String
    | DeletingAgentSession String
    | StoppingAgentTurn String


type alias PendingQuestion =
    { multi : Bool
    , options : List String
    , picked : Set Int
    }


questionAnswerLabel : PendingQuestion -> List Int -> String
questionAnswerLabel question numbers =
    numbers
        |> List.filterMap (\number -> List.getAt (number - 1) question.options)
        |> String.join ", "


keepPicksForSameQuestion : Maybe PendingQuestion -> Maybe PendingQuestion -> Maybe PendingQuestion
keepPicksForSameQuestion previous next =
    case ( previous, next ) of
        ( Just before, Just after ) ->
            if ( before.multi, before.options ) == ( after.multi, after.options ) then
                Just { after | picked = before.picked }

            else
                next

        _ ->
            next


type alias AgentLiveTurn =
    { turnId : String
    , finished : Bool
    , entries : List ChatEntry
    , chunkBuffer : String
    , pendingQuestion : Maybe PendingQuestion
    , streamError : Maybe String
    , pendingSteer : Maybe String
    }


liveTurnFor : String -> AgentSessionView -> AgentLiveTurn
liveTurnFor turnId view =
    { turnId = turnId
    , finished = False
    , entries = persistedTranscript view
    , chunkBuffer = ""
    , pendingQuestion = persistedQuestion view
    , streamError = Nothing
    , pendingSteer = Nothing
    }


liveTurnSurvives : Maybe String -> AgentLiveTurn -> Bool
liveTurnSurvives activeTurnId live =
    String.isEmpty live.turnId || Just live.turnId == activeTurnId


type alias AgentState =
    { sessions : ApiData (List AgentSessionSummary)
    , sessionViews : Dict String (ApiData AgentSessionView)
    , liveTurns : Dict String AgentLiveTurn
    , selectedSessionId : Maybe String
    , isPanelOpen : Bool
    , isSessionListOpen : Bool
    , isFocusMode : Bool
    , showArchived : Bool
    , changesetOperation : Maybe ChangesetOperation
    , request : Maybe AgentRequest
    , sessionNameEdit : Maybe AgentSessionNameEdit
    , sessionRenames : Dict String ( String, SessionTimestamp )
    , highlightTurnId : Maybe String
    , lastChat : Maybe String
    , isRestoringChat : Bool
    }


initAgentState : AgentState
initAgentState =
    { sessions = NotAsked
    , sessionViews = Dict.empty
    , liveTurns = Dict.empty
    , selectedSessionId = Nothing
    , isPanelOpen = False
    , isSessionListOpen = False
    , isFocusMode = False
    , showArchived = False
    , changesetOperation = Nothing
    , request = Nothing
    , sessionNameEdit = Nothing
    , sessionRenames = Dict.empty
    , highlightTurnId = Nothing
    , lastChat = Nothing
    , isRestoringChat = False
    }


agentMutationPending : AgentState -> Bool
agentMutationPending agentState =
    (agentState.request /= Nothing)
        || (agentState.changesetOperation /= Nothing)
        || (agentState.sessionNameEdit
                |> Maybe.map .saving
                |> Maybe.withDefault False
           )


failLatestPendingChatTurn : String -> List ChatEntry -> List ChatEntry
failLatestPendingChatTurn error =
    mapLastChatTurn (finishPending (ChatFailed error))


ingestLiveChunk : String -> AgentLiveTurn -> AgentLiveTurn
ingestLiveChunk chunk live =
    let
        combined =
            live.chunkBuffer ++ chunk

        ( completeBlock, remainder ) =
            splitOnLastNewline combined

        rawLines =
            if String.isEmpty completeBlock then
                []

            else
                String.split "\n" completeBlock

        keptLines =
            List.filter (not << String.isEmpty) rawLines
    in
    { live
        | chunkBuffer = remainder
        , entries = List.foldl appendChatLine live.entries keptLines
        , pendingQuestion = List.foldl pendingQuestionAfterLine live.pendingQuestion keptLines
        , pendingSteer = List.foldl pendingSteerAfterLine live.pendingSteer keptLines
        , streamError = Nothing
    }


persistedTranscript : AgentSessionView -> List ChatEntry
persistedTranscript view =
    List.foldl appendPersistedTurn [] (replayedTurns view)


persistedQuestion : AgentSessionView -> Maybe PendingQuestion
persistedQuestion =
    List.foldl pendingQuestionAfterLine Nothing << replayedTurnLogLines


replayedTurnLogLines : AgentSessionView -> List String
replayedTurnLogLines =
    List.concatMap turnLogLines << replayedTurns


replayedTurns : AgentSessionView -> List AgentTurn
replayedTurns view =
    List.map (withoutLiveStreamedLog view) view.turns


withoutLiveStreamedLog : AgentSessionView -> AgentTurn -> AgentTurn
withoutLiveStreamedLog view turn =
    if Just turn.turnId == view.session.activeTurnId then
        { turn | turnLog = "" }

    else
        turn


turnLogLines : AgentTurn -> List String
turnLogLines =
    List.filter (not << String.isEmpty) << String.split "\n" << .turnLog


isChangesetLifecycleTurn : AgentTurn -> Bool
isChangesetLifecycleTurn turn =
    turn.turnPrompt == "Apply proposed changeset" || turn.turnPrompt == "Discard proposed changeset"


appendPersistedTurn : AgentTurn -> List ChatEntry -> List ChatEntry
appendPersistedTurn turn entries =
    if isChangesetLifecycleTurn turn then
        entries ++ [ ChatChangesetEntry (changesetFromLifecycleTurn turn) ]

    else
        let
            prompt =
                if String.isEmpty (String.trim turn.turnPrompt) then
                    "Prompt unavailable"

                else
                    turn.turnPrompt

            seeded =
                entries ++ [ ChatTurnEntry { turnId = turn.turnId, prompt = prompt, assistant = "", status = chatStatusFromTurn turn } ]

            logLines =
                turnLogLines turn

            replayed =
                List.foldl appendChatLine seeded logLines
        in
        if turn.turnStatus == "running" then
            replayed

        else
            mapLastChatTurn (finishPending (chatStatusFromTurn turn)) replayed


changesetFromLifecycleTurn : AgentTurn -> ChatChangeset
changesetFromLifecycleTurn turn =
    let
        state =
            if turn.turnPrompt == "Discard proposed changeset" then
                ChatChangesetDiscarded

            else
                ChatChangesetApplied

        ( description, diff ) =
            parseChangesetLog turn.turnLog
    in
    { state = state
    , description =
        if String.isEmpty description then
            defaultChangesetDescription state

        else
            description
    , diff = diff
    }


defaultChangesetDescription : ChatChangesetState -> String
defaultChangesetDescription state =
    case state of
        ChatChangesetProposed ->
            "Review this changeset, then apply it to the target branch or discard it."

        ChatChangesetNeedsReview _ ->
            "This changeset could not be prepared cleanly. Resolve the issue by continuing the conversation, or discard the changeset."

        ChatChangesetApplied ->
            "This changeset was applied. You can continue the conversation from the applied state."

        ChatChangesetDiscarded ->
            "This changeset was discarded. No changes were applied."


parseChangesetLog : String -> ( String, String )
parseChangesetLog logText =
    let
        ( descriptionLines, diffLines ) =
            splitChangesetDiffMarker (String.split "\n" logText)

        description =
            descriptionLines
                |> List.map changesetLogLineBody
                |> List.filter (not << String.isEmpty)
                |> String.join "\n"
                |> String.trim
    in
    ( description, String.trimRight (String.join "\n" diffLines) )


changesetDiffMarker : String
changesetDiffMarker =
    "[system] changeset-diff"


splitChangesetDiffMarker : List String -> ( List String, List String )
splitChangesetDiffMarker lines =
    case lines of
        [] ->
            ( [], [] )

        line :: rest ->
            if line == changesetDiffMarker then
                ( [], rest )

            else
                let
                    ( before, after ) =
                        splitChangesetDiffMarker rest
                in
                ( line :: before, after )


changesetLogLineBody : String -> String
changesetLogLineBody line =
    let
        ( prefix, body ) =
            splitLogPrefix line
    in
    case prefix of
        "stdout" ->
            body

        "stderr" ->
            body

        "system" ->
            ""

        _ ->
            line


chatStatusFromTurn : AgentTurn -> ChatTurnStatus
chatStatusFromTurn turn =
    case turn.turnStatus of
        "running" ->
            ChatPending

        "failed" ->
            ChatFailed (turnFailureMessage turn)

        "stopped" ->
            ChatStopped

        _ ->
            ChatDone


turnFailureMessage : AgentTurn -> String
turnFailureMessage turn =
    case turn.turnExitCode of
        Just code ->
            "exit code " ++ String.fromInt code

        Nothing ->
            "agent failed"


appendChatLine : String -> List ChatEntry -> List ChatEntry
appendChatLine rawLine entries =
    let
        ( prefix, body ) =
            splitLogPrefix rawLine
    in
    case prefix of
        "stdout" ->
            appendAssistantLine body entries

        "stderr" ->
            appendAssistantLine body entries

        "steering" ->
            case Decode.decodeString Decode.string (String.trim body) of
                Ok prompt ->
                    mapLastChatTurn (finishPending ChatDone) entries
                        ++ [ ChatTurnEntry { turnId = "", prompt = prompt, assistant = "", status = ChatPending } ]

                Err _ ->
                    entries

        "runner" ->
            entries

        _ ->
            entries


splitLogPrefix : String -> ( String, String )
splitLogPrefix line =
    if String.startsWith "[stdout] " line then
        ( "stdout", String.dropLeft 9 line )

    else if String.startsWith "[stderr] " line then
        ( "stderr", String.dropLeft 9 line )

    else if String.startsWith "[runner] " line then
        ( "runner", String.dropLeft 9 line )

    else if String.startsWith "[steering] " line then
        ( "steering", String.dropLeft 11 line )

    else if String.startsWith "[question] " line then
        ( "question", String.dropLeft 11 line )

    else if String.startsWith "[system] " line then
        ( "system", String.dropLeft 9 line )

    else
        ( "unknown", line )


appendToCurrentAssistant : String -> List ChatEntry -> List ChatEntry
appendToCurrentAssistant body =
    mapLastChatTurn
        (\last ->
            { last
                | assistant =
                    if String.isEmpty last.assistant then
                        body

                    else
                        last.assistant ++ "\n" ++ body
            }
        )


appendAssistantLine : String -> List ChatEntry -> List ChatEntry
appendAssistantLine body entries =
    if List.member (String.trim body) retiredNarrationMarkers then
        entries

    else
        appendToCurrentAssistant body entries


retiredNarrationMarkers : List String
retiredNarrationMarkers =
    [ "*Loading the project context*"
    , "*Thinking*"
    ]


pendingQuestionAfterLine : String -> Maybe PendingQuestion -> Maybe PendingQuestion
pendingQuestionAfterLine rawLine pending =
    case splitLogPrefix rawLine of
        ( "question", body ) ->
            Decode.decodeString pendingQuestionDecoder (String.trim body)
                |> Result.toMaybe
                |> keepPicksForSameQuestion pending

        ( "steering", _ ) ->
            Nothing

        ( "system", body ) ->
            if isTurnFinishedLine body then
                Nothing

            else
                pending

        _ ->
            pending


pendingSteerAfterLine : String -> Maybe String -> Maybe String
pendingSteerAfterLine rawLine pending =
    case splitLogPrefix rawLine of
        ( "steering", _ ) ->
            Nothing

        ( "system", body ) ->
            if isTurnFinishedLine body then
                Nothing

            else
                pending

        _ ->
            pending


pendingQuestionDecoder : Decode.Decoder PendingQuestion
pendingQuestionDecoder =
    Decode.map3 PendingQuestion
        (Decode.field "multi" Decode.bool)
        (Decode.field "options" (Decode.list Decode.string))
        (Decode.succeed Set.empty)


isTurnFinishedLine : String -> Bool
isTurnFinishedLine =
    String.startsWith "Agent turn finished with exit code "


splitOnLastNewline : String -> ( String, String )
splitOnLastNewline text =
    case String.indexes "\n" text |> List.reverse |> List.head of
        Just idx ->
            ( String.left idx text, String.dropLeft (idx + 1) text )

        Nothing ->
            ( "", text )


mapLastChatTurn : (ChatTurn -> ChatTurn) -> List ChatEntry -> List ChatEntry
mapLastChatTurn update entries =
    case List.reverse entries of
        (ChatTurnEntry last) :: rest ->
            List.reverse (ChatTurnEntry (update last) :: rest)

        (ChatChangesetEntry _) :: _ ->
            entries

        [] ->
            entries


finishPending : ChatTurnStatus -> ChatTurn -> ChatTurn
finishPending status turn =
    if turn.status == ChatPending then
        { turn | status = status }

    else
        turn


agentSessionArchived : String -> Bool
agentSessionArchived status =
    status == "archived" || status == "discarded" || status == "applied"


selectedSessionSummary : AgentState -> Maybe AgentSessionSummary
selectedSessionSummary agentState =
    agentState.selectedSessionId |> Maybe.andThen (\sessionId -> sessionSummary sessionId agentState)


selectedSessionView : AgentState -> Maybe AgentSessionView
selectedSessionView =
    selectedSessionViewData >> Maybe.andThen ApiData.toMaybe


selectedSessionViewData : AgentState -> Maybe (ApiData AgentSessionView)
selectedSessionViewData agentState =
    agentState.selectedSessionId |> Maybe.andThen (\sessionId -> Dict.get sessionId agentState.sessionViews)


type alias AutocompleteJob =
    { fieldKey : String
    , commit : Maybe String
    , template : String
    , autocomplete : String
    , context : Dict String String
    , query : String
    , limit : Int
    }


type alias AutocompleteState =
    { query : String
    , suggestions : ApiData (List String)
    , activeIndex : Int
    , activeRequest : Maybe AutocompleteJob
    }


initAutocompleteState : AutocompleteState
initAutocompleteState =
    { query = "", suggestions = NotAsked, activeIndex = 0, activeRequest = Nothing }


type Model
    = Model
        { projects : Table ProjectRecord
        , route : Route
        , origin : String
        , key : Browser.Navigation.Key
        , toasts : List Toast
        , nextToastId : Int
        , nextClientId : Int
        , modalConfirm : ModalConfirmConfig
        , downstreamEntities : Dict Int (List Int)
        , searchBox : SelectState
        , stepConfig : ApiData StepConfig
        , presets : ApiData Presets
        , commitHash : ApiData String
        , userRepoInfo : ApiData UserRepoInfo
        , uploadProgress : Dict Int UploadProgress
        , stepLogs : Dict String (ApiData String)
        , notices : Dict String (ApiData (List Notice))
        , stepStatusHooks : Dict Int (Flow Model ())
        , stepStatusBuffer : Dict Int ( String, Status )
        , pendingBuilds : Dict Int String
        , openDiff : Maybe ( Int, Float )
        , reviewDraft : Maybe ReviewDraft
        , autocomplete : Dict String AutocompleteState
        , autocompleteDebounce : Debounce AutocompleteJob
        , gutterDrag : Maybe GutterDrag
        , compareState : CompareState
        , now : Time.Posix
        , agent : AgentState
        , clusterStatus : ClusterStatus
        , runningStepIds : List Int
        , statusBarOpen : Bool
        }


type alias GutterDrag =
    { target : Route.HighlightTarget
    , recordId : Int
    , path : List String
    , anchor : Int
    , current : Int
    , moved : Bool
    , clearOnClick : Bool
    }


type CompareState
    = CompareIdle
    | CompareSelecting CompareSelection
    | CompareActive CompareActiveData


type alias CompareActiveData =
    { left : CompareSelection
    , right : CompareSelection
    , leftContent : ApiData CompareFile
    , rightContent : ApiData CompareFile
    , leftInspect : Bool
    , rightInspect : Bool
    }


type alias CompareFile =
    { text : String
    , delimitedGrid : Maybe DelimitedGrid
    }


type alias CompareSelection =
    { projectId : Int
    , recordId : Int
    , path : List String
    , fileName : String
    , mimeType : Maybe String
    , source : CompareSource
    }


type CompareSource
    = FromOutput String
    | FromSrc (Maybe String)


type CompareMode
    = CompareImage
    | CompareHtml
    | CompareText


compareSelectionMode : CompareSelection -> CompareMode
compareSelectionMode sel =
    let
        mime =
            Maybe.withDefault "" sel.mimeType

        extension =
            String.toLower sel.fileName
                |> String.split "."
                |> List.last
                |> Maybe.withDefault ""
    in
    if String.startsWith "image/" mime then
        CompareImage

    else if mime == "text/html" || extension == "html" || extension == "htm" then
        CompareHtml

    else
        CompareText


getProjects : Model -> Table ProjectRecord
getProjects (Model model) =
    model.projects


getRoute : Model -> Route
getRoute (Model model) =
    model.route


getClusterStatus : Model -> ClusterStatus
getClusterStatus (Model model) =
    model.clusterStatus


getRunningStepIds : Model -> List Int
getRunningStepIds (Model model) =
    model.runningStepIds


getStatusBarOpen : Model -> Bool
getStatusBarOpen (Model model) =
    model.statusBarOpen


getOrigin : Model -> String
getOrigin (Model model) =
    model.origin


getKey : Model -> Browser.Navigation.Key
getKey (Model model) =
    model.key


getToasts : Model -> List Toast
getToasts (Model model) =
    model.toasts


getNextToastId : Model -> Int
getNextToastId (Model model) =
    model.nextToastId


getNextClientId : Model -> Int
getNextClientId (Model model) =
    model.nextClientId


getStepConfig : Model -> ApiData StepConfig
getStepConfig (Model model) =
    model.stepConfig


getPresets : Model -> ApiData Presets
getPresets (Model model) =
    model.presets


effectiveTemplates : Presets -> TemplateSource -> List String
effectiveTemplates presets source =
    case source of
        FromPreset name ->
            Dict.get name presets |> Maybe.unwrap [] .templates

        CustomTemplates templates ->
            templates


defaultTemplateSource : Presets -> TemplateSource
defaultTemplateSource presets =
    Dict.toList presets
        |> List.sortBy (Tuple.second >> .sortKey >> Maybe.withDefault 999999)
        |> List.head
        |> Maybe.unwrap (CustomTemplates []) (FromPreset << Tuple.first)


validationErrorsFor : Presets -> StepConfig -> TemplateSource -> List String
validationErrorsFor presets stepConfig source =
    case source of
        FromPreset name ->
            if Dict.member name presets then
                []

            else
                [ "Unknown preset `" ++ name ++ "`. Pick another preset in the edit form." ]

        CustomTemplates templates ->
            case List.filter (\t -> not (Dict.member t stepConfig)) templates of
                [] ->
                    []

                missing ->
                    [ "Unknown templates: " ++ String.join ", " missing ++ ". Remove them in the edit form." ]


partitionStepsByTemplate : List String -> List StepRecord -> ( Dict String (List StepRecord), List StepRecord )
partitionStepsByTemplate effective steps =
    let
        ( recognized, orphans ) =
            List.partition (\s -> List.member s.type_ effective) steps
    in
    ( List.foldl
        (\step -> Dict.update step.type_ (Maybe.map ((::) step)))
        (Dict.fromList (List.map (\t -> ( t, [] )) effective))
        recognized
    , orphans
    )


repartitionProjectSteps : Presets -> StepConfig -> ProjectRecord -> ProjectRecord
repartitionProjectSteps presets stepConfig proj =
    let
        effective =
            effectiveTemplates presets proj.templateSource
                |> List.filter (\t -> Dict.member t stepConfig)

        allSteps =
            (Dict.values proj.tables |> List.concatMap (ApiData.withDefault [] << .records))
                ++ proj.orphanedSteps

        ( buckets, orphans ) =
            partitionStepsByTemplate effective allSteps

        newTables =
            buckets
                |> Dict.map
                    (\name_ recs ->
                        case Dict.get name_ proj.tables of
                            Just old ->
                                { old | records = Success recs }

                            Nothing ->
                                { initialTable | records = Success recs }
                    )
    in
    { proj
        | tables = newTables
        , orphanedSteps = orphans
        , validationErrors = validationErrorsFor presets stepConfig proj.templateSource
    }


getCommitHash : Model -> ApiData String
getCommitHash (Model model) =
    model.commitHash


stepRevision : Model -> StepRecord -> Maybe String
stepRevision model record =
    Maybe.orElse (viewedRevision model) (Maybe.map .revision record.review)


viewedRevision : Model -> Maybe String
viewedRevision model =
    case (getRoute model).page of
        Route.Project { mCommit } ->
            Maybe.orElse (ApiData.toMaybe (getCommitHash model)) mCommit

        _ ->
            ApiData.toMaybe (getCommitHash model)


getUserRepoInfo : Model -> ApiData UserRepoInfo
getUserRepoInfo (Model model) =
    model.userRepoInfo


getStepLogs : Model -> Dict String (ApiData String)
getStepLogs (Model model) =
    model.stepLogs


getNotices : Model -> Dict String (ApiData (List Notice))
getNotices (Model model) =
    model.notices


stepLogKey : Int -> Maybe String -> String
stepLogKey id commit =
    String.fromInt id
        ++ "@"
        ++ (case commit of
                Just hash ->
                    "commit:" ++ hash

                Nothing ->
                    "current"
           )


getUploadProgress : Model -> Dict Int UploadProgress
getUploadProgress (Model model) =
    model.uploadProgress


getStepStatusHooks : Model -> Dict Int (Flow Model ())
getStepStatusHooks (Model model) =
    model.stepStatusHooks


getStepStatusBuffer : Model -> Dict Int ( String, Status )
getStepStatusBuffer (Model model) =
    model.stepStatusBuffer


getPendingBuilds : Model -> Dict Int String
getPendingBuilds (Model model) =
    model.pendingBuilds


getOpenDiff : Model -> Maybe ( Int, Float )
getOpenDiff (Model model) =
    model.openDiff


getAutocomplete : Model -> Dict String AutocompleteState
getAutocomplete (Model model) =
    model.autocomplete


getAutocompleteDebounce : Model -> Debounce AutocompleteJob
getAutocompleteDebounce (Model model) =
    model.autocompleteDebounce


getGutterDrag : Model -> Maybe GutterDrag
getGutterDrag (Model model) =
    model.gutterDrag


getCompareState : Model -> CompareState
getCompareState (Model model) =
    model.compareState


getAgent : Model -> AgentState
getAgent (Model model) =
    model.agent


getNow : Model -> Time.Posix
getNow (Model model) =
    model.now


dndSystem : DnDList.System a DnDList.Msg
dndSystem =
    let
        config =
            { beforeUpdate = \_ _ list -> list
            , movement = DnDList.Free
            , listen = DnDList.OnDrag
            , operation = DnDList.Rotate
            }
    in
    DnDList.create config identity


initialTable : Table a
initialTable =
    { records = NotAsked
    , isOpen = True
    , showHiddenRecords = False
    , edited = Nothing
    , drafts = Dict.empty
    , newDraft = Nothing
    , addMode = AddNew
    , nameEditOnly = False
    , dnd = dndSystem.model
    , dndAffected = []
    , selectExistingSteps = initSelectState
    , argSelectStates = Dict.empty
    , isUpdating = False
    }


type alias Flags =
    { origin : String
    , lastChat : Maybe String
    }


type StepStatusEvent
    = SSESnapshot { projectId : Int, commit : String, steps : List { stepId : Int, status : Status } }
    | SSEHeartbeat
    | SSEError String


type AgentTurnEvent
    = AgentTurnChunk { sessionId : String, chunk : String }
    | AgentTurnDone String
    | AgentTurnHeartbeat
    | AgentTurnError { sessionId : String, message : String }


initialModel : Browser.Navigation.Key -> Route -> Flags -> Model
initialModel key route flags =
    Model
        { projects = initialTable
        , route = route
        , origin = flags.origin
        , key = key
        , toasts = []
        , nextToastId = 0
        , nextClientId = 0
        , modalConfirm = initModalConfirmConfig
        , downstreamEntities = Dict.empty
        , searchBox = initSelectState
        , stepConfig = NotAsked
        , presets = NotAsked
        , commitHash = NotAsked
        , userRepoInfo = NotAsked
        , stepLogs = Dict.empty
        , notices = Dict.empty
        , uploadProgress = Dict.empty
        , stepStatusHooks = Dict.empty
        , stepStatusBuffer = Dict.empty
        , pendingBuilds = Dict.empty
        , openDiff = Nothing
        , reviewDraft = Nothing
        , autocomplete = Dict.empty
        , autocompleteDebounce = Debounce.init
        , gutterDrag = Nothing
        , compareState = CompareIdle
        , now = Time.millisToPosix 0
        , agent = { initAgentState | lastChat = flags.lastChat }
        , clusterStatus = ClusterUnknown
        , runningStepIds = []
        , statusBarOpen = False
        }


plainLineHeight : Int
plainLineHeight =
    17


type alias ScrollMetrics =
    { scrollTop : Float
    , clientHeight : Float
    , scrollHeight : Float
    }


countLines : String -> Int
countLines text =
    String.foldl
        (\char count ->
            if char == '\n' then
                count + 1

            else
                count
        )
        1
        text


type alias FileView =
    { isViewing : Bool
    , zoom : Float
    , plainScrollTop : Float
    }


type SeekDirection
    = Before
    | After


type alias SeekWindow =
    { chunks : List FileChunk
    , loading : Maybe SeekDirection
    }


emptySeekWindow : SeekWindow
emptySeekWindow =
    { chunks = [], loading = Nothing }


insertChunk : SeekWindow -> FileChunk -> SeekWindow
insertChunk window chunk =
    let
        append existingChunks =
            case existingChunks of
                [ _, _, _ ] ->
                    List.drop 1 existingChunks ++ [ chunk ]

                _ ->
                    existingChunks ++ [ chunk ]

        updatedChunks =
            case ( List.head window.chunks, List.last window.chunks ) of
                ( Nothing, Nothing ) ->
                    [ chunk ]

                ( Just first, Just last ) ->
                    if List.any (\existing -> existing.startOffset == chunk.startOffset) window.chunks then
                        window.chunks

                    else if chunk.endOffset == first.startOffset then
                        List.take 3 (chunk :: window.chunks)

                    else if last.endOffset == chunk.startOffset then
                        append window.chunks

                    else
                        window.chunks

                _ ->
                    [ chunk ]
    in
    { chunks = updatedChunks, loading = Nothing }


windowLineCount : SeekWindow -> Int
windowLineCount window =
    case ( List.head window.chunks, List.last window.chunks ) of
        ( Just first, Just last ) ->
            max 1 (last.endLine - first.startLine + 1)

        _ ->
            1


windowStartOffset : SeekWindow -> Maybe Int
windowStartOffset window =
    List.head window.chunks |> Maybe.map .startOffset


windowEndOffset : SeekWindow -> Maybe Int
windowEndOffset window =
    List.last window.chunks |> Maybe.map .endOffset


windowEof : SeekWindow -> Bool
windowEof window =
    List.last window.chunks |> Maybe.unwrap False .eof


windowStartLine : SeekWindow -> Int
windowStartLine window =
    List.head window.chunks |> Maybe.unwrap 1 .startLine


type alias FileChunk =
    { content : String
    , startOffset : Int
    , endOffset : Int
    , startLine : Int
    , endLine : Int
    , eof : Bool
    }


type alias DirectoryFile =
    { content : ApiData String
    , size : Int
    , viewable : Bool
    , seekable : Bool
    , seekWindow : ApiData SeekWindow
    , mimeType : Maybe String
    , view : FileView
    , delimitedGrid : Maybe DelimitedGrid
    , plainLineCount : Int
    , editedContent : Maybe String
    , isNew : Bool
    , isDeleted : Bool
    }


type alias DirectoryFolder =
    { children : ApiData (Dict String DirectoryItem)
    , expanded : Bool
    , extras : ApiData (Dict String Value)
    , size : Maybe Int
    , mimeType : Maybe String
    }


type DirectoryItem
    = File DirectoryFile
    | Folder DirectoryFolder


hasFileChanges : DirectoryFile -> Bool
hasFileChanges file_ =
    file_.isNew || file_.isDeleted || Maybe.isJust file_.editedContent


srcFileChangePaths : List String -> DirectoryFolder -> List (List String)
srcFileChangePaths parent folder_ =
    folder_.children
        |> ApiData.toMaybe
        |> Maybe.unwrap []
            (Dict.toList
                >> List.concatMap
                    (\( name, item ) ->
                        let
                            path =
                                parent ++ [ name ]
                        in
                        case item of
                            File file_ ->
                                if hasFileChanges file_ then
                                    [ path ]

                                else
                                    []

                            Folder child ->
                                srcFileChangePaths path child
                    )
            )


closeDirectoryFileViews : DirectoryFolder -> DirectoryFolder
closeDirectoryFileViews folder_ =
    { folder_ | children = ApiData.map (Dict.map (always closeDirectoryFileView)) folder_.children }


closeDirectoryFileView : DirectoryItem -> DirectoryItem
closeDirectoryFileView item =
    case item of
        File file_ ->
            File { file_ | view = closeFileView file_.view }

        Folder child ->
            Folder (closeDirectoryFileViews child)


closeFileView : FileView -> FileView
closeFileView view_ =
    { view_ | isViewing = False }


discardDirectoryFileChanges : DirectoryFolder -> DirectoryFolder
discardDirectoryFileChanges folder_ =
    { folder_
        | children =
            ApiData.map
                (Dict.toList
                    >> List.filterMap
                        (\( name, item ) ->
                            case item of
                                File file_ ->
                                    if file_.isNew then
                                        Nothing

                                    else
                                        Just ( name, File { file_ | editedContent = Nothing, isDeleted = False, view = closeFileView file_.view } )

                                Folder child ->
                                    Just ( name, Folder (discardDirectoryFileChanges child) )
                        )
                    >> Dict.fromList
                )
                folder_.children
    }


updateDirectoryChildren : Dict String DirectoryItem -> Dict String DirectoryItem -> Dict String DirectoryItem
updateDirectoryChildren fetched previous =
    Dict.union
        (Dict.map
            (\key fetchedItem ->
                case ( fetchedItem, Dict.get key previous ) of
                    ( File new, Just (File old) ) ->
                        File
                            { new
                                | content = old.content
                                , view = old.view
                                , delimitedGrid = old.delimitedGrid
                                , plainLineCount = old.plainLineCount
                                , editedContent = old.editedContent
                                , isDeleted = old.isDeleted
                            }

                    ( Folder new, Just (Folder old) ) ->
                        Folder
                            { new
                                | children = old.children
                                , expanded = old.expanded
                                , extras = old.extras
                            }

                    _ ->
                        fetchedItem
            )
            fetched
        )
        (Dict.filter (always directoryItemIsNew) previous)


directoryItemIsNew : DirectoryItem -> Bool
directoryItemIsNew item =
    case item of
        File file_ ->
            file_.isNew

        Folder _ ->
            False


type alias ColumnMeta =
    { columnType : Grid.ColumnType
    , nullable : Bool
    }


type alias TableMeta =
    { columns : List ColumnMeta
    }


type alias DelimitedGrid =
    { grid : Grid.State
    }


type DelimitedFileKind
    = CsvFile
    | TsvFile


delimitedGridFromFile : List String -> Maybe String -> String -> Maybe TableMeta -> Maybe DelimitedGrid
delimitedGridFromFile path mimeType content mTableMeta =
    detectDelimitedFile path mimeType
        |> Maybe.andThen
            (\fileKind ->
                case Csv.Parser.parse { fieldSeparator = delimitedSeparator fileKind } content of
                    Ok (header :: rows) ->
                        Just (buildDelimitedGrid header rows mTableMeta)

                    _ ->
                        Nothing
            )


detectDelimitedFile : List String -> Maybe String -> Maybe DelimitedFileKind
detectDelimitedFile path mimeType =
    let
        fileName =
            path
                |> List.reverse
                |> List.head
                |> Maybe.withDefault ""
                |> String.toLower

        normalizedMimeType =
            mimeType
                |> Maybe.withDefault ""
                |> String.toLower
    in
    if String.endsWith ".tsv" fileName || String.startsWith "text/tab-separated-values" normalizedMimeType then
        Just TsvFile

    else if String.endsWith ".csv" fileName || String.startsWith "text/csv" normalizedMimeType || String.startsWith "application/csv" normalizedMimeType then
        Just CsvFile

    else
        Nothing


delimitedSeparator : DelimitedFileKind -> Char
delimitedSeparator fileKind =
    case fileKind of
        CsvFile ->
            ','

        TsvFile ->
            '\t'


buildDelimitedGrid : List String -> List (List String) -> Maybe TableMeta -> DelimitedGrid
buildDelimitedGrid header rows mTableMeta =
    let
        columnCount =
            (header :: rows)
                |> List.map List.length
                |> List.foldl max 0

        normalizedHeader =
            padDelimitedCells columnCount header

        normalizedRows =
            List.map (padDelimitedCells columnCount) rows

        effectiveColMetas =
            case mTableMeta of
                Just meta ->
                    meta.columns
                        ++ List.repeat (max 0 (columnCount - List.length meta.columns)) { columnType = Grid.Text, nullable = True }

                Nothing ->
                    List.repeat columnCount { columnType = Grid.Text, nullable = False }

        maxCharsByColumn =
            maxLengthsByColumn normalizedRows normalizedHeader

        delimitedColumns =
            List.map2 Tuple.pair normalizedHeader maxCharsByColumn
                |> List.indexedMap
                    (\index ( title, maxChars ) ->
                        let
                            colMeta =
                                List.getAt index effectiveColMetas
                                    |> Maybe.withDefault { columnType = Grid.Text, nullable = True }
                        in
                        { id = "column-" ++ String.fromInt index
                        , title =
                            if String.isEmpty (String.trim title) then
                                "Column " ++ String.fromInt (index + 1)

                            else
                                title
                        , width = delimitedColumnWidth maxChars
                        , type_ = colMeta.columnType
                        }
                    )
    in
    { grid = Grid.init delimitedColumns (List.map Array.fromList normalizedRows)
    }


padDelimitedCells : Int -> List String -> List String
padDelimitedCells targetLength cells =
    if List.length cells >= targetLength then
        cells

    else
        cells ++ List.repeat (targetLength - List.length cells) ""


maxLengthsByColumn : List (List String) -> List String -> List Int
maxLengthsByColumn rows header =
    List.foldl
        (List.map2 max << List.map String.length)
        (List.map String.length header)
        rows


delimitedColumnWidth : Int -> Int
delimitedColumnWidth maxChars =
    max 88 ((maxChars + 2) * 9)


updateStepRecordTable : Table StepRecord -> Table StepRecord -> Table StepRecord
updateStepRecordTable new old =
    let
        mergeRecords =
            List.foldl
                (\oldRecord ->
                    List.updateIf
                        (\newRecord -> newRecord.id == oldRecord.id)
                        (\newRecord -> { newRecord | runState = oldRecord.runState, review = Maybe.orElse newRecord.review oldRecord.review })
                )

        mergedRecords =
            ApiData.update mergeRecords new.records old.records
    in
    { old | records = mergedRecords }


updateProjectRecordList : List ProjectRecord -> List ProjectRecord -> List ProjectRecord
updateProjectRecordList =
    List.foldl
        (\oldRecord ->
            List.updateIf
                (\newRecord -> newRecord.id == oldRecord.id)
                (\newRecord ->
                    { newRecord
                        | tables = Dict.map (\k -> updateStepRecordTable <| Maybe.withDefault initialTable <| Dict.get k newRecord.tables) oldRecord.tables
                        , hideOrphans = oldRecord.hideOrphans
                    }
                )
        )


getSortKey : BaseRecord a -> ( Int, Int, Int )
getSortKey record =
    ( if Maybe.isJust record.sortKey then
        0

      else
        1
    , record.sortKey |> Maybe.withDefault 0
    , record.id |> Maybe.withDefault 2147483647
    )


type alias RunningStepSummary =
    { stepId : Int
    , stepName : String
    , projectId : Int
    , projectName : String
    }


getRunningStepSummaries : Model -> List RunningStepSummary
getRunningStepSummaries (Model model) =
    let
        projectsList =
            model.projects.records
                |> ApiData.withDefault []
                |> List.sortBy getSortKey

        stepInProject : Int -> ProjectRecord -> Maybe RunningStepSummary
        stepInProject stepId project =
            let
                allSteps =
                    Dict.values project.tables
                        |> List.concatMap (\t -> ApiData.withDefault [] t.records)
            in
            case List.filter (\s -> s.id == Just stepId) allSteps of
                first :: _ ->
                    project.id
                        |> Maybe.map
                            (\pid ->
                                { stepId = stepId
                                , stepName = first.name
                                , projectId = pid
                                , projectName = project.name
                                }
                            )

                [] ->
                    Nothing
    in
    model.runningStepIds
        |> List.unique
        |> List.filterMap (\stepId -> List.findMap (stepInProject stepId) projectsList)


getModalConfirm : Model -> ModalConfirmConfig
getModalConfirm (Model model) =
    model.modalConfirm


getReviewDraft : Model -> Maybe ReviewDraft
getReviewDraft (Model model) =
    model.reviewDraft


getSearchBox : Model -> SelectState
getSearchBox (Model model) =
    model.searchBox
