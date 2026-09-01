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
import Json.Decode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Shadow exposing (Presets, StepArgValue, StepConfig, StepType)
import Route exposing (Route)
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


type alias SrcFileDraft =
    { name : String
    , content : String
    }


type alias StepRecord =
    BaseRecord
        { type_ : String
        , note : String
        , runState : ApiData StepRunState
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


-- | RFC3339 "updatedAt" with the nanosecond fraction kept, so renames that
-- | land within the same millisecond still order correctly.


type alias SessionTimestamp =
    { posix : Time.Posix
    , nanos : Int
    }


sessionTimestampAtLeast : SessionTimestamp -> SessionTimestamp -> Bool
sessionTimestampAtLeast a b =
    -- Compare millis directly (// wraps at 2^31); the fraction lives in nanos.
    Time.posixToMillis a.posix > Time.posixToMillis b.posix
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


type alias AgentApplyView =
    { sessionView : AgentSessionView
    , invalidatedProjectIds : List Int
    , invalidatedStepIds : List Int
    }


type ChatTurnStatus
    = ChatPending
    | ChatDone
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
    | ArchivingAgentSession String
    | DeletingAgentSession String
    | StoppingAgentTurn String


type alias AgentState =
    { sessions : ApiData (List AgentSessionView)
    , selectedSessionId : Maybe String
    , isPanelOpen : Bool
    , isSessionListOpen : Bool
    , isFocusMode : Bool
    , activeTurnStream : Maybe String
    , chatEntries : List ChatEntry
    , chunkBuffer : String
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
    , selectedSessionId = Nothing
    , isPanelOpen = False
    , isSessionListOpen = False
    , isFocusMode = False
    , activeTurnStream = Nothing
    , chatEntries = []
    , chunkBuffer = ""
    , showArchived = False
    , changesetOperation = Nothing
    , request = Nothing
    , sessionNameEdit = Nothing
    , sessionRenames = Dict.empty
    , highlightTurnId = Nothing
    , lastChat = Nothing
    , isRestoringChat = False
    }


agentOperationActive : AgentState -> Bool
agentOperationActive agentState =
    (agentState.request /= Nothing)
        || (agentState.activeTurnStream /= Nothing)
        || (agentState.changesetOperation /= Nothing)
        || (agentState.sessionNameEdit
                |> Maybe.map .saving
                |> Maybe.withDefault False
           )


agentInteractionsBlocked : AgentState -> Bool
agentInteractionsBlocked agentState =
    agentOperationActive agentState
        || (case agentState.sessions of
                Loading _ ->
                    True

                _ ->
                    False
           )


agentSessionArchived : String -> Bool
agentSessionArchived status =
    status == "archived" || status == "discarded" || status == "applied"



selectedSessionView : AgentState -> Maybe AgentSessionView
selectedSessionView agentState =
    agentState.selectedSessionId
        |> Maybe.andThen
            (\sid ->
                agentState.sessions
                    |> ApiData.toMaybe
                    |> Maybe.andThen (List.head << List.filter (\view -> view.session.sessionId == sid))
            )


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
    | FromSrc


type CompareMode
    = CompareImage
    | CompareHtml
    | CompareText


compareSelectionMode : CompareSelection -> CompareMode
compareSelectionMode sel =
    let
        mime =
            Maybe.withDefault "" sel.mimeType
    in
    if String.startsWith "image/" mime then
        CompareImage

    else
        let
            extension =
                String.toLower sel.fileName
                    |> String.split "."
                    |> List.last
                    |> Maybe.withDefault ""

            previewableHtml =
                case sel.source of
                    FromOutput _ ->
                        True

                    FromSrc ->
                        False
        in
        if previewableHtml && (mime == "text/html" || extension == "html" || extension == "htm") then
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
    = AgentTurnChunk { turnId : String, chunk : String }
    | AgentTurnDone String
    | AgentTurnHeartbeat
    | AgentTurnError String


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

        sampleRows =
            List.take 100 normalizedRows

        delimitedColumns =
            List.indexedMap
                (\index title ->
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
                    , width = delimitedColumnWidth title (List.map (listCell index) sampleRows)
                    , type_ = colMeta.columnType
                    }
                )
                normalizedHeader
    in
    { grid = Grid.init delimitedColumns (List.map Array.fromList normalizedRows)
    }


padDelimitedCells : Int -> List String -> List String
padDelimitedCells targetLength cells =
    if List.length cells >= targetLength then
        cells

    else
        cells ++ List.repeat (targetLength - List.length cells) ""


listCell : Int -> List String -> String
listCell index cells =
    List.getAt index cells |> Maybe.withDefault ""


delimitedColumnWidth : String -> List String -> Int
delimitedColumnWidth title values =
    let
        maxChars =
            values
                |> List.map String.length
                |> List.foldl max (String.length title)
    in
    clamp 88 320 ((maxChars + 2) * 9)


updateStepRecordTable : Table StepRecord -> Table StepRecord -> Table StepRecord
updateStepRecordTable new old =
    let
        mergeRecords =
            List.foldl
                (\oldRecord ->
                    List.updateIf
                        (\newRecord -> newRecord.id == oldRecord.id)
                        (\newRecord -> { newRecord | runState = oldRecord.runState })
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


getSearchBox : Model -> SelectState
getSearchBox (Model model) =
    model.searchBox
