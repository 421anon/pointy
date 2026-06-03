module Model.Core exposing (..)

import Api.ApiData as ApiData exposing (ApiData(..))
import Array
import Browser.Navigation
import Components.Select exposing (SelectState, initSelectState)
import Csv.Parser
import Debounce
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


type alias StepRecord =
    BaseRecord
        { type_ : String
        , note : String
        , runState : ApiData StepRunState
        , args : Dict String StepArgValue
        , srcFiles : DirectoryFolder
        }


type alias Notice =
    { field : Maybe String
    , severity : String
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
    , inspected : Bool
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
        , autocomplete : Dict String AutocompleteState
        , autocompleteDebounce : Debounce.Debounce AutocompleteJob
        , gutterDrag : Maybe GutterDrag
        , now : Time.Posix
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


getProjects : Model -> Table ProjectRecord
getProjects (Model model) =
    model.projects


getRoute : Model -> Route
getRoute (Model model) =
    model.route


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


getAutocomplete : Model -> Dict String AutocompleteState
getAutocomplete (Model model) =
    model.autocomplete


getAutocompleteDebounce : Model -> Debounce.Debounce AutocompleteJob
getAutocompleteDebounce (Model model) =
    model.autocompleteDebounce


getGutterDrag : Model -> Maybe GutterDrag
getGutterDrag (Model model) =
    model.gutterDrag


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
    , inspected = False
    , dnd = dndSystem.model
    , dndAffected = []
    , selectExistingSteps = initSelectState
    , argSelectStates = Dict.empty
    , isUpdating = False
    }


type alias Flags =
    { origin : String }


type StepStatusEvent
    = SSESnapshot { projectId : Int, commit : String, steps : List { stepId : Int, status : Status } }
    | SSEHeartbeat
    | SSEError String


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
        , autocomplete = Dict.empty
        , autocompleteDebounce = Debounce.init
        , gutterDrag = Nothing
        , now = Time.millisToPosix 0
        }


type alias FileView =
    { isViewing : Bool
    , zoom : Float
    }


type alias DirectoryFile =
    { content : ApiData String
    , size : Int
    , viewable : Bool
    , mimeType : Maybe String
    , view : FileView
    , delimitedGrid : Maybe DelimitedGrid
    }


type alias DirectoryFolder =
    { children : ApiData (Dict String DirectoryItem)
    , expanded : Bool
    , extras : ApiData (Dict String Value)
    }


type DirectoryItem
    = File DirectoryFile
    | Folder DirectoryFolder


extractDirectoryItemBase : DirectoryItem -> DirectoryItem
extractDirectoryItemBase item =
    case item of
        File file ->
            File
                { content = file.content
                , size = file.size
                , viewable = file.viewable
                , mimeType = file.mimeType
                , view = { isViewing = file.view.isViewing, zoom = file.view.zoom }
                , delimitedGrid = file.delimitedGrid
                }

        Folder folder ->
            Folder <| extractDirectoryFolderBase folder


updateDirectoryItemBase : DirectoryItem -> DirectoryItem -> DirectoryItem
updateDirectoryItemBase item baseItem =
    case ( item, baseItem ) of
        ( File file, File base ) ->
            File
                { file
                    | content = base.content
                    , size = base.size
                    , viewable = base.viewable
                    , mimeType = base.mimeType
                    , view =
                        let
                            view =
                                file.view
                        in
                        { view
                            | isViewing = base.view.isViewing
                        }
                }

        ( Folder folder, Folder base ) ->
            Folder <| updateDirectoryFolderBase folder base

        _ ->
            item


extractDirectoryFolderBase : DirectoryFolder -> DirectoryFolder
extractDirectoryFolderBase folder =
    { children = ApiData.map (Dict.map (\_ -> extractDirectoryItemBase)) folder.children
    , expanded = folder.expanded
    , extras = folder.extras
    }


updateDirectoryFolderBase : DirectoryFolder -> DirectoryFolder -> DirectoryFolder
updateDirectoryFolderBase folder base =
    let
        mergeDirectoryItems a b =
            Dict.merge
                Dict.insert
                (\key old -> Dict.insert key << updateDirectoryItemBase old)
                (\_ _ -> identity)
                a
                b
                Dict.empty
    in
    { folder
        | children = ApiData.update mergeDirectoryItems folder.children base.children
        , expanded = base.expanded
        , extras = base.extras
    }


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
                    , tooltip =
                        "Column type: "
                            ++ Grid.columnTypeLabel colMeta.columnType
                            ++ (if colMeta.nullable then
                                    " (nullable)"

                                else
                                    ""
                               )
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

        refreshedEdited =
            if old.inspected then
                old.edited
                    |> Maybe.andThen .id
                    |> Maybe.andThen
                        (\editedId ->
                            mergedRecords
                                |> ApiData.toMaybe
                                |> Maybe.andThen (List.find (\record -> record.id == Just editedId))
                        )

            else
                old.edited
    in
    { old
        | records = mergedRecords
        , edited = refreshedEdited
        , inspected = old.inspected && Maybe.isJust refreshedEdited
    }


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


getModalConfirm : Model -> ModalConfirmConfig
getModalConfirm (Model model) =
    model.modalConfirm


getSearchBox : Model -> SelectState
getSearchBox (Model model) =
    model.searchBox
