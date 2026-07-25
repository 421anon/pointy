module Api.Decode exposing (..)

import Api.ApiData exposing (ApiData(..))
import Components.Select as Select
import Dict exposing (Dict)
import Extra.Decode exposing (firstMatching)
import Iso8601
import Json.Decode as Decode exposing (Decoder, maybe)
import Json.Decode.Pipeline exposing (optional, required)
import Model.Core as Model exposing (DirectoryItem(..), FileView, ProjectRecord, Status(..), StepRecord, StepStatusEvent(..), TemplateSource(..), initialTable)
import Model.Shadow exposing (ArgType, Preset, Presets, StepArgType(..), StepArgValue(..), StepConfig, StepConfigEntry, StepType(..), TStringDisplay(..), WithSrcFiles(..))


stepStatusEvent : Decoder StepStatusEvent
stepStatusEvent =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "snapshot" ->
                        Decode.field "data" snapshot |> Decode.map SSESnapshot

                    "heartbeat" ->
                        Decode.succeed SSEHeartbeat

                    "error" ->
                        Decode.field "data" Decode.string |> Decode.map SSEError

                    _ ->
                        Decode.fail ("Unknown SSE event type: " ++ type_)
            )


userRepoInfo : Decoder Model.UserRepoInfo
userRepoInfo =
    Decode.succeed Model.UserRepoInfo
        |> required "url" Decode.string
        |> required "branch" Decode.string


snapshot : Decoder { projectId : Int, commit : String, steps : List { stepId : Int, status : Status } }
snapshot =
    Decode.succeed (\pid c s -> { projectId = pid, commit = c, steps = s })
        |> required "projectId" Decode.int
        |> required "commit" Decode.string
        |> required "steps"
            (Decode.list
                (Decode.succeed (\sid st mErr -> { stepId = sid, status = applyError st mErr })
                    |> required "stepId" Decode.int
                    |> required "status" status
                    |> optional "error" (Decode.map Just Decode.string) Nothing
                )
            )


applyError : Status -> Maybe String -> Status
applyError st mErr =
    case st of
        StatusFailure _ ->
            StatusFailure mErr

        other ->
            other


status : Decoder Status
status =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "not-started" ->
                        Decode.succeed StatusNotStarted

                    "running" ->
                        Decode.succeed StatusRunning

                    "success" ->
                        Decode.succeed StatusSuccess

                    "failure" ->
                        Decode.succeed (StatusFailure Nothing)

                    _ ->
                        Decode.fail ("Unknown status: " ++ str)
            )


preset : Decoder Preset
preset =
    Decode.succeed Preset
        |> required "displayName" Decode.string
        |> optional "description" (maybe Decode.string) Nothing
        |> optional "sortKey" (maybe Decode.int) Nothing
        |> required "templates" (Decode.list Decode.string)


presets : Decoder Presets
presets =
    Decode.dict preset


projectRecord : Presets -> StepConfig -> Decoder ProjectRecord
projectRecord presets_ stepConfig_ =
    let
        resolveSource id_ mPreset mTemplates =
            case ( mPreset, mTemplates ) of
                ( Just _, Just _ ) ->
                    Err ("Project `" ++ String.fromInt id_ ++ "` cannot define both `preset` and `templates`.")

                ( Nothing, Nothing ) ->
                    Err ("Project `" ++ String.fromInt id_ ++ "` must define either `preset` or `templates`.")

                ( Just p, Nothing ) ->
                    Ok (FromPreset p)

                ( Nothing, Just ts ) ->
                    Ok (CustomTemplates ts)

        build fields source =
            let
                effective =
                    Model.effectiveTemplates presets_ source
                        |> List.filter (\t -> Dict.member t stepConfig_)

                ( tablesByType, orphans ) =
                    Model.partitionStepsByTemplate effective fields.steps
            in
            { id = Just fields.id
            , clientId = Nothing
            , hidden = fields.hidden
            , sortKey = fields.sortKey
            , name = fields.name
            , tables = Dict.map (\_ recs -> { initialTable | records = Success recs }) tablesByType
            , templateSource = source
            , orphanedSteps = orphans
            , validationErrors = fields.validationErrors
            , hideOrphans = False
            , presetSelect = Select.initSelectState
            , templatesSelect = Select.initSelectState
            , isUpdating = False
            , lastModifiedAt = fields.lastModifiedAt
            }
    in
    Decode.succeed
        (\id name hidden sortKey lastModifiedAt mPreset mTemplates steps validationErrors ->
            { id = id
            , name = name
            , hidden = hidden
            , sortKey = sortKey
            , lastModifiedAt = lastModifiedAt
            , mPreset = mPreset
            , mTemplates = mTemplates
            , steps = steps
            , validationErrors = validationErrors
            }
        )
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "hidden" Decode.bool
        |> required "sortKey" (maybe Decode.int)
        |> optional "lastModifiedAt" (maybe Iso8601.decoder) Nothing
        |> required "preset" (maybe Decode.string)
        |> required "templates" (maybe (Decode.list Decode.string))
        |> required "steps" (Decode.list (stepRecord stepConfig_))
        |> optional "validationErrors" (Decode.list Decode.string) []
        |> Decode.andThen
            (\fields ->
                case resolveSource fields.id fields.mPreset fields.mTemplates of
                    Ok source ->
                        Decode.succeed (build fields source)

                    Err msg ->
                        Decode.fail msg
            )


stepRecord : StepConfig -> Decoder StepRecord
stepRecord stepConfig_ =
    Decode.succeed
        (\def hidden sortKey ->
            { def
                | hidden = hidden
                , sortKey = sortKey
            }
        )
        |> required "def" (stepValueOnlyFromConfig stepConfig_)
        |> required "hidden" Decode.bool
        |> required "sortKey" (maybe Decode.int)


stepValueOnlyFromConfig : StepConfig -> Decoder StepRecord
stepValueOnlyFromConfig stepConfig_ =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\typeName ->
                case Dict.get typeName stepConfig_ of
                    Just entry ->
                        stepValueOnly entry.stepType

                    Nothing ->
                        Decode.fail ("Unknown step type: " ++ typeName)
            )


stepValueOnly : StepType -> Decoder StepRecord
stepValueOnly stepType_ =
    Decode.succeed
        (\id name type_ note args lastModifiedAt ->
            { id = Just id
            , clientId = Nothing
            , type_ = type_
            , hidden = False
            , sortKey = Nothing
            , name = name
            , note = note
            , runState = NotAsked
            , args = args
            , isUpdating = False
            , lastModifiedAt = lastModifiedAt
            , srcFiles =
                { children = NotAsked
                , expanded = False
                , extras = NotAsked
                , size = Nothing
                , mimeType = Nothing
                }
            }
        )
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "type" Decode.string
        |> optional "note" Decode.string ""
        |> required "args" (stepArgs stepType_)
        |> optional "lastModifiedAt" (maybe Iso8601.decoder) Nothing


noticeSeverity : Decoder Model.NoticeSeverity
noticeSeverity =
    Decode.string
        |> Decode.andThen
            (\severity ->
                case severity of
                    "info" ->
                        Decode.succeed Model.Info

                    _ ->
                        Decode.fail ("Unknown notice severity: " ++ severity)
            )


notice : Decoder Model.Notice
notice =
    Decode.succeed
        (\field severity message ->
            { field = field
            , severity = severity
            , message = message
            }
        )
        |> optional "field" (maybe Decode.string) Nothing
        |> required "severity" noticeSeverity
        |> required "message" Decode.string


directoryItem : FileView -> Decoder ( String, DirectoryItem )
directoryItem fileView =
    Decode.field "isDir" Decode.bool
        |> Decode.andThen
            (\isDir ->
                if isDir then
                    Decode.succeed
                        (\name size mimeType ->
                            ( name
                            , Folder
                                { children = NotAsked
                                , expanded = False
                                , extras = NotAsked
                                , size =
                                    if mimeType == Just "application/zip" then
                                        Just size

                                    else
                                        Nothing
                                , mimeType = mimeType
                                }
                            )
                        )
                        |> required "name" Decode.string
                        |> required "size" Decode.int
                        |> required "mimeType" (Decode.nullable Decode.string)

                else
                    Decode.succeed
                        (\name size viewable seekable mimeType ->
                            ( name
                            , File
                                { content = NotAsked
                                , size = size
                                , viewable = viewable
                                , seekable = seekable
                                , seekWindow = NotAsked
                                , mimeType = mimeType
                                , view = fileView
                                , delimitedGrid = Nothing
                                , plainLineCount = 1
                                }
                            )
                        )
                        |> required "name" Decode.string
                        |> required "size" Decode.int
                        |> required "viewable" Decode.bool
                        |> optional "seekable" Decode.bool False
                        |> required "mimeType" (Decode.nullable Decode.string)
            )


fileChunk : Decoder Model.FileChunk
fileChunk =
    Decode.succeed Model.FileChunk
        |> required "content" Decode.string
        |> required "startOffset" Decode.int
        |> required "endOffset" Decode.int
        |> required "startLine" Decode.int
        |> required "endLine" Decode.int
        |> required "eof" Decode.bool


directoryItemGeneric : Decoder ( String, DirectoryItem )
directoryItemGeneric =
    directoryItem { isViewing = False, zoom = 1.0, plainScrollTop = 0 }


stepArgType : Decoder StepArgType
stepArgType =
    firstMatching
        [ Decode.field "string" <|
            Decode.map2 TString
                (Decode.field "display" tStringDisplay)
                (Decode.maybe (Decode.field "autocomplete" Decode.string))
        , Decode.map2 TEnum (Decode.field "enum" (Decode.list Decode.string)) (Decode.oneOf [ Decode.field "enumDisplayNames" (Decode.dict Decode.string), Decode.succeed Dict.empty ])
        , Decode.field "step" (Decode.map TStep <| maybe <| Decode.field "allowedTypes" (Decode.list Decode.string))
        , Decode.field "list" (Decode.map TList (Decode.lazy (\() -> stepArgType)))
        , Decode.field "record" (Decode.map TRecord (Decode.field "fields" (Decode.dict (Decode.lazy (\() -> argType)))))
        ]


tStringDisplay : Decoder TStringDisplay
tStringDisplay =
    Decode.value
        |> Decode.andThen
            (\displayJson ->
                case Decode.decodeValue (Decode.field "code" Decode.value) displayJson of
                    Ok codeJson ->
                        case Decode.decodeValue (Decode.field "language" Decode.string) codeJson of
                            Ok language ->
                                Decode.succeed (Code language)

                            Err err ->
                                Decode.fail ("Invalid code display: " ++ Decode.errorToString err)

                    Err _ ->
                        firstMatching
                            [ Decode.field "textarea" (Decode.succeed TextArea)
                            , Decode.field "command" (Decode.map Command Decode.string)
                            , Decode.succeed TextField
                            ]
            )


stepArgValue : StepArgType -> Decoder StepArgValue
stepArgValue argType_ =
    case argType_ of
        TString _ _ ->
            Decode.string |> Decode.map TStringValue

        TStep _ ->
            Decode.field "step" Decode.int |> Decode.map TStepValue

        TList itemType ->
            Decode.list (Decode.lazy (\() -> stepArgValue itemType))
                |> Decode.map TListValue

        TUploadHash ->
            Decode.field "hash" Decode.string |> Decode.map TUploadHashValue

        TEnum _ _ ->
            Decode.string |> Decode.map TEnumValue

        TRecord fieldTypes ->
            let
                decodeField fieldName fieldJson =
                    case Dict.get fieldName fieldTypes of
                        Nothing ->
                            Decode.fail ("Unknown record field: " ++ fieldName)

                        Just { type_ } ->
                            case Decode.decodeValue (Decode.lazy (\() -> stepArgValue type_)) fieldJson of
                                Ok val ->
                                    Decode.succeed val

                                Err err ->
                                    Decode.fail ("Invalid value for field '" ++ fieldName ++ "': " ++ Decode.errorToString err)
            in
            Decode.dict Decode.value
                |> Decode.andThen
                    (Dict.foldl
                        (\fieldName fieldJson ->
                            Decode.map2 (Dict.insert fieldName) (decodeField fieldName fieldJson)
                        )
                        (Decode.succeed Dict.empty)
                    )
                |> Decode.map TRecordValue


stepArgs : StepType -> Decoder (Dict String StepArgValue)
stepArgs stepType_ =
    case stepType_ of
        FileUpload _ ->
            Decode.dict (stepArgValue TUploadHash)

        Derivation argTypes _ ->
            let
                decodeArg argName argJson =
                    case Dict.get argName argTypes of
                        Nothing ->
                            Decode.fail ("Unknown step arg: " ++ argName)

                        Just { type_ } ->
                            case Decode.decodeValue (stepArgValue type_) argJson of
                                Ok argValue ->
                                    Decode.succeed argValue

                                Err decodeError ->
                                    Decode.fail ("Invalid value for arg '" ++ argName ++ "': " ++ Decode.errorToString decodeError)
            in
            Decode.dict Decode.value
                |> Decode.andThen
                    (Dict.foldl
                        (\argName argJson ->
                            Decode.map2 (Dict.insert argName) (decodeArg argName argJson)
                        )
                        (Decode.succeed Dict.empty)
                    )

        Download ->
            Decode.map2
                (\url_ maybeDownloadedAt ->
                    Dict.singleton "url" (TStringValue url_)
                        |> (case maybeDownloadedAt of
                                Just ts ->
                                    Dict.insert "downloadedAt" (TStringValue ts)

                                Nothing ->
                                    identity
                           )
                )
                (Decode.field "url" Decode.string)
                (Decode.maybe (Decode.at [ "downloaded", "downloadedAt" ] Decode.string))


argType : Decoder ArgType
argType =
    Decode.succeed ArgType
        |> required "description" Decode.string
        |> required "type" stepArgType
        |> optional "displayName" (maybe Decode.string) Nothing


stepType : Decoder StepType
stepType =
    let
        withSrcFiles =
            Decode.bool
                |> Decode.map
                    (\x ->
                        if x then
                            WithSrcFiles

                        else
                            WithoutSrcFiles
                    )
    in
    firstMatching
        [ Decode.field "derivation"
            (Decode.succeed Derivation
                |> required "args" (Decode.dict argType)
                |> optional "withSrcFiles" withSrcFiles WithoutSrcFiles
            )
        , Decode.field "fileUpload"
            (maybe (Decode.field "allowedExtensions" (Decode.list Decode.string))
                |> Decode.map FileUpload
            )
        , Decode.field "download" (Decode.dict Decode.value)
            |> Decode.map (always Download)
        ]


stepConfigEntry : Decoder StepConfigEntry
stepConfigEntry =
    Decode.succeed StepConfigEntry
        |> required "type" stepType
        |> optional "sortKey" (maybe Decode.int) Nothing
        |> optional "displayName" (maybe Decode.string) Nothing
        |> optional "description" (maybe Decode.string) Nothing


stepConfig : Decoder StepConfig
stepConfig =
    Decode.dict stepConfigEntry
