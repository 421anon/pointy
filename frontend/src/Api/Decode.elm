module Api.Decode exposing (..)

import Api.ApiData exposing (ApiData(..))
import Components.Select as Select
import Dict exposing (Dict)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, maybe)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Model.Core as Model exposing (DirectoryItem(..), FileView, ProjectRecord, Status(..), StepRecord, StepStatusEvent(..), TemplateSource(..), initialTable)
import Model.Shadow exposing (Artifact, Field, Preset, Presets, StepArgValue(..), StepConfig, StepConfigEntry, StepType(..), Widget(..), WithSrcFiles(..))


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
        (\id name type_ note args reviewedRevision reviewedBy comments lastModifiedAt ->
            { id = Just id
            , clientId = Nothing
            , type_ = type_
            , hidden = False
            , sortKey = Nothing
            , name = name
            , note = note
            , runState = NotAsked
            , review = Maybe.map (\revision -> { revision = revision, reviewedBy = reviewedBy, comments = comments, comparison = NotAsked }) reviewedRevision
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
            , srcFileDraft = Nothing
            , srcFileWriting = False
            }
        )
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "type" Decode.string
        |> optional "note" Decode.string ""
        |> required "args" (stepArgs stepType_)
        |> optional "reviewedRevision" (maybe Decode.string) Nothing
        |> optional "reviewedBy" Decode.string ""
        |> optional "reviewComments" Decode.string ""
        |> optional "lastModifiedAt" (maybe Iso8601.decoder) Nothing


reviewComparison : String -> Maybe String -> Decoder (Maybe (ApiData Model.ReviewComparison))
reviewComparison comparison detail =
    case comparison of
        "no-review" ->
            Decode.succeed Nothing

        "same-out-path" ->
            Decode.succeed (Just (Success Model.SameOutPath))

        "same-content" ->
            Decode.succeed (Just (Success Model.SameContent))

        "different-content" ->
            Decode.succeed (Just (Success Model.DifferentContent))

        "viewed-output-unbuilt" ->
            Decode.succeed (Just (Success Model.ViewedOutputUnbuilt))

        "reviewed-output-unbuilt" ->
            Decode.succeed (Just (Success Model.ReviewedOutputUnbuilt))

        "unresolvable" ->
            Decode.succeed (Just (Error (Http.BadBody (Maybe.withDefault "The review check failed." detail))))

        other ->
            Decode.fail ("Unknown step review comparison: " ++ other)


reviewReport : Decoder Model.ReviewReport
reviewReport =
    Decode.succeed (\revision reviewedBy comments reviewedStatus_ reviewedStatusError comparison detail -> { revision = revision, reviewedBy = reviewedBy, comments = comments, reviewedStatus = Maybe.map (\status_ -> applyError status_ reviewedStatusError) reviewedStatus_, comparison = comparison, detail = detail })
        |> optional "reviewedRevision" (maybe Decode.string) Nothing
        |> optional "reviewedBy" Decode.string ""
        |> optional "reviewComments" Decode.string ""
        |> optional "reviewedStatus" (maybe status) Nothing
        |> optional "reviewedStatusError" (maybe Decode.string) Nothing
        |> required "comparison" Decode.string
        |> optional "comparisonDetail" (maybe Decode.string) Nothing
        |> Decode.andThen
            (\fields ->
                reviewComparison fields.comparison fields.detail
                    |> Decode.map
                        (\mComparison ->
                            { review = Maybe.map2 (\revision comparison -> { revision = revision, reviewedBy = fields.reviewedBy, comments = fields.comments, comparison = comparison }) fields.revision mComparison
                            , reviewedStatus = Maybe.andThen (always fields.reviewedStatus) mComparison
                            }
                        )
            )


reviewReports : Decoder (Dict Int Model.ReviewReport)
reviewReports =
    Decode.keyValuePairs reviewReport
        |> Decode.map (List.filterMap (\( key, outcome ) -> Maybe.map (\stepId -> ( stepId, outcome )) (String.toInt key)) >> Dict.fromList)


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
                                , editedContent = Nothing
                                , isNew = False
                                , isDeleted = False
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


widget : Decoder Widget
widget =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "text" ->
                        Decode.map WText (maybe (Decode.field "hook" Decode.string))

                    "textarea" ->
                        Decode.succeed WTextarea

                    "code" ->
                        Decode.map WCode (Decode.field "language" Decode.string)

                    "command" ->
                        Decode.map WCommand (Decode.field "prefix" Decode.string)

                    "number" ->
                        Decode.succeed WNumber

                    "checkbox" ->
                        Decode.succeed WCheckbox

                    "select" ->
                        Decode.succeed (WSelect [])

                    "tokens" ->
                        Decode.map WTokens (maybe (Decode.field "hook" Decode.string))

                    "list" ->
                        Decode.succeed (WList (WText Nothing))

                    "step" ->
                        Decode.succeed (WStep emptyArtifact)

                    "steps" ->
                        Decode.succeed (WSteps emptyArtifact)

                    "record" ->
                        Decode.succeed (WRecord [])

                    "datetime" ->
                        Decode.succeed WDatetime

                    other ->
                        Decode.fail ("Unknown widget: " ++ other)
            )


emptyArtifact : Artifact
emptyArtifact =
    { accepts = Nothing, proven = Nothing, create = False }


artifact : Decoder Artifact
artifact =
    Decode.succeed Artifact
        |> required "accepts" (maybe (Decode.list Decode.string))
        |> optional "proven" (maybe (Decode.list Decode.string)) Nothing
        |> optional "create" Decode.bool False


option : Decoder ( String, String )
option =
    Decode.map2 (\value label -> ( value, Maybe.withDefault value label ))
        (Decode.field "value" Decode.string)
        (maybe (Decode.field "label" Decode.string))


fieldWidget : Decoder Widget
fieldWidget =
    Decode.map2 Tuple.pair (Decode.field "widget" widget) (Decode.field "shape" (Decode.field "kind" Decode.string))
        |> Decode.andThen
            (\( control, kind ) ->
                case ( control, kind ) of
                    ( WText hook, "text" ) ->
                        Decode.succeed (WText hook)

                    ( WTextarea, "text" ) ->
                        Decode.succeed WTextarea

                    ( WCode language, "text" ) ->
                        Decode.succeed (WCode language)

                    ( WCommand prefix, "text" ) ->
                        Decode.succeed (WCommand prefix)

                    ( WDatetime, "text" ) ->
                        Decode.succeed WDatetime

                    ( WNumber, "int" ) ->
                        Decode.succeed WNumber

                    ( WCheckbox, "bool" ) ->
                        Decode.succeed WCheckbox

                    ( WSelect _, "choice" ) ->
                        Decode.map WSelect (Decode.field "shape" (Decode.field "options" (Decode.list option)))

                    ( WTokens hook, "list" ) ->
                        Decode.map2 Tuple.pair
                            (Decode.field "shape" (Decode.field "element" (Decode.field "shape" (Decode.field "kind" Decode.string))))
                            (Decode.field "shape" (Decode.field "element" (Decode.field "widget" (maybe (Decode.field "hook" Decode.string)))))
                            |> Decode.andThen
                                (\( elementShape, elementHook ) ->
                                    if elementShape == "text" then
                                        Decode.succeed (WTokens (declaredHook hook elementHook))

                                    else
                                        Decode.fail "A token list needs a text element"
                                )

                    ( WList _, "list" ) ->
                        Decode.map WList (Decode.field "shape" (Decode.field "element" (Decode.lazy (\() -> fieldWidget))))

                    ( WSteps _, "list" ) ->
                        Decode.map WSteps (Decode.field "shape" (Decode.field "element" (Decode.field "shape" artifact)))

                    ( WStep _, "artifact" ) ->
                        Decode.map WStep (Decode.field "shape" artifact)

                    ( WRecord _, "record" ) ->
                        Decode.map WRecord (Decode.field "shape" (Decode.field "fields" (Decode.list (Decode.lazy (\() -> argField)))))

                    ( _, other ) ->
                        Decode.fail ("Unsupported value shape: " ++ other)
            )


declaredHook : Maybe String -> Maybe String -> Maybe String
declaredHook fieldHook elementHook =
    case fieldHook of
        Just _ ->
            fieldHook

        Nothing ->
            elementHook


argField : Decoder Field
argField =
    Decode.succeed Field
        |> required "name" Decode.string
        |> optional "label" (maybe Decode.string) Nothing
        |> optional "help" Decode.string ""
        |> optional "readOnly" Decode.bool False
        |> optional "path" (maybe (Decode.list Decode.string)) Nothing
        |> custom (Decode.lazy (\() -> fieldWidget))


uploadHash : Decoder StepArgValue
uploadHash =
    Decode.map TUploadHashValue (Decode.field "hash" Decode.string)


stepArgValue : Widget -> Decoder StepArgValue
stepArgValue widget_ =
    case widget_ of
        WNumber ->
            Decode.map TIntValue Decode.int

        WCheckbox ->
            Decode.map TBoolValue Decode.bool

        WSelect _ ->
            Decode.map TEnumValue Decode.string

        WTokens _ ->
            Decode.map (TListValue << List.map TStringValue) (Decode.list Decode.string)

        WList element ->
            Decode.list (Decode.lazy (\() -> stepArgValue element))
                |> Decode.map TListValue

        WStep _ ->
            Decode.map TStepValue (Decode.field "step" Decode.int)

        WSteps _ ->
            Decode.map (TListValue << List.map TStepValue) (Decode.list (Decode.field "step" Decode.int))

        WRecord fields ->
            let
                fieldTypes =
                    Dict.fromList (List.map (\f -> ( f.name, f )) fields)

                decodeField fieldName fieldJson =
                    case Dict.get fieldName fieldTypes of
                        Nothing ->
                            Decode.fail ("Unknown record field: " ++ fieldName)

                        Just f ->
                            case Decode.decodeValue (Decode.lazy (\() -> stepArgValue f.widget)) fieldJson of
                                Ok value ->
                                    Decode.succeed value

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

        _ ->
            Decode.map TStringValue Decode.string


stepArgs : StepType -> Decoder (Dict String StepArgValue)
stepArgs stepType_ =
    case stepType_ of
        FileUpload _ ->
            Decode.dict uploadHash

        Derivation fields _ ->
            stepArgsFromFields fields

        Download fields ->
            stepArgsFromFields fields


stepArgsFromFields : List Field -> Decoder (Dict String StepArgValue)
stepArgsFromFields fields =
    let
        fieldByName =
            Dict.fromList (List.map (\f -> ( f.name, f )) fields)

        pathHeads =
            List.filterMap (.argsPath >> Maybe.andThen List.head) fields

        valueFor raw f =
            case f.argsPath of
                Just argsPath ->
                    Decode.decodeValue (Decode.at argsPath Decode.value) raw

                Nothing ->
                    Decode.decodeValue (Decode.field f.name Decode.value) raw

        decodeField raw f =
            case valueFor raw f of
                Err _ ->
                    Decode.succeed Nothing

                Ok valueJson ->
                    case Decode.decodeValue (Decode.lazy (\() -> stepArgValue f.widget)) valueJson of
                        Ok value ->
                            Decode.succeed (Just value)

                        Err err ->
                            Decode.fail ("Invalid value for arg '" ++ f.name ++ "': " ++ Decode.errorToString err)
    in
    Decode.value
        |> Decode.andThen
            (\raw ->
                case Decode.decodeValue (Decode.dict Decode.value) raw of
                    Err err ->
                        Decode.fail ("Invalid args object: " ++ Decode.errorToString err)

                    Ok dict ->
                        let
                            unknown =
                                Dict.keys dict
                                    |> List.filter
                                        (\key ->
                                            not (Dict.member key fieldByName)
                                                && not (List.member key pathHeads)
                                        )
                        in
                        if List.isEmpty unknown then
                            fields
                                |> List.map (\f -> Decode.map (Maybe.map (Tuple.pair f.name)) (decodeField raw f))
                                |> List.foldl (\decoder acc -> Decode.map2 (::) decoder acc) (Decode.succeed [])
                                |> Decode.map (List.filterMap identity >> Dict.fromList)

                        else
                            Decode.fail ("Unknown step arg(s): " ++ String.join ", " unknown)
            )


stepConfig : Decoder StepConfig
stepConfig =
    Decode.field "version" Decode.int
        |> Decode.andThen
            (\version ->
                if version == 4 then
                    Decode.field "templates" (Decode.dict stepConfigEntry)

                else
                    Decode.fail ("Unsupported step-config version: " ++ String.fromInt version)
            )


stepConfigEntry : Decoder StepConfigEntry
stepConfigEntry =
    Decode.succeed
        (\stepType_ sortKey displayName description icon ->
            { stepType = stepType_
            , sortKey = sortKey
            , displayName = displayName
            , description = description
            , icon = icon
            }
        )
        |> custom stepType
        |> optional "sortKey" (maybe Decode.int) Nothing
        |> optional "displayName" (maybe Decode.string) Nothing
        |> optional "description" (maybe Decode.string) Nothing
        |> optional "icon" (maybe Decode.string) Nothing


stepType : Decoder StepType
stepType =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "derivation" ->
                        Decode.map2 Derivation
                            (Decode.field "fields" (Decode.list (Decode.lazy (\() -> argField))))
                            (Decode.maybe (Decode.field "withSrcFiles" withSrcFiles)
                                |> Decode.map (Maybe.withDefault WithoutSrcFiles)
                            )

                    "upload" ->
                        Decode.map FileUpload (maybe (Decode.field "accepts" (Decode.list Decode.string)))

                    "download" ->
                        Decode.map Download (Decode.field "fields" (Decode.list (Decode.lazy (\() -> argField))))

                    other ->
                        Decode.fail ("Unknown step kind: " ++ other)
            )


withSrcFiles : Decoder WithSrcFiles
withSrcFiles =
    Decode.bool
        |> Decode.map
            (\x ->
                if x then
                    WithSrcFiles

                else
                    WithoutSrcFiles
            )
