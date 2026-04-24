module Api.Decode exposing (..)

import Api.ApiData exposing (ApiData(..))
import Dict exposing (Dict)
import Extra.Decode exposing (firstMatching)
import Json.Decode as Decode exposing (Decoder, maybe)
import Json.Decode.Pipeline exposing (optional, required)
import Model.Core exposing (DirectoryItem(..), FileView, ProjectRecord, Status(..), StepRecord, StepStatusEvent(..), initialTable)
import Model.Shadow exposing (ArgType, StepArgType(..), StepArgValue(..), StepConfig, StepConfigEntry, StepType(..), TStringDisplay(..), WithSrcFiles(..))


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


userRepoInfo : Decoder Model.Core.UserRepoInfo
userRepoInfo =
    Decode.succeed Model.Core.UserRepoInfo
        |> required "url" Decode.string
        |> required "branch" Decode.string


snapshot : Decoder { projectId : Int, commit : String, steps : List { stepId : Int, status : Status, outPath : String } }
snapshot =
    Decode.succeed (\pid c s -> { projectId = pid, commit = c, steps = s })
        |> required "projectId" Decode.int
        |> required "commit" Decode.string
        |> required "steps"
            (Decode.list
                (Decode.succeed (\sid st op mErr -> { stepId = sid, status = applyError st mErr, outPath = op })
                    |> required "stepId" Decode.int
                    |> required "status" status
                    |> required "outPath" Decode.string
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


projectRecord : StepConfig -> Decoder ProjectRecord
projectRecord stepConfig_ =
    Decode.succeed
        (\id name hidden sortKey steps ->
            let
                stepsByType =
                    List.foldl
                        (\step -> Dict.update step.type_ (Just << (::) step << Maybe.withDefault []))
                        (Dict.map (always <| always []) stepConfig_)
                        steps
            in
            { id = Just id
            , clientId = Nothing
            , hidden = hidden
            , sortKey = sortKey
            , name = name
            , tables = Dict.map (\_ recs -> { initialTable | records = Success recs }) stepsByType
            , isUpdating = False
            }
        )
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "hidden" Decode.bool
        |> required "sortKey" (maybe Decode.int)
        |> required "steps" (Decode.list (stepRecord stepConfig_))


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
        (\id name type_ note args ->
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
            , srcFiles =
                { children = NotAsked
                , expanded = False
                }
            }
        )
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "type" Decode.string
        |> optional "note" Decode.string ""
        |> required "args" (stepArgs stepType_)


directoryItem : FileView -> Decoder ( String, DirectoryItem )
directoryItem fileView =
    Decode.field "isDir" Decode.bool
        |> Decode.andThen
            (\isDir ->
                if isDir then
                    Decode.succeed
                        (\name ->
                            ( name
                            , Folder
                                { children = NotAsked
                                , expanded = False
                                }
                            )
                        )
                        |> required "name" Decode.string

                else
                    Decode.succeed
                        (\name size viewable mimeType ->
                            ( name
                            , File
                                { content = NotAsked
                                , size = size
                                , viewable = viewable
                                , mimeType = mimeType
                                , view = fileView
                                }
                            )
                        )
                        |> required "name" Decode.string
                        |> required "size" Decode.int
                        |> required "viewable" Decode.bool
                        |> required "mimeType" (Decode.nullable Decode.string)
            )


directoryItemGeneric : Decoder ( String, DirectoryItem )
directoryItemGeneric =
    directoryItem { isViewing = False, zoom = 1.0 }


stepArgType : Decoder StepArgType
stepArgType =
    firstMatching
        [ Decode.field "string" (Decode.succeed TString |> required "display" tStringDisplay)
        , Decode.field "step" (Decode.map TStep <| maybe <| Decode.field "allowedTypes" (Decode.list Decode.string))
        , Decode.field "list" (Decode.map TList (Decode.lazy (\() -> stepArgType)))
        ]


tStringDisplay : Decoder TStringDisplay
tStringDisplay =
    firstMatching
        [ Decode.field "textarea" (Decode.succeed TextArea)
        , Decode.field "command" (Decode.map Command Decode.string)
        , Decode.succeed TextField
        ]


stepArgValue : StepArgType -> Decoder StepArgValue
stepArgValue argType_ =
    case argType_ of
        TString _ ->
            Decode.string |> Decode.map TStringValue

        TStep _ ->
            Decode.field "step" Decode.int |> Decode.map TStepValue

        TList itemType ->
            Decode.list (Decode.lazy (\() -> stepArgValue itemType))
                |> Decode.map TListValue

        TUploadHash ->
            Decode.field "hash" Decode.string |> Decode.map TUploadHashValue


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


argType : Decoder ArgType
argType =
    Decode.succeed ArgType
        |> required "description" Decode.string
        |> required "type" stepArgType


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
        ]


stepConfigEntry : Decoder StepConfigEntry
stepConfigEntry =
    Decode.succeed StepConfigEntry
        |> required "type" stepType
        |> optional "sortKey" (maybe Decode.int) Nothing


stepConfig : Decoder StepConfig
stepConfig =
    Decode.dict stepConfigEntry
