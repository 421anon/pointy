module Api.Encode exposing (..)

import Api.ApiData as ApiData
import Dict exposing (Dict)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Model.Core exposing (ProjectRecord, ReviewDraft, StepRecord, TemplateSource(..))
import Model.Shadow exposing (Field, StepType(..), StepArgValue(..), Widget(..))


stepArgValue : Widget -> StepArgValue -> Maybe Encode.Value
stepArgValue widget_ value =
    case ( widget_, value ) of
        ( WList element, TListValue items ) ->
            encodeList (stepArgValue element) items

        ( WTokens _, TListValue items ) ->
            encodeList (stepArgValue (WText Nothing)) items

        ( WSteps artifact_, TListValue items ) ->
            encodeList (stepArgValue (WStep artifact_)) items

        ( WRecord fields, TRecordValue values ) ->
            fields
                |> List.filterMap
                    (\f ->
                        Dict.get f.name values
                            |> Maybe.andThen (stepArgValue f.widget)
                            |> Maybe.map (Tuple.pair f.name)
                    )
                |> Encode.object
                |> Just

        ( _, TStringValue str ) ->
            Just (Encode.string str)

        ( _, TIntValue n ) ->
            Just (Encode.int n)

        ( _, TBoolValue b ) ->
            Just (Encode.bool b)

        ( _, TEnumValue str ) ->
            Just (Encode.string str)

        ( _, TStepValue stepId ) ->
            Just (Encode.object [ ( "step", Encode.int stepId ) ])

        _ ->
            Nothing


encodeList : (StepArgValue -> Maybe Encode.Value) -> List StepArgValue -> Maybe Encode.Value
encodeList encodeItem items =
    items
        |> List.map encodeItem
        |> Maybe.combine
        |> Maybe.map (Encode.list identity)


stepArgsValue : StepType -> Dict String StepArgValue -> Encode.Value
stepArgsValue stepType args =
    case stepType of
        FileUpload _ ->
            args
                |> Dict.toList
                |> List.filterMap
                    (\( name, value ) ->
                        uploadHashValue value |> Maybe.map (Tuple.pair name)
                    )
                |> Encode.object

        Derivation fields _ ->
            fieldsToValue fields args

        Download fields ->
            fieldsToValue fields args


fieldsToValue : List Field -> Dict String StepArgValue -> Encode.Value
fieldsToValue fields args =
    fields
        |> List.filter (not << .readOnly)
        |> List.filterMap
            (\f ->
                Dict.get f.name args
                    |> Maybe.andThen (stepArgValue f.widget)
                    |> Maybe.map (Tuple.pair f.name)
            )
        |> Encode.object


uploadHashValue : StepArgValue -> Maybe Encode.Value
uploadHashValue value =
    case value of
        TUploadHashValue hash ->
            Just (Encode.object [ ( "hash", Encode.string hash ) ])

        _ ->
            Nothing


stepValue : StepType -> StepRecord -> Encode.Value
stepValue stepType record =
    Encode.object
        [ ( "name", Encode.string record.name )
        , ( "note", Encode.string record.note )
        , ( "type", Encode.string record.type_ )
        , ( "args", stepArgsValue stepType record.args )
        ]


stepRef : StepRecord -> Encode.Value
stepRef record =
    Encode.object
        [ ( "id", Maybe.unwrap Encode.null Encode.int record.id )
        , ( "hidden", Encode.bool record.hidden )
        , ( "sortKey", Maybe.unwrap Encode.null Encode.int record.sortKey )
        ]


projectRecord : ProjectRecord -> Encode.Value
projectRecord record =
    let
        extractRecords table =
            ApiData.withDefault [] table.records

        steps =
            (Dict.values record.tables |> List.concatMap extractRecords)
                ++ record.orphanedSteps

        sourceField =
            case record.templateSource of
                FromPreset name ->
                    ( "preset", Encode.string name )

                CustomTemplates ts ->
                    ( "templates", Encode.list Encode.string ts )
    in
    Encode.object
        [ ( "name", Encode.string record.name )
        , ( "hidden", Encode.bool record.hidden )
        , ( "sortKey", Maybe.unwrap Encode.null Encode.int record.sortKey )
        , sourceField
        , ( "steps", Encode.list stepRef steps )
        ]


reviewDraft : ReviewDraft -> Encode.Value
reviewDraft draft =
    Encode.object
        [ ( "reviewedBy", Encode.string draft.reviewedBy )
        , ( "reviewComments", Encode.string draft.comments )
        ]
