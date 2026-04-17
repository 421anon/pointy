module Api.Encode exposing (..)

import Api.ApiData exposing (ApiData(..))
import Dict exposing (Dict)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Model.Core exposing (ProjectRecord, StepRecord)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepType(..))


stepArgValue : StepArgType -> StepArgValue -> Maybe Encode.Value
stepArgValue argType arg =
    case ( argType, arg ) of
        ( TString _, TStringValue str ) ->
            Just (Encode.string str)

        ( TStep _, TStepValue id ) ->
            Just (Encode.object [ ( "step", Encode.int id ) ])

        ( TList itemType, TListValue items ) ->
            List.map (stepArgValue itemType) items
                |> Maybe.combine
                |> Maybe.map (Encode.list identity)

        ( TUploadHash, TUploadHashValue hash ) ->
            Just (Encode.object [ ( "hash", Encode.string hash ) ])

        _ ->
            Nothing


stepArgsValue : StepType -> Dict String StepArgValue -> Encode.Value
stepArgsValue stepType args =
    case stepType of
        FileUpload _ ->
            args
                |> Dict.toList
                |> List.filterMap
                    (\( name, value ) ->
                        stepArgValue TUploadHash value
                            |> Maybe.map (Tuple.pair name)
                    )
                |> Dict.fromList
                |> Encode.dict identity identity

        Derivation argTypes _ ->
            args
                |> Dict.toList
                |> List.filterMap
                    (\( name, value ) ->
                        Dict.get name argTypes
                            |> Maybe.andThen
                                (\{ type_ } ->
                                    stepArgValue type_ value
                                        |> Maybe.map (Tuple.pair name)
                                )
                    )
                |> Dict.fromList
                |> Encode.dict identity identity


stepValue : StepType -> StepRecord -> Encode.Value
stepValue stepType record =
    Encode.object
        [ ( "name", Encode.string record.name )
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
            case table.records of
                Success recs ->
                    recs

                _ ->
                    []

        steps =
            Dict.values record.tables
                |> List.concatMap extractRecords
    in
    Encode.object
        [ ( "name", Encode.string record.name )
        , ( "hidden", Encode.bool record.hidden )
        , ( "sortKey", Maybe.unwrap Encode.null Encode.int record.sortKey )
        , ( "steps", Encode.list stepRef steps )
        ]
