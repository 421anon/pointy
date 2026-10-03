module Api.Encode exposing (..)

import Dict exposing (Dict)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildRef, ProjectRecord, ReviewDraft, StepRecord, TemplateSource(..), TreeOp(..))
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


childRef : ChildRef -> Encode.Value
childRef ref =
    Encode.object [ ( Model.childKindName ref.kind, Encode.object [ ( "id", Encode.int ref.id ) ] ) ]


treeOp : TreeOp -> Encode.Value
treeOp op =
    let
        entry name fields =
            Encode.object (( "op", Encode.string name ) :: fields)
    in
    case op of
        LinkOp parent ref ->
            entry "link" [ ( "parent", Encode.int parent ), ( "child", childRef ref ) ]

        UnlinkOp parent ref ->
            entry "unlink" [ ( "parent", Encode.int parent ), ( "child", childRef ref ) ]

        OrderOp parent refs ->
            entry "order" [ ( "parent", Encode.int parent ), ( "children", Encode.list childRef refs ) ]

        HideOp parent ref hidden_ ->
            entry "hide" [ ( "parent", Encode.int parent ), ( "child", childRef ref ), ( "hidden", Encode.bool hidden_ ) ]

        DeleteOp ref ->
            entry "delete" [ ( "child", childRef ref ) ]


treeOps : List TreeOp -> Encode.Value
treeOps ops =
    Encode.list treeOp ops


projectRecord : ProjectRecord -> Encode.Value
projectRecord record =
    let
        sourceField =
            case record.templateSource of
                FromPreset name ->
                    ( "preset", Encode.string name )

                CustomTemplates ts ->
                    ( "templates", Encode.list Encode.string ts )
    in
    Encode.object
        [ ( "name", Encode.string record.name )
        , sourceField
        ]


reviewDraft : ReviewDraft -> Encode.Value
reviewDraft draft =
    Encode.object
        [ ( "reviewedBy", Encode.string draft.reviewedBy )
        , ( "reviewComments", Encode.string draft.comments )
        ]
