module FormInputs exposing
    ( FieldInput
    , argsInputId
    , defaultArgValue
    , fieldInputs
    , isSrcFileDraftNameInputId
    , isTextWidget
    , listRows
    , nameInputId
    , noteInputId
    , srcFileDraftContentInputId
    , srcFileDraftNameInputId
    , srcFileInputId
    , stepTypeFields
    , uncontrolled
    )

import Dict exposing (Dict)
import Html
import Html.Attributes
import Json.Encode as Encode
import Model.Shadow exposing (Field, StepArgValue(..), StepType(..), Widget(..))


uncontrolled : String -> Bool -> List (Html.Attribute msg)
uncontrolled initialValue tracked =
    Html.Attributes.property "defaultValue" (Encode.string initialValue)
        :: (if tracked then
                [ Html.Attributes.attribute "data-track-changes" "" ]

            else
                []
           )


type alias FieldInput =
    { id : String
    , field : Field
    , value : Maybe StepArgValue
    }


formInputId : String -> Maybe Int -> String -> String
formInputId tableName mRecordId inputName =
    tableName ++ "-" ++ Maybe.withDefault "new" (Maybe.map String.fromInt mRecordId) ++ "-" ++ inputName


nameInputId : String -> Maybe Int -> String
nameInputId tableName mRecordId =
    formInputId tableName mRecordId "name-input"


noteInputId : String -> Maybe Int -> String
noteInputId tableName mRecordId =
    formInputId tableName mRecordId "note-input"


argsInputId : String -> Maybe Int -> String
argsInputId tableName mRecordId =
    formInputId tableName mRecordId "arg"


srcFileInputId : String -> Int -> List String -> String
srcFileInputId tableName recordId path =
    formInputId tableName (Just recordId) ("src-file:" ++ String.join "/" path)


srcFileDraftNameInputSuffix : String
srcFileDraftNameInputSuffix =
    "src-draft-name"


srcFileDraftNameInputId : String -> Int -> String
srcFileDraftNameInputId tableName recordId =
    formInputId tableName (Just recordId) srcFileDraftNameInputSuffix


isSrcFileDraftNameInputId : String -> Bool
isSrcFileDraftNameInputId =
    String.endsWith ("-" ++ srcFileDraftNameInputSuffix)


srcFileDraftContentInputId : String -> Int -> String
srcFileDraftContentInputId tableName recordId =
    formInputId tableName (Just recordId) "src-draft-content"


fieldInputs : String -> List Field -> Dict String StepArgValue -> List FieldInput
fieldInputs parentId fields values =
    List.map
        (\field ->
            { id = parentId ++ "-" ++ field.name
            , field = field
            , value = Dict.get field.name values
            }
        )
        fields


listRows : String -> List StepArgValue -> List ( String, StepArgValue )
listRows parentId items =
    List.indexedMap (\index item -> ( parentId ++ "-" ++ String.fromInt index, item )) items


isTextWidget : Widget -> Bool
isTextWidget widget =
    case widget of
        WText _ ->
            True

        WTextarea ->
            True

        WCode _ ->
            True

        WCommand _ ->
            True

        WDatetime ->
            True

        _ ->
            False


defaultArgValue : Widget -> StepArgValue
defaultArgValue widget =
    case widget of
        WCheckbox ->
            TBoolValue False

        WSelect (( first, _ ) :: _) ->
            TEnumValue first

        WSelect [] ->
            TEnumValue ""

        WRecord recordFields ->
            TRecordValue (Dict.fromList (List.map (\f -> ( f.name, defaultArgValue f.widget )) recordFields))

        WList _ ->
            TListValue []

        WTokens _ ->
            TListValue []

        WSteps _ ->
            TListValue []

        _ ->
            TStringValue ""


stepTypeFields : StepType -> List Field
stepTypeFields stepType =
    case stepType of
        FileUpload _ ->
            []

        Derivation fields _ ->
            fields

        Download fields ->
            fields
