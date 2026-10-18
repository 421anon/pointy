module ConfigForm exposing
    ( PathStep(..)
    , addEntry
    , encode
    , init
    , isOptional
    , providerNames
    , removeEntry
    , toggleTable
    , update
    )

import Dict
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core exposing (ConfigField, ConfigForm, ConfigKind(..), ConfigNode(..), ConfigValues)


type PathStep
    = Key String
    | Index Int


init : List ConfigField -> Value -> ConfigForm
init fields values =
    { fields = fields, values = initValues fields values }


initValues : List ConfigField -> Value -> ConfigValues
initValues fields json =
    fields
        |> List.map (\field -> ( field.key, initNode field (fieldValue field json) ))
        |> Dict.fromList


fieldValue : ConfigField -> Value -> Maybe Value
fieldValue field json =
    Decode.decodeValue (Decode.field field.key Decode.value) json
        |> Result.toMaybe
        |> Maybe.orElse field.default


initNode : ConfigField -> Maybe Value -> ConfigNode
initNode field value =
    case field.kind of
        TableKind fields ->
            ConfigTable (Maybe.map (initValues fields) value)

        TablesKind fields ->
            value
                |> Maybe.andThen (decodeMaybe (Decode.list Decode.value))
                |> Maybe.withDefault []
                |> List.map (initValues fields)
                |> ConfigTables

        kind ->
            value
                |> Maybe.andThen (decodeMaybe (inputDecoder kind))
                |> Maybe.withDefault (onlyChoice field)
                |> ConfigInput


inputDecoder : ConfigKind -> Decoder String
inputDecoder kind =
    case kind of
        ListKind ->
            Decode.map (String.join "\n") (Decode.list Decode.string)

        MapKind ->
            Decode.map (List.map (\( key, value ) -> key ++ " = " ++ value) >> String.join "\n") (Decode.keyValuePairs Decode.string)

        IntegerKind ->
            Decode.map (Encode.encode 0) Decode.value

        NumberKind ->
            Decode.map (Encode.encode 0) Decode.value

        BoolKind ->
            Decode.map (Encode.encode 0) Decode.value

        _ ->
            Decode.string


onlyChoice : ConfigField -> String
onlyChoice field =
    case ( field.kind, field.required ) of
        ( EnumKind [ only ], True ) ->
            only

        _ ->
            ""


decodeMaybe : Decoder a -> Value -> Maybe a
decodeMaybe decoder =
    Decode.decodeValue decoder >> Result.toMaybe


isOptional : ConfigField -> Bool
isOptional field =
    not field.required && Maybe.isNothing field.default


encode : ConfigForm -> Value
encode form =
    encodeValues form.fields form.values


encodeValues : List ConfigField -> ConfigValues -> Value
encodeValues fields values =
    fields
        |> List.filterMap (\field -> Dict.get field.key values |> Maybe.andThen (encodeNode field) |> Maybe.map (Tuple.pair field.key))
        |> Encode.object


encodeNode : ConfigField -> ConfigNode -> Maybe Value
encodeNode field node =
    case ( field.kind, node ) of
        ( TableKind fields, ConfigTable table ) ->
            Maybe.map (encodeValues fields) table

        ( TablesKind fields, ConfigTables entries ) ->
            Just (Encode.list (encodeValues fields) entries)

        ( kind, ConfigInput input ) ->
            encodeInput (isOptional field) kind input

        _ ->
            Nothing


encodeInput : Bool -> ConfigKind -> String -> Maybe Value
encodeInput optional kind input =
    case kind of
        IntegerKind ->
            encodeNumber String.toInt Encode.int input

        NumberKind ->
            encodeNumber String.toFloat Encode.float input

        BoolKind ->
            nonEmpty input |> Maybe.map ((==) "true" >> Encode.bool)

        EnumKind _ ->
            nonEmpty input |> Maybe.map Encode.string

        ListKind ->
            Just (Encode.list Encode.string (inputLines input))

        MapKind ->
            Just (Encode.object (List.map mapEntry (inputLines input)))

        _ ->
            if optional && String.isEmpty input then
                Nothing

            else
                Just (Encode.string input)


encodeNumber : (String -> Maybe number) -> (number -> Value) -> String -> Maybe Value
encodeNumber parse encodeWith input =
    nonEmpty (String.trim input)
        |> Maybe.map (\trimmed -> parse trimmed |> Maybe.unwrap (Encode.string trimmed) encodeWith)


nonEmpty : String -> Maybe String
nonEmpty input =
    if String.isEmpty input then
        Nothing

    else
        Just input


inputLines : String -> List String
inputLines =
    String.lines >> List.map String.trim >> List.filter (not << String.isEmpty)


mapEntry : String -> ( String, Value )
mapEntry line =
    case String.split "=" line of
        key :: value ->
            ( String.trim key, Encode.string (String.trim (String.join "=" value)) )

        [] ->
            ( line, Encode.string "" )


update : List PathStep -> (ConfigNode -> ConfigNode) -> ConfigValues -> ConfigValues
update path change values =
    case path of
        (Key key) :: rest ->
            Dict.update key (Maybe.map (updateNode rest change)) values

        _ ->
            values


updateNode : List PathStep -> (ConfigNode -> ConfigNode) -> ConfigNode -> ConfigNode
updateNode path change node =
    case ( path, node ) of
        ( [], _ ) ->
            change node

        ( (Index index) :: rest, ConfigTables entries ) ->
            ConfigTables (List.updateAt index (update rest change) entries)

        ( _, ConfigTable (Just values) ) ->
            ConfigTable (Just (update path change values))

        _ ->
            node


toggleTable : List ConfigField -> Bool -> ConfigNode -> ConfigNode
toggleTable fields enabled _ =
    if enabled then
        ConfigTable (Just (initValues fields (Encode.object [])))

    else
        ConfigTable Nothing


addEntry : List ConfigField -> ConfigNode -> ConfigNode
addEntry fields node =
    case node of
        ConfigTables entries ->
            ConfigTables (entries ++ [ initValues fields (Encode.object []) ])

        _ ->
            node


removeEntry : Int -> ConfigNode -> ConfigNode
removeEntry index node =
    case node of
        ConfigTables entries ->
            ConfigTables (List.removeAt index entries)

        _ ->
            node


providerNames : ConfigValues -> List String
providerNames values =
    case Dict.get "providers" values of
        Just (ConfigTables entries) ->
            List.filterMap
                (\entry ->
                    case Dict.get "name" entry of
                        Just (ConfigInput name) ->
                            nonEmpty name

                        _ ->
                            Nothing
                )
                entries

        _ ->
            []
