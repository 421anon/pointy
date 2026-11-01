module Keyboard exposing
    ( Binding
    , Combination
    , Key(..)
    , Modifier(..)
    , arrowDown
    , arrowLeft
    , arrowRight
    , arrowUp
    , backspace
    , combination
    , decodeCombinations
    , delete
    , enter
    , escape
    , keyName
    , mapBindingMsg
    , modName
    , primary
    , simpleCombinations
    , space
    , toName
    )

import Basics.Extra exposing (flip, uncurry)
import Extra.Decode as Decode
import Extra.Events as Events exposing (CustomEvent)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra as Decode


type Combination
    = Combination (List Modifier) Key


type alias Binding msg =
    ( Combination, Decoder (CustomEvent msg) )


mapBindingMsg : (a -> b) -> Binding a -> Binding b
mapBindingMsg =
    Tuple.mapSecond << Decode.map << Events.map


type Key
    = Escape
    | Enter
    | ArrowUp
    | ArrowDown
    | ArrowLeft
    | ArrowRight
    | Tab
    | Backspace
    | Delete
    | Home
    | End
    | PageUp
    | PageDown
    | Character Char


type Modifier
    = Alt
    | Shift
    | Ctrl
    | Meta


decodeCombinations : List ( Combination, Decoder msg ) -> Decoder msg
decodeCombinations =
    Decode.firstMatching << List.map (uncurry decodeSingle)


simpleCombinations : List ( Combination, msg ) -> Decoder (CustomEvent msg)
simpleCombinations =
    Decode.firstMatching << List.map (uncurry decodeSingle) << List.map (Tuple.mapSecond <| Decode.succeed << Events.withDefaults)


toName : Combination -> String
toName (Combination mods key) =
    String.join "-" <| List.map modName mods ++ [ keyName key ]


combination : List Modifier -> Key -> Combination
combination =
    Combination


primary : Key -> List Combination
primary key =
    [ Combination [ Ctrl ] key, Combination [ Meta ] key ]


delete : Combination
delete =
    Combination [] Delete


arrowUp : Combination
arrowUp =
    Combination [] ArrowUp


arrowDown : Combination
arrowDown =
    Combination [] ArrowDown


arrowLeft : Combination
arrowLeft =
    Combination [] ArrowLeft


arrowRight : Combination
arrowRight =
    Combination [] ArrowRight


enter : Combination
enter =
    Combination [] Enter


space : Combination
space =
    Combination [] (Character ' ')


backspace : Combination
backspace =
    Combination [] Backspace


escape : Combination
escape =
    Combination [] Escape


keyValue : Key -> String
keyValue key =
    case key of
        Escape ->
            "Escape"

        Enter ->
            "Enter"

        ArrowUp ->
            "ArrowUp"

        ArrowDown ->
            "ArrowDown"

        ArrowLeft ->
            "ArrowLeft"

        ArrowRight ->
            "ArrowRight"

        Tab ->
            "Tab"

        Backspace ->
            "Backspace"

        Delete ->
            "Delete"

        Home ->
            "Home"

        End ->
            "End"

        PageUp ->
            "PageUp"

        PageDown ->
            "PageDown"

        Character char ->
            String.fromChar char


keyName : Key -> String
keyName key =
    case key of
        Escape ->
            "Esc"

        Enter ->
            "↵"

        ArrowUp ->
            "Up"

        ArrowDown ->
            "Down"

        ArrowLeft ->
            "Left"

        ArrowRight ->
            "Right"

        Tab ->
            "Tab"

        Backspace ->
            "⌫"

        Delete ->
            "Del"

        Home ->
            "Home"

        End ->
            "End"

        PageUp ->
            "PgUp"

        PageDown ->
            "PgDn"

        Character ' ' ->
            "⎵"

        Character char ->
            String.fromChar (Char.toUpper char)


modName : Modifier -> String
modName mod =
    case mod of
        Ctrl ->
            "Ctrl"

        Meta ->
            "Cmd"

        Alt ->
            "Alt"

        Shift ->
            "⇧"


modifierToPname : Modifier -> String
modifierToPname mod =
    case mod of
        Alt ->
            altPname

        Shift ->
            shiftPname

        Ctrl ->
            ctrlPname

        Meta ->
            metaPname


allModifierPnames : List String
allModifierPnames =
    [ altPname, ctrlPname, shiftPname, metaPname ]


metaPname : String
metaPname =
    "metaKey"


shiftPname : String
shiftPname =
    "shiftKey"


ctrlPname : String
ctrlPname =
    "ctrlKey"


altPname : String
altPname =
    "altKey"


processModifiers : List Modifier -> Decoder a -> Decoder a
processModifiers mods =
    allModifierPnames
        |> List.partition (\modPname -> mods |> List.map modifierToPname |> List.member modPname)
        |> Tuple.mapFirst (List.map <| flip Tuple.pair True)
        |> Tuple.mapSecond (List.map <| flip Tuple.pair False)
        |> (\( a, b ) -> a ++ b)
        |> List.map (\( name, expected ) -> Decode.when (Decode.field name Decode.bool) ((==) expected))
        |> List.foldl (<<) identity


decodeSingle : Combination -> Decoder msg -> Decoder msg
decodeSingle (Combination mods key) msgDecoder =
    processModifiers mods <|
        Decode.when (Decode.field "key" Decode.string)
            (\pressed -> String.toLower pressed == String.toLower (keyValue key))
            msgDecoder
