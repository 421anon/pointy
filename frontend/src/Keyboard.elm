module Keyboard exposing
    ( Binding
    , Combination
    , Modifier(..)
    , arrowDown
    , arrowUp
    , backspace
    , ctrlA
    , ctrlC
    , ctrlV
    , ctrlX
    , ctrlZ
    , decodeCombinations
    , delete
    , enter
    , escape
    , keyName
    , mapBindingMsg
    , metaA
    , metaC
    , metaV
    , metaX
    , metaZ
    , modName
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
    | Space
    | ArrowUp
    | ArrowDown
    | Tab
    | Backspace
    | Delete
    | KeyA
    | KeyC
    | KeyV
    | KeyX
    | KeyZ
    | KeyK
    | KeyN
    | KeyR


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


ctrlC : Combination
ctrlC =
    Combination [ Ctrl ] KeyC


ctrlA : Combination
ctrlA =
    Combination [ Ctrl ] KeyA


ctrlV : Combination
ctrlV =
    Combination [ Ctrl ] KeyV


ctrlX : Combination
ctrlX =
    Combination [ Ctrl ] KeyX


ctrlZ : Combination
ctrlZ =
    Combination [ Ctrl ] KeyZ


metaA : Combination
metaA =
    Combination [ Meta ] KeyA


metaC : Combination
metaC =
    Combination [ Meta ] KeyC


metaV : Combination
metaV =
    Combination [ Meta ] KeyV


metaX : Combination
metaX =
    Combination [ Meta ] KeyX


metaZ : Combination
metaZ =
    Combination [ Meta ] KeyZ


delete : Combination
delete =
    Combination [] Delete


arrowUp : Combination
arrowUp =
    Combination [] ArrowUp


arrowDown : Combination
arrowDown =
    Combination [] ArrowDown


enter : Combination
enter =
    Combination [] Enter


space : Combination
space =
    Combination [] Space


backspace : Combination
backspace =
    Combination [] Backspace


escape : Combination
escape =
    Combination [] Escape


toCode : Key -> Int
toCode key =
    case key of
        Escape ->
            27

        Enter ->
            13

        Space ->
            32

        ArrowUp ->
            38

        ArrowDown ->
            40

        Tab ->
            9

        Backspace ->
            8

        Delete ->
            46

        KeyA ->
            65

        KeyC ->
            67

        KeyV ->
            86

        KeyX ->
            88

        KeyZ ->
            90

        KeyK ->
            75

        KeyN ->
            78

        KeyR ->
            82


keyName : Key -> String
keyName key =
    case key of
        Escape ->
            "Esc"

        Enter ->
            "↵"

        Space ->
            "⎵"

        ArrowUp ->
            "Up"

        ArrowDown ->
            "Down"

        Tab ->
            "Tab"

        Backspace ->
            "⌫"

        Delete ->
            "Del"

        KeyA ->
            "A"

        KeyC ->
            "C"

        KeyV ->
            "V"

        KeyX ->
            "X"

        KeyZ ->
            "Z"

        KeyK ->
            "K"

        KeyN ->
            "N"

        KeyR ->
            "R"


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
        Decode.when (Decode.field "keyCode" Decode.int)
            ((==) (toCode key))
            msgDecoder
