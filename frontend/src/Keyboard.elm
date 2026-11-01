module Keyboard exposing
    ( Binding
    , Combination(..)
    , Key(..)
    , Modifier(..)
    , arrowDown
    , arrowLeft
    , arrowRight
    , arrowUp
    , backspace
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
    | Space
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
    | KeyA
    | KeyB
    | KeyC
    | KeyD
    | KeyE
    | KeyF
    | KeyG
    | KeyH
    | KeyI
    | KeyJ
    | KeyK
    | KeyL
    | KeyM
    | KeyN
    | KeyO
    | KeyP
    | KeyQ
    | KeyR
    | KeyS
    | KeyT
    | KeyU
    | KeyV
    | KeyW
    | KeyX
    | KeyY
    | KeyZ
    | Digit0
    | Digit1
    | Digit2
    | Digit3
    | Digit4
    | Digit5
    | Digit6
    | Digit7
    | Digit8
    | Digit9


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
    Combination [] Space


backspace : Combination
backspace =
    Combination [] Backspace


escape : Combination
escape =
    Combination [] Escape


keyInfo : Key -> { code : Int, name : String }
keyInfo key =
    case key of
        Escape ->
            { code = 27, name = "Esc" }

        Enter ->
            { code = 13, name = "↵" }

        Space ->
            { code = 32, name = "⎵" }

        ArrowUp ->
            { code = 38, name = "Up" }

        ArrowDown ->
            { code = 40, name = "Down" }

        ArrowLeft ->
            { code = 37, name = "Left" }

        ArrowRight ->
            { code = 39, name = "Right" }

        Tab ->
            { code = 9, name = "Tab" }

        Backspace ->
            { code = 8, name = "⌫" }

        Delete ->
            { code = 46, name = "Del" }

        Home ->
            { code = 36, name = "Home" }

        End ->
            { code = 35, name = "End" }

        PageUp ->
            { code = 33, name = "PgUp" }

        PageDown ->
            { code = 34, name = "PgDn" }

        KeyA ->
            { code = 65, name = "A" }

        KeyB ->
            { code = 66, name = "B" }

        KeyC ->
            { code = 67, name = "C" }

        KeyD ->
            { code = 68, name = "D" }

        KeyE ->
            { code = 69, name = "E" }

        KeyF ->
            { code = 70, name = "F" }

        KeyG ->
            { code = 71, name = "G" }

        KeyH ->
            { code = 72, name = "H" }

        KeyI ->
            { code = 73, name = "I" }

        KeyJ ->
            { code = 74, name = "J" }

        KeyK ->
            { code = 75, name = "K" }

        KeyL ->
            { code = 76, name = "L" }

        KeyM ->
            { code = 77, name = "M" }

        KeyN ->
            { code = 78, name = "N" }

        KeyO ->
            { code = 79, name = "O" }

        KeyP ->
            { code = 80, name = "P" }

        KeyQ ->
            { code = 81, name = "Q" }

        KeyR ->
            { code = 82, name = "R" }

        KeyS ->
            { code = 83, name = "S" }

        KeyT ->
            { code = 84, name = "T" }

        KeyU ->
            { code = 85, name = "U" }

        KeyV ->
            { code = 86, name = "V" }

        KeyW ->
            { code = 87, name = "W" }

        KeyX ->
            { code = 88, name = "X" }

        KeyY ->
            { code = 89, name = "Y" }

        KeyZ ->
            { code = 90, name = "Z" }

        Digit0 ->
            { code = 48, name = "0" }

        Digit1 ->
            { code = 49, name = "1" }

        Digit2 ->
            { code = 50, name = "2" }

        Digit3 ->
            { code = 51, name = "3" }

        Digit4 ->
            { code = 52, name = "4" }

        Digit5 ->
            { code = 53, name = "5" }

        Digit6 ->
            { code = 54, name = "6" }

        Digit7 ->
            { code = 55, name = "7" }

        Digit8 ->
            { code = 56, name = "8" }

        Digit9 ->
            { code = 57, name = "9" }


keyName : Key -> String
keyName =
    .name << keyInfo


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
            ((==) (keyInfo key).code)
            msgDecoder
