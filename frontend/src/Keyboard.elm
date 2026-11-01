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

        ArrowLeft ->
            37

        ArrowRight ->
            39

        Tab ->
            9

        Backspace ->
            8

        Delete ->
            46

        Home ->
            36

        End ->
            35

        PageUp ->
            33

        PageDown ->
            34

        KeyA ->
            65

        KeyB ->
            66

        KeyC ->
            67

        KeyD ->
            68

        KeyE ->
            69

        KeyF ->
            70

        KeyG ->
            71

        KeyH ->
            72

        KeyI ->
            73

        KeyJ ->
            74

        KeyK ->
            75

        KeyL ->
            76

        KeyM ->
            77

        KeyN ->
            78

        KeyO ->
            79

        KeyP ->
            80

        KeyQ ->
            81

        KeyR ->
            82

        KeyS ->
            83

        KeyT ->
            84

        KeyU ->
            85

        KeyV ->
            86

        KeyW ->
            87

        KeyX ->
            88

        KeyY ->
            89

        KeyZ ->
            90

        Digit0 ->
            48

        Digit1 ->
            49

        Digit2 ->
            50

        Digit3 ->
            51

        Digit4 ->
            52

        Digit5 ->
            53

        Digit6 ->
            54

        Digit7 ->
            55

        Digit8 ->
            56

        Digit9 ->
            57


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

        KeyA ->
            "A"

        KeyB ->
            "B"

        KeyC ->
            "C"

        KeyD ->
            "D"

        KeyE ->
            "E"

        KeyF ->
            "F"

        KeyG ->
            "G"

        KeyH ->
            "H"

        KeyI ->
            "I"

        KeyJ ->
            "J"

        KeyK ->
            "K"

        KeyL ->
            "L"

        KeyM ->
            "M"

        KeyN ->
            "N"

        KeyO ->
            "O"

        KeyP ->
            "P"

        KeyQ ->
            "Q"

        KeyR ->
            "R"

        KeyS ->
            "S"

        KeyT ->
            "T"

        KeyU ->
            "U"

        KeyV ->
            "V"

        KeyW ->
            "W"

        KeyX ->
            "X"

        KeyY ->
            "Y"

        KeyZ ->
            "Z"

        Digit0 ->
            "0"

        Digit1 ->
            "1"

        Digit2 ->
            "2"

        Digit3 ->
            "3"

        Digit4 ->
            "4"

        Digit5 ->
            "5"

        Digit6 ->
            "6"

        Digit7 ->
            "7"

        Digit8 ->
            "8"

        Digit9 ->
            "9"


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
