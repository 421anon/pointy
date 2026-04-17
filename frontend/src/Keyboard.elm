module Keyboard exposing
    ( Binding
    , Combination
    , Modifier(..)
    , arrowDown
    , arrowUp
    , backspace
    , ctrlC
    , decodeCombinations
    , enter
    , escape
    , keyName
    , mapBindingMsg
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
    | KeyA
    | KeyC
    | KeyK
    | KeyN
    | KeyR


type Modifier
    = Alt
    | Shift
    | Ctrl


{-| Combine a list of shortcut - decoder pairs.
Optionally pass a UserPlatform for combinations that use `CtrlOrCmd` to detect Cmd instead of Ctrl for MacOS systems.
If none of the combinations use `CtrlOrCmd`, it's safe to pass `Nothing` as the first argument.
-}
decodeCombinations : List ( Combination, Decoder msg ) -> Decoder msg
decodeCombinations =
    Decode.firstMatching << List.map (uncurry decodeSingle)


{-| If all event handlers:

  - ignore the event object's content
  - don't stop propagation and don't prevent default
    Then this function offers a simpler way to define keyboard bindings.

-}
simpleCombinations : List ( Combination, msg ) -> Decoder (CustomEvent msg)
simpleCombinations =
    Decode.firstMatching << List.map (uncurry decodeSingle) << List.map (Tuple.mapSecond <| Decode.succeed << Events.withDefaults)


toName : Combination -> String
toName (Combination mods key) =
    String.join "-" <| List.map modName mods ++ [ keyName key ]



-- COMBINATIONS
-- Note that any modifiers that do not appear in the modifier list have to be explicitly NOT pressed.
-- If you want a shortcut that does not care about a modifier key, either use 2 combinations with the same result
-- or implement ternary logic in this module.


ctrlC : Combination
ctrlC =
    Combination [ Ctrl ] KeyC


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



-- PRIVATE


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

        KeyA ->
            65

        KeyC ->
            67

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

        KeyA ->
            "A"

        KeyC ->
            "C"

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
