module Components.Combobox exposing (Config, view)

import Extra.Decode as ExtraDecode
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, id, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Json.Decode as Decode
import Keyboard
import View.Icons exposing (iconCustom)


type alias Config item msg =
    { selected : List item
    , availableItems : List item
    , loading : Bool
    , error : Maybe String
    , toKey : item -> String
    , toLabel : item -> String
    , isInvalid : item -> Bool
    , isPending : item -> Bool
    , onSelect : item -> msg
    , onRemove : Int -> msg
    , onCreate : String -> msg
    , onInput : String -> msg
    , onActiveIndexChange : Int -> msg
    , inputValue : String
    , activeIndex : Int
    , allowFreeText : Bool
    , readOnly : Bool
    , placeholder : String
    , id : String
    , hasChanged : Bool
    , label : String
    , mHint : Maybe String
    }


view : Config item msg -> Html msg
view config =
    let
        itemCount =
            List.length config.availableItems

        clampedIndex =
            if itemCount == 0 then
                0

            else
                clamp 0 (itemCount - 1) config.activeIndex

        viewLabel =
            if String.isEmpty config.label then
                Html.nothing

            else
                case config.mHint of
                    Nothing ->
                        Html.label [ class "form-label" ] [ Html.text config.label ]

                    Just hint ->
                        Html.div [ class "form-label-group" ]
                            [ Html.label [ class "form-label" ] [ Html.text config.label ]
                            , Html.small [ class "form-hint" ] [ Html.text hint ]
                            ]

        viewChips =
            List.indexedMap
                (\i item ->
                    ( "combobox-chip-" ++ String.fromInt i ++ "-" ++ config.toKey item
                    , Html.div
                        ([ class "tag"
                         , classList
                            [ ( "tag-invalid", config.isInvalid item )
                            , ( "shimmer-text", config.isPending item )
                            , ( "shimmer-text--low-contrast", config.isPending item )
                            ]
                         ]
                            ++ (if config.isInvalid item then
                                    [ title "Not found in suggestions." ]

                                else
                                    []
                               )
                        )
                        [ Html.text (config.toLabel item)
                        , if config.readOnly then
                            Html.nothing

                          else
                            iconCustom True
                                "close_small"
                                [ class "remove-selected-icon"
                                , Events.preventDefaultOn "click"
                                    (Decode.succeed ( config.onRemove i, True ))
                                ]
                        ]
                    )
                )
                config.selected

        viewInput =
            ( config.id ++ "-input-pad"
            , if config.readOnly then
                Html.nothing

              else
                let
                    inputVal =
                        Decode.at [ "target", "value" ] Decode.string

                    activeSuggestion : Maybe item
                    activeSuggestion =
                        config.availableItems
                            |> List.drop clampedIndex
                            |> List.head

                    inputEmpty =
                        inputVal |> Decode.map (String.trim >> String.isEmpty)

                    arrowDownMsg =
                        if itemCount > 0 then
                            config.onActiveIndexChange (Basics.min (itemCount - 1) (clampedIndex + 1))

                        else
                            config.onActiveIndexChange 0

                    arrowUpMsg =
                        if itemCount > 0 then
                            config.onActiveIndexChange (Basics.max 0 (clampedIndex - 1))

                        else
                            config.onActiveIndexChange 0

                    backspaceRemoveLast =
                        if List.isEmpty config.selected then
                            Decode.fail "no selected items"

                        else
                            ExtraDecode.ifM inputEmpty
                                (Decode.succeed (config.onRemove (List.length config.selected - 1)))

                    allKeyBindings =
                        [ ( Keyboard.arrowDown, Decode.succeed arrowDownMsg )
                        , ( Keyboard.arrowUp, Decode.succeed arrowUpMsg )
                        , ( Keyboard.escape, Decode.succeed (config.onInput "") )
                        , ( Keyboard.enter
                          , let
                                enterSelectSuggestion =
                                    case activeSuggestion of
                                        Just item ->
                                            Decode.succeed (config.onSelect item)

                                        Nothing ->
                                            Decode.fail "no suggestion active"

                                enterCreateFreeText =
                                    if config.allowFreeText then
                                        ExtraDecode.ifM (inputEmpty |> Decode.map not)
                                            (inputVal |> Decode.map (\v -> config.onCreate (String.trim v)))

                                    else
                                        Decode.fail "free text not allowed"
                            in
                            Decode.oneOf
                                [ enterSelectSuggestion
                                , enterCreateFreeText
                                ]
                          )
                        , ( Keyboard.space
                          , if config.allowFreeText then
                                ExtraDecode.ifM (inputEmpty |> Decode.map not)
                                    (inputVal |> Decode.map (\v -> config.onCreate (String.trim v)))

                            else
                                Decode.fail "free text not allowed"
                          )
                        , ( Keyboard.backspace
                          , backspaceRemoveLast
                          )
                        ]

                    handleKey =
                        Keyboard.decodeCombinations allKeyBindings
                in
                Html.input
                    [ id config.id
                    , type_ "text"
                    , Events.preventDefaultOn "keydown" (Decode.map (\msg -> ( msg, True )) handleKey)
                    , Events.onInput config.onInput
                    , value config.inputValue
                    , Html.Attributes.placeholder config.placeholder
                    , class "list-field-input"
                    , attribute "autocomplete" "off"
                    , Events.on "blur"
                        (Decode.at [ "target", "value" ] Decode.string
                            |> Decode.map
                                (\v ->
                                    if String.isEmpty (String.trim v) then
                                        config.onInput ""

                                    else
                                        case activeSuggestion of
                                            Just item ->
                                                config.onSelect item

                                            Nothing ->
                                                if config.allowFreeText then
                                                    config.onCreate (String.trim v)

                                                else
                                                    config.onInput v
                                )
                        )
                    ]
                    []
            )

        dropdown =
            if String.isEmpty (String.trim config.inputValue) then
                Html.nothing

            else if config.loading then
                Html.div [ class "list-field-suggestions" ]
                    [ Html.div [ class "list-field-suggestion-meta" ] [ Html.text "Loading suggestions..." ] ]

            else
                case config.error of
                    Just _ ->
                        Html.div [ class "list-field-suggestions" ]
                            [ Html.div [ class "list-field-suggestion-meta" ] [ Html.text "Could not load suggestions." ] ]

                    Nothing ->
                        if List.isEmpty config.availableItems then
                            Html.nothing

                        else
                            Html.div [ id (config.id ++ "-suggestions"), class "list-field-suggestions" ]
                                (List.indexedMap
                                    (\i item ->
                                        Html.button
                                            [ id (config.id ++ "-suggestion-" ++ String.fromInt i)
                                            , type_ "button"
                                            , class "list-field-suggestion"
                                            , classList [ ( "active", i == clampedIndex ) ]
                                            , Events.preventDefaultOn "mousedown"
                                                (Decode.succeed ( config.onSelect item, True ))
                                            ]
                                            [ Html.text (config.toLabel item) ]
                                    )
                                    config.availableItems
                                )
    in
    Html.div
        [ class "form-field" ]
        [ viewLabel
        , Html.div [ class "list-field-autocomplete" ]
            [ Html.Keyed.node "div"
                [ class "tag-wrapper"
                , class "form-input"
                , classList [ ( "field-changed", config.hasChanged ) ]
                , classList [ ( "disabled", config.readOnly ) ]
                ]
                (viewChips ++ [ viewInput ])
            , dropdown
            ]
        ]
