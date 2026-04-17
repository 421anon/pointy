module Components.Select exposing (Item, SelectState, initSelectState, selected, view)

import Accessors exposing (An_Optic, Lens, get, lens, over, set)
import Browser.Dom
import Extra.Accessors exposing (remkT)
import Extra.Decode as Decode
import Flow exposing (Flow)
import Html exposing (Html, map)
import Html.Attributes exposing (class, classList, disabled, id, style, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import Keyboard
import List.Extra as List
import Maybe.Extra as Maybe
import Task
import View.Icons exposing (iconCustom)


type alias SelectState =
    { selected : List Item
    , input : String
    , active : Bool
    , activeIndex : Int
    , scrollLock : Bool
    }


type alias Item =
    { id : Maybe Int
    , name : String
    }


initSelectState : SelectState
initSelectState =
    { selected = []
    , input = ""
    , active = False
    , activeIndex = 0
    , scrollLock = False
    }


type ChipAction
    = ChipClick
    | ChipRemove


type MenuAction
    = MenuSelect
    | MenuHover


viewChip : Bool -> Bool -> String -> String -> List (Html.Attribute Never) -> Html ChipAction
viewChip readOnly clickable name tooltip extraAttrs =
    Html.div
        ([ class "tag"
         , title tooltip
         , classList [ ( "clickable", clickable ) ]
         ]
            ++ List.map (Html.Attributes.map never) extraAttrs
            ++ (if clickable then
                    [ Events.onClick ChipClick, style "cursor" "pointer" ]

                else
                    []
               )
        )
        [ Html.text name
        , if readOnly then
            Html.nothing

          else
            iconCustom True
                "close_small"
                [ class "remove-selected-icon"
                , Events.stopPropagationOn "click"
                    (Decode.succeed ( ChipRemove, True ))
                ]
        ]


viewMenuItem : Bool -> String -> String -> String -> Html MenuAction
viewMenuItem isActive domId name tooltip =
    Html.div
        [ class "select-menu-item"
        , id domId
        , classList [ ( "active", isActive ) ]
        , Events.preventDefaultOn "mousedown" (Decode.succeed ( MenuSelect, True ))
        , Events.onMouseEnter MenuHover
        , title tooltip
        ]
        [ Html.text name ]


view :
    { optic : An_Optic pr ls s SelectState
    , selectState : SelectState
    , selected_ : List Item
    , availableItems : List Item
    , readOnly : Bool
    , hasChanged : Bool
    , label : String
    , placeholder : String
    , inputIcon : Maybe String
    , toInputItemName : Item -> String
    , toInputItemTooltip : Item -> List String
    , onInputItemClick : Item -> Maybe (Flow s ())
    , toMenuItemName : Item -> String
    , toMenuItemTooltip : Item -> List String
    , activeAfterSelect : Bool
    , clearInputAfterSelect : Bool
    , onChange : Flow s ()
    , onRemove : Item -> Flow s ()
    , onSelect : Item -> Flow s ()
    , alignRight : Bool
    , inputItemStyle : Item -> List (Html.Attribute Never)
    }
    -> Html (Flow s ())
view { optic, selectState, selected_, availableItems, readOnly, hasChanged, label, placeholder, inputIcon, toInputItemName, toInputItemTooltip, onInputItemClick, toMenuItemName, toMenuItemTooltip, onChange, onRemove, activeAfterSelect, clearInputAfterSelect, onSelect, alignRight, inputItemStyle } =
    let
        optic_ =
            remkT optic

        filteredAvailableItems =
            availableItems |> List.filter (\item -> String.contains (String.toLower selectState.input) (String.toLower (toMenuItemName item)))

        keyedSelectedChips =
            selected_
                |> List.indexedMap
                    (\index item ->
                        ( "item-" ++ String.fromInt index
                        , Html.Lazy.lazy5 viewChip
                            readOnly
                            (Maybe.isJust (onInputItemClick item))
                            (toInputItemName item)
                            (String.join "\n" (toInputItemTooltip item))
                            (inputItemStyle item)
                            |> map
                                (\action ->
                                    case action of
                                        ChipClick ->
                                            Maybe.withDefault Flow.none (onInputItemClick item)

                                        ChipRemove ->
                                            Flow.modify (over (optic_ << selected) (List.filter (\i -> i.id /= item.id)))
                                                |> Flow.seq onChange
                                                |> Flow.seq (onRemove item)
                                )
                        )
                    )

        menuItemId index =
            "select-menu-item-" ++ String.fromInt index

        setInputAction clearInput item s =
            if clearInput then
                set input "" s

            else
                set input (toMenuItemName item) s

        inputElement =
            ( "select-input"
            , Html.input
                ([ id "select-input"
                 , type_ "text"
                 , value selectState.input
                 , Html.Attributes.autocomplete False
                 ]
                    ++ (if String.isEmpty placeholder then
                            []

                        else
                            [ Html.Attributes.placeholder placeholder ]
                       )
                    ++ (if readOnly then
                            [ disabled True ]

                        else
                            let
                                handleKey =
                                    Keyboard.decodeCombinations
                                        [ ( Keyboard.arrowDown
                                          , Decode.succeed
                                                (Flow.get
                                                    |> Flow.map (get activeIndex)
                                                    |> Flow.andThen
                                                        (\activeIndex_ ->
                                                            let
                                                                nextIndex =
                                                                    min (List.length filteredAvailableItems - 1) (activeIndex_ + 1)
                                                            in
                                                            Flow.modify (set activeIndex nextIndex)
                                                                |> Flow.seq (ensureVisible (menuItemId nextIndex))
                                                        )
                                                    |> Flow.locking scrollLock
                                                    |> Flow.via optic_
                                                )
                                          )
                                        , ( Keyboard.arrowUp
                                          , Decode.succeed
                                                (Flow.get
                                                    |> Flow.map (get activeIndex)
                                                    |> Flow.andThen
                                                        (\activeIndex_ ->
                                                            let
                                                                nextIndex =
                                                                    max 0 (activeIndex_ - 1)
                                                            in
                                                            Flow.modify (set activeIndex nextIndex)
                                                                |> Flow.seq (ensureVisible (menuItemId nextIndex))
                                                        )
                                                    |> Flow.locking scrollLock
                                                    |> Flow.via optic_
                                                )
                                          )
                                        , ( Keyboard.enter
                                          , filteredAvailableItems
                                                |> List.drop selectState.activeIndex
                                                |> List.head
                                                |> Maybe.unwrap Flow.none
                                                    (\item ->
                                                        Flow.modify (over optic_ (over selected (List.push item) >> setInputAction clearInputAfterSelect item >> set activeIndex 0))
                                                            |> Flow.seq (Flow.modify (set (optic_ << active) activeAfterSelect))
                                                            |> Flow.seq (onSelect item)
                                                            |> Flow.seq onChange
                                                    )
                                                |> Decode.succeed
                                          )
                                        , ( Keyboard.escape
                                          , Decode.succeed (Flow.modify (set (optic_ << active) False))
                                          )
                                        , ( Keyboard.backspace
                                          , Decode.ifM (Decode.at [ "target", "value" ] Decode.string |> Decode.map String.isEmpty) <|
                                                Decode.succeed
                                                    (Maybe.unwrap Flow.none onRemove (List.last selected_)
                                                        |> Flow.seq
                                                            (Flow.modify (over (optic_ << selected) (List.take (List.length selectState.selected - 1)))
                                                                |> Flow.seq onChange
                                                            )
                                                    )
                                          )
                                        ]
                            in
                            [ Events.on "keydown" handleKey
                            , Events.onInput (\inp -> Flow.modify (over optic_ (set input inp << set active True << set activeIndex 0)))
                            , Events.onClick (Flow.modify (set (optic_ << active) True))
                            , Events.onBlur (Flow.modify (over optic_ (set active False << set activeIndex 0)))
                            ]
                       )
                )
                []
            )
    in
    Html.div
        [ class "form-field", class "select-container", classList [ ( "select-container--right", alignRight ) ] ]
        [ Html.viewIf (not <| String.isEmpty label) (Html.label [ class "form-label" ] [ Html.text label ])
        , Html.Keyed.node "div"
            [ class "tag-wrapper", class "form-input", classList [ ( "disabled", readOnly ), ( "field-changed", hasChanged ) ] ]
            ((inputIcon |> Maybe.unwrap [] (\icon -> [ ( "form-input-icon", iconCustom True icon [ class "form-input-icon" ] ) ]))
                ++ keyedSelectedChips
                ++ [ inputElement ]
            )
        , if selectState.active && not (List.isEmpty filteredAvailableItems) then
            Html.div
                [ class "select-menu", id "select-menu" ]
                (filteredAvailableItems
                    |> List.indexedMap
                        (\index item_ ->
                            Html.Lazy.lazy4 viewMenuItem
                                (index == selectState.activeIndex)
                                (menuItemId index)
                                (toMenuItemName item_)
                                (String.join "\n" (toMenuItemTooltip item_))
                                |> map
                                    (\action ->
                                        case action of
                                            MenuSelect ->
                                                Flow.modify (over optic_ (over selected (List.push item_) >> setInputAction clearInputAfterSelect item_))
                                                    |> Flow.seq (Flow.modify (set (optic_ << active) activeAfterSelect))
                                                    |> Flow.seq (onSelect item_)
                                                    |> Flow.seq onChange

                                            MenuHover ->
                                                Flow.modify (set (optic_ << activeIndex) index)
                                    )
                        )
                )

          else
            Html.nothing
        ]


ensureVisible : String -> Flow s ()
ensureVisible menuItemId =
    let
        menuId =
            "select-menu"
    in
    Browser.Dom.getViewportOf menuId
        |> Task.andThen
            (\vp ->
                Browser.Dom.getElement menuItemId
                    |> Task.andThen
                        (\element ->
                            Browser.Dom.getElement menuId
                                |> Task.andThen
                                    (\container ->
                                        let
                                            itemTop =
                                                element.element.y

                                            containerTop =
                                                container.element.y
                                        in
                                        if itemTop < containerTop then
                                            Browser.Dom.setViewportOf menuId vp.viewport.x (vp.viewport.y - (containerTop - itemTop))

                                        else
                                            let
                                                containerBottom =
                                                    containerTop + container.element.height

                                                itemBottom =
                                                    itemTop + element.element.height
                                            in
                                            if itemBottom > containerBottom then
                                                Browser.Dom.setViewportOf menuId vp.viewport.x (vp.viewport.y + (itemBottom - containerBottom))

                                            else
                                                Task.succeed ()
                                    )
                        )
            )
        |> Flow.attemptTask


selected : Lens ls { a | selected : b } b x y
selected =
    lens "selected" .selected (\t selected_ -> { t | selected = selected_ })


input : Lens ls { a | input : b } b x y
input =
    lens "input" .input (\t input_ -> { t | input = input_ })


active : Lens ls { a | active : b } b x y
active =
    lens "active" .active (\t active_ -> { t | active = active_ })


activeIndex : Lens ls { a | activeIndex : b } b x y
activeIndex =
    lens "activeIndex" .activeIndex (\t activeIndex_ -> { t | activeIndex = activeIndex_ })


scrollLock : Lens ls { a | scrollLock : Bool } Bool x y
scrollLock =
    lens "scrollLock" .scrollLock (\t scrollLock_ -> { t | scrollLock = scrollLock_ })
