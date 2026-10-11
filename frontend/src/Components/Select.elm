module Components.Select exposing (Item, RelatedItem, SelectState, initSelectState, itemRef, rankedItems, selected, view)

import Accessors exposing (An_Optic, Lens, get, lens, over, set)
import Browser.Dom
import Extra.Accessors exposing (remkT)
import Extra.Decode as Decode
import Flow exposing (Flow)
import Fuzzy
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


type alias SelectState ref =
    { selected : List (Item ref)
    , input : String
    , active : Bool
    , activeIndex : Int
    , scrollLock : Bool
    }


type alias Item ref =
    { id : Maybe Int
    , name : String
    , mProjectId : Maybe Int
    , ref : Maybe ref
    }


type alias RelatedItem ref =
    { item : Item ref
    , detail : String
    }


itemRef : Lens ls (Item ref) (Maybe ref) x y
itemRef =
    lens ".ref" .ref (\item ref_ -> { item | ref = ref_ })


initSelectState : SelectState ref
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


type alias MenuItem ref tag =
    { item : Item ref
    , match : Fuzzy.Match tag
    , isRelated : Bool
    , detail : Maybe String
    }


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


viewMenuItem : Bool -> String -> List Fuzzy.Segment -> Maybe (List Fuzzy.Segment) -> String -> Html MenuAction
viewMenuItem isActive domId label mDetail tooltip =
    Html.div
        [ class "select-menu-item"
        , id domId
        , classList [ ( "active", isActive ) ]
        , Events.preventDefaultOn "mousedown" (Decode.succeed ( MenuSelect, True ))
        , title tooltip
        ]
        [ Html.span [ class "select-menu-item-label" ] (List.map viewSegment label)
        , Html.viewMaybe
            (\detail -> Html.span [ class "select-menu-item-detail" ] (List.map viewSegment detail))
            mDetail
        ]


viewRelatedHeading : Html (Flow s ())
viewRelatedHeading =
    Html.div
        [ class "select-menu-heading"
        , Events.preventDefaultOn "mousedown" (Decode.succeed ( Flow.pure (), True ))
        ]
        [ Html.text "Related" ]


rankedItems : Fuzzy.Query -> (a -> Fuzzy.Document tag) -> List a -> List ( a, Fuzzy.Match tag )
rankedItems query toDocument items =
    let
        ranked =
            Fuzzy.rank query toDocument items
    in
    if Fuzzy.isEmpty query then
        ranked

    else
        List.take 50 ranked


viewSegment : Fuzzy.Segment -> Html msg
viewSegment segment =
    case segment of
        Fuzzy.Plain text ->
            Html.text text

        Fuzzy.Marked text ->
            Html.mark [ class "select-menu-item-match" ] [ Html.text text ]


view :
    { optic : An_Optic pr ls s (SelectState ref)
    , selectState : SelectState ref
    , selected_ : List (Item ref)
    , availableItems : List (Item ref)
    , readOnly : Bool
    , hasChanged : Bool
    , label : String
    , mHint : Maybe String
    , placeholder : String
    , inputIcon : Maybe String
    , toInputItemName : Item ref -> String
    , toInputItemTooltip : Item ref -> List String
    , onInputItemClick : Item ref -> Maybe (Flow s ())
    , toSearchDocument : Item ref -> Fuzzy.Document tag
    , related : List (RelatedItem ref)
    , toMenuItemName : Item ref -> String
    , toMenuItemTooltip : Item ref -> List String
    , toMenuItemDetail : Fuzzy.Query -> Fuzzy.Match tag -> Item ref -> Maybe String
    , activeAfterSelect : Bool
    , clearInputAfterSelect : Bool
    , onChange : Flow s ()
    , onInputChange : String -> Flow s ()
    , onRemove : Item ref -> Flow s ()
    , onSelect : Fuzzy.Match tag -> Item ref -> Flow s ()
    , alignRight : Bool
    , inputItemStyle : Item ref -> List (Html.Attribute Never)
    }
    -> Html (Flow s ())
view { optic, selectState, selected_, availableItems, readOnly, hasChanged, label, mHint, placeholder, inputIcon, toInputItemName, toInputItemTooltip, onInputItemClick, toSearchDocument, related, toMenuItemName, toMenuItemTooltip, toMenuItemDetail, onChange, onInputChange, onRemove, activeAfterSelect, clearInputAfterSelect, onSelect, alignRight, inputItemStyle } =
    let
        optic_ =
            remkT optic

        query =
            Fuzzy.parse selectState.input

        menuItems : List (MenuItem ref tag)
        menuItems =
            let
                filteredAvailableItems =
                    rankedItems query toSearchDocument availableItems

                fuzzyEntries =
                    List.map
                        (\( item, match ) ->
                            { item = item
                            , match = match
                            , isRelated = False
                            , detail = toMenuItemDetail query match item
                            }
                        )
                        filteredAvailableItems

                relatedEntries : List (MenuItem ref tag)
                relatedEntries =
                    let
                        shownIds : List Int
                        shownIds =
                            List.filterMap (\( item, _ ) -> item.id) filteredAvailableItems

                        notShown entry =
                            case entry.item.id of
                                Just id ->
                                    not (List.member id shownIds)

                                Nothing ->
                                    True
                    in
                    related
                        |> List.filter notShown
                        |> List.map
                            (\entry ->
                                { item = entry.item
                                , match = { score = 0, fields = [] }
                                , isRelated = True
                                , detail = Just entry.detail
                                }
                            )
            in
            fuzzyEntries ++ relatedEntries

        menuItemDetail : MenuItem ref tag -> Maybe (List Fuzzy.Segment)
        menuItemDetail menuItem =
            menuItem.detail |> Maybe.map (Fuzzy.highlight Fuzzy.Words query)

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
                                                                    min (List.length menuItems - 1) (activeIndex_ + 1)
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
                                          , menuItems
                                                |> List.drop selectState.activeIndex
                                                |> List.head
                                                |> Maybe.unwrap Flow.none
                                                    (\menuItem ->
                                                        Flow.modify (over optic_ (over selected (List.push menuItem.item) >> setInputAction clearInputAfterSelect menuItem.item >> set activeIndex 0))
                                                            |> Flow.seq (Flow.modify (set (optic_ << active) activeAfterSelect))
                                                            |> Flow.seq (onSelect menuItem.match menuItem.item)
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
                            , Events.onInput (\inp -> Flow.modify (over optic_ (set input inp << set active True << set activeIndex 0)) |> Flow.seq (onInputChange inp))
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
        [ Html.viewIf (not <| String.isEmpty label) <|
            case mHint of
                Nothing ->
                    Html.label [ class "form-label" ] [ Html.text label ]

                Just hint ->
                    Html.div [ class "form-label-group" ]
                        [ Html.label [ class "form-label" ] [ Html.text label ]
                        , Html.small [ class "form-hint" ] [ Html.text hint ]
                        ]
        , Html.Keyed.node "div"
            [ class "tag-wrapper", class "form-input", classList [ ( "disabled", readOnly ), ( "field-changed", hasChanged ) ] ]
            ((inputIcon |> Maybe.unwrap [] (\icon -> [ ( "form-input-icon", iconCustom True icon [ class "form-input-icon" ] ) ]))
                ++ keyedSelectedChips
                ++ [ inputElement ]
            )
        , if not readOnly && selectState.active && not (List.isEmpty menuItems) then
            let
                firstRelatedIndex =
                    List.findIndex .isRelated menuItems
            in
            Html.div
                [ class "select-menu", id "select-menu" ]
                (menuItems
                    |> List.indexedMap
                        (\index menuItem ->
                            [ Html.viewIf (Just index == firstRelatedIndex) viewRelatedHeading
                            , viewMenuItem
                                (index == selectState.activeIndex)
                                (menuItemId index)
                                (Fuzzy.highlight Fuzzy.Loose query (toMenuItemName menuItem.item))
                                (menuItemDetail menuItem)
                                (String.join "\n" (toMenuItemTooltip menuItem.item))
                                |> map
                                    (\MenuSelect ->
                                        Flow.modify (over optic_ (over selected (List.push menuItem.item) >> setInputAction clearInputAfterSelect menuItem.item))
                                            |> Flow.seq (Flow.modify (set (optic_ << active) activeAfterSelect))
                                            |> Flow.seq (onSelect menuItem.match menuItem.item)
                                            |> Flow.seq onChange
                                    )
                            ]
                        )
                    |> List.concat
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
