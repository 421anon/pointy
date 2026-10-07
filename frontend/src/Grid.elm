module Grid exposing
    ( Column
    , ColumnType(..)
    , Row
    , SortDir(..)
    , State
    , init
    , showPlain
    , view
    )

import Array exposing (Array)
import Dict exposing (Dict)
import Flow exposing (Flow)
import Grid.Aggregate as Aggregate
import Grid.Filter as Filter exposing (Filter)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import InfiniteList
import Json.Decode as Decode
import Maybe.Extra as Maybe
import View.Icons exposing (icon)


type ColumnType
    = Text
    | Int
    | Float


type SortDir
    = Asc
    | Desc


type alias Column =
    { id : String
    , title : String
    , width : Int
    , type_ : ColumnType
    }


type alias Row =
    Array String


type alias State =
    { columns : List Column
    , rows : List Row
    , rowCount : Int
    , sortColumn : Maybe ( Int, SortDir )
    , filters : Dict Int String
    , infiniteList : InfiniteList.Model
    , visible : Array ( Int, Row )
    , showGrid : Bool
    , reducers : List Aggregate.Reducer
    , summaryValues : List (List String)
    , summaryMenuOpen : Bool
    }


init : List Column -> List Row -> State
init columns rows =
    refreshVisible
        { columns = columns
        , rows = rows
        , rowCount = List.length rows
        , sortColumn = Nothing
        , filters = Dict.empty
        , infiniteList = InfiniteList.init
        , visible = Array.empty
        , showGrid = True
        , reducers = []
        , summaryValues = []
        , summaryMenuOpen = False
        }


summaryGutterWidth : Int
summaryGutterWidth =
    96


filterHint : ColumnType -> String
filterHint colType =
    if isNumericType colType then
        "Number: >10, >=10, <5, <=5, =7, !=0 or range 10-100"

    else
        "Text: substring, =exact or !=exact"


isNumericType : ColumnType -> Bool
isNumericType colType =
    case colType of
        Int ->
            True

        Float ->
            True

        Text ->
            False


defaultColumn : Column
defaultColumn =
    { id = "", title = "", width = 88, type_ = Text }


columnAt : Int -> List Column -> Column
columnAt index columns =
    columns |> List.drop index |> List.head |> Maybe.withDefault defaultColumn


parsedFilters : State -> List ( Int, Filter )
parsedFilters { columns, filters } =
    filters
        |> Dict.toList
        |> List.filterMap
            (\( index, value ) ->
                if String.trim value == "" then
                    Nothing

                else
                    parseColumnFilter (columnAt index columns) value
                        |> Maybe.map (Tuple.pair index)
            )


parseColumnFilter : Column -> String -> Maybe Filter
parseColumnFilter column value =
    Filter.parse (isNumericType column.type_) value


invalidFilter : Column -> String -> Bool
invalidFilter column value =
    String.trim value /= "" && Maybe.isNothing (parseColumnFilter column value)


cellAt : Int -> Row -> String
cellAt index row =
    Array.get index row |> Maybe.withDefault ""


rowPasses : List ( Int, Filter ) -> Row -> Bool
rowPasses parsed row =
    List.all
        (\( index, filter ) -> Filter.matches filter (cellAt index row))
        parsed


visibleRows : State -> List ( Int, Row )
visibleRows model =
    let
        parsed =
            parsedFilters model

        filtered =
            model.rows
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, row ) -> rowPasses parsed row)
    in
    case model.sortColumn of
        Just ( colIndex, Asc ) ->
            List.sortWith (\( _, a ) ( _, b ) -> compareRowsByColumn colIndex model.columns a b) filtered

        Just ( colIndex, Desc ) ->
            List.sortWith (\( _, a ) ( _, b ) -> reverseOrder (compareRowsByColumn colIndex model.columns a b)) filtered

        Nothing ->
            filtered


refreshVisible : State -> State
refreshVisible state =
    let
        visible =
            visibleRows state
    in
    { state
        | visible = Array.fromList visible
        , summaryValues = summaryValuesFor state visible
    }


summaryValuesFor : State -> List ( Int, Row ) -> List (List String)
summaryValuesFor state visible =
    List.map (columnSummaryFor state.columns visible) state.reducers


columnSummaryFor :
    List Column
    -> List ( Int, Row )
    -> Aggregate.Reducer
    -> List String
columnSummaryFor columns visible reducer =
    List.indexedMap
        (\index column ->
            if Aggregate.appliesTo (isNumericType column.type_) reducer then
                Aggregate.compute reducer (cellTexts index visible)

            else
                ""
        )
        columns


cellTexts : Int -> List ( Int, Row ) -> List String
cellTexts index visible =
    List.map (\( _, row ) -> cellAt index row) visible


compareRowsByColumn : Int -> List Column -> Row -> Row -> Order
compareRowsByColumn colIndex columns a b =
    let
        ca =
            Array.get colIndex a |> Maybe.withDefault ""

        cb =
            Array.get colIndex b |> Maybe.withDefault ""
    in
    case (columnAt colIndex columns).type_ of
        Int ->
            numericCompare String.toInt ca cb

        Float ->
            numericCompare String.toFloat ca cb

        Text ->
            compare (String.toLower ca) (String.toLower cb)


numericCompare : (String -> Maybe comparable) -> String -> String -> Order
numericCompare parseNum ca cb =
    case ( parseNum (String.trim ca), parseNum (String.trim cb) ) of
        ( Just x, Just y ) ->
            compare x y

        ( Just _, Nothing ) ->
            LT

        ( Nothing, Just _ ) ->
            GT

        ( Nothing, Nothing ) ->
            compare (String.toLower ca) (String.toLower cb)


reverseOrder : Order -> Order
reverseOrder order =
    case order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


toggleSort : Int -> State -> State
toggleSort colIndex model =
    let
        newSort =
            case model.sortColumn of
                Just ( idx, Asc ) ->
                    if idx == colIndex then
                        Just ( idx, Desc )

                    else
                        Just ( colIndex, Asc )

                Just ( idx, Desc ) ->
                    if idx == colIndex then
                        Nothing

                    else
                        Just ( colIndex, Asc )

                Nothing ->
                    Just ( colIndex, Asc )
    in
    refreshVisible { model | sortColumn = newSort }


setFilter : Int -> String -> State -> State
setFilter colIndex value model =
    let
        newFilters =
            if value == "" then
                Dict.remove colIndex model.filters

            else
                Dict.insert colIndex value model.filters
    in
    refreshVisible { model | filters = newFilters }


clearFilters : State -> State
clearFilters model =
    refreshVisible { model | filters = Dict.empty }


hasActiveFilters : State -> Bool
hasActiveFilters model =
    List.any (\( _, value ) -> String.trim value /= "") (Dict.toList model.filters)


toggleReducer : Aggregate.Reducer -> State -> State
toggleReducer reducer model =
    let
        active =
            if List.member reducer model.reducers then
                List.filter ((/=) reducer) model.reducers

            else
                reducer :: model.reducers
    in
    refreshVisible
        { model | reducers = List.filter (\r -> List.member r active) Aggregate.catalog }


toggleSummaryMenu : State -> State
toggleSummaryMenu model =
    { model | summaryMenuOpen = not model.summaryMenuOpen }


boolString : Bool -> String
boolString value =
    if value then
        "true"

    else
        "false"


stopClick : msg -> Html.Attribute msg
stopClick msg =
    Html.Events.stopPropagationOn "click" (Decode.succeed ( msg, True ))


gutterCells : Bool -> List (Html msg)
gutterCells hasSummaries =
    if hasSummaries then
        [ Html.div [ Html.Attributes.class "delimited-grid-gutter" ] [] ]

    else
        []


view : (Flow State () -> msg) -> (() -> Html msg) -> State -> Html msg
view toMsg viewPlainContent model =
    Html.div [ Html.Attributes.class "delimited-grid-shell" ]
        [ Html.div [ Html.Attributes.class "delimited-grid-toolbar" ]
            [ Html.viewIf model.showGrid
                (Html.span [ Html.Attributes.class "delimited-grid-row-count" ]
                    [ Html.text (rowCountLabel model) ]
                )
            , Html.viewIf model.showGrid (clearFiltersButton toMsg model)
            , Html.viewIf model.showGrid (summaryMenu toMsg model)
            , viewModeToggle model.showGrid (stopClick (toMsg (Flow.modify toggleShowGrid)))
            ]
        , if model.showGrid then
            Html.map toMsg (viewGrid model)

          else
            viewPlainContent ()
        ]


clearFiltersButton : (Flow State () -> msg) -> State -> Html msg
clearFiltersButton toMsg model =
    Html.viewIf (hasActiveFilters model)
        (Html.button
            [ Html.Attributes.class "btn delimited-grid-toolbar-btn"
            , Html.Attributes.type_ "button"
            , Html.Attributes.title "Clear filters"
            , stopClick (toMsg (Flow.modify clearFilters))
            ]
            [ icon True "filter_alt_off"
            , Html.span [ Html.Attributes.class "delimited-grid-toolbar-btn-label" ]
                [ Html.text "Clear filters" ]
            ]
        )


summaryMenu : (Flow State () -> msg) -> State -> Html msg
summaryMenu toMsg model =
    Html.div
        [ Html.Attributes.class "delimited-grid-summary-control"
        , stopClick (toMsg Flow.none)
        ]
        [ Html.button
            [ Html.Attributes.class "btn delimited-grid-toolbar-btn"
            , Html.Attributes.type_ "button"
            , Html.Attributes.title "Summary rows"
            , Html.Attributes.attribute "aria-haspopup" "true"
            , Html.Attributes.attribute "aria-expanded" (boolString model.summaryMenuOpen)
            , stopClick (toMsg (Flow.modify toggleSummaryMenu))
            ]
            [ icon True "functions"
            , Html.span [ Html.Attributes.class "delimited-grid-toolbar-btn-label" ]
                [ Html.text "Summary rows" ]
            ]
        , Html.viewIf model.summaryMenuOpen
            (Html.div [ Html.Attributes.class "delimited-grid-summary-menu" ]
                (List.map (viewSummaryMenuItem toMsg model.reducers) Aggregate.catalog)
            )
        ]


viewSummaryMenuItem :
    (Flow State () -> msg)
    -> List Aggregate.Reducer
    -> Aggregate.Reducer
    -> Html msg
viewSummaryMenuItem toMsg active reducer =
    let
        isActive =
            List.member reducer active
    in
    Html.button
        [ Html.Attributes.class "delimited-grid-summary-menu-item"
        , Html.Attributes.classList [ ( "active", isActive ) ]
        , Html.Attributes.type_ "button"
        , Html.Attributes.attribute "aria-pressed" (boolString isActive)
        , stopClick (toMsg (Flow.modify (toggleReducer reducer)))
        ]
        [ icon True
            (if isActive then
                "check_box"

             else
                "check_box_outline_blank"
            )
        , Html.span [] [ Html.text (Aggregate.label reducer) ]
        ]


toggleShowGrid : State -> State
toggleShowGrid model =
    if model.showGrid then
        { model | showGrid = False, summaryMenuOpen = False }

    else
        { model | showGrid = True, infiniteList = InfiniteList.init }


showPlain : State -> State
showPlain model =
    { model | showGrid = False, summaryMenuOpen = False }


viewModeToggle : Bool -> Html.Attribute msg -> Html msg
viewModeToggle showingGrid clickAttr =
    Html.button
        [ Html.Attributes.class "btn file-view-mode-toggle"
        , Html.Attributes.type_ "button"
        , Html.Attributes.title
            (if showingGrid then
                "Show regular file viewer"

             else
                "Show grid viewer"
            )
        , Html.Attributes.attribute "aria-pressed"
            (if showingGrid then
                "true"

             else
                "false"
            )
        , clickAttr
        ]
        [ icon True
            (if showingGrid then
                "description"

             else
                "table_chart"
            )
        , Html.span [ Html.Attributes.class "file-view-mode-toggle-label" ]
            [ Html.text
                (if showingGrid then
                    "Regular viewer"

                 else
                    "Grid viewer"
                )
            ]
        ]


viewGrid : State -> Html (Flow State ())
viewGrid model =
    let
        hasSummaries =
            not (List.isEmpty model.reducers)

        summaryRows =
            List.map2 Tuple.pair model.reducers model.summaryValues

        gutterWidth =
            if hasSummaries then
                summaryGutterWidth

            else
                0

        totalWidth =
            List.foldl (\col acc -> acc + col.width) 0 model.columns
                + gutterWidth
    in
    Html.div
        [ Html.Attributes.class "delimited-grid-viewer"
        , InfiniteList.onScroll (\listModel -> Flow.modify (setInfiniteList listModel))
        ]
        [ Html.div
            [ Html.Attributes.class "delimited-grid"
            , Html.Attributes.style "width" (String.fromInt totalWidth ++ "px")
            ]
            [ Html.div [ Html.Attributes.class "delimited-grid-sticky" ]
                (Html.div [ Html.Attributes.class "delimited-grid-header" ]
                    (gutterCells hasSummaries ++ List.indexedMap (viewHeaderCell model) model.columns)
                    :: List.map (viewSummaryRow model) summaryRows
                )
            , InfiniteList.viewArray (listConfig model.columns hasSummaries) model.infiniteList model.visible
            ]
        ]


rowCountLabel : State -> String
rowCountLabel model =
    let
        filteredCount =
            Array.length model.visible

        filteredLabel =
            rowLabel filteredCount
    in
    if Dict.isEmpty model.filters || filteredCount == model.rowCount then
        filteredLabel

    else
        filteredLabel ++ " (filtered from " ++ rowLabel model.rowCount ++ ")"


rowLabel : Int -> String
rowLabel count =
    String.fromInt count
        ++ " "
        ++ (if count == 1 then
                "row"

            else
                "rows"
           )


rowHeight : Int
rowHeight =
    28


viewportEstimate : Int
viewportEstimate =
    1000


listConfig :
    List Column
    -> Bool
    -> InfiniteList.Config ( Int, Row ) (Flow State ())
listConfig columns hasSummaries =
    InfiniteList.config
        { itemView = \_ _ ( _, row ) -> viewRow columns hasSummaries row
        , itemHeight = InfiniteList.withConstantHeight rowHeight
        , containerHeight = viewportEstimate
        }
        |> InfiniteList.withOffset viewportEstimate


setInfiniteList : InfiniteList.Model -> State -> State
setInfiniteList listModel state =
    { state | infiniteList = listModel }


columnWidthStyle : Int -> Column -> Html.Attribute msg
columnWidthStyle index col =
    Html.Attributes.style "width"
        ("var(--dg-col-" ++ String.fromInt index ++ ", " ++ String.fromInt col.width ++ "px)")


viewHeaderCell : State -> Int -> Column -> Html (Flow State ())
viewHeaderCell model index col =
    let
        sorted =
            case model.sortColumn of
                Just ( i, _ ) ->
                    i == index

                Nothing ->
                    False

        sortArrow =
            case model.sortColumn of
                Just ( i, dir ) ->
                    if i == index then
                        Html.span
                            [ Html.Attributes.class "sort-arrow"
                            , Html.Attributes.classList
                                [ ( "asc", dir == Asc )
                                , ( "desc", dir == Desc )
                                ]
                            ]
                            []

                    else
                        Html.nothing

                Nothing ->
                    Html.nothing

        currentFilter =
            Dict.get index model.filters |> Maybe.withDefault ""

        filterInvalid =
            invalidFilter col currentFilter
    in
    Html.div
        [ Html.Attributes.class "delimited-grid-th"
        , columnWidthStyle index col
        , Html.Attributes.classList [ ( "sorted", sorted ) ]
        , Html.Events.onClick (Flow.modify (toggleSort index))
        , Html.Attributes.title (filterHint col.type_)
        ]
        [ Html.div [ Html.Attributes.class "delimited-grid-header-cell" ]
            [ Html.span [ Html.Attributes.class "delimited-grid-header-title" ]
                [ Html.text col.title, sortArrow ]
            , Html.input
                [ Html.Attributes.class "delimited-grid-filter-input"
                , Html.Attributes.classList [ ( "invalid", filterInvalid ) ]
                , Html.Attributes.type_ "text"
                , Html.Attributes.value currentFilter
                , Html.Attributes.title (filterHint col.type_)
                , Html.Attributes.attribute "aria-invalid" (boolString filterInvalid)
                , Html.Events.onInput (\v -> Flow.modify (setFilter index v))
                , stopClick Flow.none
                , Html.Attributes.placeholder ""
                ]
                []
            ]
        , Html.div
            [ Html.Attributes.class "delimited-grid-resize-handle"
            , Html.Attributes.attribute "data-col-index" (String.fromInt index)
            ]
            []
        ]


viewRow : List Column -> Bool -> Row -> Html msg
viewRow columns hasSummaries row =
    Html.div [ Html.Attributes.class "delimited-grid-body-row" ]
        (gutterCells hasSummaries ++ List.indexedMap (\i column -> viewCell i column row) columns)


viewCell : Int -> Column -> Row -> Html msg
viewCell index column row =
    Html.div
        [ Html.Attributes.class "delimited-grid-td"
        , columnWidthStyle index column
        ]
        [ Html.text (cellAt index row) ]


viewSummaryRow :
    State
    -> ( Aggregate.Reducer, List String )
    -> Html (Flow State ())
viewSummaryRow model ( reducer, values ) =
    let
        label =
            Aggregate.label reducer

        removeLabel =
            "Remove " ++ label ++ " row"
    in
    Html.div [ Html.Attributes.class "delimited-grid-summary-row" ]
        (Html.div
            [ Html.Attributes.class "delimited-grid-gutter delimited-grid-summary-gutter" ]
            [ Html.span [ Html.Attributes.class "delimited-grid-summary-label" ] [ Html.text label ]
            , Html.button
                [ Html.Attributes.class "icon-btn delimited-grid-summary-remove"
                , Html.Attributes.type_ "button"
                , Html.Attributes.attribute "aria-label" removeLabel
                , Html.Attributes.title removeLabel
                , stopClick (Flow.modify (toggleReducer reducer))
                ]
                [ icon True "close" ]
            ]
            :: List.indexedMap
                (\index column -> viewSummaryCell label index column (valueAt index values))
                model.columns
        )


viewSummaryCell : String -> Int -> Column -> String -> Html msg
viewSummaryCell label index column value =
    Html.div
        [ Html.Attributes.class "delimited-grid-summary-cell"
        , columnWidthStyle index column
        , Html.Attributes.title (label ++ " of " ++ column.title)
        ]
        [ Html.text value ]


valueAt : Int -> List String -> String
valueAt index values =
    List.drop index values |> List.head |> Maybe.withDefault ""
