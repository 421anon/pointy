module Grid exposing
    ( Column, ColumnType(..), SortDir(..)
    , State, Row
    , init, view
    )

{-| A resizable, sortable, filterable data table for delimited text files.

Embed in a parent application via `Flow.via`:

    Grid.view (Flow.via gridLens) gridState


## Column Types

Each column has a `ColumnType` that controls how filtering and sorting
work. `Text` columns use case-insensitive string comparison. `Int` and
`Float` columns parse cell values; rows that fail to parse sort after
successfully-parsed values.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode


-- TYPES


{-| Column data type for filter/sort semantics.
-}
type ColumnType
    = Text
    | Int
    | Float


{-| Sort direction.
-}
type SortDir
    = Asc
    | Desc


{-| Column configuration.
-}
type alias Column =
    { id : String
    , title : String
    , tooltip : String
    , width : Int
    , type_ : ColumnType
    }


{-| A single data row; each cell is a plain string.
-}
type alias Row =
    Array String


{-| Full table state: columns, data, sort and filter state.
-}
type alias State =
    { columns : List Column
    , rows : List Row
    , sortColumn : Maybe ( Int, SortDir )
    , filters : Dict Int String
    , resizing : Maybe { colIndex : Int, startX : Float, startWidth : Int }
    }


-- INIT


{-| Create initial table state from column configurations and row data.
-}
init : List Column -> List Row -> State
init columns rows =
    { columns = columns
    , rows = rows
    , sortColumn = Nothing
    , filters = Dict.empty
    , resizing = Nothing
    }


-- VISIBLE ROWS (filter + sort)


{-| Return sorted and filtered rows.
-}
visibleRows : State -> List Row
visibleRows model =
    let
        filtered =
            List.filter (rowPassesFilters model) model.rows

        sorted =
            case model.sortColumn of
                Just ( colIndex, Asc ) ->
                    List.sortWith (compareRowsByColumn colIndex model.columns) filtered

                Just ( colIndex, Desc ) ->
                    List.sortWith (\a b -> reverseOrder (compareRowsByColumn colIndex model.columns a b)) filtered

                Nothing ->
                    filtered
    in
    sorted


rowPassesFilters : State -> Row -> Bool
rowPassesFilters { columns, filters } row =
    Dict.foldl
        (\colIndex filterValue acc ->
            if not acc then
                False

            else
                let
                    cell =
                        Array.get colIndex row |> Maybe.withDefault ""

                    col =
                        columns
                            |> List.drop colIndex
                            |> List.head
                            |> Maybe.withDefault
                                { id = "", title = "", tooltip = "", width = 88, type_ = Text }

                    predicate =
                        case col.type_ of
                            Int ->
                                parseIntFilter filterValue

                            _ ->
                                parseFilter filterValue
                in
                predicate cell
        )
        True
        filters


compareRowsByColumn : Int -> List Column -> Row -> Row -> Order
compareRowsByColumn colIndex columns a b =
    let
        col =
            columns
                |> List.drop colIndex
                |> List.head
                |> Maybe.withDefault
                    { id = "", title = "", tooltip = "", width = 88, type_ = Text }

        ca =
            Array.get colIndex a |> Maybe.withDefault ""

        cb =
            Array.get colIndex b |> Maybe.withDefault ""
    in
    case col.type_ of
        Int ->
            case ( String.toInt (String.trim ca), String.toInt (String.trim cb) ) of
                ( Just ia, Just ib ) ->
                    compare ia ib

                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Just _ ) ->
                    GT

                ( Nothing, Nothing ) ->
                    compare (String.toLower ca) (String.toLower cb)

        Float ->
            case ( String.toFloat (String.trim ca), String.toFloat (String.trim cb) ) of
                ( Just fa, Just fb ) ->
                    compare fa fb

                ( Just _, Nothing ) ->
                    LT

                ( Nothing, Just _ ) ->
                    GT

                ( Nothing, Nothing ) ->
                    compare (String.toLower ca) (String.toLower cb)

        Text ->
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


-- FILTER PARSING


parseFilter : String -> (String -> Bool)
parseFilter filterValue =
    if filterValue == "" then
        always True

    else if String.startsWith "=" filterValue then
        \cell -> String.toLower cell == String.toLower (String.dropLeft 1 filterValue |> String.trim)

    else if String.startsWith "<" filterValue then
        \cell ->
            let
                needle =
                    String.dropLeft 1 filterValue |> String.trim
            in
            case String.toFloat cell of
                Just cellNum ->
                    case String.toFloat needle of
                        Just needleNum ->
                            cellNum < needleNum

                        Nothing ->
                            String.toLower cell < String.toLower needle

                Nothing ->
                    String.toLower cell < String.toLower needle

    else if String.startsWith ">" filterValue then
        \cell ->
            let
                needle =
                    String.dropLeft 1 filterValue |> String.trim
            in
            case String.toFloat cell of
                Just cellNum ->
                    case String.toFloat needle of
                        Just needleNum ->
                            cellNum > needleNum

                        Nothing ->
                            String.toLower cell > String.toLower needle

                Nothing ->
                    String.toLower cell > String.toLower needle

    else
        \cell -> String.contains (String.toLower filterValue) (String.toLower cell)


parseIntFilter : String -> (String -> Bool)
parseIntFilter filterValue =
    if filterValue == "" then
        always True

    else if String.startsWith "=" filterValue then
        let
            needle =
                String.dropLeft 1 filterValue |> String.trim
        in
        \cell ->
            case String.toInt cell of
                Just cellVal ->
                    case String.toInt needle of
                        Just needleVal ->
                            cellVal == needleVal

                        Nothing ->
                            False

                Nothing ->
                    False

    else if String.startsWith "<" filterValue then
        let
            needle =
                String.dropLeft 1 filterValue |> String.trim
        in
        \cell ->
            case String.toInt cell of
                Just cellVal ->
                    case String.toInt needle of
                        Just needleVal ->
                            cellVal < needleVal

                        Nothing ->
                            False

                Nothing ->
                    False

    else if String.startsWith ">" filterValue then
        let
            needle =
                String.dropLeft 1 filterValue |> String.trim
        in
        \cell ->
            case String.toInt cell of
                Just cellVal ->
                    case String.toInt needle of
                        Just needleVal ->
                            cellVal > needleVal

                        Nothing ->
                            False

                Nothing ->
                    False

    else
        \cell ->
            case String.toInt cell of
                Just cellVal ->
                    String.contains (String.toLower filterValue) (String.toLower (String.fromInt cellVal))

                Nothing ->
                    False


-- STATE TRANSITIONS


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
    { model | sortColumn = newSort }


setFilter : Int -> String -> State -> State
setFilter colIndex value model =
    let
        newFilters =
            if value == "" then
                Dict.remove colIndex model.filters

            else
                Dict.insert colIndex value model.filters
    in
    { model | filters = newFilters }


-- VIEW


{-| Render the table.

The callback `embed : Flow State () -> Flow parent ()` should be
`Flow.via lens` where `lens` targets the `State` in the parent model.

-}
view : (Flow State () -> Flow parent ()) -> State -> Html (Flow parent ())
view embed model =
    let
        activeRows =
            visibleRows model

        totalWidth =
            List.foldl (\col acc -> acc + col.width) 0 model.columns

        viewCol col =
            Html.col
                [ Html.Attributes.style "width" (String.fromInt col.width ++ "px") ]
                []

        viewHeaderCell index col =
            let
                sorted =
                    case model.sortColumn of
                        Just ( i, _ ) ->
                            i == index

                        Nothing ->
                            False

                sortDir =
                    model.sortColumn |> Maybe.map Tuple.second

                sortArrow =
                    if sorted then
                        Html.span
                            [ Html.Attributes.class "sort-arrow"
                            , Html.Attributes.classList
                                [ ( "asc", sortDir == Just Asc )
                                , ( "desc", sortDir == Just Desc )
                                ]
                            ]
                            []

                    else
                        Html.text ""

                currentFilter =
                    Dict.get index model.filters |> Maybe.withDefault ""
            in
            Html.th
                [ Html.Attributes.style "width" (String.fromInt col.width ++ "px")
                , Html.Attributes.style "overflow" "visible"
                , Html.Attributes.style "cursor" "pointer"
                , Html.Attributes.style "font-weight" "normal"
                , Html.Attributes.style "font-style"
                    (if sorted then
                        "italic"

                     else
                        "normal"
                    )
                , Html.Attributes.style "background-image" "linear-gradient(var(--bg-secondary), var(--bg-elevated))"
                , Html.Events.onClick (embed (Flow.modify (toggleSort index)))
                , Html.Attributes.title col.tooltip
                ]
                [ Html.div
                    [ Html.Attributes.style "display" "flex"
                    , Html.Attributes.style "flex-direction" "column"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "overflow" "hidden"
                    ]
                    [ Html.span
                        [ Html.Attributes.style "flex" "0 0 auto"
                        , Html.Attributes.style "overflow" "hidden"
                        , Html.Attributes.style "white-space" "nowrap"
                        , Html.Attributes.style "line-height" "28px"
                        , Html.Attributes.style "padding" "0 2px"
                        ]
                        [ Html.text col.title, sortArrow ]
                    , Html.input
                        [ Html.Attributes.style "width" "calc(100% - 12px)"
                        , Html.Attributes.style "height" "22px"
                        , Html.Attributes.style "flex" "0 0 auto"
                        , Html.Attributes.style "margin" "0 6px 2px"
                        , Html.Attributes.style "padding" "0 6px"
                        , Html.Attributes.class "delimited-grid-filter-input"
                        , Html.Attributes.type_ "text"
                        , Html.Attributes.value currentFilter
                        , Html.Events.onInput (\v -> embed (Flow.modify (setFilter index v)))
                        , Html.Events.stopPropagationOn "click" (Decode.succeed ( embed Flow.none, True ))
                        , Html.Attributes.placeholder ""
                        ]
                        []
                    ]
                , Html.div
                    [ Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "right" "0"
                    , Html.Attributes.style "top" "0"
                    , Html.Attributes.style "bottom" "0"
                    , Html.Attributes.style "width" "5px"
                    , Html.Attributes.style "cursor" "col-resize"
                    , Html.Attributes.attribute "data-col-index" (String.fromInt index)
                    , Html.Attributes.class "delimited-grid-resize-handle"
                    ]
                    []
                ]

        viewCell index col row =
            let
                cell =
                    Array.get index row |> Maybe.withDefault ""
            in
            Html.td
                []
                [ Html.text cell ]

        viewRow row =
            Html.tr
                [ Html.Attributes.class "delimited-grid-body-row"
                ]
                (List.indexedMap (\i col -> viewCell i col row) model.columns)

        colIndexes =
            List.indexedMap (\i _ -> i) model.columns
    in
    Html.div
        [ Html.Attributes.class "delimited-grid-viewer"
        , Html.Attributes.style "max-width" "100%"
        , Html.Attributes.style "overflow" "auto"
        , Html.Attributes.style "resize" "vertical"
        , Html.Attributes.style "max-height" "70vh"
        , Html.Attributes.style "background" "var(--bg-elevated)"
        , Html.Attributes.style "border" "1px solid var(--border-color)"
        , Html.Attributes.style "border-radius" "var(--radius-sm)"
        ]
        [ Html.table
            [ Html.Attributes.style "table-layout" "fixed"
            , Html.Attributes.style "width" (String.fromInt totalWidth ++ "px")
            , Html.Attributes.style "min-width" "100%"
            , Html.Attributes.style "border-collapse" "separate"
            , Html.Attributes.style "border-spacing" "0"
            , Html.Attributes.class "delimited-grid-table"
            ]
            [ Html.colgroup [] (List.map viewCol model.columns)
            , Html.thead []
                [ Html.tr
                    [ Html.Attributes.class "delimited-grid-header"
                    ]
                    (List.map2 (\i c -> viewHeaderCell i c) colIndexes model.columns)
                ]
            , Html.tbody []
                (List.map viewRow activeRows)
            ]
        ]
