module Grid exposing
    ( Column
    , ColumnType(..)
    , Row
    , SortDir(..)
    , State
    , columnTypeLabel
    , init
    , view
    )

{-| Resizable, sortable, filterable data table for delimited text files.

`Text` columns use case-insensitive string comparison; `Int`/`Float` columns
parse cell values and sort unparseable rows after parsed ones.

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode


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
    , tooltip : String
    , width : Int
    , type_ : ColumnType
    }


type alias Row =
    Array String


type alias State =
    { columns : List Column
    , rows : List Row
    , sortColumn : Maybe ( Int, SortDir )
    , filters : Dict Int String
    }


init : List Column -> List Row -> State
init columns rows =
    { columns = columns
    , rows = rows
    , sortColumn = Nothing
    , filters = Dict.empty
    }


columnTypeLabel : ColumnType -> String
columnTypeLabel colType =
    case colType of
        Int ->
            "int"

        Float ->
            "float"

        Text ->
            "string"


defaultColumn : Column
defaultColumn =
    { id = "", title = "", tooltip = "", width = 88, type_ = Text }


columnAt : Int -> List Column -> Column
columnAt index columns =
    columns |> List.drop index |> List.head |> Maybe.withDefault defaultColumn



-- VISIBLE ROWS (filter + sort)


visibleRows : State -> List Row
visibleRows model =
    let
        filtered =
            List.filter (rowPassesFilters model) model.rows
    in
    case model.sortColumn of
        Just ( colIndex, Asc ) ->
            List.sortWith (compareRowsByColumn colIndex model.columns) filtered

        Just ( colIndex, Desc ) ->
            List.sortWith (\a b -> reverseOrder (compareRowsByColumn colIndex model.columns a b)) filtered

        Nothing ->
            filtered


rowPassesFilters : State -> Row -> Bool
rowPassesFilters { columns, filters } row =
    Dict.foldl
        (\colIndex filterValue acc ->
            if not acc then
                False

            else
                let
                    parser =
                        case (columnAt colIndex columns).type_ of
                            Int ->
                                parseIntFilter

                            _ ->
                                parseFilter
                in
                parser filterValue (Array.get colIndex row |> Maybe.withDefault "")
        )
        True
        filters


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



-- FILTER PARSING


type alias FilterOps =
    { eq : String -> String -> Bool
    , lt : String -> String -> Bool
    , gt : String -> String -> Bool
    , contains : String -> String -> Bool
    }


parseFilterWith : FilterOps -> String -> String -> Bool
parseFilterWith ops filterValue =
    if filterValue == "" then
        always True

    else
        let
            ( op, needle ) =
                if String.startsWith "=" filterValue then
                    ( ops.eq, String.dropLeft 1 filterValue |> String.trim )

                else if String.startsWith "<" filterValue then
                    ( ops.lt, String.dropLeft 1 filterValue |> String.trim )

                else if String.startsWith ">" filterValue then
                    ( ops.gt, String.dropLeft 1 filterValue |> String.trim )

                else
                    ( ops.contains, filterValue )
        in
        \cell -> op cell needle


parseFilter : String -> String -> Bool
parseFilter =
    parseFilterWith
        { eq = \cell needle -> String.toLower cell == String.toLower needle
        , lt = numericFallback (<) (<)
        , gt = numericFallback (>) (>)
        , contains = \cell needle -> String.contains (String.toLower needle) (String.toLower cell)
        }


parseIntFilter : String -> String -> Bool
parseIntFilter =
    parseFilterWith
        { eq = intOp (==)
        , lt = intOp (<)
        , gt = intOp (>)
        , contains =
            \cell needle ->
                case String.toInt cell of
                    Just v ->
                        String.contains (String.toLower needle) (String.toLower (String.fromInt v))

                    Nothing ->
                        False
        }


numericFallback : (Float -> Float -> Bool) -> (String -> String -> Bool) -> String -> String -> Bool
numericFallback fOp sOp cell needle =
    case ( String.toFloat cell, String.toFloat needle ) of
        ( Just c, Just n ) ->
            fOp c n

        _ ->
            sOp (String.toLower cell) (String.toLower needle)


intOp : (Int -> Int -> Bool) -> String -> String -> Bool
intOp op cell needle =
    case ( String.toInt cell, String.toInt needle ) of
        ( Just c, Just n ) ->
            op c n

        _ ->
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


view : State -> Html (Flow State ())
view model =
    let
        totalWidth =
            List.foldl (\col acc -> acc + col.width) 0 model.columns
    in
    Html.div [ Html.Attributes.class "delimited-grid-viewer" ]
        [ Html.table
            [ Html.Attributes.class "delimited-grid-table"
            , Html.Attributes.style "width" (String.fromInt totalWidth ++ "px")
            ]
            [ Html.colgroup [] (List.map viewCol model.columns)
            , Html.thead []
                [ Html.tr [ Html.Attributes.class "delimited-grid-header" ]
                    (List.indexedMap (viewHeaderCell model) model.columns)
                ]
            , Html.tbody []
                (List.map (viewRow model.columns) (visibleRows model))
            ]
        ]


viewCol : Column -> Html msg
viewCol col =
    Html.col
        [ Html.Attributes.style "width" (String.fromInt col.width ++ "px") ]
        []


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
    in
    Html.th
        [ Html.Attributes.style "width" (String.fromInt col.width ++ "px")
        , Html.Attributes.classList [ ( "sorted", sorted ) ]
        , Html.Events.onClick (Flow.modify (toggleSort index))
        , Html.Attributes.title col.tooltip
        ]
        [ Html.div [ Html.Attributes.class "delimited-grid-header-cell" ]
            [ Html.span [ Html.Attributes.class "delimited-grid-header-title" ]
                [ Html.text col.title, sortArrow ]
            , Html.input
                [ Html.Attributes.class "delimited-grid-filter-input"
                , Html.Attributes.type_ "text"
                , Html.Attributes.value currentFilter
                , Html.Events.onInput (\v -> Flow.modify (setFilter index v))
                , Html.Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
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


viewRow : List Column -> Row -> Html msg
viewRow columns row =
    Html.tr [ Html.Attributes.class "delimited-grid-body-row" ]
        (List.indexedMap (\i _ -> viewCell i row) columns)


viewCell : Int -> Row -> Html msg
viewCell index row =
    Html.td []
        [ Html.text (Array.get index row |> Maybe.withDefault "") ]
