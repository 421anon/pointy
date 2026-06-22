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
import InfiniteList
import Json.Decode as Decode
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


filterHint : ColumnType -> String
filterHint colType =
    case colType of
        Text ->
            "Filter by substring"

        _ ->
            "Filter by substring or >, <, ="


defaultColumn : Column
defaultColumn =
    { id = "", title = "", width = 88, type_ = Text }


columnAt : Int -> List Column -> Column
columnAt index columns =
    columns |> List.drop index |> List.head |> Maybe.withDefault defaultColumn



-- VISIBLE ROWS (filter + sort)


visibleRows : State -> List ( Int, Row )
visibleRows model =
    let
        filtered =
            model.rows
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, row ) -> rowPassesFilters model row)
    in
    case model.sortColumn of
        Just ( colIndex, Asc ) ->
            List.sortWith (\( _, a ) ( _, b ) -> compareRowsByColumn colIndex model.columns a b) filtered

        Just ( colIndex, Desc ) ->
            List.sortWith (\( _, a ) ( _, b ) -> reverseOrder (compareRowsByColumn colIndex model.columns a b)) filtered

        Nothing ->
            filtered


{-| Recompute cached filtered/sorted rows after changes to columns, rows,
filters, or sort. Scroll updates only `infiniteList` and must not run this
O(n) path.
-}
refreshVisible : State -> State
refreshVisible state =
    { state | visible = Array.fromList (visibleRows state) }


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



-- VIEW


view : (Flow State () -> msg) -> (() -> Html msg) -> State -> Html msg
view toMsg viewPlainContent model =
    Html.div [ Html.Attributes.class "delimited-grid-shell" ]
        [ Html.div [ Html.Attributes.class "delimited-grid-toolbar" ]
            [ if model.showGrid then
                Html.span [ Html.Attributes.class "delimited-grid-row-count" ]
                    [ Html.text (rowCountLabel model) ]

              else
                Html.nothing
            , viewModeToggle model.showGrid (Html.Events.stopPropagationOn "click" (Decode.succeed ( toMsg (Flow.modify toggleShowGrid), True )))
            ]
        , if model.showGrid then
            Html.map toMsg (viewGrid model)

          else
            viewPlainContent ()
        ]


toggleShowGrid : State -> State
toggleShowGrid model =
    { model | showGrid = not model.showGrid }


showPlain : State -> State
showPlain model =
    { model | showGrid = False }


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
        totalWidth =
            List.foldl (\col acc -> acc + col.width) 0 model.columns
    in
    Html.div
        [ Html.Attributes.class "delimited-grid-viewer"
        , InfiniteList.onScroll (\listModel -> Flow.modify (setInfiniteList listModel))
        ]
        [ Html.div
            [ Html.Attributes.class "delimited-grid"
            , Html.Attributes.style "width" (String.fromInt totalWidth ++ "px")
            ]
            [ Html.div [ Html.Attributes.class "delimited-grid-header" ]
                (List.indexedMap (viewHeaderCell model) model.columns)
            , InfiniteList.viewArray (listConfig model.columns) model.infiniteList model.visible
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


{-| Fixed body-row height in pixels. Must stay in sync with
`$delimited-grid-row-height` in the stylesheet, since the virtual list
positions rows using this constant.
-}
rowHeight : Int
rowHeight =
    28


viewportEstimate : Int
viewportEstimate =
    1000


listConfig : List Column -> InfiniteList.Config ( Int, Row ) (Flow State ())
listConfig columns =
    InfiniteList.config
        { itemView = \_ _ ( _, row ) -> viewRow columns row
        , itemHeight = InfiniteList.withConstantHeight rowHeight
        , containerHeight = viewportEstimate
        }
        |> InfiniteList.withOffset viewportEstimate


setInfiniteList : InfiniteList.Model -> State -> State
setInfiniteList listModel state =
    { state | infiniteList = listModel }


{-| Cell width uses JS-updated `--dg-col-N`, falling back to the model width
before resize.
-}
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
    Html.div [ Html.Attributes.class "delimited-grid-body-row" ]
        (List.indexedMap (\i column -> viewCell i column row) columns)


viewCell : Int -> Column -> Row -> Html msg
viewCell index column row =
    Html.div
        [ Html.Attributes.class "delimited-grid-td"
        , columnWidthStyle index column
        ]
        [ Html.text (Array.get index row |> Maybe.withDefault "") ]
