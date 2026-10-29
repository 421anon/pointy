module View.PlainText exposing (Config, Layout(..), LineSelection, view, viewChunks, viewText)

import Html exposing (Html)
import Html.Attributes exposing (class, classList, id, style)
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import List.Extra
import Maybe.Extra as Maybe
import Model.Core exposing (FileChunk, ScrollMetrics, SeekDirection(..), plainLineHeight)
import View.Icons exposing (icon)


type alias Config msg =
    { id : String
    , layout : Layout
    , scrollTop : Float
    , onScroll : Maybe (ScrollMetrics -> msg)
    , lineSelection : Maybe (LineSelection msg)
    , loading : Maybe SeekDirection
    , startLine : Int
    , lineCount : Int
    }


type Layout
    = FitLines
    | FillParent Float


type alias LineSelection msg =
    { from : Int
    , to : Int
    , onPointerDown : Int -> msg
    , onPointerEnter : Maybe (Int -> msg)
    }


view : Config msg -> Html msg -> Html msg
view config content =
    let
        loadingBefore =
            config.loading == Just Before

        loadingAfter =
            config.loading == Just After

        hiddenFirstLines =
            if loadingBefore then
                1

            else
                0

        hiddenLastLines =
            if loadingAfter then
                1

            else
                0

        startLine =
            config.startLine

        lineCount =
            max 1 config.lineCount

        lineIndexes =
            visibleLineIndexes (containerHeight config.layout lineCount) config.scrollTop lineCount

        firstVisibleIndex =
            List.head lineIndexes |> Maybe.withDefault 0

        gutterWidth =
            "calc(" ++ String.fromInt (String.length (String.fromInt (startLine + lineCount - 1))) ++ "ch + var(--spacing-sm) + var(--spacing-xs))"

        lineHeightStyles =
            [ style "height" (px plainLineHeight)
            , style "line-height" (px plainLineHeight)
            ]

        lineEventAttrs selection =
            Html.Events.on "pointerdown" (Decode.map selection.onPointerDown lineNumberDecoder)
                :: Maybe.unwrap [] (\onEnter -> [ Html.Events.on "pointerenter" (Decode.map onEnter lineNumberDecoder) ]) selection.onPointerEnter

        ( selectedFrom, selectedTo ) =
            Maybe.unwrap ( 0, 0 ) (\selection -> ( selection.from, selection.to )) config.lineSelection

        renderLineNumber lineIndex =
            if (loadingBefore && lineIndex == 0) || (loadingAfter && lineIndex == lineCount - 1) then
                Html.div (class "file-line-number" :: lineHeightStyles) []

            else
                let
                    lineNum =
                        startLine + lineIndex
                in
                Html.div
                    (classList
                        [ ( "file-line-number", True )
                        , ( "is-gutter", Maybe.isJust config.lineSelection )
                        , ( "highlighted", selectedFrom > 0 && lineNum >= selectedFrom && lineNum <= selectedTo )
                        ]
                        :: Html.Attributes.attribute "data-line" (String.fromInt lineNum)
                        :: lineHeightStyles
                        ++ Maybe.unwrap [] lineEventAttrs config.lineSelection
                    )
                    [ Html.text (String.fromInt lineNum) ]

        overlayFrom =
            max (startLine + hiddenFirstLines) selectedFrom

        overlayTo =
            min selectedTo (startLine + lineCount - 1 - hiddenLastLines)

        highlightOverlay =
            Html.viewIf (selectedFrom > 0 && overlayFrom <= overlayTo) <|
                Html.div
                    [ class "file-highlight-overlay"
                    , style "top" (px ((overlayFrom - startLine) * plainLineHeight))
                    , style "height" (px ((overlayTo - overlayFrom + 1) * plainLineHeight))
                    ]
                    []

        layoutAttrs =
            case config.layout of
                FitLines ->
                    [ style "max-height" (px (fitLinesHeight lineCount)) ]

                FillParent _ ->
                    [ class "is-filling" ]

        scrollAttrs =
            Maybe.unwrap [] (\onScroll -> [ Html.Events.on "scroll" (Decode.map onScroll scrollMetricsDecoder) ]) config.onScroll
    in
    Html.div
        (class "file-content" :: id config.id :: layoutAttrs ++ scrollAttrs)
        [ Html.div
            [ class "file-body"
            , style "height" (px (lineCount * plainLineHeight))
            ]
            [ Html.div
                [ class "file-gutter"
                , style "width" gutterWidth
                ]
                [ Html.div
                    [ class "file-gutter-window"
                    , style "top" (px (firstVisibleIndex * plainLineHeight))
                    ]
                    (List.map renderLineNumber lineIndexes)
                ]
            , highlightOverlay
            , content
            ]
        ]


viewText : String -> Html msg
viewText text =
    Html.div [ class "file-text" ] [ Html.text text ]


viewChunks : Maybe SeekDirection -> List FileChunk -> Html msg
viewChunks loading chunks =
    let
        dropFirstLine texts =
            case texts of
                [] ->
                    []

                first :: rest ->
                    case List.head (String.indexes "\n" first) of
                        Just newline ->
                            String.dropLeft (newline + 1) first :: rest

                        Nothing ->
                            "" :: dropFirstLine rest

        dropLastLine texts =
            let
                drop reversed =
                    case reversed of
                        [] ->
                            []

                        last :: rest ->
                            case List.Extra.last (String.indexes "\n" last) of
                                Just newline ->
                                    String.left newline last :: rest

                                Nothing ->
                                    "" :: drop rest
            in
            texts |> List.reverse |> drop |> List.reverse

        loadingLine =
            Html.div [ class "seek-loading-line" ]
                [ Html.span [ class "seek-loading-icon" ] [ icon True "progress_activity" ] ]

        chunkTexts =
            List.map .content chunks

        children =
            case loading of
                Just Before ->
                    loadingLine :: List.map Html.text (dropFirstLine chunkTexts)

                Just After ->
                    List.map Html.text (dropLastLine chunkTexts) ++ [ loadingLine ]

                Nothing ->
                    List.map Html.text chunkTexts
    in
    Html.div [ class "file-text" ] children


px : Int -> String
px value =
    String.fromInt value ++ "px"


lineNumberDecoder : Decode.Decoder Int
lineNumberDecoder =
    Decode.at [ "currentTarget", "dataset", "line" ] Decode.string
        |> Decode.andThen
            (\lineStr ->
                case String.toInt lineStr of
                    Just line ->
                        Decode.succeed line

                    Nothing ->
                        Decode.fail ("Invalid line number: " ++ lineStr)
            )


scrollMetricsDecoder : Decode.Decoder ScrollMetrics
scrollMetricsDecoder =
    Decode.map3 ScrollMetrics
        (Decode.at [ "target", "scrollTop" ] Decode.float)
        (Decode.at [ "target", "clientHeight" ] Decode.float)
        (Decode.at [ "target", "scrollHeight" ] Decode.float)


plainViewerPadding : Int
plainViewerPadding =
    16


plainViewerMaxHeight : Int
plainViewerMaxHeight =
    600


plainViewerMinHeight : Int
plainViewerMinHeight =
    100


plainViewerOverscan : Int
plainViewerOverscan =
    600


containerHeight : Layout -> Int -> Int
containerHeight layout lineCount =
    case layout of
        FitLines ->
            cappedLinesHeight lineCount

        FillParent measuredHeight ->
            max plainViewerMaxHeight (ceiling measuredHeight)


fitLinesHeight : Int -> Int
fitLinesHeight =
    cappedLinesHeight >> max plainViewerMinHeight


cappedLinesHeight : Int -> Int
cappedLinesHeight lineCount =
    min (lineCount * plainLineHeight + plainViewerPadding) plainViewerMaxHeight


visibleLineIndexes : Int -> Float -> Int -> List Int
visibleLineIndexes height scrollTop lineCount =
    let
        visibleCount =
            (height + 2 * plainViewerOverscan) // plainLineHeight + 1

        firstIndex =
            max 0 ((floor scrollTop - plainViewerOverscan) // plainLineHeight)

        boundedFirstIndex =
            min firstIndex (max 0 (lineCount - 1))

        lastIndexExclusive =
            min lineCount (boundedFirstIndex + visibleCount)
    in
    if lastIndexExclusive <= boundedFirstIndex then
        []

    else
        List.range boundedFirstIndex (lastIndexExclusive - 1)
