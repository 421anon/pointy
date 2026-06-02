module View.Compare exposing (viewComparePanel)

import Accessors exposing (try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..))
import Array exposing (Array)
import Extra.Accessors exposing (by)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, src)
import Html.Events
import Html.Extra as Html
import Maybe.Extra as Maybe
import Model.Core as Model exposing (CompareActiveData, CompareMode(..), CompareSelection, CompareSource(..), CompareState(..), Model)
import Model.Lenses exposing (projects, records)
import View.Icons exposing (icon)


viewComparePanel : Model -> Html (Flow Model ())
viewComparePanel model =
    case Model.getCompareState model of
        CompareIdle ->
            Html.nothing

        CompareSelecting left ->
            viewBanner (selectionLabel model left)

        CompareActive d ->
            viewPanel (selectionLabel model d.left) (selectionLabel model d.right) d


selectionLabel : Model -> CompareSelection -> String
selectionLabel model sel =
    case try (projects << records << ApiData.success << by .id (Just sel.projectId)) model of
        Just project ->
            project.name ++ " / " ++ sel.fileName

        Nothing ->
            sel.fileName


viewBanner : String -> Html (Flow Model ())
viewBanner leftLabel =
    Html.div [ class "compare-banner" ]
        [ Html.text ("Comparing: " ++ leftLabel ++ " — click another file, or ")
        , Html.button
            [ class "compare-banner-cancel"
            , Html.Events.onClick Actions.cancelCompare
            ]
            [ Html.text "Cancel" ]
        ]


viewPanel : String -> String -> CompareActiveData -> Html (Flow Model ())
viewPanel leftLabel rightLabel d =
    Html.div [ class "compare-panel" ]
        [ Html.div [ class "compare-panel-header" ]
            [ Html.span [ class "compare-panel-title" ]
                [ Html.text ("Comparing: " ++ leftLabel ++ " ↔ " ++ rightLabel) ]
            , Html.button
                [ class "compare-panel-close"
                , Html.Events.onClick Actions.cancelCompare
                ]
                [ icon True "close" ]
            ]
        , Html.div [ class "compare-panel-body" ] [ viewBody d ]
        ]


viewBody : CompareActiveData -> Html msg
viewBody d =
    case bothTextSuccess d of
        Just ( leftStr, rightStr ) ->
            viewTextDiff d.left d.right leftStr rightStr

        Nothing ->
            Html.div [ class "compare-panes" ]
                [ viewPane d.left d.leftContent
                , viewPane d.right d.rightContent
                ]


bothTextSuccess : CompareActiveData -> Maybe ( String, String )
bothTextSuccess d =
    case ( Model.compareSelectionMode d.left, Model.compareSelectionMode d.right, ( d.leftContent, d.rightContent ) ) of
        ( CompareText, CompareText, ( Success l, Success r ) ) ->
            Just ( l, r )

        _ ->
            Nothing


viewPane : CompareSelection -> ApiData String -> Html msg
viewPane sel content =
    Html.div [ class "compare-pane" ]
        [ Html.div [ class "compare-pane-label" ] [ Html.text sel.fileName ]
        , viewPaneBody sel content
        ]


viewPaneBody : CompareSelection -> ApiData String -> Html msg
viewPaneBody sel content =
    case Model.compareSelectionMode sel of
        CompareImage ->
            Html.img [ src (rawUrl sel), class "compare-image" ] []

        CompareHtml ->
            Html.node "iframe"
                [ src (rawUrl sel)
                , Html.Attributes.attribute "sandbox" "allow-same-origin allow-scripts"
                , class "compare-iframe"
                ]
                []

        CompareText ->
            viewTextContent content


viewTextContent : ApiData String -> Html msg
viewTextContent content =
    case content of
        Success s ->
            Html.div [ class "compare-pane-text" ]
                (List.map (\line -> Html.div [ class "diff-line" ] [ Html.text line ]) (String.lines s))

        Error _ ->
            Html.div [ class "compare-error" ] [ Html.text "Failed to load file." ]

        _ ->
            Html.div [ class "compare-loading" ] [ Html.text "Loading…" ]


rawUrl : CompareSelection -> String
rawUrl sel =
    case sel.source of
        FromOutput commit_ ->
            Api.stepFileRawUrl sel.recordId (Just commit_) sel.path

        FromSrc ->
            Api.srcFileDownloadUrl sel.recordId sel.path


viewTextDiff : CompareSelection -> CompareSelection -> String -> String -> Html msg
viewTextDiff left right leftStr rightStr =
    let
        leftLines =
            String.lines leftStr

        rightLines =
            String.lines rightStr

        rows =
            if textKind left == textKind right then
                alignLines leftLines rightLines

            else
                alignByPosition leftLines rightLines

        viewDiffPane pick =
            Html.div [ class "compare-diff-pane" ]
                [ Html.div [ class "compare-diff-lines" ]
                    (List.map
                        (\( l, r ) ->
                            Html.div
                                [ classList [ ( "diff-line", True ), ( "diff-line-diff", l /= r ) ] ]
                                [ Html.text (Maybe.unwrap "\u{00A0}" identity (pick ( l, r ))) ]
                        )
                        rows
                    )
                ]
    in
    Html.div [ class "compare-diff" ] [ viewDiffPane Tuple.first, viewDiffPane Tuple.second ]


textKind : CompareSelection -> String
textKind sel =
    let
        mime =
            Maybe.withDefault "" sel.mimeType
    in
    String.toLower sel.fileName
        |> String.split "."
        |> List.reverse
        |> List.head
        |> Maybe.unwrap mime identity


alignLines : List String -> List String -> List ( Maybe String, Maybe String )
alignLines aList bList =
    let
        a =
            Array.fromList aList

        b =
            Array.fromList bList

        m =
            Array.length a

        n =
            Array.length b

        cols =
            n + 1

        dpAt i j arr =
            Array.get (i * cols + j) arr |> Maybe.withDefault 0

        lineMatches i j =
            Array.get (i - 1) a == Array.get (j - 1) b

        dp =
            List.foldl
                (\i acc ->
                    List.foldl
                        (\j r ->
                            Array.set (i * cols + j)
                                (if lineMatches i j then
                                    dpAt (i - 1) (j - 1) r + 1

                                 else
                                    max (dpAt (i - 1) j r) (dpAt i (j - 1) r)
                                )
                                r
                        )
                        acc
                        (List.range 1 n)
                )
                (Array.repeat ((m + 1) * cols) 0)
                (List.range 1 m)

        backtrack i j acc =
            if i == 0 && j == 0 then
                acc

            else if i > 0 && j > 0 && lineMatches i j then
                backtrack (i - 1) (j - 1) (( Array.get (i - 1) a, Array.get (j - 1) b ) :: acc)

            else if i == 0 || (j > 0 && dpAt i (j - 1) dp > dpAt (i - 1) j dp) then
                backtrack i (j - 1) (( Nothing, Array.get (j - 1) b ) :: acc)

            else
                backtrack (i - 1) j (( Array.get (i - 1) a, Nothing ) :: acc)
    in
    backtrack m n []


alignByPosition : List String -> List String -> List ( Maybe String, Maybe String )
alignByPosition left right =
    let
        go a b acc =
            case ( a, b ) of
                ( [], [] ) ->
                    List.reverse acc

                ( l :: ls, r :: rs ) ->
                    go ls rs (( Just l, Just r ) :: acc)

                ( l :: ls, [] ) ->
                    go ls [] (( Just l, Nothing ) :: acc)

                ( [], r :: rs ) ->
                    go [] rs (( Nothing, Just r ) :: acc)
    in
    go left right []
