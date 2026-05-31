module View.Compare exposing (viewComparePanel)

import Actions
import Api.Api as Api
import Api.ApiData exposing (ApiData(..))
import Array exposing (Array)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, src)
import Html.Events
import Html.Extra as Html
import Maybe.Extra as Maybe
import Model.Core as Model exposing (CompareSelection, CompareSource(..), CompareState(..), LeftRight, Model)
import View.Icons exposing (icon)


viewComparePanel : Model -> Html (Flow Model ())
viewComparePanel model =
    case Model.getCompareState model of
        CompareIdle ->
            Html.nothing

        CompareSelecting left ->
            viewBanner left

        CompareActive { left, right, contents } ->
            viewPanel left right contents


viewBanner : CompareSelection -> Html (Flow Model ())
viewBanner left =
    Html.div [ class "compare-banner" ]
        [ Html.text ("Comparing: " ++ left.fileName ++ " — click another file, or ")
        , Html.button
            [ class "compare-banner-cancel"
            , Html.Events.onClick Actions.cancelCompare
            ]
            [ Html.text "Cancel" ]
        ]


viewPanel : CompareSelection -> CompareSelection -> ApiData LeftRight -> Html (Flow Model ())
viewPanel left right contents =
    Html.div [ class "compare-panel" ]
        [ Html.div [ class "compare-panel-header" ]
            [ Html.span [ class "compare-panel-title" ]
                [ Html.text ("Comparing: " ++ left.fileName ++ " ↔ " ++ right.fileName) ]
            , Html.button
                [ class "compare-panel-close"
                , Html.Events.onClick Actions.cancelCompare
                ]
                [ icon True "close" ]
            ]
        , Html.div [ class "compare-panel-body" ] [ viewBody left right contents ]
        ]


viewBody : CompareSelection -> CompareSelection -> ApiData LeftRight -> Html msg
viewBody left right contents =
    case ( Model.compareSelectionIsImage left, Model.compareSelectionIsImage right, contents ) of
        ( True, True, _ ) ->
            viewImagePair left right

        ( False, False, Success pair ) ->
            viewTextDiff pair.left pair.right

        ( False, False, Error _ ) ->
            Html.div [ class "compare-error" ] [ Html.text "Failed to load files for comparison." ]

        ( False, False, _ ) ->
            Html.div [ class "compare-loading" ] [ Html.text "Loading…" ]

        _ ->
            Html.div [ class "compare-error" ] [ Html.text "Compare requires two image files or two text files." ]


viewImagePair : CompareSelection -> CompareSelection -> Html msg
viewImagePair left right =
    Html.div [ class "compare-images" ] (List.map viewImagePane [ left, right ])


viewImagePane : CompareSelection -> Html msg
viewImagePane sel =
    Html.div [ class "compare-image-pane" ]
        [ Html.div [ class "compare-image-label" ] [ Html.text sel.fileName ]
        , Html.img [ src (rawUrl sel), class "compare-image" ] []
        ]


rawUrl : CompareSelection -> String
rawUrl sel =
    case sel.source of
        FromOutput commit_ ->
            Api.stepFileRawUrl sel.recordId (Just commit_) sel.path

        FromSrc ->
            Api.srcFileDownloadUrl sel.recordId sel.path


viewTextDiff : String -> String -> Html msg
viewTextDiff leftStr rightStr =
    let
        rows =
            alignLines (String.lines leftStr) (String.lines rightStr)

        viewPane pick =
            Html.div [ class "compare-diff-pane" ]
                (List.map
                    (\( l, r ) ->
                        Html.div
                            [ classList [ ( "diff-line", True ), ( "diff-line-diff", l /= r ) ] ]
                            [ Html.text (Maybe.unwrap "\u{00A0}" identity (pick ( l, r ))) ]
                    )
                    rows
                )
    in
    Html.div [ class "compare-diff" ] [ viewPane Tuple.first, viewPane Tuple.second ]


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
