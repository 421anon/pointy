module View.Compare exposing (viewCompareBanner, viewCompareDialog)

import Accessors exposing (An_Optic, just, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Array exposing (Array)
import Extra.Accessors exposing (by, remkT)
import Flow exposing (Flow)
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class, classList, id, src)
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (CompareActiveData, CompareFile, CompareMode(..), CompareSelection, CompareSource(..), CompareState(..), Model)
import Model.Lenses exposing (compareActive, compareLeftContent, compareRightContent, compareState, fileDelimitedGrid, gridState, projects, records)
import Route exposing (Route)
import View.Icons exposing (icon)


viewCompareBanner : Model -> Html (Flow Model ())
viewCompareBanner model =
    case Model.getCompareState model of
        CompareSelecting left ->
            viewBanner (selectionLabel model left)

        _ ->
            Html.nothing


viewCompareDialog : Model -> Html (Flow Model ())
viewCompareDialog model =
    Html.node "dialog"
        [ id "compare-dialog"
        , class "dialog compare-dialog"
        , Html.Events.on "close" (Decode.succeed Actions.cancelCompare)
        , Html.Events.onClick (Actions.closeDialog "compare-dialog")
        ]
        [ case Model.getCompareState model of
            CompareActive d ->
                viewDialogContent model d

            _ ->
                Html.nothing
        ]


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
        [ Html.span [ class "compare-banner-text" ]
            [ Html.text ("Comparing " ++ leftLabel ++ ". Pick another file to compare, or ") ]
        , Html.button
            [ class "compare-banner-cancel"
            , Html.Events.onClick Actions.cancelCompare
            ]
            [ Html.text "Cancel" ]
        ]


viewDialogContent : Model -> CompareActiveData -> Html (Flow Model ())
viewDialogContent model d =
    Html.div
        [ class "dialog-content compare-dialog-content"
        , Html.Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
        ]
        [ Html.div [ class "compare-dialog-header" ]
            [ Html.span [ class "compare-dialog-title" ]
                [ Html.text ("Comparing " ++ selectionLabel model d.left ++ " ↔ " ++ selectionLabel model d.right) ]
            , Html.button
                [ class "icon-btn compare-dialog-close"
                , Html.Attributes.title "Close comparison"
                , Html.Events.onClick (Actions.closeDialog "compare-dialog")
                ]
                [ icon True "close" ]
            ]
        , Html.div [ class "compare-dialog-body" ] [ viewBody model d ]
        ]


viewBody : Model -> CompareActiveData -> Html (Flow Model ())
viewBody model d =
    case bothTextSuccess d of
        Just ( leftStr, rightStr ) ->
            viewTextDiff model d.left d.right leftStr rightStr

        Nothing ->
            Html.div [ class "compare-panes" ]
                [ viewPane model d.left d.leftContent (gridUpdate compareLeftContent)
                , viewPane model d.right d.rightContent (gridUpdate compareRightContent)
                ]


bothTextSuccess : CompareActiveData -> Maybe ( String, String )
bothTextSuccess d =
    case ( Model.compareSelectionMode d.left, Model.compareSelectionMode d.right, ( d.leftContent, d.rightContent ) ) of
        ( CompareText, CompareText, ( Success l, Success r ) ) ->
            if Maybe.isNothing l.delimitedGrid && Maybe.isNothing r.delimitedGrid then
                Just ( l.text, r.text )

            else
                Nothing

        _ ->
            Nothing


viewPane : Model -> CompareSelection -> ApiData CompareFile -> (Flow Grid.State () -> Flow Model ()) -> Html (Flow Model ())
viewPane model sel content gridFlow =
    Html.div [ class "compare-pane" ]
        [ viewPaneHeader model sel
        , Html.div [ class "compare-pane-body" ] [ viewPaneBody sel content gridFlow ]
        ]


gridUpdate : An_Optic pr ls CompareActiveData (ApiData CompareFile) -> Flow Grid.State () -> Flow Model ()
gridUpdate contentLens =
    Flow.via (compareState << compareActive << remkT contentLens << success << fileDelimitedGrid << just << gridState)


viewPaneHeader : Model -> CompareSelection -> Html (Flow Model ())
viewPaneHeader model sel =
    Html.div [ class "compare-pane-header" ]
        [ Html.div [ class "compare-pane-label" ] [ Html.text (selectionLabel model sel) ]
        , Html.button
            [ class "dir-item-icon-btn compare-pane-source-btn"
            , Html.Attributes.title "Open source in project"
            , Html.Events.onClick (openSource sel)
            ]
            [ icon True "arrow_outward" ]
        ]


openSource : CompareSelection -> Flow Model ()
openSource sel =
    Actions.cancelCompare
        |> Flow.seq (Actions.closeDialog "compare-dialog")
        |> Flow.seq (Actions.goToRoute (sourceRoute sel))
        |> Flow.seq (Actions.addToast True "Opened source")


sourceRoute : CompareSelection -> Route
sourceRoute sel =
    Route.Project
        { projectId = sel.projectId
        , mHighlight =
            Just
                { id = sel.recordId
                , target =
                    case sel.source of
                        FromOutput _ ->
                            Route.Output

                        FromSrc ->
                            Route.Source
                , path = sel.path
                , range = Nothing
                }
        , mCommit =
            case sel.source of
                FromOutput commit_ ->
                    Just commit_

                FromSrc ->
                    Nothing
        }


viewPaneBody : CompareSelection -> ApiData CompareFile -> (Flow Grid.State () -> Flow Model ()) -> Html (Flow Model ())
viewPaneBody sel content gridFlow =
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
            viewTextContent gridFlow content


viewTextContent : (Flow Grid.State () -> Flow Model ()) -> ApiData CompareFile -> Html (Flow Model ())
viewTextContent gridFlow content =
    case content of
        Success file ->
            case file.delimitedGrid of
                Just grid ->
                    Grid.view grid.grid |> Html.map gridFlow

                Nothing ->
                    Html.div [ class "compare-pane-text" ]
                        (List.map (\line -> Html.div [ class "diff-line" ] [ Html.text line ]) (String.lines file.text))

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


viewTextDiff : Model -> CompareSelection -> CompareSelection -> String -> String -> Html (Flow Model ())
viewTextDiff model left right leftStr rightStr =
    let
        leftLines =
            String.lines leftStr

        rightLines =
            String.lines rightStr

        maxAlignmentCells =
            1000000

        rows =
            if textKind left == textKind right && List.length leftLines * List.length rightLines <= maxAlignmentCells then
                alignLines leftLines rightLines

            else
                alignByPosition leftLines rightLines

        viewDiffPane sel pick =
            Html.div [ class "compare-diff-pane" ]
                [ viewPaneHeader model sel
                , Html.div [ class "compare-diff-lines" ]
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
    Html.div [ class "compare-diff" ] [ viewDiffPane left Tuple.first, viewDiffPane right Tuple.second ]


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
