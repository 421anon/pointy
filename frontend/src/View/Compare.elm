module View.Compare exposing (viewCompareBanner, viewCompareDialog)

import Accessors exposing (An_Optic, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Dict exposing (Dict)
import Extra.Accessors exposing (remkT)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class, classList, id, src)
import Html.Events
import Html.Extra as Html
import Html.Lazy
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (CompareActiveData, CompareContent(..), CompareMode(..), ComparePane, CompareSelection, CompareSource(..), Model, ScrollMetrics, StepRecord, TextContent)
import Model.Lenses exposing (compareActive, compareLeftContent, compareLeftPane, compareLeftScrollTop, compareLeftViewportHeight, compareRightContent, compareRightPane, compareRightScrollTop, compareRightViewportHeight, compareSelecting, compareState, projectRecordById, stepRecordById)
import Model.Shadow as Shadow exposing (Field, StepArgValue(..), Widget(..))
import Route exposing (Route)
import View.Icons exposing (icon)
import View.PlainText as PlainText
import View.TextContent as TextContent


viewCompareBanner : Model -> Html (Flow Model ())
viewCompareBanner model =
    try (compareState << compareSelecting) model
        |> Html.viewMaybe (selectionLabel model >> viewBanner)


viewCompareDialog : Model -> Html (Flow Model ())
viewCompareDialog model =
    Html.node "dialog"
        [ id "compare-dialog"
        , class "dialog compare-dialog"
        , Html.Events.on "close" (Decode.succeed Actions.cancelCompare)
        , Html.Events.onClick (Actions.closeDialog "compare-dialog")
        ]
        [ try (compareState << compareActive) model
            |> Html.viewMaybe (viewDialogContent model)
        ]


selectionLabel : Model -> CompareSelection -> String
selectionLabel model sel =
    let
        projectName =
            try (projectRecordById (Route.pathProjectId sel.projectPath)) model
                |> Maybe.unwrap "" .name

        stepName =
            try (stepRecordById sel.recordId) model
                |> Maybe.unwrap "" .name
    in
    ([ projectName, stepName ] ++ sel.path)
        |> List.filter ((/=) "")
        |> String.join " / "


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
                [ Html.text ("Comparing " ++ selectionLabel model d.left)
                , icon True "compare_arrows"
                , Html.text (selectionLabel model d.right)
                ]
            , Html.button
                [ class "icon-btn"
                , Html.Attributes.title "Close comparison"
                , Html.Events.onClick (Actions.closeDialog "compare-dialog")
                ]
                [ icon True "close" ]
            ]
        , Html.div [ class "compare-dialog-body" ] [ viewBody model d ]
        ]


viewBody : Model -> CompareActiveData -> Html (Flow Model ())
viewBody model d =
    let
        mTextPair =
            case ( Model.compareSelectionMode d.left, Model.compareSelectionMode d.right, ( d.leftPane.content, d.rightPane.content ) ) of
                ( CompareText, CompareText, ( Success (CompareFullText l), Success (CompareFullText r) ) ) ->
                    if Maybe.isNothing l.delimitedGrid && Maybe.isNothing r.delimitedGrid then
                        Just ( l.text, r.text )

                    else
                        Nothing

                _ ->
                    Nothing

        matchesActive candidate =
            candidate.left == d.left && candidate.right == d.right

        panes =
            Html.div [ class "compare-panes" ]
                [ viewPane model
                    "compare-left-text"
                    d.left
                    d.leftPane
                    (contentUpdate compareLeftPane)
                    (toggleFlag compareLeftPane)
                    (Actions.onCompareScroll matchesActive compareLeftContent compareLeftScrollTop compareLeftViewportHeight "compare-left-text" d.left)
                , viewPane model
                    "compare-right-text"
                    d.right
                    d.rightPane
                    (contentUpdate compareRightPane)
                    (toggleFlag compareRightPane)
                    (Actions.onCompareScroll matchesActive compareRightContent compareRightScrollTop compareRightViewportHeight "compare-right-text" d.right)
                ]
    in
    mTextPair
        |> Maybe.unwrap panes (\( l, r ) -> viewTextDiff model d l r)


viewPane : Model -> String -> CompareSelection -> ComparePane -> ((TextContent -> TextContent) -> Flow Model ()) -> Flow Model () -> (ScrollMetrics -> Flow Model ()) -> Html (Flow Model ())
viewPane model viewerId sel pane updateContent toggle onScroll =
    let
        mParams =
            derivationParamsFor model sel
    in
    Html.div [ class "compare-pane" ]
        [ viewPaneHeader model sel (Maybe.isJust mParams) pane.inspectOpen toggle
        , Html.div [ class "compare-pane-body" ]
            [ viewPaneBody viewerId sel pane updateContent onScroll
            , viewInlineParams model sel pane.inspectOpen mParams
            ]
        ]


contentUpdate : An_Optic pr ls CompareActiveData ComparePane -> (TextContent -> TextContent) -> Flow Model ()
contentUpdate paneLens changeText =
    Flow.over (compareState << compareActive << remkT paneLens) <|
        \pane ->
            case pane.content of
                Success (CompareFullText text) ->
                    { pane | content = Success (CompareFullText (changeText text)) }

                _ ->
                    pane


toggleFlag : An_Optic pr ls CompareActiveData ComparePane -> Flow Model ()
toggleFlag paneLens =
    Flow.over (compareState << compareActive << remkT paneLens) (\pane -> { pane | inspectOpen = not pane.inspectOpen })


viewPaneHeader : Model -> CompareSelection -> Bool -> Bool -> Flow Model () -> Html (Flow Model ())
viewPaneHeader model sel hasParams inspectOpen toggle =
    Html.div [ class "compare-pane-header" ]
        [ Html.div [ class "compare-pane-label" ] [ Html.text (selectionLabel model sel) ]
        , Html.div [ class "compare-pane-actions" ]
            [ viewInspectButton hasParams inspectOpen toggle
            , Html.button
                [ class "dir-item-icon-btn"
                , Html.Attributes.title "Open source in project"
                , Html.Events.onClick (openSource sel)
                ]
                [ icon True "arrow_outward" ]
            ]
        ]


viewInspectButton : Bool -> Bool -> Flow Model () -> Html (Flow Model ())
viewInspectButton hasParams inspectOpen toggle =
    Html.viewIf hasParams <|
        Html.button
            [ classList
                [ ( "dir-item-icon-btn", True )
                , ( "compare-pane-inspect-btn", True )
                , ( "active", inspectOpen )
                ]
            , Html.Attributes.title
                (if inspectOpen then
                    "Hide parameters"

                 else
                    "Inspect parameters"
                )
            , Html.Events.onClick toggle
            ]
            [ icon True "data_info_alert" ]


derivationParamsFor : Model -> CompareSelection -> Maybe ( StepRecord, List Field )
derivationParamsFor model sel =
    try (stepRecordById sel.recordId) model
        |> Maybe.andThen
            (\step ->
                Model.getStepConfig model
                    |> ApiData.toMaybe
                    |> Maybe.andThen (Dict.get step.type_)
                    |> Maybe.andThen
                        (\entry ->
                            case entry.stepType of
                                Shadow.Derivation fields _ ->
                                    Just fields

                                Shadow.Download fields ->
                                    Just fields

                                _ ->
                                    Nothing
                        )
                    |> Maybe.map (\fields -> ( step, fields ))
            )


viewInlineParams : Model -> CompareSelection -> Bool -> Maybe ( StepRecord, List Field ) -> Html (Flow Model ())
viewInlineParams model sel inspectOpen mParams =
    Html.viewIf inspectOpen <|
        Html.viewMaybe
            (\( step, fields ) ->
                Html.div [ class "compare-params" ]
                    [ Html.div [ class "compare-params-label" ] [ Html.text "Parameters" ]
                    , Html.div [ class "compare-params-form" ] (viewNote step.note ++ viewNamedArgs model step.args fields)
                    ]
            )
            mParams


viewNote : String -> List (Html (Flow Model ()))
viewNote note =
    if String.isEmpty (String.trim note) then
        []

    else
        [ viewParamRow "Note" (Html.text note) ]


viewNamedArgs : Model -> Dict String StepArgValue -> List Field -> List (Html (Flow Model ()))
viewNamedArgs model values fields =
    List.map
        (\f ->
            viewParamRow (Maybe.unwrap f.name identity f.label)
                (viewArgValue model f.widget (Dict.get f.name values))
        )
        fields


viewParamRow : String -> Html (Flow Model ()) -> Html (Flow Model ())
viewParamRow label valueHtml =
    Html.div [ class "compare-param" ]
        [ Html.div [ class "compare-param-label" ] [ Html.text label ]
        , Html.div [ class "compare-param-value" ] [ valueHtml ]
        ]


viewArgValue : Model -> Widget -> Maybe StepArgValue -> Html (Flow Model ())
viewArgValue model widget_ mValue =
    case ( widget_, mValue ) of
        ( WText _, Just (TStringValue s) ) ->
            orEmptyValue s (Html.text s)

        ( WTextarea, Just (TStringValue s) ) ->
            orEmptyValue s (Html.text s)

        ( WDatetime, Just (TStringValue s) ) ->
            orEmptyValue s (Html.text s)

        ( WCode _, Just (TStringValue s) ) ->
            orEmptyValue s (Html.pre [ class "compare-param-code" ] [ Html.text s ])

        ( WCommand prefix, Just (TStringValue s) ) ->
            orEmptyValue s (Html.code [ class "compare-param-code" ] [ Html.text (String.trim (prefix ++ " " ++ s)) ])

        ( WNumber, Just (TIntValue n) ) ->
            Html.text (String.fromInt n)

        ( WCheckbox, Just (TBoolValue b) ) ->
            Html.text
                (if b then
                    "true"

                 else
                    "false"
                )

        ( WStep _, Just (TStepValue ref) ) ->
            Html.text (stepNameOf model ref.step)

        ( WSelect options, Just (TEnumValue v) ) ->
            Html.text (Maybe.withDefault v (Dict.get v (Dict.fromList options)))

        ( WTokens _, Just (TListValue vs) ) ->
            viewValueList model (WText Nothing) vs

        ( WList element, Just (TListValue vs) ) ->
            viewValueList model element vs

        ( WSteps artifact_, Just (TListValue vs) ) ->
            viewValueList model (WStep artifact_) vs

        ( WRecord fields, Just (TRecordValue dict) ) ->
            Html.div [ class "compare-param-record" ] (viewNamedArgs model dict fields)

        _ ->
            emptyValue


viewValueList : Model -> Widget -> List StepArgValue -> Html (Flow Model ())
viewValueList model widget_ values =
    if List.isEmpty values then
        emptyValue

    else
        Html.ul [ class "compare-param-list" ]
            (List.map (\v -> Html.li [] [ viewArgValue model widget_ (Just v) ]) values)


orEmptyValue : String -> Html (Flow Model ()) -> Html (Flow Model ())
orEmptyValue s html =
    if String.isEmpty (String.trim s) then
        emptyValue

    else
        html


emptyValue : Html (Flow Model ())
emptyValue =
    Html.span [ class "compare-param-empty" ] [ Html.text "—" ]


stepNameOf : Model -> Int -> String
stepNameOf model stepId =
    try (stepRecordById stepId) model
        |> Maybe.unwrap ("#" ++ String.fromInt stepId) .name


openSource : CompareSelection -> Flow Model ()
openSource sel =
    Actions.goToRoute (sourceRoute sel)


sourceRoute : CompareSelection -> Route
sourceRoute sel =
    let
        ( target, mCommit ) =
            case sel.source of
                FromOutput commit_ ->
                    ( Route.Output, Just commit_ )

                FromSrc commit_ ->
                    ( Route.Source, commit_ )
    in
    Route.fromPage
        (Route.Project
            { projectPath = sel.projectPath
            , mHighlight = Just { id = sel.recordId, target = target, path = sel.path, range = Nothing }
            , mCommit = mCommit
            , mCompare = Nothing
            }
        )


viewPaneBody : String -> CompareSelection -> ComparePane -> ((TextContent -> TextContent) -> Flow Model ()) -> (ScrollMetrics -> Flow Model ()) -> Html (Flow Model ())
viewPaneBody viewerId sel pane updateContent onScroll =
    case Model.compareSelectionMode sel of
        CompareImage ->
            Html.img [ src (rawUrl sel), class "compare-image" ] []

        CompareHtml ->
            Html.node "iframe"
                [ src (rawUrl sel)
                , Html.Attributes.attribute "sandbox" "allow-same-origin allow-scripts allow-top-navigation-by-user-activation allow-popups allow-popups-to-escape-sandbox"
                , class "compare-iframe"
                ]
                []

        CompareText ->
            viewTextPane viewerId pane updateContent onScroll


viewTextPane : String -> ComparePane -> ((TextContent -> TextContent) -> Flow Model ()) -> (ScrollMetrics -> Flow Model ()) -> Html (Flow Model ())
viewTextPane viewerId pane updateContent onScroll =
    case pane.content of
        Success (CompareFullText file) ->
            TextContent.view viewerId updateContent [] (Success file)

        Success (CompareSeekedText window_) ->
            PlainText.view
                { id = viewerId
                , layout = PlainText.FillParent pane.viewportHeight
                , scrollTop = pane.scrollTop
                , onScroll = Just onScroll
                , lineSelection = Nothing
                , loading = window_.loading
                , startLine = Model.windowStartLine window_
                , lineCount = Model.windowLineCount window_
                }
                (Html.Lazy.lazy2 PlainText.viewChunks window_.loading window_.chunks)

        Error _ ->
            Html.div [ class "text-content-error" ] [ Html.text "Failed to load file." ]

        _ ->
            Html.div [ class "text-content-loading" ]
                [ Html.span [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading file…" ] ]


rawUrl : CompareSelection -> String
rawUrl sel =
    case sel.source of
        FromOutput commit_ ->
            Api.stepFileBundleUrl sel.recordId commit_ sel.path

        FromSrc commit_ ->
            Api.srcFileRawUrl sel.recordId commit_ sel.path


viewTextDiff : Model -> CompareActiveData -> String -> String -> Html (Flow Model ())
viewTextDiff model d leftStr rightStr =
    let
        rows =
            alignLines (String.lines leftStr) (String.lines rightStr)

        viewDiffPane sel pane toggle pick =
            let
                mParams =
                    derivationParamsFor model sel
            in
            Html.div [ class "compare-diff-pane" ]
                [ viewPaneHeader model sel (Maybe.isJust mParams) pane.inspectOpen toggle
                , Html.div [ class "compare-diff-scroll" ]
                    [ Html.div [ class "compare-diff-lines" ]
                        (List.map
                            (\( l, r ) ->
                                Html.div
                                    [ classList [ ( "diff-line", True ), ( "diff-line-diff", l /= r ) ] ]
                                    [ Html.text (Maybe.unwrap "\u{00A0}" identity (pick ( l, r ))) ]
                            )
                            rows
                        )
                    , viewInlineParams model sel pane.inspectOpen mParams
                    ]
                ]
    in
    Html.div [ class "compare-diff" ]
        [ viewDiffPane d.left d.leftPane (toggleFlag compareLeftPane) Tuple.first
        , viewDiffPane d.right d.rightPane (toggleFlag compareRightPane) Tuple.second
        ]


alignLines : List String -> List String -> List ( Maybe String, Maybe String )
alignLines left right =
    let
        commonRun a b =
            List.map2 Tuple.pair a b
                |> List.takeWhile (\( x, y ) -> x == y)
                |> List.length

        prefix =
            commonRun left right

        leftRest =
            List.drop prefix left

        rightRest =
            List.drop prefix right

        suffix =
            commonRun (List.reverse leftRest) (List.reverse rightRest)

        equalPairs =
            List.map (\l -> ( Just l, Just l ))

        dropSuffix lines =
            List.take (List.length lines - suffix) lines

        keepSuffix lines =
            List.drop (List.length lines - suffix) lines
    in
    equalPairs (List.take prefix left)
        ++ alignByPosition (dropSuffix leftRest) (dropSuffix rightRest)
        ++ equalPairs (keepSuffix leftRest)


alignByPosition : List String -> List String -> List ( Maybe String, Maybe String )
alignByPosition left right =
    let
        n =
            max (List.length left) (List.length right)

        pad lines =
            List.map Just lines ++ List.repeat (n - List.length lines) Nothing
    in
    List.map2 Tuple.pair (pad left) (pad right)
