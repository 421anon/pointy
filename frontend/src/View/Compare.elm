module View.Compare exposing (viewCompareBanner, viewCompareDialog)

import Accessors exposing (An_Optic, just, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Dict exposing (Dict)
import Extra.Accessors exposing (by, remkT)
import Flow exposing (Flow)
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class, classList, id, src)
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (CompareActiveData, CompareFile, CompareMode(..), CompareSelection, CompareSource(..), Model, StepRecord)
import Model.Lenses exposing (compareActive, compareLeftContent, compareLeftInspect, compareRightContent, compareRightInspect, compareSelecting, compareState, fileDelimitedGrid, gridState, projectStep, projects, records)
import Model.Shadow as Shadow exposing (ArgType, StepArgType(..), StepArgValue(..), TStringDisplay(..))
import Route exposing (Route)
import View.Icons exposing (icon)


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
            try (projects << records << success << by .id (Just sel.projectId)) model
                |> Maybe.unwrap "" .name

        stepName =
            try (projectStep (Just sel.projectId) (Just sel.recordId)) model
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
    let
        mTextPair =
            case ( Model.compareSelectionMode d.left, Model.compareSelectionMode d.right, ( d.leftContent, d.rightContent ) ) of
                ( CompareText, CompareText, ( Success l, Success r ) ) ->
                    if Maybe.isNothing l.delimitedGrid && Maybe.isNothing r.delimitedGrid then
                        Just ( l.text, r.text )

                    else
                        Nothing

                _ ->
                    Nothing

        panes =
            Html.div [ class "compare-panes" ]
                [ viewPane model d.left d.leftContent (gridUpdate compareLeftContent) d.leftInspect (toggleFlag compareLeftInspect)
                , viewPane model d.right d.rightContent (gridUpdate compareRightContent) d.rightInspect (toggleFlag compareRightInspect)
                ]
    in
    mTextPair
        |> Maybe.unwrap panes (\( l, r ) -> viewTextDiff model d l r)


viewPane : Model -> CompareSelection -> ApiData CompareFile -> (Flow Grid.State () -> Flow Model ()) -> Bool -> Flow Model () -> Html (Flow Model ())
viewPane model sel content gridFlow inspectOpen toggle =
    let
        mParams =
            derivationParamsFor model sel
    in
    Html.div [ class "compare-pane" ]
        [ viewPaneHeader model sel (Maybe.isJust mParams) inspectOpen toggle
        , Html.div [ class "compare-pane-body" ]
            [ viewPaneBody sel content gridFlow
            , viewInlineParams model sel inspectOpen mParams
            ]
        ]


gridUpdate : An_Optic pr ls CompareActiveData (ApiData CompareFile) -> Flow Grid.State () -> Flow Model ()
gridUpdate contentLens =
    Flow.via (compareState << compareActive << remkT contentLens << success << fileDelimitedGrid << just << gridState)


toggleFlag : An_Optic pr ls CompareActiveData Bool -> Flow Model ()
toggleFlag flagLens =
    Flow.over (compareState << compareActive << remkT flagLens) not


viewPaneHeader : Model -> CompareSelection -> Bool -> Bool -> Flow Model () -> Html (Flow Model ())
viewPaneHeader model sel hasParams inspectOpen toggle =
    Html.div [ class "compare-pane-header" ]
        [ Html.div [ class "compare-pane-label" ] [ Html.text (selectionLabel model sel) ]
        , Html.div [ class "compare-pane-actions" ]
            [ viewInspectButton hasParams inspectOpen toggle
            , Html.button
                [ class "dir-item-icon-btn compare-pane-source-btn"
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


derivationParamsFor : Model -> CompareSelection -> Maybe ( StepRecord, Dict String ArgType )
derivationParamsFor model sel =
    try (projectStep (Just sel.projectId) (Just sel.recordId)) model
        |> Maybe.andThen
            (\step ->
                Model.getStepConfig model
                    |> ApiData.toMaybe
                    |> Maybe.andThen (Dict.get step.type_)
                    |> Maybe.andThen (\entry -> try Shadow.derivation entry.stepType)
                    |> Maybe.map (\( argTypes, _ ) -> ( step, argTypes ))
            )


viewInlineParams : Model -> CompareSelection -> Bool -> Maybe ( StepRecord, Dict String ArgType ) -> Html (Flow Model ())
viewInlineParams model sel inspectOpen mParams =
    Html.viewIf inspectOpen <|
        Html.viewMaybe
            (\( step, argTypes ) ->
                Html.div [ class "compare-params" ]
                    [ Html.div [ class "compare-params-label" ] [ Html.text "Parameters" ]
                    , Html.div [ class "compare-params-form" ] (viewNote step.note ++ viewNamedArgs model sel.projectId step.args argTypes)
                    ]
            )
            mParams


viewNote : String -> List (Html (Flow Model ()))
viewNote note =
    if String.isEmpty (String.trim note) then
        []

    else
        [ viewParamRow "Note" (Html.text note) ]


viewNamedArgs : Model -> Int -> Dict String StepArgValue -> Dict String ArgType -> List (Html (Flow Model ()))
viewNamedArgs model projectId values fields =
    Dict.toList fields
        |> List.map
            (\( name, field ) ->
                viewParamRow (Maybe.unwrap name identity field.displayName)
                    (viewArgValue model projectId field.type_ (Dict.get name values))
            )


viewParamRow : String -> Html (Flow Model ()) -> Html (Flow Model ())
viewParamRow label valueHtml =
    Html.div [ class "compare-param" ]
        [ Html.div [ class "compare-param-label" ] [ Html.text label ]
        , Html.div [ class "compare-param-value" ] [ valueHtml ]
        ]


viewArgValue : Model -> Int -> StepArgType -> Maybe StepArgValue -> Html (Flow Model ())
viewArgValue model projectId argType mValue =
    case ( argType, mValue ) of
        ( TString (Code _) _, Just (TStringValue s) ) ->
            orEmptyValue s (Html.pre [ class "compare-param-code" ] [ Html.text s ])

        ( TString (Command prefix) _, Just (TStringValue s) ) ->
            orEmptyValue s (Html.code [ class "compare-param-code" ] [ Html.text (String.trim (prefix ++ " " ++ s)) ])

        ( TString _ _, Just (TStringValue s) ) ->
            orEmptyValue s (Html.text s)

        ( TStep _, Just (TStepValue stepId) ) ->
            Html.text (stepNameOf model projectId stepId)

        ( TUploadHash, Just (TUploadHashValue h) ) ->
            orEmptyValue h (Html.text h)

        ( TEnum _ labels, Just (TEnumValue v) ) ->
            Html.text (Maybe.unwrap v identity (Dict.get v labels))

        ( TList inner, Just (TListValue vs) ) ->
            if List.isEmpty vs then
                emptyValue

            else
                Html.ul [ class "compare-param-list" ]
                    (List.map (\v -> Html.li [] [ viewArgValue model projectId inner (Just v) ]) vs)

        ( TRecord fields, Just (TRecordValue dict) ) ->
            Html.div [ class "compare-param-record" ] (viewNamedArgs model projectId dict fields)

        _ ->
            emptyValue


orEmptyValue : String -> Html (Flow Model ()) -> Html (Flow Model ())
orEmptyValue s html =
    if String.isEmpty (String.trim s) then
        emptyValue

    else
        html


emptyValue : Html (Flow Model ())
emptyValue =
    Html.span [ class "compare-param-empty" ] [ Html.text "—" ]


stepNameOf : Model -> Int -> Int -> String
stepNameOf model projectId stepId =
    try (projectStep (Just projectId) (Just stepId)) model
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

                FromSrc ->
                    ( Route.Source, Nothing )
    in
    Route.Project
        { projectId = sel.projectId
        , mHighlight = Just { id = sel.recordId, target = target, path = sel.path, range = Nothing }
        , mCommit = mCommit
        , mCompare = Nothing
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
            let
                plain () =
                    Html.div [ class "compare-pane-text" ]
                        (List.map (\line -> Html.div [ class "diff-line" ] [ Html.text line ]) (String.lines file.text))
            in
            case file.delimitedGrid of
                Just grid ->
                    Grid.view gridFlow plain grid.grid

                Nothing ->
                    plain ()

        Error _ ->
            Html.div [ class "compare-error" ] [ Html.text "Failed to load file." ]

        _ ->
            Html.div [ class "compare-loading" ]
                [ Html.span [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading file…" ] ]


rawUrl : CompareSelection -> String
rawUrl sel =
    case sel.source of
        FromOutput commit_ ->
            Api.stepFileRawUrl sel.recordId (Just commit_) sel.path

        FromSrc ->
            Api.srcFileDownloadUrl sel.recordId sel.path


viewTextDiff : Model -> CompareActiveData -> String -> String -> Html (Flow Model ())
viewTextDiff model d leftStr rightStr =
    let
        rows =
            alignLines (String.lines leftStr) (String.lines rightStr)

        viewDiffPane sel inspectOpen toggle pick =
            let
                mParams =
                    derivationParamsFor model sel
            in
            Html.div [ class "compare-diff-pane" ]
                [ viewPaneHeader model sel (Maybe.isJust mParams) inspectOpen toggle
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
                    , viewInlineParams model sel inspectOpen mParams
                    ]
                ]
    in
    Html.div [ class "compare-diff" ]
        [ viewDiffPane d.left d.leftInspect (toggleFlag compareLeftInspect) Tuple.first
        , viewDiffPane d.right d.rightInspect (toggleFlag compareRightInspect) Tuple.second
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
