module Components.StatusBar exposing (view)

import Accessors exposing (just, set, try)
import Actions
import Api.ApiData as ApiData
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, target, title, type_)
import Html.Events as Events
import Html.Extra as Html
import Model.Core as Model exposing (ClusterStatus(..), Model, RunningStepSummary)
import Model.Lenses exposing (mCommit, route)
import Route
import View.Icons exposing (iconCustom)


view : Model -> Html (Flow Model ())
view model =
    let
        requestedOpen =
            Model.getStatusBarOpen model

        summaries =
            Model.getRunningStepSummaries model

        runningCount =
            List.length summaries

        isOpen =
            requestedOpen && runningCount > 0
    in
    Html.div [ class "status-bar-dock" ]
        [ Html.div
            [ classList
                [ ( "status-bar", True )
                , ( "status-bar--open", isOpen )
                ]
            ]
            ((if isOpen then
                [ viewPanel summaries ]

              else
                []
             )
                ++ [ Html.div [ class "status-bar__surface" ]
                        [ viewMainControl model runningCount isOpen
                        , viewRepoContext model
                        , viewIndependentControls model
                        ]
                   ]
            )
        ]


viewMainControl : Model -> Int -> Bool -> Html (Flow Model ())
viewMainControl model runningCount isOpen =
    let
        ( stateClass, stateText ) =
            clusterState (Model.getClusterStatus model)
    in
    Html.button
        ([ class "status-bar__main"
         , type_ "button"
         , Events.onClick Actions.toggleStatusBar
         , disabled (runningCount == 0)
         , attribute "aria-expanded" (boolText isOpen)
         , attribute "aria-label"
            (if runningCount == 0 then
                stateText ++ ", Idle"

             else
                stateText ++ ", " ++ runningText runningCount ++ ". Toggle running steps"
            )
         ]
            ++ (if isOpen then
                    [ attribute "aria-controls" "status-bar-panel" ]

                else
                    []
               )
        )
        [ Html.span
            [ classList
                [ ( "status-bar__state", True )
                , ( "status-bar__state--" ++ stateClass, True )
                ]
            ]
            [ Html.span
                [ class "status-bar__state-indicator"
                , attribute "aria-hidden" "true"
                ]
                []
            ]
        , Html.span
            [ class "status-bar__running-count"
            , attribute "aria-live" "polite"
            ]
            [ Html.text (runningText runningCount) ]
        , if runningCount > 0 then
            iconCustom False
                (if isOpen then
                    "keyboard_arrow_down"

                 else
                    "keyboard_arrow_up"
                )
                [ class "status-bar__expand-icon"
                , attribute "aria-hidden" "true"
                ]

          else
            Html.nothing
        ]


viewRepoContext : Model -> Html msg
viewRepoContext model =
    Maybe.map2
        (\repo commit ->
            let
                repoLabel =
                    repo.branch ++ " @ " ++ String.left 7 commit
            in
            case try (route << Route.project << mCommit << just) model of
                Just historicalCommit ->
                    let
                        switchLabel =
                            "Read-only view of past commit "
                                ++ historicalCommit
                                ++ ". View current version"
                    in
                    Html.a
                        [ class "status-bar__control status-bar__repo status-bar__repo--past"
                        , Route.href (set (Route.project << mCommit) Nothing (Model.getRoute model))
                        , title switchLabel
                        , attribute "aria-label" switchLabel
                        ]
                        [ Html.span [] [ Html.text repoLabel ]
                        , Html.span []
                            [ Html.span [ class "status-bar__repo-state" ] [ Html.text "Past" ]
                            , Html.text " · View current"
                            ]
                        ]

                Nothing ->
                    Html.span [ class "status-bar__repo" ] [ Html.text repoLabel ]
        )
        (ApiData.toMaybe (Model.getUserRepoInfo model))
        (ApiData.toMaybe (Model.getCommitHash model))
        |> Maybe.withDefault Html.nothing


viewIndependentControls : Model -> Html (Flow Model ())
viewIndependentControls model =
    let
        agent =
            Model.getAgent model
    in
    Html.div [ class "status-bar__actions" ]
        [ Html.button
            [ classList
                [ ( "status-bar__control", True )
                , ( "status-bar__agent", True )
                , ( "status-bar__agent--open", agent.isPanelOpen )
                ]
            , type_ "button"
            , Events.onClick Actions.toggleAgentPanel
            , title
                (if agent.isPanelOpen then
                    "Close AI agent"

                 else
                    "Open AI agent"
                )
            , attribute "aria-label"
                (if agent.isPanelOpen then
                    "Close AI agent"

                 else
                    "Open AI agent"
                )
            , attribute "aria-controls" "agent-panel"
            , attribute "aria-expanded" (boolText agent.isPanelOpen)
            ]
            [ iconCustom False "smart_toy" [ attribute "aria-hidden" "true" ]
            , Html.span [] [ Html.text "Agent" ]
            ]
        , Html.a
            [ class "status-bar__control status-bar__help"
            , href "https://pointy.cloud/"
            , target "_blank"
            , rel "noopener noreferrer"
            , title "Open documentation"
            , attribute "aria-label" "Open documentation"
            ]
            [ iconCustom False "help_outline" [ attribute "aria-hidden" "true" ]
            , Html.span [] [ Html.text "Docs" ]
            ]
        , Html.button
            [ class "status-bar__control status-bar__theme"
            , type_ "button"
            , Events.onClick Actions.toggleTheme
            , title "Toggle light/dark theme"
            , attribute "aria-label" "Toggle light/dark theme"
            ]
            [ iconCustom False
                "light_mode"
                [ class "status-bar__icon-dark"
                , attribute "aria-hidden" "true"
                ]
            , iconCustom False
                "dark_mode"
                [ class "status-bar__icon-light"
                , attribute "aria-hidden" "true"
                ]
            ]
        ]


viewPanel : List RunningStepSummary -> Html (Flow Model ())
viewPanel summaries =
    Html.section
        [ class "status-bar__panel"
        , id "status-bar-panel"
        ]
        [ Html.ul [ class "status-bar__list" ]
            (List.map viewRunningStep summaries)
        ]


viewRunningStep : RunningStepSummary -> Html (Flow Model ())
viewRunningStep summary =
    Html.li [ class "status-bar__item" ]
        [ Html.button
            [ class "status-bar__step"
            , type_ "button"
            , Events.onClick (Actions.openRunningStep summary.stepId)
            , attribute "aria-label"
                ("Open step ["
                    ++ String.fromInt summary.stepId
                    ++ "] "
                    ++ summary.stepName
                    ++ " in project "
                    ++ summary.projectName
                )
            ]
            [ Html.span
                [ class "status-indicator status-running status-bar__running-indicator"
                , attribute "aria-hidden" "true"
                ]
                []
            , Html.span [ class "status-bar__step-details" ]
                [ Html.span [ class "status-bar__step-name" ]
                    [ Html.text ("[" ++ String.fromInt summary.stepId ++ "] " ++ summary.stepName) ]
                , Html.span [ class "status-bar__project-name" ]
                    [ Html.text summary.projectName ]
                ]
            , iconCustom False
                "chevron_right"
                [ class "status-bar__chevron"
                , attribute "aria-hidden" "true"
                ]
            ]
        ]


clusterState : ClusterStatus -> ( String, String )
clusterState status =
    case status of
        ClusterAvailable ->
            ( "available", "Cluster available" )

        ClusterDegraded ->
            ( "degraded", "Cluster degraded" )

        ClusterUnavailable ->
            ( "unavailable", "Cluster unavailable" )

        ClusterUnknown ->
            ( "unknown", "Cluster status unknown" )


runningText : Int -> String
runningText count =
    case count of
        0 ->
            "Idle"

        1 ->
            "1 running"

        _ ->
            String.fromInt count ++ " running"


boolText : Bool -> String
boolText value =
    if value then
        "true"

    else
        "false"
