module Components.StatusBar exposing (view)

import Accessors exposing (try)
import Actions
import Api.ApiData as ApiData exposing (ApiData)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, href, id, rel, target, title, type_)
import Html.Events as Events
import Html.Extra as Html
import Model.Core as Model exposing (ClusterStatus(..), Model, TrayState(..), TrayStep)
import Model.Lenses exposing (route)
import Model.Lib exposing (canonicalNamePath, canonicalPathNames, rootProjectName)
import Route
import Set exposing (Set)
import Time
import View.Icons exposing (iconCustom)
import View.Lib exposing (boolText)
import View.Table as Table


type GroupKind
    = RunningGroup
    | QueuedGroup
    | StartingGroup
    | TransferGroup


type alias Group =
    { kind : GroupKind
    , steps : List TrayStep
    }


groupOrder : List GroupKind
groupOrder =
    [ RunningGroup, QueuedGroup, StartingGroup, TransferGroup ]


view : Model -> Html (Flow Model ())
view model =
    let
        steps =
            Model.getTraySteps model

        groups =
            groupSteps steps

        isOpen =
            Model.getStatusBarOpen model && not (List.isEmpty steps)
    in
    Html.div [ class "status-bar-dock" ]
        [ Html.div
            [ classList
                [ ( "status-bar", True )
                , ( "status-bar--open", isOpen )
                ]
            ]
            ((if isOpen then
                [ viewPanel model groups ]

              else
                []
             )
                ++ [ Html.div [ class "status-bar__surface" ]
                        [ viewHealth model
                        , viewSummary groups (not (List.isEmpty steps)) isOpen
                        , viewRepoContext model
                        , viewIndependentControls model
                        ]
                   ]
            )
        ]


groupKind : TrayState -> GroupKind
groupKind state =
    case state of
        TrayRunning _ ->
            RunningGroup

        TrayQueued _ _ ->
            QueuedGroup

        TrayStarting _ ->
            StartingGroup

        TrayTransferring _ ->
            TransferGroup


groupKey : GroupKind -> String
groupKey kind =
    case kind of
        RunningGroup ->
            "running"

        QueuedGroup ->
            "queued"

        StartingGroup ->
            "starting"

        TransferGroup ->
            "transferring"


groupTitle : GroupKind -> String
groupTitle kind =
    case kind of
        RunningGroup ->
            "Running"

        QueuedGroup ->
            "Queued"

        StartingGroup ->
            "Starting"

        TransferGroup ->
            "Uploading"


groupSteps : List TrayStep -> List Group
groupSteps steps =
    groupOrder
        |> List.filterMap
            (\kind ->
                case List.filter (.state >> groupKind >> (==) kind) steps of
                    [] ->
                        Nothing

                    members ->
                        Just
                            { kind = kind
                            , steps = List.sortBy stepOrder members
                            }
            )


stepOrder : TrayStep -> ( Int, Int )
stepOrder step =
    case step.state of
        TrayRunning since ->
            ( Time.posixToMillis since, step.stepId )

        TrayQueued since _ ->
            ( Time.posixToMillis since, step.stepId )

        _ ->
            ( 0, step.stepId )


viewHealth : Model -> Html msg
viewHealth model =
    let
        state =
            clusterState (Model.getClusterStatus model) (Model.getClusterDetail model)
    in
    Html.span
        [ classList
            [ ( "status-bar__state", True )
            , ( "status-bar__state--" ++ state.className, True )
            ]
        , attribute "role" "status"
        , title state.sentence
        , attribute "aria-label" state.sentence
        ]
        [ Html.span
            [ class "status-bar__state-indicator"
            , attribute "aria-hidden" "true"
            ]
            []
        , Html.span [ class "status-bar__state-label" ] [ Html.text state.label ]
        , case state.detail of
            Just detail ->
                Html.span [ class "status-bar__state-detail" ] [ Html.text detail ]

            Nothing ->
                Html.nothing
        ]


viewSummary : List Group -> Bool -> Bool -> Html (Flow Model ())
viewSummary groups expandable isOpen =
    let
        counts =
            List.map summaryCount groups

        spoken =
            if List.isEmpty counts then
                "Idle"

            else
                String.join ", " (List.map (\count -> String.fromInt count.count ++ " " ++ count.label) counts)
    in
    Html.button
        ([ class "status-bar__main"
         , type_ "button"
         , Events.onClick Actions.toggleStatusBar
         , disabled (not expandable)
         , title spoken
         , attribute "aria-expanded" (boolText isOpen)
         , attribute "aria-label"
            (if expandable then
                spoken ++ ". Toggle activity"

             else
                spoken
            )
         ]
            ++ (if isOpen then
                    [ attribute "aria-controls" "status-bar-panel" ]

                else
                    []
               )
        )
        [ Html.span
            [ class "status-bar__counts"
            , attribute "aria-live" "polite"
            ]
            (if List.isEmpty counts then
                [ Html.span [ class "status-bar__count status-bar__count--idle" ] [ Html.text "Idle" ] ]

             else
                List.map viewCount counts
            )
        , if expandable then
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


type alias SummaryCount =
    { kind : String
    , count : Int
    , label : String
    }


summaryCount : Group -> SummaryCount
summaryCount group =
    let
        count =
            List.length group.steps
    in
    case group.kind of
        RunningGroup ->
            { kind = "running", count = count, label = "running" }

        QueuedGroup ->
            { kind = "queued", count = count, label = "queued" }

        StartingGroup ->
            { kind = "starting", count = count, label = "starting" }

        TransferGroup ->
            { kind = "transferring", count = count, label = "uploading" }


viewCount : SummaryCount -> Html msg
viewCount count =
    Html.span [ class ("status-bar__count status-bar__count--" ++ count.kind) ]
        [ Html.span [ class "status-bar__marker", attribute "aria-hidden" "true" ] []
        , Html.span [ class "status-bar__count-number" ] [ Html.text (String.fromInt count.count) ]
        , Html.span [ class "status-bar__count-label" ] [ Html.text count.label ]
        ]


viewRepoContext : Model -> Html msg
viewRepoContext model =
    Maybe.map2
        (\repo commit ->
            case try (route << Route.page << Route.viewedCommitT) model of
                Just historicalCommit ->
                    let
                        switchLabel =
                            "Read-only view of past commit "
                                ++ historicalCommit
                                ++ ". View current version"
                    in
                    Html.a
                        [ class "status-bar__control status-bar__repo status-bar__repo--past"
                        , Route.href (Route.fromPage (Route.backToHead (Model.getRoute model).page))
                        , title switchLabel
                        , attribute "aria-label" switchLabel
                        ]
                        [ Html.span [] [ Html.text (repo.branch ++ " @ " ++ String.left 7 commit) ]
                        , Html.span []
                            [ Html.span [ class "status-bar__repo-state" ] [ Html.text "Past" ]
                            , Html.text " · View current"
                            ]
                        ]

                Nothing ->
                    let
                        repoTitle =
                            "Branch " ++ repo.branch ++ " at commit " ++ commit
                    in
                    Html.span
                        [ class "status-bar__repo"
                        , title repoTitle
                        , attribute "aria-label" repoTitle
                        ]
                        [ iconCustom False "commit" [ class "status-bar__repo-icon", attribute "aria-hidden" "true" ]
                        , Html.span [] [ Html.text repo.branch ]
                        ]
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


viewPanel : Model -> List Group -> Html (Flow Model ())
viewPanel model groups =
    let
        collapsed =
            Model.getStatusBarCollapsed model
    in
    Html.section
        [ class "status-bar__panel"
        , id "status-bar-panel"
        ]
        [ Html.div [ class "status-bar__groups" ]
            (List.map (viewGroup model collapsed) groups)
        ]


viewGroup : Model -> Set String -> Group -> Html (Flow Model ())
viewGroup model collapsed group =
    let
        key =
            groupKey group.kind

        isCollapsed =
            Set.member key collapsed

        listId =
            "status-bar-group-" ++ key
    in
    Html.div [ class ("status-bar__group status-bar__group--" ++ key) ]
        [ Html.button
            [ class "status-bar__group-header"
            , type_ "button"
            , Events.onClick (Actions.toggleStatusBarGroup key)
            , attribute "aria-expanded" (boolText (not isCollapsed))
            , attribute "aria-controls" listId
            ]
            [ iconCustom False
                (if isCollapsed then
                    "chevron_right"

                 else
                    "expand_more"
                )
                [ class "listing-group-icon", attribute "aria-hidden" "true" ]
            , Html.text (groupTitle group.kind)
            , Html.span [ class "listing-group-count" ] [ Html.text ("(" ++ String.fromInt (List.length group.steps) ++ ")") ]
            ]
        , if isCollapsed then
            Html.nothing

          else
            Html.ul [ class "status-bar__list", id listId ]
                (List.map (viewStep model) group.steps)
        ]


viewStep : Model -> TrayStep -> Html (Flow Model ())
viewStep model step =
    let
        now =
            Model.getNow model

        folderPath =
            Maybe.map (canonicalNamePath model) step.projectId

        folderTrail =
            case Maybe.map (canonicalPathNames model) step.projectId of
                Just [] ->
                    rootProjectName model

                Just names ->
                    String.join " / " names

                Nothing ->
                    "Unfiled"

        meta =
            stateMeta now step.state

        offHead =
            case ( Model.getCommitHash model |> ApiData.toMaybe, step.commits ) of
                ( Just head, commit :: _ ) ->
                    if List.member head step.commits then
                        Nothing

                    else
                        Just commit

                _ ->
                    Nothing

        stoppable =
            case step.state of
                TrayRunning _ ->
                    True

                TrayQueued _ _ ->
                    True

                TrayStarting _ ->
                    True

                _ ->
                    False
    in
    Html.li [ class "status-bar__item" ]
        [ Html.button
            [ class "status-bar__step"
            , type_ "button"
            , Events.onClick (Actions.openRunningStep step.stepId)
            , title (step.stepName ++ " — " ++ meta.description)
            , attribute "aria-label"
                ("Open step ["
                    ++ String.fromInt step.stepId
                    ++ "] "
                    ++ step.stepName
                    ++ Maybe.withDefault "" (Maybe.map (\path -> " in " ++ path) folderPath)
                    ++ ". "
                    ++ meta.description
                )
            ]
            [ Html.span
                [ class ("status-bar__step-marker " ++ markerClass step.state)
                , attribute "aria-hidden" "true"
                ]
                []
            , Html.span [ class "status-bar__step-details" ]
                [ Html.span [ class "status-bar__step-line" ]
                    [ Html.span [ class "status-bar__step-id" ] [ Html.text (String.fromInt step.stepId) ]
                    , Html.span [ class "status-bar__step-name" ] [ Html.text step.stepName ]
                    , case offHead of
                        Just commit ->
                            Html.span
                                [ class "step-review-revision"
                                , title ("Building commit " ++ String.left 7 commit ++ ", not the current version")
                                ]
                                [ Html.span [ class "step-review-revision-hash" ] [ Html.text (String.left 7 commit) ] ]

                        Nothing ->
                            Html.nothing
                    ]
                , Html.span
                    [ class "status-bar__step-path"
                    , title (Maybe.withDefault "Not in any folder" folderPath)
                    ]
                    [ Html.text folderTrail ]
                ]
            , Html.span [ class ("status-bar__step-meta status-bar__step-meta--" ++ meta.tone) ]
                [ Html.text meta.short ]
            , iconCustom False
                "chevron_right"
                [ class "status-bar__chevron"
                , attribute "aria-hidden" "true"
                ]
            ]
        , if not stoppable then
            Html.nothing

          else if Set.member step.stepId (Model.getPendingStops model) then
            Table.viewStoppingIndicator

          else
            Table.viewStopButton "Stop" (Actions.stopStepAt step.stepId (List.head step.commits))
        ]


markerClass : TrayState -> String
markerClass state =
    case state of
        TrayRunning _ ->
            "status-indicator status-running"

        TrayQueued _ _ ->
            "status-bar__marker status-bar__marker--queued"

        TrayStarting _ ->
            "status-bar__marker status-bar__marker--starting"

        TrayTransferring _ ->
            "status-bar__marker status-bar__marker--transferring"


type alias Meta =
    { short : String
    , description : String
    , tone : String
    }


stateMeta : Time.Posix -> TrayState -> Meta
stateMeta now state =
    let
        since time =
            Time.posixToMillis now - Time.posixToMillis time
    in
    case state of
        TrayRunning started ->
            { short = durationText (since started)
            , description = "Running for " ++ durationWords (since started)
            , tone = "running"
            }

        TrayQueued queued reason ->
            let
                waited =
                    durationText (since queued)
            in
            { short =
                case Maybe.map queueReasonLabel reason of
                    Just label ->
                        label ++ " · " ++ waited

                    Nothing ->
                        waited
            , description =
                "Queued for "
                    ++ durationWords (since queued)
                    ++ (case reason of
                            Just slurmReason ->
                                ", " ++ queueReasonSentence slurmReason

                            Nothing ->
                                ""
                       )
            , tone = "queued"
            }

        TrayStarting (Just started) ->
            { short = durationText (since started)
            , description = "Preparing the build for " ++ durationWords (since started)
            , tone = "starting"
            }

        TrayStarting Nothing ->
            { short = "Starting"
            , description = "Starting"
            , tone = "starting"
            }

        TrayTransferring (Just progress) ->
            { short = String.fromInt (round (progress * 100)) ++ "%"
            , description = "Uploading, " ++ String.fromInt (round (progress * 100)) ++ "% done"
            , tone = "transferring"
            }

        TrayTransferring Nothing ->
            { short = "Uploading"
            , description = "Uploading"
            , tone = "transferring"
            }


queueReasonLabel : String -> String
queueReasonLabel reason =
    case reason of
        "Dependency" ->
            "Dependency"

        "Resources" ->
            "Resources"

        "Priority" ->
            "Priority"

        "BeginTime" ->
            "Scheduled"

        "JobHeldUser" ->
            "Held"

        "JobHeldAdmin" ->
            "Held"

        _ ->
            if String.startsWith "ReqNodeNotAvail" reason then
                "Nodes unavailable"

            else
                String.replace "_" " " reason


queueReasonSentence : String -> String
queueReasonSentence reason =
    case reason of
        "Dependency" ->
            "waiting for upstream steps to finish"

        "Resources" ->
            "waiting for free CPUs or memory"

        "Priority" ->
            "waiting behind higher-priority jobs"

        "BeginTime" ->
            "scheduled to start later"

        "JobHeldUser" ->
            "held by its owner"

        "JobHeldAdmin" ->
            "held by an administrator"

        _ ->
            if String.startsWith "ReqNodeNotAvail" reason then
                "waiting for unavailable nodes"

            else
                "Slurm reason: " ++ reason


durationText : Int -> String
durationText millis =
    let
        seconds =
            max 0 millis // 1000
    in
    if seconds < 60 then
        String.fromInt seconds ++ "s"

    else if seconds < 3600 then
        String.fromInt (seconds // 60) ++ "m"

    else if seconds < 86400 then
        String.fromInt (seconds // 3600) ++ "h " ++ String.fromInt (modBy 60 (seconds // 60)) ++ "m"

    else
        String.fromInt (seconds // 86400) ++ "d " ++ String.fromInt (modBy 24 (seconds // 3600)) ++ "h"


durationWords : Int -> String
durationWords millis =
    let
        seconds =
            max 0 millis // 1000

        plural count unit =
            String.fromInt count
                ++ " "
                ++ unit
                ++ (if count == 1 then
                        ""

                    else
                        "s"
                   )
    in
    if seconds < 60 then
        plural seconds "second"

    else if seconds < 3600 then
        plural (seconds // 60) "minute"

    else if seconds < 86400 then
        plural (seconds // 3600) "hour" ++ " " ++ plural (modBy 60 (seconds // 60)) "minute"

    else
        plural (seconds // 86400) "day" ++ " " ++ plural (modBy 24 (seconds // 3600)) "hour"


type alias ClusterState =
    { className : String
    , label : String
    , sentence : String
    , detail : Maybe String
    }


clusterState : ApiData ClusterStatus -> Maybe String -> ClusterState
clusterState apiStatus detail =
    let
        ( className, label, baseSentence ) =
            case apiStatus of
                ApiData.NotAsked ->
                    ( "loading", "Connecting", "Loading cluster status" )

                ApiData.Loading _ ->
                    ( "loading", "Connecting", "Loading cluster status" )

                ApiData.Error _ ->
                    ( "unknown", "Cluster unknown", "Cluster status unknown" )

                ApiData.Success status ->
                    case status of
                        ClusterAvailable ->
                            ( "available", "Cluster OK", "Cluster available" )

                        ClusterDegraded ->
                            ( "degraded", "Degraded", "Cluster degraded" )

                        ClusterUnavailable ->
                            ( "unavailable", "Unavailable", "Cluster unavailable" )

                        ClusterUnknown ->
                            ( "unknown", "Cluster unknown", "Cluster status unknown" )

        reported =
            Maybe.andThen nonEmptyDetail detail
    in
    { className = className
    , label = label
    , sentence =
        case reported of
            Just text ->
                baseSentence ++ ": " ++ text

            Nothing ->
                baseSentence
    , detail = reported
    }


nonEmptyDetail : String -> Maybe String
nonEmptyDetail detail =
    if String.isEmpty (String.trim detail) then
        Nothing

    else
        Just detail
