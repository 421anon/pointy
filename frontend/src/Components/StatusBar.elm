module Components.StatusBar exposing (view)

import Accessors exposing (get, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (action, attribute, class, classList, disabled, href, id, method, rel, style, target, title, type_)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import List.Extra as List
import Model.Core as Model exposing (ClusterStatus(..), Model, TrayState(..), TrayStep)
import Model.Lenses exposing (loadedProjects, route, toastHistory)
import Model.Lib exposing (canonicalNamePath, canonicalPathNames, rootProjectName)
import Route
import Set exposing (Set)
import Time
import Toast
import View.Icons exposing (iconCustom)
import View.Lib exposing (boolText)
import View.Organize as Organize
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

        summary =
            summarize (Model.getClusterStatus model) groups

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
                        , viewSummary summary (not (List.isEmpty steps)) isOpen
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


viewSummary : Summary -> Bool -> Bool -> Html (Flow Model ())
viewSummary summary expandable isOpen =
    let
        spoken =
            spokenSummary summary
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
            (case summary of
                SummaryLoading ->
                    []

                SummaryIdle ->
                    [ Html.span [ class "status-bar__count status-bar__count--idle" ] [ Html.text "Idle" ] ]

                SummaryActive counts ->
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


type Summary
    = SummaryLoading
    | SummaryIdle
    | SummaryActive (List SummaryCount)


summarize : ApiData ClusterStatus -> List Group -> Summary
summarize clusterStatus groups =
    case List.map summaryCount groups of
        [] ->
            if ApiData.settled clusterStatus then
                SummaryIdle

            else
                SummaryLoading

        counts ->
            SummaryActive counts


spokenSummary : Summary -> String
spokenSummary summary =
    case summary of
        SummaryLoading ->
            "Loading activity"

        SummaryIdle ->
            "Idle"

        SummaryActive counts ->
            String.join ", " (List.map (\count -> String.fromInt count.count ++ " " ++ count.label) counts)


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
        [ viewToastHistory model
        , Html.button
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
        , viewUserMenu model
        ]


viewToastHistory : Model -> Html (Flow Model ())
viewToastHistory model =
    let
        popoverId =
            "toast-history-popover"
    in
    Html.span [ class "toast-history" ]
        [ Html.button
            [ class "status-bar__control"
            , type_ "button"
            , attribute "popovertarget" popoverId
            , style "anchor-name" ("--anchor-" ++ popoverId)
            , title "Notifications"
            , attribute "aria-label" "Notifications"
            ]
            [ iconCustom False "notifications" [ attribute "aria-hidden" "true" ] ]
        , Html.Keyed.node "div"
            [ class "listing-popover toast-history__popover"
            , id popoverId
            , attribute "popover" "auto"
            , style "position-anchor" ("--anchor-" ++ popoverId)
            ]
            (( "header", Html.strong [ class "toast-history__header" ] [ Html.text "Notifications" ] )
                :: (case get toastHistory model of
                        [] ->
                            [ ( "empty", Html.span [ class "toast-history__empty" ] [ Html.text "Nothing yet" ] ) ]

                        history ->
                            List.map (Toast.viewPast (Model.getNow model)) history
                   )
            )
        ]


viewUserMenu : Model -> Html (Flow Model ())
viewUserMenu model =
    Html.viewMaybe
        (\session ->
            let
                user =
                    Maybe.withDefault session.account session.user

                label =
                    "Signed in as " ++ user ++ " · " ++ session.account
            in
            Organize.viewMenuPopover
                { popoverId = "status-bar-user-menu"
                , wrapperClass = "status-bar__user"
                , triggerAttrs =
                    [ class "status-bar__control"
                    , type_ "button"
                    , title label
                    , attribute "aria-label" label
                    ]
                , triggerContent =
                    [ iconCustom False "account_circle" [ attribute "aria-hidden" "true" ]
                    , Html.span [ class "status-bar__user-name" ] [ Html.text user ]
                    ]
                , content =
                    [ Html.div [ class "status-bar__user-identity" ]
                        [ Html.strong [] [ Html.text user ]
                        , Html.span [] [ Html.text session.account ]
                        ]
                    , Html.form [ method "post", action "/token-logout" ]
                        [ Html.button [ class "listing-menu-item", type_ "submit" ]
                            [ iconCustom False "logout" [ attribute "aria-hidden" "true" ]
                            , Html.text "Log out"
                            ]
                        ]
                    ]
                }
        )
        (Model.getTokenSession model)


stepRowHeight : Int
stepRowHeight =
    36


groupHeaderHeight : Int
groupHeaderHeight =
    26


groupBorderWidth : Int
groupBorderWidth =
    1


panelMaxHeight : Int
panelMaxHeight =
    10 * stepRowHeight


rowOverscan : Int
rowOverscan =
    panelMaxHeight


type alias RowWindow =
    { first : Int
    , last : Int
    }


rowWindow : Int -> Int -> Int -> RowWindow
rowWindow top bottom count =
    let
        first =
            clamp 0 count (top // stepRowHeight)
    in
    { first = first
    , last = clamp first count ((bottom + stepRowHeight - 1) // stepRowHeight)
    }


viewPanel : Model -> List Group -> Html (Flow Model ())
viewPanel model groups =
    let
        collapsed =
            Model.getStatusBarCollapsed model

        listHeight group =
            if Set.member (groupKey group.kind) collapsed then
                0

            else
                List.length group.steps * stepRowHeight

        contentHeight =
            List.sum (List.map (\group -> groupHeaderHeight + listHeight group) groups)
                + groupBorderWidth
                * (List.length groups - 1)

        scrollTop =
            clamp 0 (max 0 (contentHeight - panelMaxHeight)) (round (Model.getStatusBarScrollTop model))

        viewPlacedGroup groupTop group =
            let
                listTop =
                    groupTop + groupHeaderHeight

                window =
                    rowWindow
                        (scrollTop - rowOverscan - listTop)
                        (scrollTop + panelMaxHeight + rowOverscan - listTop)
                        (List.length group.steps)
            in
            ( listTop + listHeight group + groupBorderWidth
            , ( groupKey group.kind, viewGroup model collapsed window group )
            )
    in
    Html.section
        [ class "status-bar__panel"
        , id "status-bar-panel"
        ]
        [ Html.Keyed.node "div"
            [ class "status-bar__groups"
            , Events.on "scroll" (Decode.map Actions.setStatusBarScrollTop (Decode.at [ "target", "scrollTop" ] Decode.float))
            ]
            (Tuple.second (List.mapAccuml viewPlacedGroup 0 groups))
        ]


viewGroup : Model -> Set String -> RowWindow -> Group -> Html (Flow Model ())
viewGroup model collapsed window group =
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
            Html.Keyed.ul
                [ class "status-bar__list"
                , id listId
                , style "height" (String.fromInt (List.length group.steps * stepRowHeight) ++ "px")
                , style "padding-top" (String.fromInt (window.first * stepRowHeight) ++ "px")
                ]
                (group.steps
                    |> List.drop window.first
                    |> List.take (window.last - window.first)
                    |> List.map (\step -> ( String.fromInt step.stepId, viewStep model group.kind step ))
                )
        ]


viewStep : Model -> GroupKind -> TrayStep -> Html (Flow Model ())
viewStep model kind step =
    let
        meta =
            stateMeta (Model.getNow model) step.state

        ( folderTrail, folderPath ) =
            case ( try loadedProjects model, step.projectId ) of
                ( Just projects_, Just projectId ) ->
                    ( case canonicalPathNames model projectId of
                        [] ->
                            rootProjectName projects_

                        names ->
                            String.join " / " names
                    , canonicalNamePath model projectId
                    )

                _ ->
                    ( "", "" )

        offHeadRevision =
            case ( Model.getCommitHash model |> ApiData.toMaybe, step.commits ) of
                ( Just head, commit :: _ ) ->
                    if List.member head step.commits then
                        ""

                    else
                        String.left 7 commit

                _ ->
                    ""
    in
    Html.li [ class "status-bar__item" ]
        [ Html.Lazy.lazy8 viewStepButton kind step.stepId step.stepName folderTrail folderPath offHeadRevision meta.short meta.description
        , Html.Lazy.lazy4 viewStopControl (kind /= TransferGroup) (Set.member step.stepId (Model.getPendingStops model)) step.stepId step.commits
        ]


viewStepButton : GroupKind -> Int -> String -> String -> String -> String -> String -> String -> Html (Flow Model ())
viewStepButton kind stepId stepName folderTrail folderPath offHeadRevision metaShort metaDescription =
    Html.button
        [ class "status-bar__step"
        , type_ "button"
        , Events.onClick (Actions.openRunningStep stepId)
        , title (stepName ++ " — " ++ metaDescription)
        , attribute "aria-label"
            ("Open step ["
                ++ String.fromInt stepId
                ++ "] "
                ++ stepName
                ++ (if String.isEmpty folderPath then
                        ""

                    else
                        " in " ++ folderPath
                   )
                ++ ". "
                ++ metaDescription
            )
        ]
        [ Html.span
            [ class ("status-bar__step-marker " ++ markerClass kind)
            , attribute "aria-hidden" "true"
            ]
            []
        , Html.span [ class "status-bar__step-details" ]
            [ Html.span [ class "status-bar__step-line" ]
                [ Html.span [ class "status-bar__step-id" ] [ Html.text (String.fromInt stepId) ]
                , Html.span [ class "status-bar__step-name" ] [ Html.text stepName ]
                , Html.viewIf (not (String.isEmpty offHeadRevision)) <|
                    Html.span
                        [ class "step-review-revision"
                        , title ("Building commit " ++ offHeadRevision ++ ", not the current version")
                        ]
                        [ Html.span [ class "step-review-revision-hash" ] [ Html.text offHeadRevision ] ]
                ]
            , Html.viewIf (not (String.isEmpty folderTrail)) <|
                Html.span [ class "status-bar__step-path", title folderPath ] [ Html.text folderTrail ]
            ]
        , Html.span [ class ("status-bar__step-meta status-bar__step-meta--" ++ groupKey kind) ]
            [ Html.text metaShort ]
        , iconCustom False
            "chevron_right"
            [ class "status-bar__chevron"
            , attribute "aria-hidden" "true"
            ]
        ]


viewStopControl : Bool -> Bool -> Int -> List String -> Html (Flow Model ())
viewStopControl stoppable stopping stepId commits =
    if not stoppable then
        Html.nothing

    else if stopping then
        Table.viewStoppingIndicator

    else
        Table.viewStopButton "Stop" (Actions.stopStepAt stepId (List.head commits))


markerClass : GroupKind -> String
markerClass kind =
    case kind of
        RunningGroup ->
            "status-indicator status-running"

        QueuedGroup ->
            "status-bar__marker status-bar__marker--queued"

        StartingGroup ->
            "status-bar__marker status-bar__marker--starting"

        TransferGroup ->
            "status-bar__marker status-bar__marker--transferring"


type alias Meta =
    { short : String
    , description : String
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
            }

        TrayStarting (Just started) ->
            { short = durationText (since started)
            , description = "Preparing the build for " ++ durationWords (since started)
            }

        TrayStarting Nothing ->
            { short = "Starting"
            , description = "Starting"
            }

        TrayTransferring (Just progress) ->
            { short = String.fromInt (round (progress * 100)) ++ "%"
            , description = "Uploading, " ++ String.fromInt (round (progress * 100)) ++ "% done"
            }

        TrayTransferring Nothing ->
            { short = "Uploading"
            , description = "Uploading"
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
