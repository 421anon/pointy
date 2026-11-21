module Components.StatusBar exposing (view)

import Accessors exposing (get, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData)
import Dict exposing (Dict)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (action, attribute, class, classList, disabled, href, id, method, placeholder, rel, style, target, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Iso8601
import Json.Decode as Decode
import List.Extra as List
import Model.Core as Model exposing (BackendConnection(..), ClusterStatus(..), Model, StorageRevision, StorageState(..), StorageStep, StorageView, TrayState(..), TrayStep)
import Model.Lenses exposing (loadedProjects, route, toastHistory)
import Model.Lib exposing (canonicalNamePath, canonicalPathNames, rootProjectName)
import Route
import Set exposing (Set)
import Time
import Time.Distance
import Toast
import View.Icons exposing (iconCustom)
import View.Lib exposing (boolText)
import View.Organize as Organize
import View.Table as Table


type GroupKind
    = RunningGroup
    | SavingGroup
    | QueuedGroup
    | StartingGroup
    | TransferGroup


type alias Group =
    { kind : GroupKind
    , steps : List TrayStep
    }


groupOrder : List GroupKind
groupOrder =
    [ RunningGroup, SavingGroup, QueuedGroup, StartingGroup, TransferGroup ]


view : Model -> Html (Flow Model ())
view model =
    let
        steps =
            Model.getTraySteps model

        groups =
            groupSteps steps

        isConnected =
            Model.getBackendConnection model == BackendUp

        summary =
            if isConnected then
                summarize (Model.getClusterStatus model) groups

            else
                SummaryDisconnected

        expandable =
            isConnected && not (List.isEmpty steps)

        isOpen =
            Model.getStatusBarOpen model && expandable

        isStorageOpen =
            Model.getStorageOpen model
    in
    Html.div [ class "status-bar-dock" ]
        [ Html.div
            [ classList
                [ ( "status-bar", True )
                , ( "status-bar--open", isOpen || isStorageOpen )
                ]
            ]
            (Html.div [ class "status-bar__surface" ]
                [ viewHealth model
                , viewSummary summary expandable isOpen
                , viewRepoContext model
                , viewIndependentControls model
                ]
                :: (if isStorageOpen then
                        [ viewStoragePanel model ]

                    else if isOpen then
                        [ viewPanel model groups ]

                    else
                        []
                   )
            )
        ]


groupKind : TrayState -> GroupKind
groupKind state =
    case state of
        TrayRunning _ ->
            RunningGroup

        TraySaving _ ->
            SavingGroup

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

        SavingGroup ->
            "saving"

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

        SavingGroup ->
            "Saving results"

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

        TraySaving since ->
            ( Time.posixToMillis since, step.stepId )

        TrayQueued since _ ->
            ( Time.posixToMillis since, step.stepId )

        _ ->
            ( 0, step.stepId )


viewHealth : Model -> Html msg
viewHealth model =
    viewHealthState
        (case Model.getBackendConnection model of
            BackendUp ->
                clusterState (Model.getClusterStatus model) (Model.getClusterDetail model)

            BackendDown ->
                { className = "unreachable"
                , label = "Server unreachable"
                , sentence = "Lost connection to the server. Reconnecting…"
                , detail = Just "Reconnecting…"
                }
        )


viewHealthState : HealthState -> Html msg
viewHealthState state =
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

                SummaryDisconnected ->
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

        SavingGroup ->
            { kind = "saving", count = count, label = "saving results" }

        QueuedGroup ->
            { kind = "queued", count = count, label = "queued" }

        StartingGroup ->
            { kind = "starting", count = count, label = "starting" }

        TransferGroup ->
            { kind = "transferring", count = count, label = "uploading" }


type Summary
    = SummaryLoading
    | SummaryDisconnected
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

        SummaryDisconnected ->
            "Activity unavailable while reconnecting"

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
        [ viewStorageControl model
        , viewToastHistory model
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
            , href "/docs/"
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
        , Events.onClick (Actions.openStep stepId)
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

        SavingGroup ->
            "status-bar__marker status-bar__marker--saving"

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

        TraySaving started ->
            { short = durationText (since started)
            , description = "Saving results for " ++ durationWords (since started)
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


type alias HealthState =
    { className : String
    , label : String
    , sentence : String
    , detail : Maybe String
    }


clusterState : ApiData ClusterStatus -> Maybe String -> HealthState
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


viewStorageControl : Model -> Html (Flow Model ())
viewStorageControl model =
    let
        current =
            ApiData.toMaybe (Model.getStorageView model)

        meter =
            Maybe.andThen storageMeter current

        label =
            Maybe.withDefault "Storage center" <|
                Maybe.map
                    (\snapshot -> "Storage center · " ++ Model.formatStorageSize snapshot.local ++ " in step outputs")
                    current
    in
    Html.button
        [ id Actions.storageTriggerId
        , class "status-bar__control status-bar__storage-control"
        , classList [ ( "status-bar__control--active", Model.getStorageOpen model ) ]
        , type_ "button"
        , Events.onClick Actions.toggleStoragePanel
        , title "Storage center"
        , attribute "aria-label" label
        , attribute "aria-controls" Actions.storagePanelId
        , attribute "aria-expanded" (boolText (Model.getStorageOpen model))
        ]
        [ iconCustom False "hard_drive" [ attribute "aria-hidden" "true" ]
        , Html.viewMaybe
            (\size -> Html.span [ class "status-bar__storage-size" ] [ Html.text size ])
            (Maybe.map (Model.formatStorageSize << .local) current)
        , Html.viewMaybe
            (\gauge ->
                Html.span
                    [ class "status-bar__storage-meter"
                    , classList [ ( "status-bar__storage-meter--low", gauge.low ) ]
                    , attribute "aria-hidden" "true"
                    ]
                    [ Html.span [ style "width" (String.fromInt gauge.usedPercent ++ "%") ] [] ]
            )
            meter
        ]


type alias StorageMeter =
    { usedPercent : Int
    , low : Bool
    }


storageMeter : StorageView -> Maybe StorageMeter
storageMeter snapshot =
    if snapshot.diskTotal <= 0 then
        Nothing

    else
        let
            free =
                clamp 0 snapshot.diskTotal snapshot.free

            used =
                snapshot.diskTotal - free
        in
        Just
            { usedPercent = ratioPercent used snapshot.diskTotal
            , low = free * 10 < snapshot.diskTotal
            }


viewStoragePanel : Model -> Html (Flow Model ())
viewStoragePanel model =
    Html.section
        [ class "status-bar__panel storage-panel"
        , id Actions.storagePanelId
        , attribute "tabindex" "-1"
        , attribute "role" "region"
        , attribute "aria-labelledby" "storage-panel-title"
        ]
        [ Html.div [ class "storage-panel__header" ]
            [ iconCustom False "hard_drive" [ attribute "aria-hidden" "true" ]
            , Html.strong [ id "storage-panel-title" ] [ Html.text "Storage center" ]
            , Html.span [ class "storage-panel__updated" ]
                [ Html.viewMaybe
                    (\snapshot ->
                        Html.node "time"
                            [ attribute "datetime" (Iso8601.fromTime snapshot.updatedAt)
                            , title ("Last checked " ++ Iso8601.fromTime snapshot.updatedAt)
                            ]
                            [ Html.text ("Checked " ++ Time.Distance.inWords snapshot.updatedAt (Model.getNow model)) ]
                    )
                    (ApiData.toMaybe (Model.getStorageView model))
                ]
            , Html.button
                [ class "icon-btn"
                , type_ "button"
                , Events.onClick Actions.toggleStoragePanel
                , title "Close storage center"
                , attribute "aria-label" "Close storage center"
                ]
                [ iconCustom False "close" [ attribute "aria-hidden" "true" ] ]
            ]
        , case Model.getStorageView model of
            ApiData.Success snapshot ->
                viewStorageContent model snapshot

            ApiData.Loading (Just snapshot) ->
                viewStorageContent model snapshot

            ApiData.Loading Nothing ->
                Html.div [ class "storage-panel__empty" ] [ Html.text "Reading storage usage…" ]

            ApiData.NotAsked ->
                Html.div [ class "storage-panel__empty" ] [ Html.text "Reading storage usage…" ]

            ApiData.Error _ ->
                Html.div [ class "storage-panel__empty storage-panel__empty--error" ] [ Html.text "Storage usage is unavailable. Try again in a moment." ]
        ]


viewStorageContent : Model -> StorageView -> Html (Flow Model ())
viewStorageContent model snapshot =
    let
        filter =
            String.trim (Model.getStorageFilter model)

        groups =
            storageGroups (String.toLower filter) snapshot

        expandedSteps =
            Model.getStorageExpandedSteps model
    in
    Html.div [ class "storage-panel__content" ]
        [ viewStorageUsage snapshot
        , viewStorageScan snapshot
        , Html.viewIf snapshot.truncated <|
            Html.div [ class "storage-panel__notice" ]
                [ Html.text "Older history is not scanned; the scan stops after 5000 changes." ]
        , Html.viewMaybe
            (\message -> Html.div [ class "storage-panel__notice storage-panel__notice--error" ] [ Html.text message ])
            snapshot.error
        , Html.div
            [ class "storage-panel__live"
            , attribute "role" "status"
            , attribute "aria-live" "polite"
            , attribute "aria-atomic" "true"
            ]
            [ Html.text (storageLiveStatus snapshot (Model.getStorageMoving model)) ]
        , if List.isEmpty snapshot.steps then
            Html.div [ class "storage-panel__empty" ]
                [ Html.text
                    (if snapshot.scanning then
                        "Looking for step outputs…"

                     else
                        "No step outputs are on this server."
                    )
                ]

          else
            Html.div
                [ classList
                    [ ( "storage-panel__list", True )
                    , ( "storage-panel__list--cold", snapshot.coldConfigured )
                    ]
                ]
                [ viewStorageToolbar model
                , Html.div [ class "storage-panel__columns" ]
                    ([ Html.span [] [ Html.text "Step / revision" ]
                     , Html.span [ class "storage-panel__size" ] [ Html.text "Local" ]
                     ]
                        ++ (if snapshot.coldConfigured then
                                [ Html.span [ class "storage-panel__size" ] [ Html.text "Cold" ]
                                , Html.span [] []
                                ]

                            else
                                []
                           )
                    )
                , if List.isEmpty groups then
                    Html.div [ class "storage-panel__empty" ]
                        [ Html.text "No steps or revisions match this filter." ]

                  else
                    Html.div [ class "storage-panel__rows", attribute "role" "list" ]
                        (List.map
                            (viewStorageGroup model snapshot.coldConfigured expandedSteps filter)
                            groups
                        )
                ]
        , viewStorageFooter snapshot
        ]


viewStorageUsage : StorageView -> Html msg
viewStorageUsage snapshot =
    let
        reportedDiskTotal =
            max 0 snapshot.diskTotal

        diskTotal =
            storageDiskTotal snapshot

        free =
            clamp 0 diskTotal snapshot.free

        used =
            max 0 (diskTotal - free)

        stepBytes =
            max 0 snapshot.local

        otherBytes =
            max 0 (used - stepBytes)

        barOutputs =
            clamp 0 used stepBytes

        low =
            diskTotal > 0 && free * 10 < diskTotal

        partial =
            Html.viewIf snapshot.scanning <|
                Html.span [ class "storage-panel__partial" ] [ Html.text "so far" ]

        ofDisk =
            Html.viewIf (reportedDiskTotal > 0) <|
                Html.span [ class "storage-panel__of" ]
                    [ Html.text "of "
                    , Html.strong [] [ Html.text (Model.formatStorageSize reportedDiskTotal) ]
                    , Html.text " disk"
                    ]
    in
    Html.div [ class "storage-panel__usage" ]
        [ Html.div [ class "storage-panel__scope-title" ]
            [ Html.text "On this server"
            , partial
            ]
        , Html.div [ class "storage-panel__totals" ]
            [ storageTotal stepBytes "step outputs"
            , storageTotal otherBytes "other data"
            , storageTotal free "free"
            , ofDisk
            ]
        , Html.viewIf (diskTotal > 0) <|
            Html.div
                [ class "storage-panel__bar"
                , classList [ ( "storage-panel__bar--low", low ) ]
                , attribute "role" "img"
                , attribute "aria-label"
                    ("On this server: "
                        ++ Model.formatStorageSize stepBytes
                        ++ " in step outputs, "
                        ++ Model.formatStorageSize otherBytes
                        ++ " other data, "
                        ++ Model.formatStorageSize free
                        ++ " free of "
                        ++ Model.formatStorageSize diskTotal
                    )
                ]
                [ Html.span
                    [ class "storage-panel__bar-outputs"
                    , style "width" (String.fromInt (ratioPercent barOutputs diskTotal) ++ "%")
                    ]
                    []
                , Html.span
                    [ class "storage-panel__bar-other"
                    , style "width" (String.fromInt (ratioPercent otherBytes diskTotal) ++ "%")
                    ]
                    []
                ]
        , Html.viewIf snapshot.coldConfigured <|
            Html.div [ class "storage-panel__cold-line" ]
                [ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ]
                , Html.span [ class "storage-panel__cold-scope" ] [ Html.text "Off this server:" ]
                , Html.strong [] [ Html.text (Model.formatStorageSize snapshot.cold) ]
                , Html.text " in cold storage"
                , partial
                , Html.span [ class "storage-panel__cold-note" ] [ Html.text "not counted in the on-server total" ]
                ]
        ]


storageTotal : Int -> String -> Html msg
storageTotal bytes label =
    Html.span [ class "storage-panel__total" ]
        [ Html.strong [] [ Html.text (Model.formatStorageSize bytes) ]
        , Html.text (" " ++ label)
        ]


viewStorageScan : StorageView -> Html msg
viewStorageScan snapshot =
    Html.viewIf snapshot.scanning <|
        Html.div [ class "storage-panel__scan" ]
            [ Html.div [ class "storage-panel__scan-line" ]
                [ Html.text
                    ("Scanning history: "
                        ++ String.fromInt snapshot.scanned
                        ++ " of "
                        ++ String.fromInt snapshot.history
                        ++ " commits"
                    )
                , Html.span [ class "storage-panel__scan-value" ]
                    [ Html.text
                        (if snapshot.history > 0 then
                            String.fromInt (ratioPercent snapshot.scanned snapshot.history) ++ "%"

                         else
                            "…"
                        )
                    ]
                ]
            , Html.div [ class "storage-panel__scan-bar", attribute "aria-hidden" "true" ]
                [ Html.span [ style "width" (String.fromInt (ratioPercent snapshot.scanned snapshot.history) ++ "%") ] [] ]
            ]


viewStorageToolbar : Model -> Html (Flow Model ())
viewStorageToolbar model =
    Html.div [ class "storage-panel__toolbar" ]
        [ Html.input
            [ class "storage-panel__filter"
            , type_ "search"
            , placeholder "Filter by step, name, or hash"
            , attribute "aria-label" "Filter step outputs by step, name, or revision hash"
            , attribute "autocomplete" "off"
            , attribute "spellcheck" "false"
            , value (Model.getStorageFilter model)
            , Events.onInput Actions.setStorageFilter
            ]
            []
        ]


storageLiveStatus : StorageView -> Set String -> String
storageLiveStatus snapshot moving =
    let
        scan =
            if snapshot.scanning then
                "Scanning history: "
                    ++ String.fromInt snapshot.scanned
                    ++ " of "
                    ++ String.fromInt snapshot.history
                    ++ " commits."

            else
                ""

        activeMoves =
            List.filter revisionIsMoving snapshot.revisions

        movingNow =
            not (Set.isEmpty moving) || not (List.isEmpty activeMoves)

        moveStatus =
            if not movingNow then
                ""

            else if List.isEmpty activeMoves then
                " Moving revisions to cold storage."

            else
                let
                    done =
                        List.sum (List.map .done activeMoves)

                    total =
                        List.sum (List.map .total activeMoves)
                in
                " Moving "
                    ++ storageCount (List.length activeMoves) "revision" "revisions"
                    ++ " to cold storage"
                    ++ (if total > 0 then
                            ": " ++ String.fromInt (ratioPercent done total) ++ "%."

                        else
                            "."
                       )
    in
    String.trim (scan ++ moveStatus)


viewStorageFooter : StorageView -> Html msg
viewStorageFooter snapshot =
    let
        listed =
            List.length snapshot.revisions

        cap =
            if snapshot.revisionCount > listed && listed > 0 then
                " · largest " ++ String.fromInt listed ++ " listed"

            else
                ""
    in
    Html.div [ class "storage-panel__foot" ]
        [ Html.text
            (storageCount (List.length snapshot.steps) "step" "steps"
                ++ " · "
                ++ storageCount snapshot.revisionCount "revision" "revisions"
                ++ cap
                ++ (if snapshot.revisionCount > 1 then
                        " · largest first"

                    else
                        ""
                   )
            )
        ]


storageDiskTotal : StorageView -> Int
storageDiskTotal snapshot =
    if snapshot.diskTotal > 0 then
        snapshot.diskTotal

    else
        max 0 (snapshot.local + snapshot.free)


storageCount : Int -> String -> String -> String
storageCount count singular plural =
    String.fromInt count ++ " " ++ (if count == 1 then singular else plural)


type alias StorageGroup =
    { step : StorageStep
    , listedRevisions : List StorageRevision
    , revisions : List StorageRevision
    }


storageGroups : String -> StorageView -> List StorageGroup
storageGroups filter snapshot =
    let
        revisionsByStep =
            List.foldr
                (\revision accumulated ->
                    Dict.update revision.step
                        (\current ->
                            Just (revision :: Maybe.withDefault [] current)
                        )
                        accumulated
                )
                Dict.empty
                snapshot.revisions

        groups =
            List.map
                (\step ->
                    let
                        listedRevisions =
                            Maybe.withDefault [] (Dict.get step.step revisionsByStep)
                    in
                    { step = step
                    , listedRevisions = listedRevisions
                    , revisions = listedRevisions
                    }
                )
                snapshot.steps
    in
    if String.isEmpty filter then
        groups

    else
        List.filterMap (filterStorageGroup filter) groups


filterStorageGroup : String -> StorageGroup -> Maybe StorageGroup
filterStorageGroup filter group =
    let
        stepMatches =
            String.contains filter (String.toLower group.step.name)
                || String.contains filter (String.fromInt group.step.step)

        matchingRevisions =
            List.filter (revisionMatchesStorageFilter filter) group.listedRevisions

        revisions =
            if stepMatches then
                group.listedRevisions

            else
                matchingRevisions
    in
    if stepMatches || not (List.isEmpty matchingRevisions) then
        Just { group | revisions = revisions }

    else
        Nothing


revisionMatchesStorageFilter : String -> StorageRevision -> Bool
revisionMatchesStorageFilter filter revision =
    String.startsWith filter (String.toLower revision.revision)
        || String.contains filter (String.toLower revision.name)


viewStorageGroup : Model -> Bool -> Set Int -> String -> StorageGroup -> Html (Flow Model ())
viewStorageGroup model coldConfigured expandedSteps filter group =
    let
        expanded =
            not (List.isEmpty group.listedRevisions)
                && (not (String.isEmpty filter) || Set.member group.step.step expandedSteps)

        childrenId =
            "storage-step-children-" ++ String.fromInt group.step.step
    in
    Html.div [ class "storage-panel__group", attribute "role" "listitem" ]
        [ viewStorageStepRow model coldConfigured childrenId expanded group
        , Html.div
            [ id childrenId
            , class "storage-panel__children"
            , attribute "role" "list"
            ]
            (if expanded then
                List.map (viewStorageRow model coldConfigured group.step) group.revisions

             else
                []
            )
        ]


viewStorageStepRow : Model -> Bool -> String -> Bool -> StorageGroup -> Html (Flow Model ())
viewStorageStepRow model coldConfigured childrenId expanded group =
    let
        step =
            group.step

        starting =
            Set.member (Model.storageStepKey step.step) (Model.getStorageMoving model)

        blockedId =
            "storage-step-blocked-" ++ String.fromInt step.step

        unlisted =
            max 0 (step.revisionCount - List.length group.listedRevisions)

        moveButton =
            Html.viewIf (coldConfigured && step.localRevisionCount > 0) <|
                Html.button
                    [ class "small-btn storage-panel__step-move"
                    , type_ "button"
                    , disabled (starting || step.blocked /= Nothing)
                    , Events.onClick (Actions.confirmStorageStepOffload step)
                    , attribute "aria-describedby" blockedId
                    ]
                    [ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ]
                    , Html.text
                        (if starting then
                            "Starting…"

                         else
                            "Move " ++ storageCount step.localRevisionCount "revision" "revisions"
                        )
                    ]
    in
    Html.div [ class "storage-panel__step-row" ]
        ([ Html.div [ class "storage-panel__step-main" ]
            [ Html.button
                [ class "storage-panel__step-toggle"
                , type_ "button"
                , disabled (List.isEmpty group.listedRevisions)
                , Events.onClick (Actions.toggleStorageStepExpanded step.step)
                , attribute "aria-expanded" (boolText expanded)
                , attribute "aria-controls" childrenId
                ]
                [ iconCustom False
                    (if expanded then
                        "expand_more"

                     else
                        "chevron_right"
                    )
                    [ class "storage-panel__chevron", attribute "aria-hidden" "true" ]
                , Html.span [ class "storage-panel__step-name", title step.name ] [ Html.text step.name ]
                , Html.span [ class "storage-panel__step-id" ] [ Html.text ("#" ++ String.fromInt step.step) ]
                ]
            , Html.div [ class "storage-panel__meta" ]
                [ Html.text
                    (storageCount step.revisionCount "revision" "revisions"
                        ++ (if unlisted > 0 then
                                " · " ++ storageCount unlisted "smaller revision not listed" "smaller revisions not listed"

                            else
                                ""
                           )
                    )
                ]
            , storageBlockedReason blockedId step.blocked
            ]
         , Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize step.local) ]
         ]
            ++ (if coldConfigured then
                    [ Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize step.cold) ]
                    , Html.div [ class "storage-panel__action" ] [ moveButton ]
                    ]

                else
                    []
               )
        )


viewStorageRow : Model -> Bool -> StorageStep -> StorageRevision -> Html (Flow Model ())
viewStorageRow model coldConfigured parent revision =
    let
        blockedId =
            "storage-blocked-" ++ String.fromInt revision.step ++ "-" ++ revision.revision
    in
    Html.div
        [ classList
            [ ( "storage-panel__row", True )
            , ( "storage-panel__row--cold", revision.state == StorageCold )
            ]
        , attribute "role" "listitem"
        ]
        ([ Html.div [ class "storage-panel__revision-main" ]
            [ Html.div [ class "storage-panel__name-row" ]
                [ Html.span
                    [ class "step-review-revision storage-panel__revision"
                    , title ("Revision " ++ revision.revision ++ ", committed " ++ Iso8601.fromTime revision.committedAt)
                    ]
                    [ Html.span [ class "step-review-revision-hash" ] [ Html.text (String.left 7 revision.revision) ] ]
                , Html.viewIf (revision.name /= parent.name) <|
                    Html.span [ class "storage-panel__revision-step", title revision.name ] [ Html.text revision.name ]
                , Html.span [ class "storage-panel__age" ]
                    [ Html.text (Time.Distance.inWords revision.committedAt (Model.getNow model)) ]
                ]
            , storageBlockedReason blockedId revision.blocked
            , Html.viewMaybe
                (\message -> Html.div [ class "storage-panel__error" ] [ Html.text message ])
                revision.error
            ]
         , Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize revision.local) ]
         ]
            ++ (if coldConfigured then
                    [ Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize revision.cold) ]
                    , Html.div [ class "storage-panel__action" ]
                        [ viewStorageRevisionAction model revision blockedId ]
                    ]

                else
                    []
               )
        )


storageBlockedReason : String -> Maybe String -> Html msg
storageBlockedReason blockedId blocked =
    Html.div [ class "storage-panel__blocked", id blockedId ]
        (case blocked of
            Just reason ->
                [ Html.text reason ]

            Nothing ->
                []
        )


viewStorageRevisionAction : Model -> StorageRevision -> String -> Html (Flow Model ())
viewStorageRevisionAction model revision blockedId =
    case revision.state of
        StorageCold ->
            Html.span [ class "storage-panel__badge" ]
                [ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ]
                , Html.text "In cold storage"
                ]

        StorageOffloading ->
            storageProgress "Moving" revision.done revision.total

        StorageOnloading ->
            storageProgress "Copying back" revision.done revision.total

        StorageUnknown ->
            Html.span [ class "storage-panel__unavailable" ] [ Html.text "Status unavailable" ]

        StorageFailed ->
            storageRevisionMoveButton model revision blockedId

        StorageLocal ->
            storageRevisionMoveButton model revision blockedId


storageRevisionMoveButton : Model -> StorageRevision -> String -> Html (Flow Model ())
storageRevisionMoveButton model revision blockedId =
    let
        starting =
            Set.member (Model.storageRevisionKey revision) (Model.getStorageMoving model)

        label =
            if starting then
                "Starting to move revision " ++ String.left 7 revision.revision ++ " to cold storage"

            else
                "Move revision " ++ String.left 7 revision.revision ++ " to cold storage"
    in
    Html.button
        [ class "icon-btn storage-panel__rev-action"
        , classList [ ( "storage-panel__rev-action--starting", starting ) ]
        , type_ "button"
        , disabled (starting || revision.blocked /= Nothing)
        , Events.onClick (Actions.confirmOffload revision)
        , attribute "aria-label" label
        , attribute "aria-describedby" blockedId
        ]
        [ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ] ]


revisionIsMoving : StorageRevision -> Bool
revisionIsMoving revision =
    revision.state == StorageOffloading || revision.state == StorageOnloading


storageProgress : String -> Int -> Int -> Html msg
storageProgress label done total =
    let
        percent =
            ratioPercent done total
    in
    Html.span [ class "storage-panel__progress" ]
        [ Html.span [ class "storage-panel__progress-label" ]
            [ Html.text
                (if total <= 0 then
                    label ++ "…"

                 else
                    label ++ " " ++ String.fromInt percent ++ "%"
                )
            ]
        , Html.span [ class "storage-panel__progress-bar", attribute "aria-hidden" "true" ]
            [ Html.span [ style "width" (String.fromInt percent ++ "%") ] [] ]
        ]


ratioPercent : Int -> Int -> Int
ratioPercent part total =
    if total <= 0 then
        0

    else
        clamp 0 100 (round (100 * toFloat part / toFloat total))
