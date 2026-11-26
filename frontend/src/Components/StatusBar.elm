module Components.StatusBar exposing (view, viewStorageDeleteDialog)

import Accessors exposing (get, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData)
import Dict exposing (Dict)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (action, attribute, autofocus, class, classList, disabled, href, id, method, rel, style, target, title, type_)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Iso8601
import Json.Decode as Decode
import List.Extra as List
import Model.Core as Model exposing (BackendConnection(..), ClusterStatus(..), Model, StorageBlocker(..), StorageCapacity, StorageKind(..), StorageRevision, StorageState(..), StorageView, TrayState(..), TrayStep)
import Model.Lenses exposing (loadedProjects, route, toastHistory)
import Model.Lib exposing (canonicalNamePath, canonicalPathNames, rootProjectName)
import Route
import Set exposing (Set)
import Time
import Time.Distance
import Toast
import View.FileBrowser as FileBrowser
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
            Maybe.withDefault "Storage" <|
                Maybe.map
                    (\snapshot ->
                        "Storage · "
                            ++ Model.formatStorageSize snapshot.local
                            ++ " in step outputs"
                            ++ (Maybe.map (\capacity -> " · " ++ capacityRemainderString capacity) snapshot.capacity
                                    |> Maybe.withDefault ""
                               )
                    )
                    current
    in
    Html.button
        [ id Actions.storageTriggerId
        , class "status-bar__control status-bar__storage-control"
        , classList [ ( "status-bar__control--active", Model.getStorageOpen model ) ]
        , type_ "button"
        , Events.onClick Actions.toggleStoragePanel
        , title "Storage"
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
    Model.storageCapacityFraction (max 0 snapshot.local) snapshot.capacity
        |> Maybe.map (\usedPercent -> { usedPercent = usedPercent, low = usedPercent >= 90 })


viewStoragePanel : Model -> Html (Flow Model ())
viewStoragePanel model =
    Html.section
        [ class "status-bar__panel storage-panel"
        , id Actions.storagePanelId
        , attribute "tabindex" "-1"
        , attribute "role" "region"
        , attribute "aria-labelledby" "storage-panel-title"
        , Events.stopPropagationOn "keydown" storagePanelEscapeDecoder
        ]
        [ Html.div [ class "storage-panel__header" ]
            [ iconCustom False "hard_drive" [ attribute "aria-hidden" "true" ]
            , Html.strong [ id "storage-panel-title" ] [ Html.text "Storage" ]
            , Html.viewMaybe viewStorageUsage (ApiData.toMaybe (Model.getStorageView model))
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
                , title "Close storage"
                , attribute "aria-label" "Close storage"
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


storagePanelEscapeDecoder : Decode.Decoder ( Flow Model (), Bool )
storagePanelEscapeDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed ( Actions.closeStoragePanel, True )

                else
                    Decode.fail "Not Escape"
            )


viewStorageContent : Model -> StorageView -> Html (Flow Model ())
viewStorageContent model snapshot =
    let
        groups =
            storageGroups snapshot

        expandedSteps =
            Model.getStorageExpandedSteps model

        stepRoutes =
            storageStepRoutes model
    in
    Html.div [ class "storage-panel__content" ]
        [ viewStorageScan snapshot
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
        , if List.isEmpty snapshot.revisions then
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
                [ Html.div [ class "storage-panel__columns" ]
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
                , Html.div [ class "storage-panel__rows", attribute "role" "list" ]
                    (List.map
                        (viewStorageGroup model snapshot.coldConfigured stepRoutes expandedSteps)
                        groups
                    )
                ]
        , viewStorageFooter snapshot (List.length groups)
        ]


viewStorageUsage : StorageView -> Html msg
viewStorageUsage snapshot =
    Html.div [ class "storage-panel__usage" ]
        [ viewStorageWarmTier snapshot
        , Html.viewIf snapshot.coldConfigured <|
            viewStorageColdTier snapshot
        ]


viewStorageWarmTier : StorageView -> Html msg
viewStorageWarmTier snapshot =
    let
        local =
            max 0 snapshot.local

        summary =
            Model.formatStorageSize local
                ++ " of step outputs on this server"
                ++ (case snapshot.capacity of
                        Just capacity ->
                            " · " ++ capacityRemainderString capacity

                        Nothing ->
                            ""
                   )
    in
    Html.div [ class "storage-panel__tier", title summary ]
        (Html.strong [] [ Html.text (Model.formatStorageSize local) ]
            :: (case snapshot.capacity of
                    Just capacity ->
                        [ viewStorageCapacityBar False capacity local
                        , Html.span [ class "storage-panel__remainder" ] [ Html.text (capacityRemainderShort capacity) ]
                        ]

                    Nothing ->
                        []
               )
        )


viewStorageColdTier : StorageView -> Html msg
viewStorageColdTier snapshot =
    let
        summary =
            Model.formatStorageSize snapshot.cold
                ++ " in cold storage"
                ++ (case snapshot.coldCapacity of
                        Just capacity ->
                            " · " ++ Model.formatStorageSize capacity.availableBytes ++ " free"

                        Nothing ->
                            ""
                   )
                ++ ". Freeze moves outputs to cold storage to free space on this server. Frozen files stay listed and must be rewarmed before they can be opened or run."
    in
    Html.div [ class "storage-panel__tier storage-panel__tier--cold", title summary ]
        ([ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ]
         , Html.strong [] [ Html.text (Model.formatStorageSize snapshot.cold) ]
         ]
            ++ (case snapshot.coldCapacity of
                    Just capacity ->
                        [ viewStorageCapacityBar True capacity snapshot.cold
                        , Html.span [ class "storage-panel__remainder" ] [ Html.text (Model.formatStorageSize capacity.availableBytes ++ " free") ]
                        ]

                    Nothing ->
                        []
               )
        )


viewStorageCapacityBar : Bool -> StorageCapacity -> Int -> Html msg
viewStorageCapacityBar cold capacity used =
    Html.viewMaybe
        (\percent ->
            Html.div
                [ class "storage-panel__bar"
                , classList
                    [ ( "storage-panel__bar--low", not cold && percent >= 90 )
                    , ( "storage-panel__bar--cold", cold )
                    ]
                , attribute "role" "img"
                , attribute "aria-label"
                    (Model.formatStorageSize used
                        ++ (if cold then " cold" else " step outputs")
                        ++ "; "
                        ++ (if capacity.kind == Model.CapacityQuota then capacityRemainderString capacity else Model.formatStorageSize capacity.availableBytes ++ " free")
                    )
                ]
                [ Html.span [ class "storage-panel__bar-used", style "width" (String.fromInt percent ++ "%") ] [] ]
        )
        (Model.storageCapacityFraction used (Just capacity))


capacityRemainderShort : StorageCapacity -> String
capacityRemainderShort capacity =
    Model.formatStorageSize capacity.availableBytes
        ++ (case capacity.kind of
                Model.CapacityQuota ->
                    " left"

                Model.CapacityFilesystem ->
                    " free"
           )


capacityRemainderString : StorageCapacity -> String
capacityRemainderString capacity =
    Model.formatStorageSize capacity.availableBytes
        ++ (if capacity.kind == Model.CapacityQuota then
                " left of "
                    ++ Model.formatStorageSize capacity.totalBytes
                    ++ " "
                    ++ capacity.label

            else
                " free"
           )


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

        progressSuffix revisions =
            let
                done =
                    List.sum (List.map .done revisions)

                total =
                    List.sum (List.map .total revisions)
            in
            if total > 0 then
                ": " ++ String.fromInt (ratioPercent done total) ++ "%."

            else
                "."

        freezing =
            List.filter (\revision -> revision.state == StorageOffloading) snapshot.revisions

        rewarming =
            List.filter (\revision -> revision.state == StorageOnloading) snapshot.revisions

        freezeStatus =
            if List.isEmpty freezing then
                if Actions.storageMoveInFlight moving then
                    " Freezing revisions to cold storage."

                else
                    ""

            else
                " Freezing "
                    ++ storageCount (List.length freezing) "revision" "revisions"
                    ++ " to cold storage"
                    ++ progressSuffix freezing

        rewarmStatus =
            if List.isEmpty rewarming then
                if Actions.storageRewarmInFlight moving then
                    " Rewarming revisions from cold storage."

                else
                    ""

            else
                " Rewarming "
                    ++ storageCount (List.length rewarming) "revision" "revisions"
                    ++ " from cold storage"
                    ++ progressSuffix rewarming

        deleting =
            Actions.storageDeleteInFlight moving

        deleteStatus =
            if deleting > 0 then
                " Deleting " ++ storageCount deleting "revision" "revisions" ++ "."

            else
                ""
    in
    String.trim (scan ++ freezeStatus ++ rewarmStatus ++ deleteStatus)


viewStorageFooter : StorageView -> Int -> Html msg
viewStorageFooter snapshot stepCount =
    Html.div [ class "storage-panel__foot" ]
        [ Html.text
            (storageCount stepCount "step" "steps"
                ++ " · "
                ++ storageCount snapshot.revisionCount "revision" "revisions"
            )
        ]


storageCount : Int -> String -> String -> String
storageCount count singular plural =
    String.fromInt count ++ " " ++ (if count == 1 then singular else plural)


type alias StorageGroup =
    { stepId : Int
    , name : String
    , local : Int
    , cold : Int
    , revisionCount : Int
    , localRevisionCount : Int
    , movableLocal : Int
    , blocked : Maybe StorageBlocker
    , revisions : List StorageRevision
    }


storageGroups : StorageView -> List StorageGroup
storageGroups snapshot =
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
    in
    Dict.toList revisionsByStep
        |> List.map (\( stepId, revisions ) -> summarizeStorageGroup stepId revisions)
        |> List.sortWith compareStorageGroups


summarizeStorageGroup : Int -> List StorageRevision -> StorageGroup
summarizeStorageGroup stepId revisions =
    let
        name =
            case List.filterMap .currentName revisions |> List.find (\current -> not (String.isEmpty (String.trim current))) of
                Just current ->
                    current

                Nothing ->
                    case List.maximumBy (\revision -> ( Time.posixToMillis revision.committedAt, revision.revision )) revisions of
                        Just newest ->
                            if String.isEmpty (String.trim newest.name) then
                                "Step " ++ String.fromInt stepId

                            else
                                newest.name

                        Nothing ->
                            "Step " ++ String.fromInt stepId

        movableRevisions =
            List.filter storageRevisionMovable revisions
    in
    { stepId = stepId
    , name = name
    , local = List.sum (List.map .local revisions)
    , cold = List.sum (List.map .cold revisions)
    , revisionCount = List.length revisions
    , localRevisionCount = List.length movableRevisions
    , movableLocal = List.sum (List.map .local movableRevisions)
    , blocked = List.filterMap .blocked revisions |> List.head
    , revisions = revisions
    }


storageRevisionRewarmable : StorageRevision -> Bool
storageRevisionRewarmable revision =
    revision.cold > 0 && (revision.state == StorageCold || revision.state == StorageFailed)


storageRevisionMovable : StorageRevision -> Bool
storageRevisionMovable revision =
    revision.state == StorageLocal


compareStorageGroups : StorageGroup -> StorageGroup -> Order
compareStorageGroups first second =
    case compare (largerTier second) (largerTier first) of
        EQ ->
            case compare (second.local + second.cold) (first.local + first.cold) of
                EQ ->
                    compare first.stepId second.stepId

                order ->
                    order

        order ->
            order


largerTier : StorageGroup -> Int
largerTier group =
    max group.local group.cold


viewStorageGroup : Model -> Bool -> Dict Int Route.Route -> Set Int -> StorageGroup -> Html (Flow Model ())
viewStorageGroup model coldConfigured stepRoutes expandedSteps group =
    let
        expanded =
            Set.member group.stepId expandedSteps

        childrenId =
            "storage-step-children-" ++ String.fromInt group.stepId
    in
    Html.div [ class "storage-panel__group", attribute "role" "listitem" ]
        [ viewStorageStepRow model coldConfigured stepRoutes childrenId expanded group
        , Html.div
            [ id childrenId
            , class "storage-panel__children"
            , attribute "role" "list"
            ]
            (if expanded then
                List.map (viewStorageRow model coldConfigured stepRoutes group.name) group.revisions

             else
                []
            )
        ]


viewStorageStepRow : Model -> Bool -> Dict Int Route.Route -> String -> Bool -> StorageGroup -> Html (Flow Model ())
viewStorageStepRow model coldConfigured stepRoutes childrenId expanded group =
    let
        pending =
            Model.getStorageMoving model

        starting =
            Set.member (Model.storageStepKey group.stepId) pending

        blockedId =
            "storage-step-blocked-" ++ String.fromInt group.stepId

        rewarmRevisions =
            List.filter storageRevisionRewarmable group.revisions

        rewarmColdBytes =
            List.sum (List.map .cold rewarmRevisions)

        rewarming =
            Set.member (Actions.storageRewarmStepKey group.stepId) pending
                || List.any (\revision -> Set.member (Actions.storageRewarmKey revision) pending) rewarmRevisions

        busy =
            List.any (Actions.storageRevisionActionPending model) group.revisions

        rewarmCount =
            List.length rewarmRevisions

        freezingRevisions =
            List.filter (\revision -> revision.state == StorageOffloading) group.revisions

        rewarmingRevisions =
            List.filter (\revision -> revision.state == StorageOnloading) group.revisions

        progressFor label revisions =
            storageProgress label (List.sum (List.map .done revisions)) (List.sum (List.map .total revisions))

        transferProgress =
            (if List.isEmpty freezingRevisions then
                []

             else
                [ progressFor "Freezing" freezingRevisions ]
            )
                ++ (if List.isEmpty rewarmingRevisions then
                        []

                    else
                        [ progressFor "Rewarming" rewarmingRevisions ]
                   )

        rewarmButton =
            Html.viewIf (coldConfigured && rewarmCount > 0) <|
                Html.button
                    [ class "icon-btn storage-panel__rev-action"
                    , classList
                        [ ( "storage-panel__rev-action--starting", rewarming )
                        , ( "storage-panel__rev-action--rewarm", True )
                        ]
                    , type_ "button"
                    , disabled (rewarming || busy || group.blocked /= Nothing)
                    , Events.onClick (Actions.storageRewarmStepRevisions group.stepId)
                    , title
                        ("Rewarm "
                            ++ storageCount rewarmCount "cold revision" "cold revisions"
                            ++ " ("
                            ++ Model.formatStorageSize rewarmColdBytes
                            ++ ")."
                        )
                    , attribute "aria-label"
                        (if rewarming then
                            "Rewarming " ++ storageCount rewarmCount "cold revision" "cold revisions"

                         else
                            "Rewarm " ++ storageCount rewarmCount "cold revision" "cold revisions"
                        )
                    , attribute "aria-describedby" blockedId
                    ]
                    [ iconCustom False "mode_heat" [ attribute "aria-hidden" "true" ] ]

        moveButton =
            Html.viewIf (coldConfigured && group.localRevisionCount > 0) <|
                Html.button
                    [ class "icon-btn storage-panel__rev-action"
                    , classList
                        [ ( "storage-panel__rev-action--starting", starting )
                        , ( "storage-panel__rev-action--freeze", True )
                        ]
                    , type_ "button"
                    , disabled (starting || busy || group.blocked /= Nothing)
                    , Events.onClick (Actions.confirmStorageStepOffload group.stepId group.name group.movableLocal group.localRevisionCount)
                    , title
                        ("Freeze "
                            ++ storageCount group.localRevisionCount "local revision" "local revisions"
                            ++ " ("
                            ++ Model.formatStorageSize group.movableLocal
                            ++ ")."
                        )
                    , attribute "aria-label"
                        (if starting then
                            "Starting to freeze " ++ storageCount group.localRevisionCount "local revision" "local revisions" ++ " to cold storage"

                         else
                            "Freeze " ++ storageCount group.localRevisionCount "local revision" "local revisions" ++ " to cold storage"
                        )
                    , attribute "aria-describedby" blockedId
                    ]
                    [ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ] ]
    in
    Html.div [ class "storage-panel__step-row" ]
        ([ Html.div [ class "storage-panel__step-main" ]
            [ Html.div [ class "storage-panel__step-heading" ]
                [ Html.button
                    [ class "storage-panel__step-toggle"
                    , type_ "button"
                    , Events.onClick (Actions.toggleStorageStepExpanded group.stepId)
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
                    , Html.span [ class "storage-panel__step-name", title group.name ] [ Html.text group.name ]
                    ]
                , viewStorageStepId stepRoutes group.stepId
                ]
            , Html.div [ class "storage-panel__meta" ]
                [ Html.text (storageCount group.revisionCount "revision" "revisions") ]
            , storageBlockedReason group.stepId stepRoutes blockedId group.blocked
            ]
         , Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize group.local) ]
         ]
            ++ (if coldConfigured then
                    [ Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize group.cold) ]
                    , Html.div [ class "storage-panel__action" ]
                        [ Html.div [ class "storage-panel__rev-actions" ]
                            (transferProgress ++ [ rewarmButton, moveButton ])
                        ]
                    ]

                else
                    []
               )
        )


viewStorageStepId : Dict Int Route.Route -> Int -> Html (Flow Model ())
viewStorageStepId stepRoutes stepId =
    case Dict.get stepId stepRoutes of
        Just route_ ->
            Html.a
                [ href (Route.toString route_)
                , class "storage-panel__step-id"
                , title ("Open step " ++ String.fromInt stepId)
                , Events.onClick Actions.closeStoragePanel
                ]
                [ Html.text ("#" ++ String.fromInt stepId) ]

        Nothing ->
            Html.span [ class "storage-panel__step-id" ] [ Html.text ("#" ++ String.fromInt stepId) ]


storageStepRoutes : Model -> Dict Int Route.Route
storageStepRoutes model =
    let
        currentPath =
            try (Route.page << Model.Lenses.projectRoute << Model.Lenses.projectPath) (Model.getRoute model)

        openProjectId =
            Maybe.map Route.pathProjectId currentPath

        canonicalPath =
            Model.canonicalProjectPath (Maybe.withDefault [] currentPath) (Model.getProjectIndex model)

        locations =
            Actions.stepLocations (always True) (Model.getProjectIndex model) (Model.getSteps model)
    in
    locations
        |> Dict.toList
        |> List.filterMap
            (\( stepId, candidates ) ->
                Actions.stepOutputLocation openProjectId (Dict.singleton stepId candidates) stepId
                    |> Maybe.map (\location -> ( stepId, Actions.stepOutputRoute (canonicalPath location.projectId) stepId ))
            )
        |> Dict.fromList


storageKindLabel : StorageKind -> String
storageKindLabel kind =
    case kind of
        StorageDerived ->
            "Derived"

        StorageDownload ->
            "Download"

        StorageUpload ->
            "Upload"


viewStorageRow : Model -> Bool -> Dict Int Route.Route -> String -> StorageRevision -> Html (Flow Model ())
viewStorageRow model coldConfigured stepRoutes parentName revision =
    let
        blockedId =
            "storage-blocked-" ++ String.fromInt revision.step ++ "-" ++ revision.revision

        inlineAction =
            Html.viewIf (not coldConfigured) <|
                Html.div [ class "storage-panel__inline-action" ]
                    [ viewStorageRevisionAction model coldConfigured revision blockedId ]
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
                , Html.span
                    [ class "storage-panel__revision-step storage-panel__kind"
                    , title ("Output kind: " ++ storageKindLabel revision.kind)
                    ]
                    [ Html.text (storageKindLabel revision.kind) ]
                , Html.viewIf (revision.name /= parentName) <|
                    Html.span [ class "storage-panel__revision-step", title revision.name ] [ Html.text revision.name ]
                , Html.span [ class "storage-panel__age" ]
                    [ Html.text (Time.Distance.inWords revision.committedAt (Model.getNow model)) ]
                , inlineAction
                ]
            , storageBlockedReason revision.step stepRoutes blockedId revision.blocked
            , Html.viewMaybe
                (\message -> Html.div [ class "storage-panel__error" ] [ Html.text message ])
                revision.error
            ]
         , Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize revision.local) ]
         ]
            ++ (if coldConfigured then
                    [ Html.span [ class "storage-panel__size" ] [ Html.text (Model.formatStorageSize revision.cold) ]
                    , Html.div [ class "storage-panel__action" ]
                        [ viewStorageRevisionAction model coldConfigured revision blockedId ]
                    ]

                else
                    []
               )
        )


storageBlockedReason : Int -> Dict Int Route.Route -> String -> Maybe StorageBlocker -> Html (Flow Model ())
storageBlockedReason ownStepId stepRoutes blockedId blocked =
    Html.div [ class "storage-panel__blocked", id blockedId ]
        (case blocked of
            Just (StorageBlockerStep blockerStep blockerName blockerState) ->
                if blockerStep == ownStepId then
                    [ Html.text ("This step is " ++ blockerState ++ ".") ]

                else
                    [ Html.text "Step "
                    , viewStorageStepId stepRoutes blockerStep
                    , Html.text (" (" ++ blockerName ++ ") is " ++ blockerState ++ " and reads this output.")
                    ]

            Just (StorageBlockerError message) ->
                [ Html.text message ]

            Nothing ->
                []
        )


viewStorageRevisionAction : Model -> Bool -> StorageRevision -> String -> Html (Flow Model ())
viewStorageRevisionAction model coldConfigured revision blockedId =
    case revision.state of
        StorageCold ->
            storageRevisionActions model coldConfigured revision blockedId

        StorageOffloading ->
            storageProgress "Freezing" revision.done revision.total

        StorageOnloading ->
            storageProgress "Rewarming" revision.done revision.total

        StorageUnknown ->
            Html.span [ class "storage-panel__unavailable" ] [ Html.text "Status unavailable" ]

        StorageFailed ->
            storageRevisionActions model coldConfigured revision blockedId

        StorageLocal ->
            storageRevisionActions model coldConfigured revision blockedId


storageRevisionActions : Model -> Bool -> StorageRevision -> String -> Html (Flow Model ())
storageRevisionActions model coldConfigured revision blockedId =
    Html.div [ class "storage-panel__rev-actions" ]
        ((if storageRevisionRewarmable revision then
            [ storageRevisionRewarmButton model revision blockedId ]

          else
            []
         )
            ++ (if coldConfigured && storageRevisionMovable revision then
                    [ storageRevisionMoveButton model revision blockedId ]

                else
                    []
               )
            ++ [ storageRevisionDeleteButton model revision blockedId ]
        )


storageRevisionRewarmButton : Model -> StorageRevision -> String -> Html (Flow Model ())
storageRevisionRewarmButton model revision blockedId =
    let
        rewarming =
            Set.member (Actions.storageRewarmKey revision) (Model.getStorageMoving model)

        busy =
            Actions.storageRevisionActionPending model revision
    in
    Html.button
        [ class "icon-btn storage-panel__rev-action"
        , classList
            [ ( "storage-panel__rev-action--starting", rewarming )
            , ( "storage-panel__rev-action--rewarm", True )
            ]
        , type_ "button"
        , disabled (busy || revision.blocked /= Nothing)
        , Events.onClick (Actions.storageRewarmRevision revision)
        , title ("Rewarm this revision (" ++ Model.formatStorageSize revision.cold ++ ").")
        , attribute "aria-label"
            (if rewarming then
                "Rewarming revision " ++ String.left 7 revision.revision

             else
                "Rewarm revision " ++ String.left 7 revision.revision
            )
        , attribute "aria-describedby" blockedId
        ]
        [ iconCustom False "mode_heat" [ attribute "aria-hidden" "true" ] ]


storageRevisionDeleteButton : Model -> StorageRevision -> String -> Html (Flow Model ())
storageRevisionDeleteButton model revision blockedId =
    let
        pending =
            Model.getStorageMoving model

        armed =
            Set.member (Model.storageRevisionKey revision) (Model.getStorageDeleteArmed model)

        deleting =
            Set.member (Actions.storageDeleteRequestKey revision) pending

        busy =
            Actions.storageRevisionActionPending model revision

        tooltip =
            if deleting then
                "Deleting this revision."

            else if armed then
                "Click again to delete"

            else
                storageDeleteTooltip revision.kind

        label =
            if deleting then
                "Deleting revision " ++ String.left 7 revision.revision

            else if armed then
                "Click again to delete revision " ++ String.left 7 revision.revision

            else
                "Delete revision " ++ String.left 7 revision.revision ++ ". " ++ tooltip
    in
    Html.button
        [ class "icon-btn storage-panel__rev-action"
        , classList
            [ ( "storage-panel__rev-action--armed", armed )
            , ( "storage-panel__rev-action--starting", deleting )
            , ( "storage-panel__rev-action--deleting", deleting )
            , ( "storage-panel__rev-action--delete", True )
            ]
        , type_ "button"
        , disabled (busy || revision.blocked /= Nothing)
        , Events.onClick (Actions.storageDeleteRevision revision)
        , title tooltip
        , attribute "aria-label" label
        , attribute "aria-describedby" blockedId
        ]
        (if deleting then
            [ Html.text "Deleting…" ]

         else if armed then
            [ Html.text "Delete?" ]

         else
            [ iconCustom False "delete" [ attribute "aria-hidden" "true" ] ]
        )


storageDeleteTooltip : StorageKind -> String
storageDeleteTooltip kind =
    case kind of
        StorageDerived ->
            "Delete this output. This data can be re-derived as long as its inputs still exist."

        StorageDownload ->
            "Delete this output. This data can be re-downloaded as long as it still exists at its original source."

        StorageUpload ->
            "Delete this uploaded output. There is no undeleting it."


storageRevisionMoveButton : Model -> StorageRevision -> String -> Html (Flow Model ())
storageRevisionMoveButton model revision blockedId =
    let
        starting =
            Set.member (Model.storageRevisionKey revision) (Model.getStorageMoving model)

        busy =
            Actions.storageRevisionActionPending model revision

        label =
            if starting then
                "Starting to freeze revision " ++ String.left 7 revision.revision ++ " to cold storage"

            else
                "Freeze revision " ++ String.left 7 revision.revision ++ " to cold storage"
    in
    Html.button
        [ class "icon-btn storage-panel__rev-action"
        , classList
            [ ( "storage-panel__rev-action--starting", starting )
            , ( "storage-panel__rev-action--freeze", True )
            ]
        , type_ "button"
        , disabled (busy || revision.blocked /= Nothing)
        , Events.onClick (Actions.confirmOffload revision)
        , title ("Freeze this revision (" ++ Model.formatStorageSize revision.local ++ ").")
        , attribute "aria-label" label
        , attribute "aria-describedby" blockedId
        ]
        [ iconCustom False "ac_unit" [ attribute "aria-hidden" "true" ] ]


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


viewStorageDeleteDialog : Model -> Html (Flow Model ())
viewStorageDeleteDialog model =
    Html.node "dialog"
        [ id Actions.storageUploadDeleteDialogId
        , class "dialog storage-delete-dialog"
        , Events.on "close" (Decode.succeed Actions.clearStorageDeleteUpload)
        ]
        [ Html.viewMaybe (viewStorageDeleteDialogContent model) (Model.getStorageDeleteUpload model) ]


viewStorageDeleteDialogContent : Model -> Model.StorageDeleteUpload -> Html (Flow Model ())
viewStorageDeleteDialogContent model upload =
    Html.div [ class "dialog-content storage-delete-dialog-content" ]
        [ Html.span [ class "dialog-title" ] [ Html.text "Delete uploaded data" ]
        , Html.span [ class "dialog-subtitle" ]
            [ Html.text
                (upload.name
                    ++ " · #"
                    ++ String.fromInt upload.stepId
                    ++ " · "
                    ++ String.left 7 upload.revision
                )
            ]
        , Html.node "p" [ class "storage-delete-dialog-warning" ]
            [ Html.text "This data was uploaded directly. Make sure you have an alternate copy or that you really want to delete it. There is no undeleting it. You can download it here if you wish to." ]
        , Html.div [ class "storage-delete-dialog-explorer" ]
            [ FileBrowser.viewOutputDirectory model upload.stepId upload.revision upload.directory ]
        , Html.div [ class "dialog-actions" ]
            [ Html.button
                [ class "btn btn-danger"
                , type_ "button"
                , Events.onClick Actions.confirmStorageDeleteUpload
                ]
                [ Html.text "Delete" ]
            , Html.button
                [ class "btn"
                , type_ "button"
                , autofocus True
                , Events.onClick Actions.closeStorageDeleteUpload
                ]
                [ Html.text "Cancel" ]
            ]
        ]
