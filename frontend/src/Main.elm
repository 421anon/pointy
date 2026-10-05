module Main exposing (main)

import Accessors exposing (get, has, just, set, try, values)
import Actions
import Api.ApiData exposing (ApiData(..), success)
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Flow exposing (Flow)
import Http
import Ingest
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core exposing (AddMode(..), Flags, Model, initialModel)
import Model.Lenses exposing (commitHash, draftAt, gutterDrag, mCommit, mHighlight, now, presets, projectForms, projectPath, projectRollups, projects, route, runState, stepConfig, stepForms, steps, userRepoInfo)
import Model.Selection as Selection
import Organize
import Ports
import Route exposing (Route)
import Specs
import Time
import Url exposing (Url)
import View.Main exposing (view)


main : Flow.Program Flags Model ()
main =
    Flow.application
        { init = init
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = Actions.onUrlRequest
        , onUrlChange = applyRouteFromUrl False
        }


init : Flags -> Url -> Nav.Key -> ( Model, Flow Model () )
init flags url key =
    let
        initialRoute =
            Route.fromUrl url
    in
    ( initialModel key initialRoute flags
    , case initialRoute.page of
        Route.Artifact _ ->
            Flow.pure ()

        _ ->
            Flow.async (Actions.applyAgentChatFromUrl True)
                |> Flow.seq initializeWorkspace
                |> Flow.seq
                    (Flow.forAll route
                        (\currentRoute ->
                            Flow.when (currentRoute == initialRoute) (applyRouteFromUrl True url)
                        )
                    )
    )


initializeWorkspace : Flow Model ()
initializeWorkspace =
    Flow.async Actions.listenAndProcessAgentTurns
        |> Flow.seq Actions.loadUserRepoInfo
        |> Flow.seq Actions.loadStepConfig
        |> Flow.seq Actions.loadPresets
        |> Flow.seq Actions.loadProjects
        |> Flow.seq Actions.anchorAgentChatUnlessHighlighting
        |> Flow.seq (Flow.performTask Time.now |> Flow.andThen (Flow.setAll now))
        |> Flow.seq (Flow.async Actions.startClusterStatusStream)
        |> Flow.seq (Flow.async Actions.listenAndProcessStepStatus)
        |> Flow.seq (Flow.async Ingest.startIngestStream)
        |> Flow.seq Ingest.loadScratch


applyRouteFromUrl : Bool -> Url -> Flow Model ()
applyRouteFromUrl forceRevealHighlight url =
    applyRoute forceRevealHighlight (get Route.routeUrlIso url)
        |> Flow.seq (Actions.applyAgentChatFromUrl True)


applyRoute : Bool -> Route -> Flow Model ()
applyRoute forceRevealHighlight newRoute =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    currentRoute =
                        get route model

                    shouldRevealHighlight =
                        forceRevealHighlight
                            || Route.navigationTarget currentRoute
                            /= Route.navigationTarget newRoute

                    pageTarget route_ =
                        set (Route.page << Route.project << mHighlight) Nothing (Route.navigationTarget route_)

                    expandPath =
                        case newRoute.page of
                            Route.Project params ->
                                params.projectPath

                            _ ->
                                []

                    workspaceNotStarted =
                        case get userRepoInfo model of
                            NotAsked ->
                                True

                            _ ->
                                False

                    projectRoute =
                        has (Route.page << Route.project) newRoute

                    shouldInitializeWorkspace =
                        workspaceNotStarted && projectRoute

                    isDragging =
                        has (gutterDrag << just) model

                    listingFolder route_ =
                        try (Route.page << Route.project << projectPath) route_
                            |> Maybe.map Route.pathProjectId

                    listingContext route_ =
                        ( listingFolder route_, try (Route.page << Route.viewedCommitT) route_ )

                    viewedCommitOf route_ =
                        try (Route.page << Route.viewedCommitT) route_

                    resetTable table =
                        let
                            stashed =
                                case ( viewedCommitOf currentRoute, table.edited ) of
                                    ( Nothing, Just draft ) ->
                                        set (draftAt draft.id) (Just draft) table

                                    _ ->
                                        table
                        in
                        { stashed | edited = Nothing, nameEditOnly = False, addMode = AddNew }
                in
                Flow.modify (set route newRoute)
                    |> Flow.seq (Flow.when (listingContext currentRoute /= listingContext newRoute) (Flow.modify Selection.clear))
                    |> Flow.seq (Flow.when (listingFolder currentRoute /= listingFolder newRoute) (Flow.over (stepForms << values) resetTable |> Flow.seq (Flow.over projectForms resetTable)))
                    |> Flow.seq (Flow.when (viewedCommitOf currentRoute /= viewedCommitOf newRoute) (Flow.setAll projectRollups Dict.empty))
                    |> Flow.seq (Flow.when (pageTarget currentRoute /= pageTarget newRoute) Actions.resetPageScroll)
                    |> Flow.seq (Flow.when projectRoute (Actions.expandSidebarPath expandPath))
                    |> Flow.seq
                        (if shouldInitializeWorkspace then
                            initializeWorkspace

                         else
                            Flow.over projectForms resetTable
                                |> Flow.seq (Flow.over (stepForms << values) resetTable)
                                |> Flow.seq
                                    (Flow.setAll
                                        (steps << values << runState)
                                        (Api.ApiData.loading Nothing)
                                    )
                                |> Flow.seq (Flow.over projects Api.ApiData.toLoading)
                                |> Flow.seq (Flow.over commitHash Api.ApiData.toLoading)
                                |> Flow.seq Actions.loadStepConfig
                                |> Flow.seq Actions.loadPresets
                                |> Flow.seq Actions.loadProjects
                                |> Flow.when (viewedCommitOf currentRoute /= viewedCommitOf newRoute)
                                |> Flow.seq
                                    (Flow.when (listingFolder newRoute /= listingFolder currentRoute)
                                        (Flow.async Actions.loadProjectReviews)
                                    )
                        )
                    |> Flow.seq
                        (Flow.forAll route
                            (\currentRoute_ ->
                                Flow.when (currentRoute_ == newRoute) <|
                                    case newRoute.page of
                                        Route.Project { projectPath, mHighlight, mCommit } ->
                                            Actions.requestProjectStatus (Route.pathProjectId projectPath) mCommit
                                                |> Flow.seq
                                                    (case mHighlight of
                                                        Just highlight ->
                                                            if isDragging || not shouldRevealHighlight then
                                                                Flow.pure ()

                                                            else
                                                                Actions.openHighlightedEntry highlight

                                                        Nothing ->
                                                            Flow.pure ()
                                                    )
                                                |> Flow.seq (Actions.syncCompareFromRoute newRoute)

                                        Route.Artifact _ ->
                                            Actions.syncCompareFromRoute newRoute

                                        Route.NotFound _ ->
                                            Actions.syncCompareFromRoute newRoute
                            )
                        )
            )


subscriptions : Model -> Sub (Flow Model ())
subscriptions model =
    Sub.batch
        [ uploadProgressSubscription model
        , gutterDragSubscription model
        , Time.every (60 * 1000) (\time -> Flow.setAll now time |> Flow.seq Actions.refreshVisibleAgentSession)
        , agentActivitySubscription model
        , Browser.Events.onKeyDown Organize.shortcutDecoder
        , Ports.organizeDragIn Organize.onOrganizeDragEvent
        , Browser.Events.onVisibilityChange
            (\visibility ->
                if visibility == Browser.Events.Visible then
                    Actions.refreshSelectedAgentSession

                else
                    Flow.pure ()
            )
        ]


agentActivitySubscription : Model -> Sub (Flow Model ())
agentActivitySubscription model =
    if Model.Core.hasRunningToolCall model then
        Time.every 1000 (\time -> Flow.setAll now time)

    else
        Sub.none


gutterDragSubscription : Model -> Sub (Flow Model ())
gutterDragSubscription model =
    if has (gutterDrag << just) model then
        Sub.batch
            [ Browser.Events.onMouseUp (Decode.succeed Actions.endGutterDrag)
            , Ports.gutterDragEnd (\_ -> Actions.endGutterDrag)
            ]

    else
        Sub.none


uploadProgressSubscription : Model -> Sub (Flow Model ())
uploadProgressSubscription model =
    Model.Core.getUploadProgress model
        |> Dict.keys
        |> List.map
            (\stepId ->
                Http.track ("upload-" ++ String.fromInt stepId)
                    (Ingest.onUploadProgress stepId)
            )
        |> Sub.batch
