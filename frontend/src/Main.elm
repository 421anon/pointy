module Main exposing (main)

import Accessors exposing (each, get, has, just, set, try)
import Actions
import Api.ApiData exposing (ApiData(..), success)
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Flow exposing (Flow)
import Http
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core exposing (Flags, Model, initialModel)
import Model.Lenses exposing (commitHash, currentProject, currentProjectId, gutterDrag, mCommit, mHighlight, now, presets, projectStepRecords, projects, records, route, runState, stepConfig, userRepoInfo)
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
    Actions.loadUserRepoInfo
        |> Flow.seq Actions.loadStepConfig
        |> Flow.seq Actions.loadPresets
        |> Flow.seq Actions.loadProjects
        |> Flow.seq (Flow.performTask Time.now |> Flow.andThen (Flow.setAll now))
        |> Flow.seq (Flow.async Actions.startClusterStatusStream)


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

                    workspaceNotStarted =
                        case get userRepoInfo model of
                            NotAsked ->
                                True

                            _ ->
                                False

                    routeNeedsWorkspace =
                        case newRoute.page of
                            Route.Home ->
                                True

                            Route.Project _ ->
                                True

                            _ ->
                                False

                    shouldInitializeWorkspace =
                        workspaceNotStarted && routeNeedsWorkspace

                    isDragging =
                        has (gutterDrag << just) model
                in
                Flow.modify (set route newRoute)
                    |> Flow.seq (Flow.when (pageTarget currentRoute /= pageTarget newRoute) Actions.resetPageScroll)
                    |> Flow.seq
                        (if shouldInitializeWorkspace then
                            initializeWorkspace

                         else
                            let
                                mOldCommit =
                                    try (route << Route.page << Route.project << mCommit << just) model

                                mNewCommit =
                                    try (Route.page << Route.project << mCommit << just) newRoute
                            in
                            Flow.setAll
                                (projects << records << success << each << projectStepRecords << runState)
                                (Api.ApiData.loading Nothing)
                                |> Flow.seq (Flow.over (projects << records) Api.ApiData.toLoading)
                                |> Flow.seq (Flow.over commitHash Api.ApiData.toLoading)
                                |> Flow.seq Actions.loadStepConfig
                                |> Flow.seq Actions.loadPresets
                                |> Flow.seq Actions.loadProjects
                                |> Flow.when (mOldCommit /= mNewCommit)
                        )
                    |> Flow.seq
                        (Flow.forAll route
                            (\currentRoute_ ->
                                Flow.when (currentRoute_ == newRoute) <|
                                    case newRoute.page of
                                        Route.Project { projectId, mHighlight, mCommit } ->
                                            Flow.ifHas
                                                (currentProject << success)
                                                (\_ -> Flow.async <| Actions.listenAndProcessStepStatus projectId mCommit)
                                                Actions.closeStepStatusStream
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
                                            Actions.closeStepStatusStream
                                                |> Flow.seq (Actions.syncCompareFromRoute newRoute)

                                        Route.Home ->
                                            Actions.closeStepStatusStream
                                                |> Flow.seq (Actions.syncCompareFromRoute newRoute)

                                        Route.NotFound _ ->
                                            Actions.syncCompareFromRoute newRoute
                            )
                        )
            )


dndSubscription : Model -> Sub (Flow Model ())
dndSubscription model =
    let
        mProjectId =
            try currentProjectId model
    in
    Maybe.map2 Tuple.pair (try (presets << success) model) (try (stepConfig << success) model)
        |> Maybe.unwrap []
            (\( presets_, config ) ->
                Actions.dndSub model Nothing (Specs.projects presets_ config)
                    :: List.map (\( name, entry ) -> Actions.dndSub model mProjectId (Specs.steps name entry)) (Dict.toList config)
            )
        |> Sub.batch


subscriptions : Model -> Sub (Flow Model ())
subscriptions model =
    Sub.batch
        [ dndSubscription model
        , uploadProgressSubscription model
        , gutterDragSubscription model
        , Time.every (60 * 1000) (\time -> Flow.setAll now time |> Flow.seq Actions.refreshSelectedAgentSession)
        , Browser.Events.onVisibilityChange
            (\visibility ->
                if visibility == Browser.Events.Visible then
                    Actions.refreshSelectedAgentSession

                else
                    Flow.pure ()
            )
        ]


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
                    (Actions.onUploadProgress stepId)
            )
        |> Sub.batch
