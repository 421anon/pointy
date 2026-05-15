module Main exposing (main)

import Accessors exposing (each, get, has, just, set, try)
import Actions
import Api.ApiData exposing (success)
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Flow exposing (Flow)
import Time
import Http
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core exposing (Flags, Model, initialModel)
import Model.Lenses exposing (commitHash, currentProjectId, gutterDrag, mCommit, now, projectStepRecords, projects, records, route, runState, stepConfig)
import Ports
import Route
import Specs
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
    , Actions.loadUserRepoInfo
        |> Flow.seq Actions.loadStepConfig
        |> Flow.seq Actions.loadProjects
        |> Flow.seq (Flow.performTask Time.now |> Flow.andThen (Flow.setAll now))
        |> Flow.seq (applyRouteFromUrl True url)
    )


applyRouteFromUrl : Bool -> Url -> Flow Model ()
applyRouteFromUrl forceRevealHighlight url =
    let
        newRoute =
            Route.fromUrl url
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mOldCommit =
                        try (route << Route.project << mCommit << just) model

                    mNewCommit =
                        try (Route.project << mCommit << just) newRoute

                    shouldRevealHighlight =
                        forceRevealHighlight
                            || Route.navigationTarget (get route model)
                            /= Route.navigationTarget newRoute

                    isDragging =
                        has (gutterDrag << just) model
                in
                Flow.modify (set route newRoute)
                    |> Flow.seq
                        (Flow.setAll
                            (projects << records << success << each << projectStepRecords << runState)
                            (Api.ApiData.loading Nothing)
                            |> Flow.seq (Flow.over (projects << records) Api.ApiData.toLoading)
                            |> Flow.seq (Flow.over commitHash Api.ApiData.toLoading)
                            |> Flow.seq Actions.loadStepConfig
                            |> Flow.seq Actions.loadProjects
                            |> Flow.when (mOldCommit /= mNewCommit)
                        )
                    |> Flow.seq
                        (case newRoute of
                            Route.Project { projectId, mHighlight, mCommit } ->
                                (Flow.async <| Actions.listenAndProcessStepStatus projectId mCommit)
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

                            Route.Home ->
                                Actions.closeStepStatusStream

                            Route.NotFound ->
                                Flow.pure ()
                        )
            )


dndSubscription : Model -> Sub (Flow Model ())
dndSubscription model =
    let
        mProjectId =
            try currentProjectId model
    in
    try (stepConfig << success) model
        |> Maybe.unwrap []
            (\config ->
                Actions.dndSub model Nothing (Specs.projects config)
                    :: List.map (\( name, entry ) -> Actions.dndSub model mProjectId (Specs.steps name entry)) (Dict.toList config)
            )
        |> Sub.batch


subscriptions : Model -> Sub (Flow Model ())
subscriptions model =
    Sub.batch
        [ dndSubscription model
        , uploadProgressSubscription model
        , gutterDragSubscription model
        , Time.every (60 * 1000) (Flow.setAll now)
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
