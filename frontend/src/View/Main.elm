module View.Main exposing (view)

import Accessors exposing (try)
import Actions
import Api.Api as Api
import Api.ApiData exposing (success)
import Browser
import Components.AgentPanel as AgentPanel
import Components.StatusBar as StatusBar
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Html.Keyed
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (currentProject, name)
import Model.Lib as Lib
import Organize
import Route
import Toast
import View.Compare as Compare
import View.Dialog as Dialog
import View.Organize
import View.Project exposing (viewCurrentProject)
import View.Scratch exposing (viewScratchPicker)
import View.Shadow
import View.Sidebar


view : Model -> Browser.Document (Flow Model ())
view model =
    { title = try (currentProject << success << name) model |> Maybe.map (\n -> n ++ " • " ++ "Pointy Notebook") |> Maybe.withDefault "Pointy Notebook"
    , body =
        case (Model.getRoute model).page of
            Route.Artifact artifact ->
                [ viewArtifact artifact ]

            _ ->
                let
                    viewCurrentPage =
                        case (Model.getRoute model).page of
                            Route.Project _ ->
                                viewCurrentProject model

                            Route.Unfiled _ ->
                                View.Shadow.viewUnfiled model

                            Route.Artifact artifact ->
                                viewArtifact artifact

                            Route.NotFound _ ->
                                view404
                in
                [ Html.viewIf (Lib.isWorkspaceReloading model) (Html.div [ Html.Attributes.class "loading-bar" ] [])
                , Compare.viewCompareBanner model
                , Html.div
                    [ Html.Attributes.class "workspace"
                    , Html.Events.onClick Organize.closeMenu
                    ]
                    [ View.Sidebar.view model
                    , Html.div [ Html.Attributes.class "app" ] [ viewCurrentPage ]
                    , AgentPanel.view model
                    ]
                , Html.Keyed.node "div" [ Html.Attributes.class "toast-container" ] <|
                    List.map (\toast -> ( String.fromInt toast.id, Toast.view (Actions.dismissToast toast.id) toast )) (Model.getToasts model)
                , Dialog.viewConfirm (Model.getModalConfirm model)
                , View.Organize.viewOrganizeDialog model
                , View.Organize.viewContextMenu model
                , Compare.viewCompareDialog model
                , viewScratchPicker model
                , StatusBar.view model
                ]
    }


viewArtifact : Route.ArtifactParams -> Html (Flow Model ())
viewArtifact artifact =
    let
        pointyRoute =
            Route.fromPage
                (Route.Project
                    { projectPath = artifact.projectPath
                    , mHighlight = Just { id = artifact.stepId, target = Route.Output, path = artifact.path, range = Nothing }
                    , mCommit = Just artifact.commit
                    , mCompare = Nothing
                    }
                )
    in
    Html.div [ Html.Attributes.class "artifact-viewer" ]
        [ Html.node "iframe"
            [ Html.Attributes.src (Api.stepFileBundleUrl artifact.stepId artifact.commit artifact.path)
            , Html.Attributes.attribute "sandbox" "allow-same-origin allow-scripts"
            , Html.Attributes.class "artifact-viewer-frame"
            ]
            []
        , Html.a
            [ Route.href pointyRoute
            , Html.Attributes.class "artifact-pointy-link"
            ]
            [ Html.text "View in Pointy" ]
        ]


view404 : Html (Flow Model ())
view404 =
    Html.div []
        [ Html.h1 [] [ Html.text "404 - Page Not Found" ]
        , Html.p [] [ Html.text "The page you requested does not exist." ]
        , Html.a [ Route.href (Route.fromPage (Route.projectPage [] Nothing)) ] [ Html.text "Go Home" ]
        ]
