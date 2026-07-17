module View.Main exposing (view)

import Accessors exposing (try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (success)
import Browser
import Components.AgentPanel as AgentPanel
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events as Events
import Html.Extra as Html
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (currentProject, name)
import Route exposing (Route(..))
import Specs
import Toast
import View.Compare as Compare
import View.Dialog as Dialog
import View.Icons
import View.Lib exposing (viewPage, viewSearchBox)
import View.Project exposing (viewCurrentProject)
import View.Table exposing (viewTable)


view : Model -> Browser.Document (Flow Model ())
view model =
    let
        viewHome presets stepConfig =
            viewPage
                { header = [ viewSearchBox model ]
                , content =
                    viewTable
                        { model = model
                        , spec = Specs.projects presets stepConfig
                        , table = Model.getProjects model
                        , specificRecordActions = \_ -> []
                        , alwaysVisibleRecordActions = \_ -> []
                        , directorySection = \_ -> Html.nothing
                        , srcFilesSection = \_ -> Html.nothing
                        , onRecordClick = .id >> Maybe.map (\id -> Actions.goToRoute (Project { projectId = id, mHighlight = Nothing, mCommit = Nothing, mCompare = Nothing }))
                        , isOpen = always False
                        , isSrcOpen = always False
                        }
                }

        viewCurrentPage =
            case Model.getRoute model of
                Route.Home ->
                    Maybe.map2 viewHome
                        (ApiData.toMaybe (Model.getPresets model))
                        (ApiData.toMaybe (Model.getStepConfig model))
                        |> Maybe.withDefault (Html.span [ Html.Attributes.class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading workspace..." ])

                Route.Project _ ->
                    viewCurrentProject model

                Route.Artifact artifact ->
                    viewArtifact artifact

                Route.NotFound ->
                    view404
    in
    { title = try (currentProject << success << name) model |> Maybe.map (\n -> n ++ " • " ++ "Pointy Notebook") |> Maybe.withDefault "Pointy Notebook"
    , body =
        case Model.getRoute model of
            Route.Artifact artifact ->
                [ viewArtifact artifact ]

            _ ->
                [ Compare.viewCompareBanner model
                , Html.div [ Html.Attributes.class "app" ] [ viewCurrentPage ]
                , Html.div [ Html.Attributes.class "toast-container" ] <|
                    List.map Toast.view (Model.getToasts model)
                , Dialog.viewConfirm (Model.getModalConfirm model)
                , Compare.viewCompareDialog model
                , AgentPanel.view model
                , Html.a
                    [ Html.Attributes.href "https://pointy.cloud"
                    , Html.Attributes.target "_blank"
                    , Html.Attributes.title "Open documentation"
                    , Html.Attributes.class "help-btn"
                    ]
                    [ View.Icons.iconCustom False "help_outline" []
                    , Html.span [] [ Html.text "Docs" ]
                    ]
                , Html.button
                    [ Html.Attributes.class "theme-toggle-btn"
                    , Events.onClick Actions.toggleTheme
                    , Html.Attributes.title "Toggle light/dark theme"
                    ]
                    [ View.Icons.iconCustom False "light_mode" [ Html.Attributes.class "icon-dark" ]
                    , View.Icons.iconCustom False "dark_mode" [ Html.Attributes.class "icon-light" ]
                    ]
                ]
    }


viewArtifact : Route.ArtifactParams -> Html (Flow Model ())
viewArtifact artifact =
    let
        pointyRoute =
            Route.Project
                { projectId = artifact.projectId
                , mHighlight = Just { id = artifact.stepId, target = Route.Output, path = artifact.path, range = Nothing }
                , mCommit = Just artifact.commit
                , mCompare = Nothing
                }
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
        , Html.a [ Route.href Route.Home ] [ Html.text "Go Home" ]
        ]
