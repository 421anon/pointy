module View.Project exposing (viewCurrentProject)

import Accessors exposing (get, has, try)
import Api.ApiData as ApiData exposing (ApiData(..))
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Model.Core exposing (Model, ProjectRecord)
import Model.Lenses exposing (currentProject, currentProjectPath, loadedProjects, projects, stepConfig)
import Route
import View.Shadow exposing (viewProject)


viewCurrentProject : Model -> Html (Flow Model ())
viewCurrentProject model =
    case get currentProject model of
        Success proj ->
            viewLoadedProject model proj

        NotAsked ->
            case ApiData.toMaybe (get stepConfig model) of
                Nothing ->
                    Html.span [ class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading step config..." ]

                Just _ ->
                    viewLoadingProject

        Loading mProject ->
            mProject
                |> Maybe.map (viewLoadedProject model)
                |> Maybe.withDefault viewLoadingProject

        Error _ ->
            if try currentProjectPath model == Just [] && has (projects << ApiData.success) model then
                viewRootProjectMissing

            else
                viewProjectNotFound


viewLoadedProject : Model -> ProjectRecord -> Html (Flow Model ())
viewLoadedProject model proj =
    case try loadedProjects model of
        Just projects_ ->
            viewProject model projects_ proj

        Nothing ->
            viewLoadingProject


viewLoadingProject : Html msg
viewLoadingProject =
    Html.span [ class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading project..." ]


viewRootProjectMissing : Html (Flow Model ())
viewRootProjectMissing =
    Html.div []
        [ Html.h2 [] [ Html.text "Root project missing" ]
        , Html.p [] [ Html.text "The repository has no root project (projects/0.nix)." ]
        ]


viewProjectNotFound : Html (Flow Model ())
viewProjectNotFound =
    Html.div []
        [ Html.h2 [] [ Html.text "Project not found" ]
        , Html.p [] [ Html.text "The project path you entered does not exist." ]
        , Html.a [ Route.href (Route.fromPage (Route.projectPage [] Nothing)) ] [ Html.text "Go home" ]
        ]
