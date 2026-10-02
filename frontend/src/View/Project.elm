module View.Project exposing (..)

import Accessors exposing (get, has, try)
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Model.Core exposing (Model)
import Model.Lenses exposing (currentProject, currentProjectPath, projects, records, stepConfig)
import Route
import View.Shadow exposing (viewProject)


viewCurrentProject : Model -> Html (Flow Model ())
viewCurrentProject model =
    case get currentProject model of
        Success proj ->
            viewProject model proj

        NotAsked ->
            case ApiData.toMaybe (get stepConfig model) of
                Nothing ->
                    Html.span [ class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading step config..." ]

                Just _ ->
                    Html.nothing

        Loading mProject ->
            mProject
                |> Maybe.map (viewProject model)
                |> Maybe.withDefault (Html.span [ class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading project..." ])

        Error _ ->
            if try currentProjectPath model == Just [] && has (projects << records << success) model then
                viewRootProjectMissing

            else
                viewProjectNotFound


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
