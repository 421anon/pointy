module View.Project exposing (..)

import Accessors exposing (get)
import Api.ApiData as ApiData exposing (ApiData(..), visibleLoadingValue)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Model.Core exposing (Model)
import Model.Lenses exposing (currentProject, stepConfig)
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

        Loading loadingState ->
            visibleLoadingValue loadingState
                |> Maybe.map (viewProject model)
                |> Maybe.withDefault (Html.span [ class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading project..." ])

        Error _ ->
            viewProjectNotFound


viewProjectNotFound : Html (Flow Model ())
viewProjectNotFound =
    Html.div []
        [ Html.h2 [] [ Html.text "Project not found" ]
        , Html.p [] [ Html.text "The project ID you entered does not exist." ]
        , Html.a [ Route.href Route.Home ] [ Html.text "Go home" ]
        ]
