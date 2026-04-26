module View.Main exposing (view)

import Accessors exposing (try)
import Actions
import Api.ApiData as ApiData exposing (success)
import Browser
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
import View.Dialog as Dialog
import View.Icons
import View.Lib exposing (viewPage, viewSearchBox)
import View.Project exposing (viewCurrentProject)
import View.Table exposing (viewTable)


view : Model -> Browser.Document (Flow Model ())
view model =
    let
        viewHome stepConfig =
            viewPage
                { header = [ viewSearchBox model ]
                , content =
                    viewTable
                        { model = model
                        , spec = Specs.projects stepConfig
                        , table = Model.getProjects model
                        , specificRecordActions = \_ -> []
                        , alwaysVisibleRecordActions = \_ -> []
                        , directorySection = \_ -> Html.nothing
                        , srcFilesSection = \_ -> Html.nothing
                        , onRecordClick = .id >> Maybe.map (\id -> Actions.goToRoute (Project { projectId = id, mHighlight = Nothing, mCommit = Nothing }))
                        , isOpen = always False
                        , isSrcOpen = always False
                        }
                }

        viewCurrentPage =
            case Model.getRoute model of
                Route.Home ->
                    ApiData.foldVisible
                        Html.nothing
                        (Maybe.map viewHome >> Maybe.withDefault (Html.span [ Html.Attributes.class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading step config..." ]))
                        viewHome
                        (always Html.nothing)
                        (Model.getStepConfig model)

                Route.Project _ ->
                    viewCurrentProject model

                Route.NotFound ->
                    view404
    in
    { title = try (currentProject << success << name) model |> Maybe.map (\n -> n ++ " • " ++ "Pointy Notebook") |> Maybe.withDefault "Pointy Notebook"
    , body =
        [ Html.div [ Html.Attributes.class "app" ] [ viewCurrentPage ]
        , Html.div [ Html.Attributes.class "toast-container" ] <|
            List.map Toast.view (Model.getToasts model)
        , Dialog.viewConfirm (Model.getModalConfirm model)
        , Html.a
            [ Html.Attributes.href "/docs/"
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


view404 : Html (Flow Model ())
view404 =
    Html.div []
        [ Html.h1 [] [ Html.text "404 - Page Not Found" ]
        , Html.p [] [ Html.text "The page you requested does not exist." ]
        , Html.a [ Route.href Route.Home ] [ Html.text "Go Home" ]
        ]
