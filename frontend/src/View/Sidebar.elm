module View.Sidebar exposing (view)

import Accessors exposing (get, try)
import Actions
import Api.ApiData as ApiData
import Dict exposing (Dict)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, title)
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model, ProjectRecord)
import Model.Lenses exposing (currentProjectPath, listingPreferences, sidebarExpanded, sidebarOpen, sidebarScrolled)
import Model.Selection
import Route
import Set exposing (Set)
import View.Icons exposing (icon, iconCustom)
import View.Lib
import View.Organize exposing (dropTargetAttrs)


view : Model -> Html (Flow Model ())
view model =
    let
        open =
            get sidebarOpen model

        scrolled =
            get sidebarScrolled model

        expanded : Int -> Bool
        expanded nodeId =
            Set.member nodeId (get sidebarExpanded model)

        mCommit_ =
            Route.viewedCommit (Model.getRoute model).page

        editable =
            Model.Selection.listingEditable model
    in
    case ApiData.toMaybe (Model.getProjects model) of
        Nothing ->
            Html.nothing

        Just projects ->
            Html.aside
                [ class "sidebar"
                , classList [ ( "sidebar--open", open ), ( "sidebar--scrolled", scrolled ) ]
                ]
                [ Html.div [ class "sidebar-header" ]
                    [ Html.button
                        [ class "icon-btn sidebar-toggle"
                        , title
                            (if open then
                                "Hide navigation"

                             else
                                "Show navigation"
                            )
                        , Html.Events.onClick Actions.toggleSidebar
                        , attribute "aria-label" "Toggle navigation"
                        ]
                        [ icon True
                            (if open then
                                "chevron_left"

                             else
                                "chevron_right"
                            )
                        ]
                    , Html.viewIf open <|
                        Html.span [ class "sidebar-title" ] [ Html.text "Navigation" ]
                    ]
                , Html.viewIf open <|
                    Html.div
                        [ class "sidebar-body"
                        , Html.Events.on "scroll" (scrolledChangeDecoder scrolled)
                        ]
                        (viewNode model editable projects mCommit_ [] expanded rootLink)
                ]


scrolledChangeDecoder : Bool -> Decode.Decoder (Flow Model ())
scrolledChangeDecoder scrolled =
    Decode.at [ "target", "scrollTop" ] Decode.float
        |> Decode.andThen
            (\scrollTop ->
                if (scrollTop > 0) == scrolled then
                    Decode.fail "unchanged"

                else
                    Decode.succeed (Actions.setSidebarScrolled (scrollTop > 0))
            )


rootLink : Model.ChildLink
rootLink =
    Model.childLinkOf { kind = Model.ProjectChild, id = Route.rootProjectId }


viewNode : Model -> Bool -> Dict Int ProjectRecord -> Maybe String -> List Int -> (Int -> Bool) -> Model.ChildLink -> List (Html (Flow Model ()))
viewNode model editable projects mCommit_ ancestors isOpen link =
    if link.kind /= Model.ProjectChild || List.member link.id ancestors then
        []

    else
        [ viewFolderNode model editable projects mCommit_ ancestors isOpen link ]


viewFolderNode : Model -> Bool -> Dict Int ProjectRecord -> Maybe String -> List Int -> (Int -> Bool) -> Model.ChildLink -> Html (Flow Model ())
viewFolderNode model editable projects mCommit_ ancestors isOpen link =
    let
        projectId =
            link.id

        path =
            List.drop 1 (ancestors ++ [ projectId ])

        isCurrent =
            try currentProjectPath model |> Maybe.map ((==) path) |> Maybe.withDefault False

        mProject =
            Dict.get projectId projects

        projectName =
            mProject |> Maybe.unwrap ("#" ++ String.fromInt projectId) .name

        visibleChildren =
            let
                prefs =
                    get listingPreferences model
            in
            mProject
                |> Maybe.map Model.projectChildren
                |> Maybe.withDefault []
                |> List.filter (\child -> prefs.showHidden || not child.hidden)

        hasChildren =
            List.any (\child -> child.kind == Model.ProjectChild) visibleChildren

        expanded =
            isOpen projectId

        childNodes =
            if expanded then
                List.concatMap (viewNode model editable projects mCommit_ (ancestors ++ [ projectId ]) isOpen) visibleChildren

            else
                []

        expander =
            if hasChildren then
                Html.button
                    [ class "sidebar-expander"
                    , classList [ ( "expanded", expanded ) ]
                    , Html.Events.onClick (Actions.toggleSidebarNode projectId)
                    , attribute "aria-expanded" (View.Lib.boolText expanded)
                    , attribute "aria-label"
                        (if expanded then
                            "Collapse"

                         else
                            "Expand"
                        )
                    ]
                    [ iconCustom True "chevron_right" [] ]

            else
                Html.span [ class "sidebar-expander" ] []

        linkNode =
            Html.a
                ([ Route.href (Route.fromPage (Route.projectPage path mCommit_))
                 , class "sidebar-link"
                 , classList [ ( "current", isCurrent ), ( "hidden-link", link.hidden ) ]
                 , attribute "aria-current" (View.Lib.boolText isCurrent)
                 ]
                    ++ (if editable then
                            dropTargetAttrs model projectId

                        else
                            []
                       )
                )
                [ Html.span [ class "sidebar-row-icon" ] [ iconCustom False "folder" [] ]
                , Html.span [ class "sidebar-label" ] [ Html.text projectName ]
                ]
    in
    Html.div
        [ class "sidebar-node"
        , classList [ ( "expanded", expanded ) ]
        ]
        (Html.div [ class "sidebar-row" ] [ expander, linkNode ] :: childNodes)
