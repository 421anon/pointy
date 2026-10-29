module View.Main exposing (view)

import Accessors exposing (has, just, try)
import Actions
import Api.Api as Api
import Api.ApiData exposing (ApiData(..), success)
import Browser
import Components.AgentPanel as AgentPanel
import Components.StatusBar as StatusBar
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Html.Keyed
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (areaCapture, areaCaptureDragging, artifactTableContent, artifactTableFor, currentProject, name)
import Model.Lib as Lib
import Organize
import Route
import Toast
import View.Compare as Compare
import View.Dialog as Dialog
import View.Icons exposing (icon, iconCustom)
import View.Lib exposing (boolText)
import View.Organize
import View.Project exposing (viewCurrentProject)
import View.Scratch exposing (viewScratchPicker)
import View.Shadow
import View.Sidebar
import View.TextContent as TextContent


view : Model -> Browser.Document (Flow Model ())
view model =
    { title = try (currentProject << success << name) model |> Maybe.map (\n -> n ++ " • " ++ "Pointy Notebook") |> Maybe.withDefault "Pointy Notebook"
    , body =
        case (Model.getRoute model).page of
            Route.Artifact artifact ->
                [ viewArtifact model artifact ]

            _ ->
                let
                    viewCurrentPage =
                        case (Model.getRoute model).page of
                            Route.Project _ ->
                                viewCurrentProject model

                            Route.Artifact artifact ->
                                viewArtifact model artifact

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
                , viewToasts model
                , Dialog.viewConfirm (Model.getModalConfirm model)
                , View.Organize.viewOrganizeDialog model
                , View.Organize.viewContextMenu model
                , Compare.viewCompareDialog model
                , AgentPanel.viewLightbox model
                , viewScratchPicker model
                , StatusBar.view model
                ]
    }


viewToasts : Model -> Html (Flow Model ())
viewToasts model =
    Html.Keyed.node "div" [ Html.Attributes.class "toast-container" ] <|
        List.map (\toast -> ( String.fromInt toast.id, Toast.view (Actions.dismissToast toast.id) toast )) (Model.getToasts model)


viewArtifact : Model -> Route.ArtifactParams -> Html (Flow Model ())
viewArtifact model artifact =
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

        isAgentOpen =
            (Model.getAgent model).isPanelOpen

        isTable =
            Model.isDelimitedPath artifact.path
    in
    Html.div [ Html.Attributes.class "artifact-viewer" ]
        [ Html.div [ Html.Attributes.class "artifact-viewer-main" ]
            [ if isTable then
                Html.div [ Html.Attributes.class "artifact-viewer-table" ]
                    [ try (artifactTableFor artifact << artifactTableContent) model
                        |> Maybe.withDefault NotAsked
                        |> TextContent.view "artifact-viewer-text"
                            Actions.updateArtifactTable
                            [ Html.a
                                [ Html.Attributes.href (Api.stepFileDownloadUrl artifact.stepId (Just artifact.commit) artifact.path)
                                , Html.Attributes.download ""
                                , Html.Attributes.class "dir-item-icon-btn"
                                , Html.Attributes.title "Download"
                                ]
                                [ icon True "download" ]
                            ]
                    ]

              else
                Html.node "iframe"
                    [ Html.Attributes.id Actions.artifactFrameId
                    , Html.Attributes.src (Api.stepFileBundleUrl artifact.stepId artifact.commit artifact.path)
                    , Html.Attributes.attribute "sandbox" "allow-same-origin allow-scripts allow-top-navigation-by-user-activation allow-popups allow-popups-to-escape-sandbox"
                    , Html.Attributes.class "artifact-viewer-frame"
                    ]
                    []
            , Html.viewIf (not isTable) (Html.viewMaybe viewAreaCapture (Model.getAreaCapture model))
            , Html.div [ Html.Attributes.class "artifact-viewer-actions" ]
                [ Html.viewIf (not isTable)
                    (Html.button
                        [ Html.Attributes.class "artifact-viewer-action"
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick Actions.startAreaCapture
                        , Html.Attributes.attribute "aria-pressed" (boolText (has (areaCapture << just) model))
                        ]
                        [ iconCustom False "ink_selection" [ Html.Attributes.attribute "aria-hidden" "true" ], Html.text "Ask agent" ]
                    )
                , Html.button
                    [ Html.Attributes.class "artifact-viewer-action"
                    , Html.Attributes.type_ "button"
                    , Html.Events.onClick Actions.toggleAgentPanel
                    , Html.Attributes.attribute "aria-controls" "agent-panel"
                    , Html.Attributes.attribute "aria-expanded" (boolText isAgentOpen)
                    ]
                    [ Html.text "Open agent" ]
                , Html.a
                    [ Route.href pointyRoute
                    , Html.Attributes.class "artifact-viewer-action"
                    ]
                    [ Html.text "View in Pointy" ]
                ]
            ]
        , AgentPanel.view model
        , AgentPanel.viewLightbox model
        , viewToasts model
        ]


viewAreaCapture : Model.AreaCapture -> Html (Flow Model ())
viewAreaCapture capture =
    Html.div
        [ Html.Attributes.class "artifact-viewer-capture"
        , Html.Events.preventDefaultOn "mousedown" (Decode.map (\drag -> ( Actions.beginAreaDrag drag, True )) areaDragStartDecoder)
        ]
        [ Html.viewMaybe viewAreaDrag (try areaCaptureDragging capture) ]


viewAreaDrag : Model.AreaDrag -> Html msg
viewAreaDrag drag =
    let
        rect =
            Model.areaDragRect drag

        px value =
            String.fromFloat value ++ "px"
    in
    Html.div
        [ Html.Attributes.class "artifact-viewer-capture-area"
        , Html.Attributes.style "left" (px rect.x)
        , Html.Attributes.style "top" (px rect.y)
        , Html.Attributes.style "width" (px rect.width)
        , Html.Attributes.style "height" (px rect.height)
        ]
        []


areaDragStartDecoder : Decode.Decoder Model.AreaDrag
areaDragStartDecoder =
    Decode.when (Decode.field "button" Decode.int)
        ((==) 0)
        (Decode.map3
            (\client offset overlaySize ->
                { overlayOrigin = { x = client.x - offset.x, y = client.y - offset.y }
                , overlaySize = overlaySize
                , anchor = offset
                , cursor = offset
                }
            )
            Actions.clientPointDecoder
            (Model.areaPointDecoder [ "offsetX" ] [ "offsetY" ])
            (Model.areaPointDecoder [ "currentTarget", "clientWidth" ] [ "currentTarget", "clientHeight" ])
        )


view404 : Html (Flow Model ())
view404 =
    Html.div []
        [ Html.h1 [] [ Html.text "404 - Page Not Found" ]
        , Html.p [] [ Html.text "The page you requested does not exist." ]
        , Html.a [ Route.href (Route.fromPage (Route.projectPage [] Nothing)) ] [ Html.text "Go Home" ]
        ]
