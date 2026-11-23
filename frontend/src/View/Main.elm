module View.Main exposing (view)

import Accessors exposing (get, has, just, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Browser
import Components.AgentPanel as AgentPanel
import Components.StatusBar as StatusBar
import Dict
import Extra.Accessors exposing (orElseT, where_)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Html.Keyed
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (areaCapture, artifactComparison, artifactTableContent, artifactTableFor, currentProject, name, shownArtifactTitle, stepRecordById, toasts)
import Model.Lib as Lib
import Model.Shadow exposing (StepConfigEntry)
import Model.TableSpec as TableSpec
import Organize
import Route
import Specs
import Toast
import View.AreaCapture as AreaCapture
import View.Compare as Compare
import View.Dialog as Dialog
import View.Icons exposing (icon, iconCustom)
import View.Lib exposing (boolText)
import View.Organize
import View.Project exposing (viewCurrentProject)
import View.Scratch exposing (viewScratchPicker)
import View.Shadow
import View.Sidebar
import View.Table
import View.TextContent as TextContent


view : Model -> Browser.Document (Flow Model ())
view model =
    { title = try (orElseT shownArtifactTitle (currentProject << success << name)) model |> Maybe.map (\n -> n ++ " • " ++ "Pointy Notebook") |> Maybe.withDefault "Pointy Notebook"
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
    let
        viewToast mDismissAll toast =
            ( String.fromInt toast.id, Toast.view (Actions.dismissToast toast.id) mDismissAll toast )
    in
    Html.Keyed.node "div" [ Html.Attributes.class "toast-container" ] <|
        case get toasts model of
            top :: ((_ :: _) as rest) ->
                viewToast (Just Actions.dismissAllToasts) top :: List.map (viewToast Nothing) rest

            single ->
                List.map (viewToast Nothing) single


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
                    , Html.Events.on "load" (Decode.map Actions.setArtifactTitle (Decode.maybe (Decode.at [ "target", "contentDocument", "title" ] Decode.string)))
                    ]
                    []
            , Html.viewIf (not isTable) (Html.viewMaybe (AreaCapture.view (Model.getAgent model)) (Model.getAreaCapture model))
            , Html.div [ Html.Attributes.class "artifact-viewer-actions" ]
                [ Html.viewMaybe (viewNewerRevisionNotice model artifact.stepId) (newerArtifactRevision model artifact)
                , Html.viewIf (not isTable)
                    (Html.button
                        [ Html.Attributes.class "artifact-viewer-action artifact-viewer-action--agent"
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick Actions.startAreaCapture
                        , Html.Attributes.attribute "aria-pressed" (boolText (has (areaCapture << just) model))
                        ]
                        [ iconCustom False "ink_selection" [ Html.Attributes.attribute "aria-hidden" "true" ], Html.text "Ask agent" ]
                    )
                , Html.button
                    [ Html.Attributes.class "artifact-viewer-action artifact-viewer-action--agent"
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


newerArtifactRevision : Model -> Route.ArtifactParams -> Maybe { revision : String, built : Bool }
newerArtifactRevision model artifact =
    try (artifactComparison << just << where_ (\shown -> shown.stepId == artifact.stepId && shown.revision == artifact.commit)) model
        |> Maybe.andThen
            (\shown ->
                case shown.comparison of
                    Model.ViewedOutputUnbuilt ->
                        Just { revision = shown.head, built = False }

                    Model.DifferentContent ->
                        Just { revision = shown.head, built = True }

                    _ ->
                        Nothing
            )


viewNewerRevisionNotice : Model -> Int -> { revision : String, built : Bool } -> Html (Flow Model ())
viewNewerRevisionNotice model stepId { revision, built } =
    let
        shortRevision =
            String.left 12 revision

        mStep =
            stepAtRevision model stepId revision

        isBuilding =
            Maybe.andThen (stepStatus >> ApiData.toMaybe) mStep == Just Model.StatusRunning
    in
    Html.div [ Html.Attributes.class "artifact-viewer-revision-notice", Html.Attributes.attribute "role" "status" ]
        (Maybe.unwrap [] (viewRevisionStatusDot model stepId) mStep
            ++ (if built then
                    [ Html.text ("Newer revision " ++ shortRevision ++ " is built.")
                    , Html.button
                        [ Html.Attributes.class "artifact-viewer-revision-action"
                        , Html.Attributes.type_ "button"
                        , Html.Events.onClick (Actions.showArtifactRevision revision)
                        ]
                        [ Html.text "View newer revision" ]
                    ]

                else if isBuilding then
                    [ Html.text ("Newer revision " ++ shortRevision ++ " is building.") ]

                else
                    [ Html.text ("Newer revision " ++ shortRevision ++ " is not built yet.")
                    , Html.viewMaybe (viewRunRevisionButton stepId) mStep
                    ]
               )
        )


stepAtRevision : Model -> Int -> String -> Maybe ( Model.StepRecord, StepConfigEntry )
stepAtRevision model stepId revision =
    try (stepRecordById stepId) model
        |> Maybe.filter (\step -> Maybe.map .commit (ApiData.toMaybe step.runState) == Just revision)
        |> Maybe.andThen
            (\step ->
                ApiData.toMaybe (Model.getStepConfig model)
                    |> Maybe.andThen (Dict.get step.type_)
                    |> Maybe.map (Tuple.pair step)
            )


stepStatus : ( Model.StepRecord, StepConfigEntry ) -> ApiData Model.Status
stepStatus ( step, entry ) =
    TableSpec.getStatus (Specs.steps step.type_ entry) step


viewRevisionStatusDot : Model -> Int -> ( Model.StepRecord, StepConfigEntry ) -> List (Html (Flow Model ()))
viewRevisionStatusDot model stepId ( step, entry ) =
    let
        stepRevision =
            Model.stepRevision model step

        log =
            Dict.get (Model.stepLogKey stepId stepRevision) (Model.getStepLogs model)
                |> Maybe.withDefault NotAsked
    in
    [ View.Table.viewStepRecordStatus step.type_ entry False step
    , View.Table.viewStepRecordStatusPopover step.type_ entry (Model.coldInputsAt stepId stepRevision model) log False step
    ]


viewRunRevisionButton : Int -> ( Model.StepRecord, StepConfigEntry ) -> Html (Flow Model ())
viewRunRevisionButton stepId ( step, entry ) =
    let
        canRun =
            case stepStatus ( step, entry ) of
                Loading _ ->
                    False

                Success Model.StatusSuccess ->
                    False

                Success Model.StatusRunning ->
                    False

                _ ->
                    True
    in
    Html.viewIf canRun
        (Html.button
            [ Html.Attributes.class "artifact-viewer-revision-action"
            , Html.Attributes.type_ "button"
            , Html.Events.onClick (Actions.runStep (Specs.steps step.type_ entry) stepId)
            ]
            [ iconCustom True "play_arrow" [ Html.Attributes.attribute "aria-hidden" "true" ], Html.text "Run" ]
        )


view404 : Html (Flow Model ())
view404 =
    Html.div []
        [ Html.h1 [] [ Html.text "404 - Page Not Found" ]
        , Html.p [] [ Html.text "The page you requested does not exist." ]
        , Html.a [ Route.href (Route.fromPage (Route.projectPage [] Nothing)) ] [ Html.text "Go Home" ]
        ]
