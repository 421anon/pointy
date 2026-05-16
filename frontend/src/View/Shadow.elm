module View.Shadow exposing (viewProject)

import Accessors exposing (get, has, just, try)
import Actions
import Api.ApiData as ApiData
import Dict
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model, ProjectRecord, StepRecord, Table)
import Model.Lenses as Lenses exposing (currentProject, mCommit, route)
import Model.Shadow exposing (StepConfigEntry, StepType(..))
import Model.TableSpec as TableSpec
import Route
import Specs
import View.FileBrowser as FileBrowser
import View.Icons exposing (iconCustom)
import View.Lib exposing (viewLoading, viewPage, viewSearchBox)
import View.Table exposing (viewIconButtonWithTooltip, viewRunButton, viewStopButton, viewTable, viewUploadButton, viewUploadProgress)


viewProject : Model -> ProjectRecord -> Html (Flow Model ())
viewProject model proj =
    let
        mCommit_ =
            try (route << Route.project << mCommit << just) model
    in
    viewPage
        { header =
            [ Html.div [ Html.Attributes.class "project-header-container" ]
                [ Html.div [ Html.Attributes.class "project-header" ]
                    [ Html.a [ Route.href Route.Home, Html.Attributes.class "back-btn" ] [ iconCustom True "arrow_back" [ Html.Attributes.class "back-icon" ] ]
                    , Html.h2 [] [ Html.text proj.name ]
                    ]
                , Html.viewMaybe
                    (\commit ->
                        let
                            currentVersionRoute =
                                case Model.getRoute model of
                                    Route.Project params ->
                                        Route.Project { params | mCommit = Nothing }

                                    other ->
                                        other
                        in
                        Html.div [ Html.Attributes.class "read-only-badge" ]
                            [ Html.text "Read-only view (Commit: "
                            , Html.text commit
                            , Html.text ") "
                            , Html.a
                                [ Route.href currentVersionRoute ]
                                [ Html.text "View current version" ]
                            ]
                    )
                    mCommit_
                ]
            , viewSearchBox model
            ]
        , content =
            let
                orphanWarning =
                    Html.viewIf (not (List.isEmpty proj.orphanedSteps)) <|
                        Html.div [ Html.Attributes.class "project-config-warning" ]
                            [ Html.div
                                [ Html.Attributes.class "project-config-warning-header"
                                , Html.Events.onClick (Flow.over (currentProject << ApiData.success << Lenses.hideOrphans) not)
                                ]
                                [ iconCustom True
                                    (if proj.hideOrphans then
                                        "chevron_right"

                                     else
                                        "expand_more"
                                    )
                                    []
                                , Html.text "This project contains steps whose template is not active in the project configuration:"
                                ]
                            , Html.viewIf (not proj.hideOrphans) <|
                                Html.ul []
                                    (List.map
                                        (\s ->
                                            Html.li []
                                                [ Html.text (Maybe.unwrap "" (\id -> "[" ++ String.fromInt id ++ "] ") s.id ++ "(" ++ s.type_ ++ ") " ++ s.name) ]
                                        )
                                        proj.orphanedSteps
                                    )
                            ]

                configErrors =
                    Html.viewIf (not (List.isEmpty proj.validationErrors)) <|
                        Html.div [ Html.Attributes.class "project-config-error" ]
                            [ Html.ul []
                                (List.map (\msg -> Html.li [] [ Html.text msg ]) proj.validationErrors)
                            ]

                sections =
                    Html.div [ Html.Attributes.class "sections" ]
                        (configErrors
                            :: orphanWarning
                            :: (proj.tables
                                    |> Dict.toList
                                    |> List.filterMap
                                        (\( sectionName, steps ) ->
                                            Model.getStepConfig model
                                                |> ApiData.toMaybe
                                                |> Maybe.andThen (Dict.get sectionName)
                                                |> Maybe.map (\entry -> ( sectionName, entry, steps ))
                                        )
                                    |> List.sortBy (\( name, entry, _ ) -> ( entry.sortKey |> Maybe.withDefault 2147483647, name ))
                                    |> List.map (\( sectionName, entry, steps ) -> viewSection model sectionName entry steps)
                               )
                        )
            in
            case ( Model.getCommitHash model, get currentProject model ) of
                ( ApiData.Loading _, _ ) ->
                    viewLoading sections

                ( _, ApiData.Loading _ ) ->
                    viewLoading sections

                _ ->
                    sections
        }


viewSection : Model -> String -> StepConfigEntry -> Table StepRecord -> Html (Flow Model ())
viewSection model sectionName entry steps =
    let
        stepType =
            entry.stepType

        spec =
            Specs.steps sectionName entry

        isReadOnly =
            has (route << Route.project << mCommit << just) model
    in
    viewTable
        { model = model
        , spec = spec
        , table = steps
        , alwaysVisibleRecordActions =
            \r ->
                case stepType of
                    FileUpload _ ->
                        case r.id |> Maybe.andThen (\id -> Dict.get id (Model.getUploadProgress model) |> Maybe.map (Tuple.pair id)) of
                            Just ( stepId, progress ) ->
                                [ viewUploadProgress stepId progress ]

                            Nothing ->
                                []

                    Derivation _ _ ->
                        []
        , specificRecordActions =
            \r ->
                let
                    runActions =
                        case stepType of
                            Derivation _ _ ->
                                case r.id of
                                    Just id ->
                                        let
                                            isRunning =
                                                TableSpec.getStatus spec r
                                                    |> ApiData.toMaybe
                                                    |> (==) (Just Model.StatusRunning)
                                        in
                                        [ viewRunButton "Run" (Actions.runStep spec id)
                                        , Html.viewIf isRunning (viewStopButton "Stop" (Actions.stopStep spec id))
                                        ]

                                    Nothing ->
                                        []

                            FileUpload _ ->
                                []

                    uploadActions =
                        if isReadOnly then
                            []

                        else
                            case stepType of
                                FileUpload types ->
                                    case r.id |> Maybe.andThen (\id -> Dict.get id (Model.getUploadProgress model) |> Maybe.map (Tuple.pair id)) of
                                        Just _ ->
                                            []

                                        Nothing ->
                                            [ Html.viewMaybe (viewUploadButton << Actions.uploadFiles spec (Maybe.withDefault [] types)) r.id ]

                                Derivation _ _ ->
                                    []
                in
                uploadActions ++ runActions
        , directorySection = FileBrowser.viewDirectorySection model spec
        , srcFilesSection = FileBrowser.viewSrcFilesSection model stepType spec
        , onRecordClick =
            \record ->
                record.id
                    |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
        , isOpen = \r -> TableSpec.getDirectoryView spec r |> Maybe.map .expanded |> Maybe.withDefault False
        , isSrcOpen = \r -> TableSpec.getSrcFilesView spec r |> Maybe.map .expanded |> Maybe.withDefault False
        }
