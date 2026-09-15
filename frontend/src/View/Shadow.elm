module View.Shadow exposing (viewProject)

import Accessors exposing (fst, get, has, just, try)
import Actions
import Api.ApiData as ApiData
import Dict
import Extra.Accessors exposing (orElseT, where_)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Html.Lazy
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model, ProjectRecord, StepRecord, Table)
import Model.Lenses as Lenses exposing (currentProject)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepConfigEntry, StepType(..), derivation)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route
import Specs
import View.FileBrowser as FileBrowser
import View.Icons exposing (iconCustom)
import View.Lib exposing (viewPage, viewSearchBox)
import View.Table exposing (viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewStepRecordActions, viewTable, viewUploadProgress)


viewProject : Model -> ProjectRecord -> Html (Flow Model ())
viewProject model proj =
    let
        isReadOnly =
            Model.isReadOnlyRoute model

        mProjectSpec =
            Maybe.map2 Specs.projects
                (ApiData.toMaybe (Model.getPresets model))
                (ApiData.toMaybe (Model.getStepConfig model))
    in
    viewPage
        { header =
            [ Html.div [ Html.Attributes.class "project-header" ]
                [ Html.a [ Route.href (Route.fromPage Route.Home), Html.Attributes.class "back-btn" ] [ iconCustom True "arrow_back" [ Html.Attributes.class "back-icon" ] ]
                , Html.h2 [] [ Html.text proj.name ]
                , Html.viewIf (not isReadOnly) <|
                    Html.viewMaybe
                        (\spec ->
                            viewIconButtonWithTooltip "edit" True "Edit project" (Actions.toggleAddOrEditRecordForm spec proj.id)
                        )
                        mProjectSpec
                ]
            , viewSearchBox model
            ]
        , content =
            let
                projectEditForm =
                    let
                        mEditedProject =
                            try (Lenses.projects << Lenses.edited << just) model
                                |> Maybe.filter (.id >> (==) proj.id)
                    in
                    Html.viewIf (not isReadOnly)
                        (Maybe.map2 (\spec -> viewAddOrEditRecordForm model spec (get Lenses.projects model) Html.nothing)
                            mProjectSpec
                            mEditedProject
                            |> Maybe.withDefault Html.nothing
                        )

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
                        (projectEditForm
                            :: configErrors
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
            sections
        }


viewSection : Model -> String -> StepConfigEntry -> Table StepRecord -> Html (Flow Model ())
viewSection model sectionName entry steps =
    let
        spec =
            Specs.steps sectionName entry

        stepConfig_ =
            try (Lenses.stepConfig << ApiData.success) model
                |> Maybe.unwrap Dict.empty identity

        presentTypesKey =
            try (currentProject << ApiData.success << Lenses.tables) model
                |> Maybe.unwrap [] Dict.keys
                |> String.join ","

        projectIdKey =
            try Lenses.currentProjectId model
                |> Maybe.map String.fromInt
                |> Maybe.unwrap "" identity

        page =
            (Model.getRoute model).page
    in
    viewTable
        { model = model
        , spec = spec
        , table = steps
        , recordActionsPopover =
            \record ->
                Html.Lazy.lazy8 viewStepRecordActions
                    sectionName
                    entry
                    stepConfig_
                    presentTypesKey
                    projectIdKey
                    page
                    record
                    (record.id
                        |> Maybe.map (\id -> Dict.member id (Model.getUploadProgress model))
                        |> Maybe.unwrap False identity
                    )
        , alwaysVisibleRecordActions =
            \r ->
                case entry.stepType of
                    FileUpload _ ->
                        case r.id |> Maybe.andThen (\id -> Dict.get id (Model.getUploadProgress model) |> Maybe.map (Tuple.pair id)) of
                            Just ( stepId, progress ) ->
                                [ viewUploadProgress stepId progress ]

                            Nothing ->
                                []

                    Derivation _ _ ->
                        []

                    Download ->
                        []
        , directorySection = FileBrowser.viewDirectorySection model spec
        , srcFilesSection = FileBrowser.viewSrcFilesSection model entry.stepType spec
        , onRecordClick =
            \record ->
                record.id
                    |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
        }
