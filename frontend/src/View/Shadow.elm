module View.Shadow exposing (viewProject)

import Accessors exposing (fst, get, has, just, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData)
import Dict
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model, ProjectRecord, StepRecord, Table)
import Model.Lenses as Lenses exposing (currentProject, mCommit, route)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepConfigEntry, StepType(..), derivation)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route
import Specs
import View.FileBrowser as FileBrowser
import View.Icons exposing (iconCustom)
import View.Lib exposing (viewPage, viewSearchBox)
import View.Table exposing (viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewQuickCreateButton, viewRunButton, viewStopButton, viewTable, viewUploadButton, viewUploadProgress)


viewRunStop : TableSpec StepRecord -> StepRecord -> List (Html (Flow Model ()))
viewRunStop spec r =
    case r.id of
        Just id ->
            let
                status =
                    TableSpec.getStatus spec r

                isRunning =
                    status
                        |> ApiData.toMaybe
                        |> (==) (Just Model.StatusRunning)

                canRun =
                    case status of
                        ApiData.Loading _ ->
                            False

                        ApiData.Success Model.StatusSuccess ->
                            False

                        ApiData.Success Model.StatusRunning ->
                            False

                        _ ->
                            True
            in
            [ Html.viewIf canRun (viewRunButton "Run" (Actions.runStep spec id))
            , Html.viewIf isRunning (viewStopButton "Stop" (Actions.stopStep spec id))
            ]

        Nothing ->
            []


type alias ValidationChip =
    { severity : String
    , icon : String
    , label : String
    , explanation : String
    }


verdictChip : Model.StepValidation -> ValidationChip
verdictChip verdict =
    case verdict of
        Model.ValidationCurrent ->
            ValidationChip "muted" "verified" "Validated" "The current output matches the saved validation baseline."

        Model.ValidationIdentical ->
            ValidationChip "muted" "published_with_changes" "Matches" "The output contents still match the baseline. Update validation to pin the current revision."

        Model.ValidationUnbuilt ->
            ValidationChip "warning" "pending" "Needs rebuild" "This step changed since validation. Run it to compare the new output with the saved baseline."

        Model.ValidationDiffer ->
            ValidationChip "warning" "difference" "Differs" "The current output differs from the saved baseline. View the diff to review the changes."

        Model.ValidationMissing ->
            ValidationChip "warning" "cloud_off" "Baseline missing" "The validated output is no longer in the store. Rebuild the validated revision to compare, or unvalidate to start a new baseline."


viewValidationChip : Bool -> Int -> ApiData Model.StepValidation -> Html (Flow Model ())
viewValidationChip isReadOnly stepId validation =
    let
        pending =
            ValidationChip "muted" "verified" "Validated" "A validation baseline is saved for this step."

        whileChecking c =
            { c | explanation = "Checking current output. " ++ c.explanation }

        failed error =
            ValidationChip "danger" "error_outline" "Check failed" ("The validation check failed: " ++ Http.errorMessage error)

        chip =
            ApiData.foldVisible (whileChecking pending) (whileChecking << Maybe.unwrap pending verdictChip) verdictChip failed validation

        explanation =
            chip.explanation ++ " Editing is locked. Unvalidate this step in the current view to edit it."
    in
    Html.span
        [ Html.Attributes.class "step-validation"
        , Html.Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
        ]
        [ Html.span
            [ Html.Attributes.class ("step-validation-chip step-validation-" ++ chip.severity)
            , Html.Attributes.tabindex 0
            , Html.Attributes.attribute "role" "note"
            , Html.Attributes.title explanation
            , Html.Attributes.attribute "aria-label" (chip.label ++ ". " ++ explanation)
            ]
            [ iconCustom False chip.icon [ Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.text chip.label
            ]
        , Html.viewIf (not isReadOnly && ApiData.toMaybe validation == Just Model.ValidationDiffer) (viewDiffReportLink stepId)
        ]


viewDiffReportLink : Int -> Html (Flow Model ())
viewDiffReportLink stepId =
    Html.a
        [ Html.Attributes.class "step-validation-diff"
        , Html.Attributes.title "View diff (opens in a new tab)"
        , Html.Attributes.attribute "aria-label" ("View diff for step " ++ String.fromInt stepId ++ " (opens in a new tab)")
        , Html.Attributes.href (Api.stepDiffReportUrl stepId)
        , Html.Attributes.target "_blank"
        , Html.Attributes.rel "noopener"
        ]
        [ Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "View diff" ]
        , iconCustom False "open_in_new" [ Html.Attributes.attribute "aria-hidden" "true" ]
        ]


viewValidationActions : TableSpec StepRecord -> StepRecord -> List (Html (Flow Model ()))
viewValidationActions spec r =
    case ( r.id, r.isUpdating ) of
        ( Just stepId, False ) ->
            case r.validation of
                Nothing ->
                    [ Html.viewIf (ApiData.toMaybe (TableSpec.getStatus spec r) == Just Model.StatusSuccess) <|
                        viewIconButtonWithTooltip "verified" True "Validate step" (Actions.validateStep stepId)
                    ]

                Just validation ->
                    [ Html.viewIf (ApiData.toMaybe validation == Just Model.ValidationIdentical) <|
                        viewIconButtonWithTooltip "published_with_changes" True "Update validation" (Actions.validateStep stepId)
                    , viewIconButtonWithTooltip "verified_off" False "Unvalidate step" (Actions.unvalidateStep stepId)
                    ]

        _ ->
            []


viewProject : Model -> ProjectRecord -> Html (Flow Model ())
viewProject model proj =
    let
        mCommit_ =
            try (route << Route.page << Route.project << mCommit << just) model

        isReadOnly =
            Maybe.isJust mCommit_

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
        stepType =
            entry.stepType

        spec =
            Specs.steps sectionName entry

        isReadOnly =
            has (route << Route.page << Route.project << mCommit << just) model

        stepConfig_ =
            try (Lenses.stepConfig << ApiData.success) model
                |> Maybe.unwrap [] Dict.toList
    in
    viewTable
        { model = model
        , spec = spec
        , table = steps
        , alwaysVisibleRecordActions =
            \r ->
                Maybe.values
                    [ Maybe.map2 (viewValidationChip isReadOnly) r.id r.validation
                    , r.id
                        |> Maybe.andThen (\id -> Maybe.map (viewUploadProgress id) (Dict.get id (Model.getUploadProgress model)))
                    ]
        , specificRecordActions =
            \r ->
                let
                    runActions =
                        case stepType of
                            Derivation _ _ ->
                                viewRunStop spec r

                            Download ->
                                viewRunStop spec r

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
                                            [ Html.viewIf (Maybe.isNothing r.validation) <|
                                                Html.viewMaybe (viewUploadButton << Actions.uploadFiles spec (Maybe.withDefault [] types)) r.id
                                            ]

                                Derivation _ _ ->
                                    []

                                Download ->
                                    []

                    prefill argType =
                        let
                            wire allowedTypes toValue =
                                if Maybe.unwrap True (List.member r.type_) allowedTypes then
                                    Maybe.map toValue r.id

                                else
                                    Nothing
                        in
                        case argType of
                            TStep allowedTypes True ->
                                wire allowedTypes TStepValue

                            TList (TStep allowedTypes True) ->
                                wire allowedTypes (TListValue << List.singleton << TStepValue)

                            _ ->
                                Nothing

                    quickCreateActions =
                        if isReadOnly then
                            []

                        else
                            stepConfig_
                                |> List.filter (\( targetType, _ ) -> has (Lenses.currentTableOf targetType) model)
                                |> List.concatMap
                                    (\( targetType, targetEntry ) ->
                                        let
                                            targetSpec =
                                                Specs.steps targetType targetEntry
                                        in
                                        try (derivation << fst) targetEntry.stepType
                                            |> Maybe.unwrap [] Dict.toList
                                            |> List.filterMap
                                                (\( argName, arg ) ->
                                                    prefill arg.type_
                                                        |> Maybe.map
                                                            (\value ->
                                                                viewQuickCreateButton targetEntry.icon
                                                                    ("Create " ++ TableSpec.getDisplayName targetSpec)
                                                                    (Actions.addStepWithArg targetSpec argName value)
                                                            )
                                                )
                                    )
                in
                viewValidationActions spec r ++ uploadActions ++ runActions ++ quickCreateActions
        , directorySection = FileBrowser.viewDirectorySection model spec
        , srcFilesSection = FileBrowser.viewSrcFilesSection model stepType spec
        , onRecordClick =
            \record ->
                record.id
                    |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
        , isOpen = \r -> TableSpec.getDirectoryView spec r |> Maybe.map .expanded |> Maybe.withDefault False
        }
