module View.Shadow exposing (viewProject)

import Accessors exposing (fst, get, has, just, snd, try)
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
import Model.Lenses as Lenses exposing (currentProject, currentProjectId, mCommit, route)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepConfigEntry, StepType(..), derivation)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route
import Specs
import View.FileBrowser as FileBrowser
import View.Icons exposing (iconCustom)
import View.Lib exposing (viewPage, viewSearchBox)
import View.Table exposing (viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewInlineIconButtonWithTooltip, viewQuickCreateButton, viewRunButton, viewStopButton, viewTable, viewUploadButton, viewUploadProgress)


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


type alias ComparisonChip =
    { severity : String
    , icon : String
    , label : String
    , explanation : String
    }


{- | The chip describing how the viewed revision's output relates to the reviewed
output. A matching out path and a missing reviewed output add no chip: a match
needs no comparison, and a missing reviewed output is already visible as the
output's status.
-}
comparisonChip : Model.ReviewComparison -> Maybe ComparisonChip
comparisonChip comparison =
    case comparison of
        Model.SameOutPath ->
            Nothing

        Model.SameContent ->
            Just (ComparisonChip "muted" "published_with_changes" "Same content" "The viewed revision builds a different store path with identical content. Updating the review is optional.")

        Model.ViewedOutputUnbuilt ->
            Just (ComparisonChip "muted" "pending" "Not built" "The viewed revision's output is not built, so it cannot be compared with the reviewed output. Build it to compare.")

        Model.DifferentContent ->
            Just (ComparisonChip "warning" "difference" "Differs" "The viewed output differs from the reviewed output. View the diff to review the changes; the review stays until you update it.")

        Model.ReviewedOutputUnbuilt ->
            Nothing


{- | A step's review controls: the review toggle, plus the chip describing how
the viewed revision's output relates to the reviewed output and the reviewed
revision it refers to. A step with neither a review toggle nor a review shows
nothing.
-}
viewReviewControls : Model -> TableSpec StepRecord -> Int -> StepRecord -> Maybe (Html (Flow Model ()))
viewReviewControls model spec stepId record =
    let
        indicator =
            record.reviewedRevision
                |> Maybe.map
                    (\reviewedRevision ->
                        viewReviewIndicator model spec stepId (Just reviewedRevision) (Maybe.withDefault ApiData.NotAsked record.reviewComparison)
                    )
                |> Maybe.withDefault []

        content =
            viewReviewActions spec record ++ indicator
    in
    if List.isEmpty content then
        Nothing

    else
        Just <|
            Html.span
                [ Html.Attributes.class "step-review"
                , Html.Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                ]
                content


{- | The chip describing how the viewed revision's output relates to the reviewed
output, together with the reviewed revision and any comparison actions.
-}
viewReviewIndicator : Model -> TableSpec StepRecord -> Int -> Maybe String -> ApiData Model.ReviewComparison -> List (Html (Flow Model ()))
viewReviewIndicator model spec stepId mReviewedRevision comparison =
    let
        whileChecking chip =
            { chip | explanation = "Checking the viewed revision's output. " ++ chip.explanation }

        failed error =
            ComparisonChip "danger" "error_outline" "Check failed" ("The review check failed: " ++ Http.errorMessage error)

        comparisonChipWhileLoading mPrevious =
            Maybe.andThen comparisonChip mPrevious |> Maybe.map whileChecking

        mComparisonChip =
            ApiData.foldVisible
                Nothing
                comparisonChipWhileLoading
                comparisonChip
                (Just << failed)
                comparison

        currentCommit =
            try (route << Route.page << Route.project << mCommit << just) model

        reviewedRevisionNote =
            mReviewedRevision
                |> Maybe.map (\reviewedRevision -> " Reviewed revision: " ++ shortRevision reviewedRevision ++ ".")
                |> Maybe.withDefault ""

        explanation chip =
            chip.explanation ++ reviewedRevisionNote ++ " Editing is locked. Remove the review to edit it."

        viewChip chip =
            Html.span
                [ Html.Attributes.class ("step-review-chip step-review-" ++ chip.severity)
                , Html.Attributes.tabindex 0
                , Html.Attributes.attribute "role" "note"
                , Html.Attributes.title (explanation chip)
                , Html.Attributes.attribute "aria-label" (chip.label ++ ". " ++ explanation chip)
                ]
                [ iconCustom False chip.icon [ Html.Attributes.attribute "aria-hidden" "true" ]
                , Html.text chip.label
                ]

        isDiffOpen =
            Maybe.map Tuple.first (Model.getOpenDiff model) == Just stepId

        diffUrl =
            Api.reviewDiffUrl stepId (Model.viewedRevision model)

        viewDiffToggle chip =
            Html.button
                [ Html.Attributes.class "step-review-diff"
                , Html.Attributes.title (explanation chip)
                , Html.Attributes.attribute "aria-expanded"
                    (if isDiffOpen then
                        "true"

                     else
                        "false"
                    )
                , Html.Attributes.attribute "aria-label"
                    ((if isDiffOpen then
                        "Hide the diff. "

                      else
                        "View the diff. "
                     )
                        ++ explanation chip
                    )
                , Html.Events.onClick (Actions.toggleDiffPanel stepId)
                ]
                [ iconCustom False "difference" [ Html.Attributes.attribute "aria-hidden" "true" ]
                , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "View diff" ]
                , iconCustom False
                    (if isDiffOpen then
                        "expand_less"

                     else
                        "expand_more"
                    )
                    [ Html.Attributes.attribute "aria-hidden" "true" ]
                ]

        viewDiffInNewTab =
            Html.a
                [ Html.Attributes.class "step-review-diff"
                , Html.Attributes.title "Open the diff in a new tab"
                , Html.Attributes.attribute "aria-label" "Open the diff in a new tab"
                , Html.Attributes.href diffUrl
                , Html.Attributes.target "_blank"
                , Html.Attributes.rel "noopener"
                ]
                [ iconCustom False "open_in_new" [ Html.Attributes.attribute "aria-hidden" "true" ] ]

        reviewedRevisionLink =
            mReviewedRevision
                |> Maybe.filter (\reviewedRevision -> currentCommit /= Just reviewedRevision)
                |> Maybe.andThen
                    (\reviewedRevision ->
                        try currentProjectId model
                            |> Maybe.map (viewReviewedRevisionLink stepId reviewedRevision)
                    )
                |> Maybe.withDefault Html.nothing

        comparisonControls =
            if ApiData.toMaybe comparison == Just Model.DifferentContent then
                mComparisonChip
                    |> Maybe.unwrap [] (\chip -> [ viewDiffToggle chip, viewDiffInNewTab ])

            else
                [ Html.viewMaybe viewChip mComparisonChip ]
    in
    (reviewedRevisionLink :: comparisonControls)
        ++ [ Html.viewIf (ApiData.toMaybe comparison == Just Model.ViewedOutputUnbuilt) (viewBuildViewedLink model spec stepId) ]


viewDiffSection : Model -> StepRecord -> Html (Flow Model ())
viewDiffSection model record =
    case ( record.id, Maybe.andThen ApiData.toMaybe record.reviewComparison ) of
        ( Just stepId, Just Model.DifferentContent ) ->
            Html.viewIf (Maybe.map Tuple.first (Model.getOpenDiff model) == Just stepId) <|
                let
                    frameId =
                        "step-diff-" ++ String.fromInt stepId
                in
                FileBrowser.viewHtmlFrame
                    { id = frameId
                    , src = Api.reviewDiffUrl stepId (Model.viewedRevision model)
                    , zoom = Actions.zoomIframeBy (Lenses.openDiff << just << snd) frameId
                    }

        _ ->
            Html.nothing


{- | Build the viewed revision so its output can be compared with the reviewed
output. The row itself stays at the reviewed revision.
-}
viewBuildViewedLink : Model -> TableSpec StepRecord -> Int -> Html (Flow Model ())
viewBuildViewedLink model spec stepId =
    if Dict.member stepId (Model.getPendingBuilds model) then
        Html.button
            [ Html.Attributes.class "step-review-build"
            , Html.Attributes.disabled True
            , Html.Attributes.title "Building this revision"
            , Html.Attributes.attribute "aria-label" "Building this revision"
            ]
            [ iconCustom True
                "progress_activity"
                [ Html.Attributes.class "step-review-build-spinner"
                , Html.Attributes.attribute "aria-hidden" "true"
                ]
            , Html.text "Building..."
            ]

    else
        let
            titleText =
                "Build the viewed revision's output to compare with the reviewed output"
        in
        Html.button
            [ Html.Attributes.class "step-review-build"
            , Html.Attributes.title titleText
            , Html.Attributes.attribute "aria-label" titleText
            , Html.Events.onClick (Actions.buildViewedRevision spec stepId)
            ]
            [ iconCustom False "build" [ Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "Build this revision" ]
            ]


shortRevision : String -> String
shortRevision =
    String.left 7


{- | The reviewed revision is the repository at that revision, browsed
read-only like any other past commit, with the reviewed step revealed.
-}
viewReviewedRevisionLink : Int -> String -> Int -> Html (Flow Model ())
viewReviewedRevisionLink stepId reviewedRevision projectId =
    let
        shortReviewedRevision =
            shortRevision reviewedRevision

        targetRoute =
            Route.fromPage
                (Route.Project
                    { projectId = projectId
                    , mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }
                    , mCommit = Just reviewedRevision
                    , mCompare = Nothing
                    }
                )

        titleText =
            "View the repository at the reviewed revision " ++ shortReviewedRevision ++ " (read-only)"
    in
    Html.a
        [ Html.Attributes.class "step-review-revision"
        , Html.Attributes.title titleText
        , Html.Attributes.attribute "aria-label" ("View reviewed revision of step " ++ String.fromInt stepId ++ " at revision " ++ shortReviewedRevision ++ " (read-only)")
        , Route.href targetRoute
        , Html.Events.preventDefaultOn "click"
            (Decode.map4
                (\ctrl meta shift alt ->
                    ( Actions.goToRoute targetRoute, not (ctrl || meta || shift || alt) )
                )
                (Decode.field "ctrlKey" Decode.bool)
                (Decode.field "metaKey" Decode.bool)
                (Decode.field "shiftKey" Decode.bool)
                (Decode.field "altKey" Decode.bool)
            )
        ]
        [ Html.span
            [ Html.Attributes.class "step-review-revision-hash"
            , Html.Attributes.style "text-decoration" "underline"
            ]
            [ Html.text shortReviewedRevision ]
        ]


{- | Review controls, shown next to the step id. A step can be reviewed once its
output is built; an existing review can advance when the viewed output still
matches it.
-}
viewReviewActions : TableSpec StepRecord -> StepRecord -> List (Html (Flow Model ()))
viewReviewActions spec r =
    case ( r.id, r.isUpdating ) of
        ( Just stepId, False ) ->
            case r.reviewComparison of
                Nothing ->
                    [ Html.viewIf (ApiData.toMaybe (TableSpec.getStatus spec r) == Just Model.StatusSuccess) <|
                        viewInlineIconButtonWithTooltip "fact_check" False "Review step" (Actions.reviewStep stepId)
                    ]

                Just comparison ->
                    [ Html.viewIf (ApiData.toMaybe comparison == Just Model.SameContent) <|
                        viewInlineIconButtonWithTooltip "published_with_changes" True "Update review" (Actions.reviewStep stepId)
                    , viewReviewToggle stepId
                    ]

        _ ->
            []


{- | The review toggle of a reviewed step, marked with a verified modifier so a
reviewed row shows its review at a glance.
-}
viewReviewToggle : Int -> Html (Flow Model ())
viewReviewToggle stepId =
    Html.span [ Html.Attributes.class "review-toggle" ]
        [ viewInlineIconButtonWithTooltip "fact_check" True "Remove review" (Actions.removeReview stepId)
        , iconCustom True
            "verified"
            [ Html.Attributes.class "review-toggle-modifier"
            , Html.Attributes.attribute "aria-hidden" "true"
            ]
        ]


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
        stepType =
            entry.stepType

        spec =
            Specs.steps sectionName entry

        isReadOnly =
            Model.isReadOnlyRoute model

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
                    [ r.id |> Maybe.andThen (\stepId -> viewReviewControls model spec stepId r)
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
                                            [ Html.viewIf (Maybe.isNothing r.reviewComparison) <|
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
                uploadActions ++ runActions ++ quickCreateActions
        , directorySection = FileBrowser.viewDirectorySection model spec
        , srcFilesSection = FileBrowser.viewSrcFilesSection model stepType spec
        , detailSection = viewDiffSection model
        , onRecordClick =
            \record ->
                record.id
                    |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
        , isOpen = \r -> TableSpec.getDirectoryView spec r |> Maybe.map .expanded |> Maybe.withDefault False
        }
