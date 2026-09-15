module View.Shadow exposing (viewProject)

import Accessors exposing (fst, get, has, just, snd, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData
import Dict
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import Keyboard
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


type alias ComparisonChip =
    { severity : String
    , icon : String
    , label : String
    , explanation : String
    }


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
            Just (ComparisonChip "warning" "difference" "Differs" "The viewed output differs from the reviewed output. View the diff, then remove the review to review the changed output.")

        Model.ReviewedOutputUnbuilt ->
            Just (ComparisonChip "warning" "warning" "Reviewed output missing" "The reviewed output is no longer in the store. Run the step to rebuild its reviewed revision, or remove the review.")


blockedReason : ApiData.ApiData Model.ReviewComparison -> Maybe String
blockedReason comparison =
    let
        checking =
            Just "Checking the viewed revision's output."
    in
    ApiData.foldVisible checking (always checking) (Maybe.map .explanation << comparisonChip) (always (Just "The review check failed, so the review cannot be updated.")) comparison


viewReviewControls : Model -> TableSpec StepRecord -> Int -> StepRecord -> Maybe (Html (Flow Model ()))
viewReviewControls model spec stepId record =
    let
        content =
            viewReviewPopover model spec stepId record
                ++ Maybe.unwrap [] (viewReviewIndicator model spec stepId) record.review
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


viewReviewIndicator : Model -> TableSpec StepRecord -> Int -> Model.Review -> List (Html (Flow Model ()))
viewReviewIndicator model spec stepId reviewed =
    let
        mComparisonChip =
            ApiData.foldVisible
                Nothing
                (\mPrevious ->
                    Maybe.andThen comparisonChip mPrevious
                        |> Maybe.map (\chip -> { chip | explanation = "Checking the viewed revision's output. " ++ chip.explanation })
                )
                comparisonChip
                (\error -> Just (ComparisonChip "danger" "error_outline" "Check failed" ("The review check failed: " ++ Http.errorMessage error)))
                reviewed.comparison

        explanation chip =
            chip.explanation ++ " Reviewed revision: " ++ String.left 7 reviewed.revision ++ ". Editing is locked. Remove the review to edit it."

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

        ( diffExpanded, diffActionLabel, diffChevron ) =
            if Maybe.map Tuple.first (Model.getOpenDiff model) == Just stepId then
                ( "true", "Hide diff", "expand_less" )

            else
                ( "false", "View diff", "expand_more" )

        viewDiffToggle chip =
            Html.button
                [ Html.Attributes.class "step-review-diff"
                , Html.Attributes.title (explanation chip)
                , Html.Attributes.attribute "aria-expanded" diffExpanded
                , Html.Attributes.attribute "aria-label" (diffActionLabel ++ ". " ++ explanation chip)
                , Html.Events.onClick (Actions.toggleDiffPanel stepId)
                ]
                [ iconCustom False "difference" [ Html.Attributes.attribute "aria-hidden" "true" ]
                , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text diffActionLabel ]
                , iconCustom False diffChevron [ Html.Attributes.attribute "aria-hidden" "true" ]
                ]

        viewDiffInNewTab =
            Html.a
                [ Html.Attributes.class "step-review-diff"
                , Html.Attributes.title "Open the diff in a new tab"
                , Html.Attributes.attribute "aria-label" "Open the diff in a new tab"
                , Html.Attributes.href (Api.reviewDiffUrl stepId (Model.viewedRevision model))
                , Html.Attributes.target "_blank"
                , Html.Attributes.rel "noopener"
                ]
                [ iconCustom False "open_in_new" [ Html.Attributes.attribute "aria-hidden" "true" ] ]

        reviewedRevisionLink =
            Html.viewIf (try (route << Route.page << Route.project << mCommit << just) model /= Just reviewed.revision) <|
                Html.viewMaybe (viewReviewedRevisionLink stepId reviewed.revision) (try currentProjectId model)

        comparisonControls =
            if ApiData.toMaybe reviewed.comparison == Just Model.DifferentContent then
                mComparisonChip
                    |> Maybe.unwrap [] (\chip -> [ viewDiffToggle chip, viewDiffInNewTab ])

            else
                [ Html.viewMaybe viewChip mComparisonChip ]
    in
    (reviewedRevisionLink :: comparisonControls)
        ++ [ Html.viewIf (ApiData.toMaybe reviewed.comparison == Just Model.ViewedOutputUnbuilt) (viewBuildViewedLink model spec stepId) ]


viewDiffSection : Model -> StepRecord -> Html (Flow Model ())
viewDiffSection model record =
    case ( record.id, record.review |> Maybe.andThen (.comparison >> ApiData.toMaybe) ) of
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


viewBuildViewedLink : Model -> TableSpec StepRecord -> Int -> Html (Flow Model ())
viewBuildViewedLink model spec stepId =
    if Dict.member stepId (Model.getPendingBuilds model) then
        Html.button
            [ Html.Attributes.class "step-review-build"
            , Html.Attributes.disabled True
            , Html.Attributes.title "Building this revision"
            , Html.Attributes.attribute "aria-label" "Building this revision"
            ]
            [ iconCustom True "progress_activity" [ Html.Attributes.class "step-review-build-spinner", Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.text "Building..."
            ]

    else
        Html.button
            [ Html.Attributes.class "step-review-build"
            , Html.Attributes.title "Build the viewed revision's output to compare with the reviewed output"
            , Html.Attributes.attribute "aria-label" "Build the viewed revision's output to compare with the reviewed output"
            , Html.Events.onClick (Actions.buildViewedRevision spec stepId)
            ]
            [ iconCustom False "build" [ Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "Build this revision" ]
            ]


viewReviewedRevisionLink : Int -> String -> Int -> Html (Flow Model ())
viewReviewedRevisionLink stepId reviewedRevision projectId =
    let
        shortReviewedRevision =
            String.left 7 reviewedRevision

        targetRoute =
            Route.fromPage
                (Route.Project
                    { projectId = projectId
                    , mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }
                    , mCommit = Just reviewedRevision
                    , mCompare = Nothing
                    }
                )
    in
    Html.a
        [ Html.Attributes.class "step-review-revision"
        , Html.Attributes.title ("View the repository at the reviewed revision " ++ shortReviewedRevision ++ " (read-only)")
        , Html.Attributes.attribute "aria-label" ("View reviewed revision of step " ++ String.fromInt stepId ++ " at revision " ++ shortReviewedRevision ++ " (read-only)")
        , Route.href targetRoute
        ]
        [ Html.span [ Html.Attributes.class "step-review-revision-hash", Html.Attributes.style "text-decoration" "underline" ]
            [ Html.text shortReviewedRevision ]
        ]


viewReviewPopover : Model -> TableSpec StepRecord -> Int -> StepRecord -> List (Html (Flow Model ()))
viewReviewPopover model spec stepId record =
    let
        isReviewed =
            Maybe.isJust record.review

        isBuilt =
            ApiData.toMaybe (TableSpec.getStatus spec record) == Just Model.StatusSuccess

        canRecord =
            Maybe.unwrap isBuilt (\reviewed -> List.member (ApiData.toMaybe reviewed.comparison) [ Just Model.SameContent, Just Model.SameOutPath ]) record.review

        draft =
            Model.getReviewDraft model
                |> Maybe.filter (.stepId >> (==) stepId)
                |> Maybe.withDefault { stepId = stepId, reviewedBy = Maybe.unwrap "" .reviewedBy record.review, comments = Maybe.unwrap "" .comments record.review }

        popoverId =
            "step-review-popover-" ++ TableSpec.getName spec ++ "-" ++ String.fromInt stepId

        ( title, submitLabel ) =
            if isReviewed then
                ( "Review of step " ++ String.fromInt stepId, "Update review" )

            else
                ( "Review step " ++ String.fromInt stepId, "Review step" )

        trigger =
            Html.button
                [ Html.Attributes.class "icon-btn icon-btn-inline"
                , Html.Attributes.title title
                , Html.Attributes.attribute "aria-label" title
                , Html.Attributes.attribute "popovertarget" popoverId
                , Html.Attributes.style "anchor-name" ("--anchor-" ++ popoverId)
                ]
                [ iconCustom isReviewed "fact_check" []
                , Html.span [ Html.Attributes.class "icon-btn-text" ] [ Html.text title ]
                ]

        field labelText control =
            Html.label [ Html.Attributes.class "step-review-field" ]
                [ Html.span [] [ Html.text labelText ], control ]

        submit =
            Flow.when (canRecord && String.trim draft.reviewedBy /= "")
                (Actions.hidePopover popoverId |> Flow.seq (Actions.reviewStep draft))

        blockedNote =
            Html.viewMaybe (\reason -> Html.p [ Html.Attributes.class "step-review-note" ] [ Html.text reason ])
                (Maybe.andThen (.comparison >> blockedReason) (Maybe.filter (always (not canRecord)) record.review))

        popover =
            Html.div
                [ Html.Attributes.class "step-review-popover"
                , Html.Attributes.id popoverId
                , Html.Attributes.attribute "popover" "auto"
                , Html.Attributes.style "position-anchor" ("--anchor-" ++ popoverId)
                ]
                [ Html.div [ Html.Attributes.class "step-review-popover-header" ]
                    [ Html.strong [] [ Html.text title ]
                    , Html.button
                        [ Html.Attributes.class "icon-btn"
                        , Html.Attributes.title "Close"
                        , Html.Events.onClick (Actions.hidePopover popoverId)
                        ]
                        [ iconCustom True "close" [] ]
                    ]
                , Html.div [ Html.Attributes.class "step-review-popover-body" ]
                    [ blockedNote
                    , field "Reviewed by" <|
                        Html.input
                            [ Html.Attributes.class "step-review-input"
                            , Html.Attributes.type_ "text"
                            , Html.Attributes.value draft.reviewedBy
                            , Html.Attributes.readonly (not canRecord)
                            , Html.Events.onInput (\value -> Actions.setReviewDraft { draft | reviewedBy = value })
                            , Html.Events.on "keydown" (Keyboard.decodeCombinations [ ( Keyboard.enter, Decode.succeed submit ) ])
                            ]
                            []
                    , field "Comments" <|
                        Html.textarea
                            [ Html.Attributes.class "step-review-comments"
                            , Html.Attributes.placeholder "Optional"
                            , Html.Attributes.rows 4
                            , Html.Attributes.value draft.comments
                            , Html.Attributes.readonly (not canRecord)
                            , Html.Events.onInput (\value -> Actions.setReviewDraft { draft | comments = value })
                            ]
                            []
                    , Html.div [ Html.Attributes.class "step-review-popover-actions" ]
                        [ Html.viewIf canRecord <|
                            Html.button
                                [ Html.Attributes.class "btn"
                                , Html.Attributes.disabled (String.trim draft.reviewedBy == "")
                                , Html.Events.onClick submit
                                ]
                                [ Html.text submitLabel ]
                        , Html.viewIf isReviewed <|
                            Html.button
                                [ Html.Attributes.class "btn btn-danger"
                                , Html.Events.onClick (Actions.hidePopover popoverId |> Flow.seq (Actions.removeReview stepId))
                                ]
                                [ Html.text "Remove review" ]
                        ]
                    ]
                ]
    in
    if not (isReviewed || isBuilt) then
        []

    else
        [ trigger, popover ]


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
                                            [ Html.viewIf (Maybe.isNothing r.review) <|
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
