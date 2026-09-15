module View.Shadow exposing (viewProject)

import Accessors exposing (get, has, just, snd, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData
import Basics.Extra exposing (flip)
import Dict
import Extra.Accessors exposing (where_)
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Html.Extra as Html
import Html.Lazy
import Iso8601
import Json.Decode as Decode
import Keyboard
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model, ProjectRecord, StepRecord, Table)
import Model.Lenses as Lenses exposing (currentProject, currentProjectId, isReadOnlyRoute, recordId)
import Model.Shadow exposing (StepConfigEntry)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route
import Specs
import Time exposing (Posix)
import Time.Distance
import View.FileBrowser as FileBrowser
import View.Icons exposing (iconCustom)
import View.Lib exposing (viewPage, viewSearchBox)
import View.Table exposing (routeCommit, viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewStepRecordActions, viewStepRecordStatus, viewTable, viewUploadProgress)


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
            Just (ComparisonChip "muted" "published_with_changes" "Same content" "This step's inputs have changed since review but the outcome is the same. No action needed.")

        Model.ViewedOutputUnbuilt ->
            Just (ComparisonChip "muted" "pending" "Not built" "This step's inputs have changed since review. You may build the latest version to check whether the outputs change too.")

        Model.DifferentContent ->
            Just (ComparisonChip "warning" "difference" "Differs" "This step's output changed since review. The reviewed version is still shown. Inspect the difference here.")

        -- A review whose output is gone is a question for the reviewer, not a
        -- row-level flag: the popover names it as the reason updates are off.
        Model.ReviewedOutputUnbuilt ->
            Nothing


{-| Why the review cannot be updated here. Only the states the row cannot speak
for are named: every comparison that carries a chip, and the failed check, are
already explained in the row this popover hangs under.
-}
blockedReason : ApiData.ApiData Model.ReviewComparison -> Maybe String
blockedReason comparison =
    let
        checking =
            Just "Checking the viewed revision's output."
    in
    ApiData.foldVisible
        checking
        (always checking)
        (\current ->
            case current of
                Model.ReviewedOutputUnbuilt ->
                    Just "The reviewed output is no longer in the store. Run the step to rebuild its reviewed revision, or remove the review."

                _ ->
                    Nothing
        )
        (always Nothing)
        comparison


viewReviewControls : Model -> TableSpec StepRecord -> Int -> StepRecord -> Maybe (Html (Flow Model ()))
viewReviewControls model spec stepId record =
    let
        -- Record mtimes are scoped to the viewed revision while the review is
        -- read at HEAD, so the mtime dates the review on the live view only:
        -- history has no commit that added the pin.
        mReviewedAt =
            if isReadOnlyRoute model then
                Nothing

            else
                record.lastModifiedAt

        content =
            viewReviewPopover model spec stepId record
                ++ Maybe.unwrap [] (viewReviewIndicator model spec stepId mReviewedAt) record.review
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


viewReviewIndicator : Model -> TableSpec StepRecord -> Int -> Maybe Posix -> Model.Review -> List (Html (Flow Model ()))
viewReviewIndicator model spec stepId mReviewedAt reviewed =
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

        viewChip chip =
            Html.span
                [ Html.Attributes.class ("step-review-chip step-review-" ++ chip.severity)
                , Html.Attributes.tabindex 0
                , Html.Attributes.attribute "role" "note"
                , Html.Attributes.title chip.explanation
                , Html.Attributes.attribute "aria-label" (chip.label ++ ". " ++ chip.explanation)
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
                , Html.Attributes.title chip.explanation
                , Html.Attributes.attribute "aria-expanded" diffExpanded
                , Html.Attributes.attribute "aria-label" (diffActionLabel ++ ". " ++ chip.explanation)
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

        reviewedRevision =
            viewReviewedRevision model mReviewedAt reviewed.revision

        comparisonControls =
            if ApiData.toMaybe reviewed.comparison == Just Model.DifferentContent then
                mComparisonChip
                    |> Maybe.unwrap [] (\chip -> [ viewDiffToggle chip, viewDiffInNewTab ])

            else
                [ Html.viewMaybe viewChip mComparisonChip ]
    in
    (reviewedRevision :: comparisonControls)
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
            , Html.Attributes.title "Building the latest version"
            , Html.Attributes.attribute "aria-label" "Building the latest version"
            ]
            [ iconCustom True "progress_activity" [ Html.Attributes.class "step-review-build-spinner", Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.text "Building..."
            ]

    else
        Html.button
            [ Html.Attributes.class "step-review-build"
            , Html.Attributes.title "Build the viewed version's output to compare with the reviewed output"
            , Html.Attributes.attribute "aria-label" "Build the viewed version's output to compare with the reviewed output"
            , Html.Events.onClick (Actions.buildViewedRevision spec stepId)
            ]
            [ iconCustom False "build" [ Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "Build latest version" ]
            ]


viewReviewedRevision : Model -> Maybe Posix -> String -> Html (Flow Model ())
viewReviewedRevision model mReviewedAt reviewedRevision =
    let
        shortRevision =
            String.left 7 reviewedRevision

        explanation =
            reviewedAtExplanation model mReviewedAt shortRevision
    in
    Html.span
        [ Html.Attributes.class "step-review-revision"
        , Html.Attributes.title explanation
        , Html.Attributes.attribute "aria-label" explanation
        ]
        [ Html.span [ Html.Attributes.class "step-review-revision-hash" ]
            [ Html.text shortRevision ]
        ]


{-| A reviewed step is locked, so the record's mtime is the review commit's
time: it is the only review timestamp the backend reports.
-}
reviewedAtExplanation : Model -> Maybe Posix -> String -> String
reviewedAtExplanation model mReviewedAt shortRevision =
    "Reviewed at revision "
        ++ shortRevision
        ++ Maybe.unwrap "."
            (\reviewedAt ->
                ", "
                    ++ Time.Distance.inWords reviewedAt (Model.getNow model)
                    ++ ", at "
                    ++ formatUtcMinute reviewedAt
                    ++ "."
            )
            mReviewedAt


{-| `Iso8601.fromTime` always renders `YYYY-MM-DDTHH:MM:SS.sssZ`, and every
other time in the app is UTC, so a reviewed-at time reads on the same clock.
-}
formatUtcMinute : Posix -> String
formatUtcMinute posix =
    Iso8601.fromTime posix
        |> String.left 16
        |> String.replace "T" " "


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
            isReadOnlyRoute model

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
            try currentProjectId model
                |> Maybe.unwrap "" String.fromInt

        uploads =
            Model.getUploadProgress model

        page =
            (Model.getRoute model).page

        currentRouteCommit =
            routeCommit page

        stepLogs =
            Model.getStepLogs model

        recordLog record =
            record.id
                |> Maybe.andThen (\id -> Dict.get (Model.stepLogKey id currentRouteCommit) stepLogs)
                |> Maybe.unwrap ApiData.NotAsked identity
    in
    viewTable
        { model = model
        , spec = spec
        , table = steps
        , recordStatusPill =
            \record ->
                Html.Lazy.lazy5 viewStepRecordStatus
                    sectionName
                    entry
                    page
                    (recordLog record)
                    record
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
                    (has (recordId << just << where_ (flip Dict.member uploads)) record)
        , alwaysVisibleRecordActions =
            \r ->
                Maybe.values
                    [ r.id |> Maybe.andThen (\stepId -> viewReviewControls model spec stepId r)
                    , r.id
                        |> Maybe.andThen (\id -> Maybe.map (viewUploadProgress id) (Dict.get id (Model.getUploadProgress model)))
                    ]
        , directorySection = FileBrowser.viewDirectorySection model spec
        , srcFilesSection = FileBrowser.viewSrcFilesSection model entry.stepType spec
        , detailSection = viewDiffSection model
        , onRecordClick =
            \record ->
                record.id
                    |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
        }
