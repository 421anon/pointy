module View.Shadow exposing (viewProject, viewUnfiled)

import Accessors exposing (get, just, snd, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData)
import Dict
import Extra.Accessors exposing (remkT)
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
import Model.Core as Model exposing (AddMode(..), Model, ProjectRecord, Status(..), StepRecord, blankProject)
import Model.Lenses as Lenses exposing (isReadOnlyRoute)
import Model.Lib
import Model.Selection
import Model.Shadow exposing (StepConfig, StepConfigEntry)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Organize
import Route
import Set
import Specs
import Time exposing (Posix)
import Time.Distance
import View.FileBrowser as FileBrowser
import View.Icons exposing (icon, iconCustom)
import View.Lib exposing (viewPage, viewSearchBox)
import View.Organize exposing (dropTargetAttrs)
import View.Table exposing (ListingRow, actionsPopoverId, stepFormReadOnly, viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewIngestProgress, viewListing, viewProjectExtraFormFields, viewRecordActionsPopover, viewRowActions, viewStepExtraFormFields, viewStepNoteField, viewStepRecordActions, viewStepRecordStatus, viewUploadProgress)


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

        Model.ReviewedOutputUnbuilt ->
            Nothing


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
                    , fitContent = True
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

        mPresets =
            ApiData.toMaybe (Model.getPresets model)

        mStepConfig =
            ApiData.toMaybe (Model.getStepConfig model)

        mProjectSpec =
            Maybe.map Specs.allProjects mPresets

        stepConfig =
            Maybe.withDefault Dict.empty mStepConfig

        scope =
            Model.ProjectListing (Maybe.withDefault Route.rootProjectId proj.id)

        listingRows =
            List.filterMap (listingRow model scope stepConfig (presentStepTypes model scope)) proj.children

        projectEditForm =
            Html.viewIf (not isReadOnly) <|
                Html.viewMaybe
                    (\spec ->
                        try (Lenses.projectForms << Lenses.edited << just) model
                            |> Maybe.filter (\editedProject -> editedProject.id == Nothing || editedProject.id == proj.id)
                            |> Maybe.map
                                (\editedProject ->
                                    viewAddOrEditRecordForm model
                                        (Model.listingScopeProjectId scope)
                                        spec
                                        (get Lenses.projectForms model)
                                        { extraFields = viewProjectExtraFormFields model (remkT (TableSpec.getLens spec))
                                        , noteInput = Html.nothing
                                        }
                                        Html.nothing
                                        editedProject
                                )
                            |> Maybe.withDefault Html.nothing
                    )
                    mProjectSpec

        configErrors =
            Html.viewIf (not (List.isEmpty proj.validationErrors)) <|
                Html.div [ Html.Attributes.class "project-config-error" ]
                    [ Html.ul []
                        (List.map (\msg -> Html.li [] [ Html.text msg ]) proj.validationErrors)
                    ]

        listing =
            case mStepConfig of
                Nothing ->
                    Html.span [ Html.Attributes.class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading step config..." ]

                Just _ ->
                    Html.div [ Html.Attributes.class "sections" ]
                        [ projectEditForm
                        , configErrors
                        , viewPendingStepForm model (Model.listingScopeProjectId scope) stepConfig
                        , viewListing
                            { model = model
                            , scope = scope
                            , stepConfig = stepConfig
                            , rows = listingRows
                            , header = listingHeader model proj
                            }
                        ]
    in
    viewPage
        { header =
            [ Html.div [ Html.Attributes.class "project-header" ]
                [ viewBreadcrumbs model proj
                , Html.viewIf (not isReadOnly) <|
                    Html.viewMaybe
                        (\spec ->
                            viewIconButtonWithTooltip "edit" True "Edit project" (Actions.toggleAddOrEditRecordForm spec proj.id)
                        )
                        mProjectSpec
                ]
            , viewSearchBox model
            ]
        , content = listing
        }


listingHeader : Model -> ProjectRecord -> List (Html (Flow Model ()))
listingHeader model proj =
    let
        isReadOnly =
            isReadOnlyRoute model

        parentId =
            Maybe.withDefault Route.rootProjectId proj.id

        mPresets =
            ApiData.toMaybe (Model.getPresets model)

        mStepConfig =
            ApiData.toMaybe (Model.getStepConfig model)

        hiddenLinks =
            List.filter .hidden proj.children

        newMenu =
            viewNewMenu model proj

        linkExistingButton =
            Html.viewIf (not isReadOnly) <|
                Html.viewMaybe
                    (\presets ->
                        Html.viewMaybe
                            (\stepConfig ->
                                viewIconButtonWithTooltip
                                    "link"
                                    True
                                    "Link existing"
                                    (Actions.toggleAddOrEditRecordForm (Specs.allProjects presets) Nothing
                                        |> Flow.seq (Flow.setAll (Lenses.projectForms << Lenses.addMode) LinkExisting)
                                    )
                            )
                            mStepConfig
                    )
                    mPresets

        unhideAllButton =
            Html.viewIf (not isReadOnly && not (List.isEmpty hiddenLinks)) <|
                Html.button
                    [ Html.Attributes.class "btn"
                    , Html.Events.onClick (Organize.unhideAll parentId (List.map Model.childRefOf hiddenLinks))
                    ]
                    [ Html.text "Unhide all" ]
    in
    [ newMenu, linkExistingButton, unhideAllButton ]


viewNewMenu : Model -> ProjectRecord -> Html (Flow Model ())
viewNewMenu model proj =
    let
        isReadOnly =
            isReadOnlyRoute model

        mPresets =
            ApiData.toMaybe (Model.getPresets model)

        mStepConfig =
            ApiData.toMaybe (Model.getStepConfig model)

        popoverId =
            "listing-new-menu"

        menuItem action children =
            Html.button
                [ Html.Attributes.class "listing-menu-item"
                , Html.Events.onClick action
                ]
                children

        newFolderAction =
            Html.viewMaybe
                (\presets ->
                    Html.viewMaybe
                        (\stepConfig ->
                            menuItem
                                (Actions.toggleAddOrEditRecordForm (Specs.allProjects presets) Nothing
                                    |> Flow.seq (Flow.setAll (Lenses.projectForms << Lenses.newDraft) (Just { blankProject | templateSource = proj.templateSource }))
                                )
                                [ iconCustom False "create_new_folder" []
                                , Html.text "New folder"
                                ]
                        )
                        mStepConfig
                )
                mPresets

        templateNames =
            Model.effectiveTemplates (Maybe.withDefault Dict.empty mPresets) proj.templateSource

        stepItem typeName entry =
            let
                spec =
                    Specs.steps typeName entry
            in
            menuItem
                (Actions.toggleAddOrEditRecordForm spec Nothing)
                [ Html.viewMaybe (\stepIcon -> iconCustom False stepIcon []) entry.icon
                , Html.text ("New " ++ TableSpec.getDisplayName spec)
                ]

        otherTemplates =
            Maybe.withDefault Dict.empty mStepConfig
                |> Dict.toList
                |> List.filter (\( typeName, _ ) -> not (List.member typeName templateNames))

        primaryItems =
            templateNames
                |> List.filterMap (\typeName -> Dict.get typeName (Maybe.withDefault Dict.empty mStepConfig) |> Maybe.map (stepItem typeName))

        otherSubmenu =
            Html.viewIf (not (List.isEmpty otherTemplates)) <|
                Html.details [ Html.Attributes.class "listing-menu-details" ]
                    [ Html.summary
                        [ Html.Attributes.class "listing-menu-item"
                        , Html.Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                        ]
                        [ iconCustom False "more_horiz" [], Html.text "Other types" ]
                    , Html.div [ Html.Attributes.class "listing-menu-submenu" ]
                        (List.map (\( typeName, entry ) -> stepItem typeName entry) otherTemplates)
                    ]
    in
    Html.viewIf (not isReadOnly) <|
        View.Organize.viewMenuPopover
            { popoverId = popoverId
            , wrapperClass = "listing-new-menu"
            , triggerAttrs =
                [ Html.Attributes.class "icon-btn listing-new-button"
                , Html.Attributes.title "New"
                , Html.Attributes.attribute "aria-label" "New"
                ]
            , triggerContent = [ icon True "add" ]
            , content = newFolderAction :: primaryItems ++ [ otherSubmenu ]
            }


viewPendingStepForm : Model -> Maybe Int -> StepConfig -> Html (Flow Model ())
viewPendingStepForm model mParentId stepConfig =
    let
        forms =
            stepConfig
                |> Dict.toList
                |> List.filterMap
                    (\( typeName, entry ) ->
                        let
                            spec =
                                Specs.steps typeName entry
                        in
                        try (Lenses.stepFormsAt typeName << Lenses.edited << just) model
                            |> Maybe.filter (\record -> record.id == Nothing)
                            |> Maybe.filter (\_ -> not (Maybe.withDefault False (try (Lenses.stepFormsAt typeName << Lenses.nameEditOnly) model)))
                            |> Maybe.map
                                (\record ->
                                    let
                                        readOnly =
                                            stepFormReadOnly model spec record
                                    in
                                    viewAddOrEditRecordForm model
                                        mParentId
                                        spec
                                        (try (Lenses.stepFormsAt typeName) model |> Maybe.withDefault Model.initialTable)
                                        { extraFields = [ viewStepExtraFormFields model readOnly typeName entry.stepType ]
                                        , noteInput = viewStepNoteField model readOnly typeName
                                        }
                                        (FileBrowser.viewSrcFilesSection model entry.stepType spec record)
                                        record
                                )
                    )
    in
    Html.viewIf (not (List.isEmpty forms)) <|
        Html.div [ Html.Attributes.class "listing-pending-form" ] forms


presentStepTypes : Model -> Model.ListingScope -> List String
presentStepTypes model scope =
    let
        steps_ =
            Model.getSteps model
    in
    Model.Selection.folderLinks model scope
        |> List.filter (\sibling -> sibling.kind == Model.StepChild)
        |> List.filterMap (\sibling -> Dict.get sibling.id steps_ |> Maybe.map .type_)
        |> List.foldl
            (\typeName acc ->
                if List.member typeName acc then
                    acc

                else
                    acc ++ [ typeName ]
            )
            []


listingRow : Model -> Model.ListingScope -> StepConfig -> List String -> Model.ChildLink -> Maybe ListingRow
listingRow model scope stepConfig presentTypes link =
    let
        mParentId =
            Model.listingScopeProjectId scope

        folderRow project =
            let
                spec =
                    folderSpec model

                mEdited =
                    try (Lenses.projectForms << Lenses.edited << just) model

                isEditing =
                    Maybe.andThen .id mEdited == Just link.id

                nameEditOnly =
                    Maybe.withDefault False (try (Lenses.projectForms << Lenses.nameEditOnly) model)

                readOnly =
                    isReadOnlyRoute model
            in
            { link = link
            , name = project.name
            , displayName = "Folder"
            , typeName = "folder"
            , typeIcon = Just "folder"
            , statusPill = View.Lib.viewRollupSummary model mParentId link.id
            , validationErrors = project.validationErrors
            , alwaysVisibleActions = []
            , actionsPopover =
                viewRecordActionsPopover
                    (actionsPopoverId link)
                    (viewRowActionsFor model mParentId link spec readOnly project)
            , mTime = project.lastModifiedAt
            , cTime = project.createdAt
            , statusSortKey = Maybe.withDefault 5 (View.Lib.rollupChildFor model mParentId link.id |> Maybe.map (.statuses >> Model.rollupStatusRank))
            , isUpdating = project.isUpdating
            , openRow =
                Just
                    (Actions.goToRoute
                        (Route.fromPage
                            (Route.projectPage
                                (case mParentId of
                                    Just _ ->
                                        Maybe.withDefault [] (try Lenses.currentProjectPath model) ++ [ link.id ]

                                    Nothing ->
                                        Model.Lib.canonicalPathTo model link.id
                                )
                                (Route.viewedCommit (Model.getRoute model).page)
                            )
                        )
                    )
            , editName =
                if readOnly then
                    Nothing

                else
                    Just (Actions.startInlineRecordNameEdit spec project)
            , inlineRename =
                if isEditing && nameEditOnly then
                    Maybe.map
                        (\edited ->
                            { value = edited.name
                            , onInput = Actions.editRecordName (remkT (TableSpec.getLens spec))
                            , onSubmit = TableSpec.getUpsertRecord spec
                            , onCancel = Actions.stopInlineRecordNameEdit spec
                            }
                        )
                        mEdited

                else
                    Nothing
            , expanders = []
            , form =
                if isEditing && not nameEditOnly && not readOnly then
                    Maybe.map
                        (\edited ->
                            viewAddOrEditRecordForm model
                                mParentId
                                spec
                                (get Lenses.projectForms model)
                                { extraFields = viewProjectExtraFormFields model (remkT (TableSpec.getLens spec))
                                , noteInput = Html.nothing
                                }
                                Html.nothing
                                edited
                        )
                        mEdited
                        |> Maybe.withDefault Html.nothing

                else
                    Html.nothing
            }

        stepRow step entry =
            let
                spec =
                    Specs.steps step.type_ entry

                recordLog =
                    step.id
                        |> Maybe.andThen (\id -> Dict.get (Model.stepLogKey id (Model.stepRevision model step)) (Model.getStepLogs model))
                        |> Maybe.unwrap ApiData.NotAsked identity

                uploads =
                    Model.getUploadProgress model

                runningIngestJobs =
                    Model.getIngestJobs model |> Dict.filter (\_ job -> job.state == Model.IngestRunning)

                pendingIngestSteps =
                    Model.getPendingIngestSteps model

                pendingStops =
                    Model.getPendingStops model

                scratchAvailable =
                    ApiData.unwrap False Maybe.isJust (Model.getScratchState model).root

                isIngesting =
                    Maybe.unwrap False
                        (\id ->
                            Dict.member id uploads
                                || Dict.member id runningIngestJobs
                                || Set.member id pendingIngestSteps
                        )
                        step.id

                reviewControls =
                    step.id |> Maybe.andThen (\stepId -> viewReviewControls model spec stepId step)

                uploadProgressView =
                    step.id |> Maybe.andThen (\id -> Maybe.map (viewUploadProgress id) (Dict.get id uploads))

                ingestProgressView =
                    step.id
                        |> Maybe.andThen
                            (\id ->
                                case Dict.get id runningIngestJobs of
                                    Just job ->
                                        Just (viewIngestProgress { done = job.done, total = job.total })

                                    Nothing ->
                                        if Set.member id pendingIngestSteps then
                                            Just (viewIngestProgress { done = Nothing, total = Nothing })

                                        else
                                            Nothing
                            )

                mEditedId =
                    try (Lenses.stepFormsAt step.type_ << Lenses.edited << just) model |> Maybe.andThen .id

                isEditing =
                    mEditedId == step.id && not (Maybe.withDefault False (try (Lenses.stepFormsAt step.type_ << Lenses.nameEditOnly) model))

                readOnly =
                    stepFormReadOnly model spec step

                formView =
                    Html.viewIf isEditing <|
                        Html.viewMaybe
                            (\edited ->
                                viewAddOrEditRecordForm model
                                    mParentId
                                    spec
                                    (try (Lenses.stepFormsAt step.type_) model |> Maybe.withDefault Model.initialTable)
                                    { extraFields = [ viewStepExtraFormFields model readOnly step.type_ entry.stepType ]
                                    , noteInput = viewStepNoteField model readOnly step.type_
                                    }
                                    (FileBrowser.viewSrcFilesSection model entry.stepType spec step)
                                    edited
                            )
                            (try (Lenses.stepFormsAt step.type_ << Lenses.edited << just) model)

                mInlineRename =
                    try (Lenses.stepFormsAt step.type_ << Lenses.edited << just) model
                        |> Maybe.filter (\edited -> edited.id == step.id)
                        |> Maybe.filter (always (Maybe.withDefault False (try (Lenses.stepFormsAt step.type_ << Lenses.nameEditOnly) model)))
                        |> Maybe.map
                            (\edited ->
                                { value = edited.name
                                , onInput = Actions.editRecordName (remkT (TableSpec.getLens spec))
                                , onSubmit = TableSpec.getUpsertRecord spec
                                , onCancel = Actions.stopInlineRecordNameEdit spec
                                }
                            )

                directoryViewOpen =
                    TableSpec.getDirectoryView spec step |> Maybe.map .expanded |> Maybe.withDefault False

                expanders =
                    [ Html.viewIf directoryViewOpen (FileBrowser.viewDirectorySection model spec step)
                    , viewDiffSection model step
                    ]
            in
            { link = link
            , name = step.name
            , displayName = TableSpec.getDisplayName spec
            , typeName = step.type_
            , typeIcon = entry.icon
            , statusPill =
                case mParentId of
                    Just _ ->
                        Html.Lazy.lazy5 viewStepRecordStatus
                            step.type_
                            entry
                            recordLog
                            isIngesting
                            step

                    Nothing ->
                        View.Lib.viewStatusUnknown
            , validationErrors = TableSpec.getValidationErrors spec step
            , alwaysVisibleActions =
                Maybe.values
                    [ reviewControls
                    , uploadProgressView
                    , ingestProgressView
                    ]
            , actionsPopover =
                viewStepRecordActionsFor
                    model
                    mParentId
                    link
                    step.type_
                    entry
                    stepConfig
                    presentTypes
                    (Model.getRoute model).page
                    step
                    { uploading = isIngesting
                    , scratchAvailable = scratchAvailable
                    , stopping = Maybe.unwrap False (\id -> Set.member id pendingStops) step.id
                    }
            , mTime = step.lastModifiedAt
            , cTime = step.createdAt
            , statusSortKey = statusSortRank (TableSpec.getStatus spec step)
            , isUpdating = step.isUpdating
            , openRow = step.id |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
            , editName =
                if readOnly then
                    Nothing

                else
                    Just (Actions.startInlineRecordNameEdit spec step)
            , inlineRename = mInlineRename
            , expanders = expanders
            , form = formView
            }
    in
    case link.kind of
        Model.ProjectChild ->
            Dict.get link.id (Lenses.projectsDict model)
                |> Maybe.map folderRow

        Model.StepChild ->
            let
                mStep =
                    Dict.get link.id (Model.getSteps model)
            in
            Maybe.map2 stepRow
                mStep
                (mStep
                    |> Maybe.map .type_
                    |> Maybe.withDefault ""
                    |> (\typeName -> Dict.get typeName stepConfig)
                )


folderSpec : Model -> TableSpec ProjectRecord
folderSpec model =
    let
        presets =
            ApiData.toMaybe (Model.getPresets model) |> Maybe.withDefault Dict.empty
    in
    Specs.allProjects presets


statusSortRank : ApiData Status -> Int
statusSortRank status =
    case ApiData.toMaybe status of
        Just StatusRunning ->
            0

        Just StatusSuccess ->
            1

        Just StatusBuiltNotCertified ->
            2

        Just StatusNotStarted ->
            3

        Just _ ->
            4

        Nothing ->
            5


viewBreadcrumbs : Model -> ProjectRecord -> Html (Flow Model ())
viewBreadcrumbs model proj =
    let
        projectPath_ =
            try Lenses.currentProjectPath model |> Maybe.withDefault []

        mCommit_ =
            Route.viewedCommit (Model.getRoute model).page

        projectsById =
            Lenses.projectsDict model

        editable =
            Model.Selection.listingEditable model

        projectName projectId_ =
            if Just projectId_ == proj.id then
                proj.name

            else
                Dict.get projectId_ projectsById
                    |> Maybe.map .name
                    |> Maybe.withDefault ("#" ++ String.fromInt projectId_)

        crumb pathPrefix =
            Html.a
                ([ Route.href (Route.fromPage (Route.projectPage pathPrefix mCommit_))
                 , Html.Attributes.class "project-breadcrumb"
                 ]
                    ++ (if editable then
                            dropTargetAttrs model (Route.pathProjectId pathPrefix)

                        else
                            []
                       )
                )
                [ Html.text (projectName (Route.pathProjectId pathPrefix)) ]

        ancestorPaths =
            List.range 0 (List.length projectPath_ - 1)
                |> List.map (\depth -> List.take depth projectPath_)

        currentId =
            Route.pathProjectId projectPath_

        currentParentId =
            Route.pathProjectId (List.take (List.length projectPath_ - 1) projectPath_)

        currentRef =
            { kind = Model.ProjectChild, id = currentId }
    in
    Html.nav [ Html.Attributes.class "project-breadcrumbs" ]
        (List.concatMap (\pathPrefix -> [ crumb pathPrefix, iconCustom True "chevron_right" [ Html.Attributes.class "project-breadcrumb-separator" ] ]) ancestorPaths
            ++ [ Html.h2 []
                    [ crumb projectPath_
                    , View.Lib.viewAlsoInButton "also-in-breadcrumb" model (Just currentParentId) currentRef
                    ]
               ]
        )


viewUnfiled : Model -> Html (Flow Model ())
viewUnfiled model =
    let
        stepConfig =
            ApiData.toMaybe (Model.getStepConfig model)

        refs =
            Model.unfiledRefs (Model.getUnfiledMembership model) (Model.getSteps model)

        listing =
            case stepConfig of
                Nothing ->
                    Html.span [ Html.Attributes.class "shimmer-text shimmer-text--high-contrast" ] [ Html.text "Loading step config..." ]

                Just stepConfig_ ->
                    Html.div [ Html.Attributes.class "sections" ]
                        [ viewListing
                            { model = model
                            , scope = Model.UnfiledListing
                            , stepConfig = stepConfig_
                            , rows =
                                List.filterMap
                                    (listingRow model Model.UnfiledListing stepConfig_ (presentStepTypes model Model.UnfiledListing)
                                        << Model.childLinkOf
                                    )
                                    refs
                            , header = []
                            }
                        ]
    in
    viewPage
        { header =
            [ Html.div [ Html.Attributes.class "project-header" ]
                [ Html.h2 [] [ Html.text "Unfiled" ]
                , Html.span [ Html.Attributes.class "listing-header-count" ] [ Html.text ("(" ++ String.fromInt (List.length refs) ++ ")") ]
                ]
            , viewSearchBox model
            ]
        , content = listing
        }


viewRowActionsFor : Model -> Maybe Int -> Model.ChildLink -> TableSpec (Model.BaseRecord a) -> Bool -> Model.BaseRecord a -> List (Html (Flow Model ()))
viewRowActionsFor model mParentId link spec isReadOnly record =
    case mParentId of
        Just parentId ->
            viewRowActions parentId link spec isReadOnly record

        Nothing ->
            unfiledRowActions model link spec record


viewStepRecordActionsFor : Model -> Maybe Int -> Model.ChildLink -> String -> StepConfigEntry -> StepConfig -> List String -> Route.Page -> StepRecord -> { uploading : Bool, scratchAvailable : Bool, stopping : Bool } -> Html (Flow Model ())
viewStepRecordActionsFor model mParentId link name entry stepConfig presentTypes page record flags =
    case mParentId of
        Just parentId ->
            viewStepRecordActions parentId link name entry stepConfig presentTypes page record flags

        Nothing ->
            viewRecordActionsPopover
                (actionsPopoverId link)
                (unfiledRowActions model link (Specs.steps name entry) record)


unfiledRowActions : Model -> Model.ChildLink -> TableSpec (Model.BaseRecord a) -> Model.BaseRecord a -> List (Html (Flow Model ()))
unfiledRowActions model link spec record =
    let
        ref =
            Model.childRefOf link

        isReadOnly =
            isReadOnlyRoute model
    in
    [ Html.viewIf (not isReadOnly && Maybe.isJust record.id) <|
        viewIconButtonWithTooltip "drive_file_move" True "Link to..." (Organize.openOrganizeDialogFor Model.OrganizeLinkTo Model.UnfiledListing [ ref ])
    , Html.viewIf (Maybe.isJust record.id) <|
        viewIconButtonWithTooltip "edit" True "Edit" (Actions.toggleAddOrEditRecordForm spec record.id)
    , Html.viewIf (not isReadOnly && Maybe.isJust record.id) <|
        viewIconButtonWithTooltip "delete"
            False
            "Delete permanently"
            (Organize.openOrganizeDialogFor Model.OrganizeDelete Model.UnfiledListing [ ref ])
    ]
