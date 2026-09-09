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


type alias PinChip =
    { severity : String
    , icon : String
    , label : String
    , explanation : String
    }


{- | The chip describing how the latest revision's output relates to the pin.
The pin itself is represented by the always-present "Pinned" badge, so a
current pin and a missing baseline add no chip: a current pin needs no
comparison, and a missing baseline is already visible as the pinned output's
status.
-}
pinChip : Model.PinVerdict -> Maybe PinChip
pinChip verdict =
    case verdict of
        Model.PinCurrent ->
            Nothing

        Model.PinIdentical ->
            Just (PinChip "muted" "published_with_changes" "Matches" "The latest output still matches the pinned version, but at a newer revision. Updating the pin is optional.")

        Model.PinUpdatable ->
            Just (PinChip "muted" "pending" "Updatable" "Dependencies changed since the step was pinned, so the latest revision's output is not built. Build it to compare with the pinned version.")

        Model.PinDiffer ->
            Just (PinChip "warning" "difference" "Differs" "The latest output differs from the pinned version. View the diff to review the changes; the pin stays until you update it.")

        Model.PinMissing ->
            Nothing


{- | A step's pin controls: the pin toggle, plus the chip describing how the
latest output relates to the pin and the pinned revision it refers to. A step
with neither a pin toggle nor a pin shows nothing.
-}
viewPinControls : Model -> TableSpec StepRecord -> Bool -> Int -> StepRecord -> Maybe (Html (Flow Model ()))
viewPinControls model spec isReadOnly stepId record =
    let
        indicator =
            record.pinRevision
                |> Maybe.map
                    (\pin ->
                        viewPinIndicator model spec isReadOnly stepId (Just pin) (Maybe.withDefault ApiData.NotAsked record.pinVerdict)
                    )
                |> Maybe.withDefault []

        content =
            viewPinActions spec record ++ indicator
    in
    if List.isEmpty content then
        Nothing

    else
        Just <|
            Html.span
                [ Html.Attributes.class "step-pin"
                , Html.Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                ]
                content


{- | The chip describing how the latest output relates to the pin, together
with the pinned revision and any comparison actions.
-}
viewPinIndicator : Model -> TableSpec StepRecord -> Bool -> Int -> Maybe String -> ApiData Model.PinVerdict -> List (Html (Flow Model ()))
viewPinIndicator model spec isReadOnly stepId mPin verdict =
    let
        pinnedChip =
            PinChip "muted"
                "verified"
                "Pinned"
                (if ApiData.toMaybe verdict == Just Model.PinCurrent then
                    "The step is shown at its pinned revision, and the latest revision's output is unchanged."

                 else
                    "This step is pinned."
                )

        whileChecking chip =
            { chip | explanation = "Checking the latest output. " ++ chip.explanation }

        failed error =
            PinChip "danger" "error_outline" "Check failed" ("The pin check failed: " ++ Http.errorMessage error)

        verdictChipWhileLoading mPrevious =
            Maybe.andThen pinChip mPrevious |> Maybe.map whileChecking

        mVerdictChip =
            ApiData.foldVisible
                Nothing
                verdictChipWhileLoading
                pinChip
                (Just << failed)
                verdict

        currentCommit =
            try (route << Route.page << Route.project << mCommit << just) model

        pinNote =
            mPin
                |> Maybe.map (\pin -> " Pinned revision: " ++ shortRevision pin ++ ".")
                |> Maybe.withDefault ""

        explanation chip =
            chip.explanation ++ pinNote ++ " Editing is locked. Unpin this step in the current view to edit it."

        viewChip chip =
            Html.span
                [ Html.Attributes.class ("step-pin-chip step-pin-" ++ chip.severity)
                , Html.Attributes.tabindex 0
                , Html.Attributes.attribute "role" "note"
                , Html.Attributes.title (explanation chip)
                , Html.Attributes.attribute "aria-label" (chip.label ++ ". " ++ explanation chip)
                ]
                [ iconCustom False chip.icon [ Html.Attributes.attribute "aria-hidden" "true" ]
                , Html.text chip.label
                ]

        viewDiffButton chip =
            Html.a
                [ Html.Attributes.class "step-pin-diff"
                , Html.Attributes.title (explanation chip ++ " Opens in a new tab.")
                , Html.Attributes.attribute "aria-label" ("Updated. " ++ explanation chip ++ " Opens in a new tab.")
                , Html.Attributes.href (Api.stepDiffReportUrl stepId)
                , Html.Attributes.target "_blank"
                , Html.Attributes.rel "noopener"
                ]
                [ iconCustom False "pages" [ Html.Attributes.attribute "aria-hidden" "true" ]
                , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "Updated" ]
                , iconCustom False "open_in_new" [ Html.Attributes.attribute "aria-hidden" "true" ]
                ]

        pinnedVersionLink =
            mPin
                |> Maybe.filter (\pin -> currentCommit /= Just pin)
                |> Maybe.andThen
                    (\pin ->
                        try currentProjectId model
                            |> Maybe.map (viewPinnedVersionLink stepId pin)
                    )
                |> Maybe.withDefault Html.nothing

        isDiffer =
            ApiData.toMaybe verdict == Just Model.PinDiffer

        chipOrDiffButton =
            if isDiffer && not isReadOnly then
                Html.viewMaybe viewDiffButton mVerdictChip

            else
                Html.viewMaybe viewChip mVerdictChip
    in
    [ viewChip pinnedChip
    , pinnedVersionLink
    , chipOrDiffButton
    , Html.viewIf (ApiData.toMaybe verdict == Just Model.PinUpdatable) (viewBuildLatestLink model spec stepId)
    ]


{- | Build the latest revision so its output can be compared with the pinned
version. The row itself stays at the pinned revision.
-}
viewBuildLatestLink : Model -> TableSpec StepRecord -> Int -> Html (Flow Model ())
viewBuildLatestLink model spec stepId =
    if Dict.member stepId (Model.getPendingBuilds model) then
        Html.button
            [ Html.Attributes.class "step-pin-build"
            , Html.Attributes.disabled True
            , Html.Attributes.title "Building the latest revision"
            , Html.Attributes.attribute "aria-label" "Building the latest revision"
            ]
            [ iconCustom True
                "progress_activity"
                [ Html.Attributes.class "step-pin-build-spinner"
                , Html.Attributes.attribute "aria-hidden" "true"
                ]
            , Html.text "Building..."
            ]

    else
        let
            titleText =
                "Build the latest revision's output to compare with the pinned version"
        in
        Html.button
            [ Html.Attributes.class "step-pin-build"
            , Html.Attributes.title titleText
            , Html.Attributes.attribute "aria-label" titleText
            , Html.Events.onClick (Actions.buildLatest spec stepId (Model.viewedRevision model))
            ]
            [ iconCustom False "build" [ Html.Attributes.attribute "aria-hidden" "true" ]
            , Html.span [ Html.Attributes.style "text-decoration" "underline" ] [ Html.text "Build latest" ]
            ]


shortRevision : String -> String
shortRevision =
    String.left 7


{- | The pinned version is the repository at the pinned revision, browsed
read-only like any other past commit, with the pinned step revealed.
-}
viewPinnedVersionLink : Int -> String -> Int -> Html (Flow Model ())
viewPinnedVersionLink stepId pin projectId =
    let
        shortPin =
            shortRevision pin

        targetRoute =
            Route.fromPage
                (Route.Project
                    { projectId = projectId
                    , mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }
                    , mCommit = Just pin
                    , mCompare = Nothing
                    }
                )

        titleText =
            "View the repository at the pinned revision " ++ shortPin ++ " (read-only)"
    in
    Html.a
        [ Html.Attributes.class "step-pin-revision"
        , Html.Attributes.title titleText
        , Html.Attributes.attribute "aria-label" ("View pinned version of step " ++ String.fromInt stepId ++ " at revision " ++ shortPin ++ " (read-only)")
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
            [ Html.Attributes.class "step-pin-revision-hash"
            , Html.Attributes.style "text-decoration" "underline"
            ]
            [ Html.text shortPin ]
        ]


{- | Pin controls, shown next to the step id. A step can be pinned once its
output is built; an existing pin can advance when the latest output still
matches it.
-}
viewPinActions : TableSpec StepRecord -> StepRecord -> List (Html (Flow Model ()))
viewPinActions spec r =
    case ( r.id, r.isUpdating ) of
        ( Just stepId, False ) ->
            case r.pinVerdict of
                Nothing ->
                    [ Html.viewIf (ApiData.toMaybe (TableSpec.getStatus spec r) == Just Model.StatusSuccess) <|
                        viewInlineIconButtonWithTooltip "push_pin" False "Pin step" (Actions.pinStep stepId)
                    ]

                Just verdict ->
                    [ Html.viewIf (ApiData.toMaybe verdict == Just Model.PinIdentical) <|
                        viewInlineIconButtonWithTooltip "published_with_changes" True "Update pin" (Actions.pinStep stepId)
                    , viewInlineIconButtonWithTooltip "push_pin" True "Unpin step" (Actions.unpinStep stepId)
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
                    [ r.id |> Maybe.andThen (\stepId -> viewPinControls model spec isReadOnly stepId r)
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
                                            [ Html.viewIf (Maybe.isNothing r.pinVerdict) <|
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
        , onRecordClick =
            \record ->
                record.id
                    |> Maybe.map (\id -> Actions.toggleOutputEntry id Nothing [] |> Flow.map (always ()))
        , isOpen = \r -> TableSpec.getDirectoryView spec r |> Maybe.map .expanded |> Maybe.withDefault False
        }
