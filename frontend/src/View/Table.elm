module View.Table exposing (actionsPopoverId, routeCommit, viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewQuickCreateButton, viewRecordActions, viewRecordActionsPopover, viewRunButton, viewStepRecordActions, viewStepRecordStatus, viewStopButton, viewTable, viewUploadButton, viewUploadProgress)

import Accessors exposing (all, each, has, just, key, lens, over, set, try)
import Actions
import Ansi.Log as AnsiLog
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Components.Combobox as Combobox
import Components.Markdown as Markdown
import Components.Select as Select
import Dict
import Extra.Accessors exposing (by, where_)
import Extra.Decode as Decode
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Iso8601
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Keyboard
import Lib.StringColor exposing (stringToColor)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, Model, Status(..), StepRecord, Table, TableTag(..), TemplateSource(..), UploadProgress, dndSystem, getSortKey)
import Model.Lenses as Lenses exposing (allEntities, argSelectStates, args, currentProject, currentProjectId, currentTableOf, dndAffected, edited, mCommit, note, presetSelect, projectStepRecords, projects, projectsContainingEntity, recordId, records, route, selectExistingSteps, tables, templatesSelect)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepConfig, StepConfigEntry, StepType(..), TStringDisplay(..), downloadArgs, tBoolValue, tEnumValue, tIntValue, tListValue, tStepId, tStringValue)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route exposing (Route)
import Scroll
import Set
import Specs
import Time exposing (Posix)
import Time.Distance
import View.Icons exposing (icon, iconCustom)


isSuccessStatus : ApiData Status -> Bool
isSuccessStatus status =
    case status of
        Success StatusSuccess ->
            True

        _ ->
            False


sortBySortKey : List (BaseRecord a) -> List (BaseRecord a)
sortBySortKey =
    List.map (\record -> ( getSortKey record, record ))
        >> List.sortBy Tuple.first
        >> List.map Tuple.second


viewStatusCountBadge : TableSpec (BaseRecord a) -> List (BaseRecord a) -> Html msg
viewStatusCountBadge spec allRecords =
    let
        statusOf r =
            TableSpec.getStatus spec r |> ApiData.toMaybe

        statusCounts =
            List.foldl countStatus { running = 0, done = 0, failed = 0 } allRecords

        countStatus record counts =
            case statusOf record of
                Just Model.StatusRunning ->
                    { counts | running = counts.running + 1 }

                Just Model.StatusSuccess ->
                    { counts | done = counts.done + 1 }

                Just Model.StatusNotStarted ->
                    counts

                Just _ ->
                    { counts | failed = counts.failed + 1 }

                Nothing ->
                    counts

        totalCount =
            List.length allRecords

        format ( n, label ) =
            if n > 0 then
                Just (String.fromInt n ++ " " ++ label)

            else
                Nothing

        statusDetails =
            [ ( statusCounts.running, "running" )
            , ( statusCounts.done, "done" )
            , ( statusCounts.failed, "failed" )
            ]
                |> List.filterMap format
                |> String.join " · "

        labelContent =
            if String.isEmpty statusDetails then
                String.fromInt totalCount ++ " total"

            else
                String.fromInt totalCount ++ " total · " ++ statusDetails
    in
    Html.viewIf (totalCount > 0) <|
        Html.span [ class "table-header-count" ]
            [ Html.text ("(" ++ labelContent ++ ")") ]


viewTable :
    { model : Model
    , spec : TableSpec (BaseRecord a)
    , table : Table (BaseRecord a)
    , recordStatusPill : BaseRecord a -> Html (Flow Model ())
    , recordActionsPopover : BaseRecord a -> Html (Flow Model ())
    , alwaysVisibleRecordActions : BaseRecord a -> List (Html (Flow Model ()))
    , directorySection : BaseRecord a -> Html (Flow Model ())
    , srcFilesSection : BaseRecord a -> Html (Flow Model ())
    , detailSection : BaseRecord a -> Html (Flow Model ())
    , onRecordClick : BaseRecord a -> Maybe (Flow Model ())
    }
    -> Html (Flow Model ())
viewTable { model, spec, table, recordStatusPill, recordActionsPopover, alwaysVisibleRecordActions, directorySection, srcFilesSection, detailSection, onRecordClick } =
    let
        lens =
            TableSpec.getLens spec

        currentRouteCommit =
            routeCommit (Model.getRoute model).page

        highlightedEntityId =
            case (Model.getRoute model).page of
                Route.Project { mHighlight } ->
                    mHighlight

                _ ->
                    Nothing

        isReadOnly =
            Model.isReadOnlyRoute model

        isProjectsTag =
            TableSpec.getTag spec == TagProjects

        hasHiddenRecords =
            has (records << ApiData.success << where_ (List.any .hidden)) table

        now =
            Model.getNow model

        tableActionBtn action className content =
            Html.button
                [ Events.onClick action
                , Events.stopPropagationOn "click" (Decode.succeed ( action, True ))
                , class className
                ]
                content

        mProjectId =
            try currentProjectId model

        mEditedId =
            try (edited << just << recordId << just) table

        recordIsEditing record =
            Maybe.map2 (==) mEditedId record.id |> Maybe.withDefault False

        editable record =
            not isReadOnly && not (TableSpec.getIsLocked spec record)

        viewRecord index record =
            let
                isHighlighted =
                    Maybe.map2 (==) (Maybe.map .id highlightedEntityId) record.id
                        |> Maybe.withDefault False

                recordNameEditable =
                    if recordIsEditing record && table.nameEditOnly && editable record then
                        Html.input
                            [ type_ "text"
                            , value (Maybe.map .name table.edited |> Maybe.withDefault record.name)
                            , Events.onInput (Actions.editRecordName lens)
                            , class "form-input"
                            , Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                            , Events.onBlur <| TableSpec.getUpsertRecord spec
                            , Events.on "keydown" <|
                                Keyboard.decodeCombinations
                                    [ ( Keyboard.enter, Decode.succeed <| TableSpec.getUpsertRecord spec )
                                    , ( Keyboard.escape, Decode.succeed <| Actions.stopInlineRecordNameEdit spec )
                                    ]
                            ]
                            []

                    else
                        Html.span
                            [ class "record-name-container"
                            ]
                            [ Html.text record.name
                            , Html.viewMaybe
                                (\id_ ->
                                    Html.span [ class "table-record-id", title <| "id: " ++ String.fromInt id_ ]
                                        [ Html.text (String.fromInt id_) ]
                                )
                                record.id
                            , Html.viewIf (editable record) <|
                                iconCustom True
                                    "edit"
                                    [ class "edit-icon"
                                    , Events.stopPropagationOn "click" (Decode.succeed ( Actions.startInlineRecordNameEdit spec record, True ))
                                    ]
                            ]

                viewUnmovedRecord attrs mkDragAttrs mkDropAttrs =
                    let
                        itemId =
                            Maybe.unwrap (TableSpec.getName spec ++ "-new") String.fromInt record.id
                        cmap =
                            List.map (map (Actions.dndMsgToIO mProjectId spec))

                        actionsContainerClass =
                            "table-record-actions-container"

                        recordStatus =
                            TableSpec.getStatus spec record

                        validationErrors =
                            TableSpec.getValidationErrors spec record
                    in
                    Html.div
                        ([ class "table-record", id itemId ] ++ attrs ++ cmap (mkDropAttrs itemId))
                        [ Html.div
                            ([ class "table-record-header"
                             , classList
                                [ ( "hidden", record.hidden )
                                , ( "highlighted", isHighlighted )
                                , ( "no-status", isProjectsTag && List.isEmpty validationErrors )
                                ]
                             ]
                                ++ (if Maybe.isJust record.id && (isProjectsTag || isSuccessStatus recordStatus) then
                                        Maybe.unwrap []
                                            (\action ->
                                                [ Events.on "click" (Decode.field "target" (Decode.whenNotInside actionsContainerClass action))
                                                , style "cursor" "pointer"
                                                ]
                                            )
                                            (onRecordClick record)

                                    else
                                        []
                                   )
                            )
                            [ case validationErrors of
                                [] ->
                                    if isProjectsTag then
                                        Html.nothing

                                    else
                                        recordStatusPill record

                                errors ->
                                    Html.span
                                        [ class "project-error-indicator"
                                        , title (String.join "\n" errors)
                                        ]
                                        [ iconCustom True "error" [] ]
                            , Html.span [ class "table-record-name" ]
                                [ recordNameEditable
                                , Html.span [] (alwaysVisibleRecordActions record)
                                , Html.Lazy.lazy2 viewMtimeBadge record.lastModifiedAt now
                                , Html.viewIf (record.id == Nothing || record.isUpdating) <|
                                    Html.span [ class "pending-record-indicator", title "Saving..." ]
                                        [ iconCustom True "progress_activity" [ class "pending-record-icon" ]
                                        ]
                                ]
                            , let
                                popoverId =
                                    actionsPopoverId (TableSpec.getName spec) record
                              in
                              Html.div
                                [ class actionsContainerClass ]
                                [ Html.button
                                    [ class "icon-btn hamburger-icon-btn-mobile"
                                    , attribute "popovertarget" popoverId
                                    , style "anchor-name" ("--anchor-" ++ popoverId)
                                    ]
                                    [ icon True "more_vert" ]
                                , recordActionsPopover record
                                , Html.viewIf (not isReadOnly) <|
                                    Html.div (class "table-record-drag-target" :: cmap (mkDragAttrs itemId))
                                        [ icon True "drag_indicator" ]
                                , Html.Lazy.lazy2 viewMtimeBadge record.lastModifiedAt now
                                ]
                            ]
                        , let
                            editing =
                                recordIsEditing record
                          in
                          Html.viewIf (editing && not table.nameEditOnly)
                            (Html.viewMaybe
                                (viewAddOrEditRecordForm model
                                    spec
                                    table
                                    (if isReadOnly then
                                        Html.nothing

                                     else
                                        srcFilesSection record
                                    )
                                )
                                table.edited
                            )
                        , Html.viewIf (TableSpec.getDirectoryView spec record |> Maybe.map .expanded |> Maybe.withDefault False) (directorySection record)
                        , detailSection record
                        ]
            in
            Html.Keyed.node "div"
                []
                (case dndSystem.info table.dnd of
                    Just { dragIndex } ->
                        if dragIndex /= index then
                            [ ( "record-" ++ String.fromInt index, viewUnmovedRecord [] (always []) (dndSystem.dropEvents index) ) ]

                        else
                            [ ( "placeholder", viewUnmovedRecord [ class "zero-opacity" ] (always []) (always []) )
                            , ( "ghost", viewUnmovedRecord (class "dnd-ghost" :: (List.map (map (always Flow.none)) <| dndSystem.ghostStyles table.dnd)) (always []) (always []) )
                            ]

                    Nothing ->
                        [ ( "record-" ++ String.fromInt index, viewUnmovedRecord [] (dndSystem.dragEvents index) (dndSystem.dropEvents index) ) ]
                )

        viewContent =
            let
                isEmpty =
                    ApiData.unwrap False List.isEmpty table.records

                headerAttrs =
                    [ class "table-header"
                    , classList [ ( "table-header-empty", isEmpty ) ]
                    ]
                        ++ (if isEmpty then
                                []

                            else
                                [ Events.onClick (Actions.toggleTable lens) ]
                           )

                viewRecordsSection =
                    let
                        viewContents records =
                            records
                                |> (if table.showHiddenRecords then
                                        identity

                                    else
                                        List.filter (not << .hidden)
                                   )
                                |> (if Maybe.isJust (dndSystem.info table.dnd) then
                                        identity

                                    else
                                        sortBySortKey
                                   )
                                |> List.indexedMap viewRecord
                                |> Html.div [ class "table-records", Events.onMouseDown (Flow.modify (set (lens << dndAffected) [])) ]
                    in
                    ApiData.foldVisible
                        Html.nothing
                        (Maybe.map viewContents
                            >> Maybe.withDefault (Html.div [ class "table-records-loading" ] [ Html.span [ class "shimmer-text shimmer-text--medium-contrast" ] [ Html.text "Loading records..." ] ])
                        )
                        viewContents
                        (always Html.nothing)
                        table.records
            in
            Html.div [ class "table", id ("table-" ++ TableSpec.getName spec) ]
                [ Html.div headerAttrs
                    [ Html.div [ class "table-header-content" ]
                        [ iconCustom True
                            (if table.isOpen then
                                "expand_more"

                             else
                                "chevron_right"
                            )
                            [ class "table-header-chevron" ]
                        , Html.span [ class "table-content-header" ] [ Html.text (TableSpec.getDisplayName spec) ]
                        , ApiData.unwrap Html.nothing (viewStatusCountBadge spec) table.records
                        ]
                    , Html.div [ class "table-header-controls" ]
                        [ Html.viewIf (not isReadOnly && hasHiddenRecords) <|
                            tableActionBtn (Actions.toggleShowHiddenRecords lens)
                                "btn"
                                [ Html.text
                                    (if table.showHiddenRecords then
                                        "Hide Hidden"

                                     else
                                        "Show Hidden"
                                    )
                                ]
                        , Html.viewIf (not isReadOnly && hasHiddenRecords) <|
                            tableActionBtn
                                (ApiData.unwrap (Flow.pure ())
                                    (Flow.batchM << List.map (Actions.toggleRecordVisibility spec mProjectId (Just False)))
                                    table.records
                                )
                                "btn"
                                [ Html.text "Unhide All" ]
                        , Html.viewIf (not isReadOnly) <| tableActionBtn (Actions.toggleAddOrEditRecordForm spec Nothing) "icon-btn" [ icon True "add" ]
                        ]
                    ]
                , table.edited
                    |> Maybe.andThen
                        (\r ->
                            if r.id == Nothing then
                                -- Adding a new record
                                Just r

                            else if table.addMode == AddFromOtherProject then
                                -- Adding an existing record (show form so user can click Save)
                                Just r

                            else
                                -- This would be editing an existing record (handled elsewhere)
                                Nothing
                        )
                    |> Maybe.map (viewAddOrEditRecordForm model spec table Html.nothing)
                    |> Maybe.withDefault Html.nothing
                , Html.viewIf table.isOpen viewRecordsSection
                ]
    in
    viewContent


{-| The relative-time badge of a record. A row places it twice (in the name
and in the actions container, only one of which is displayed depending on the
viewport), so each call site builds its own thunk: a lazy node caches its
rendered node on itself, and one thunk in two positions would hand the same
element to both.
-}
viewMtimeBadge : Maybe Posix -> Posix -> Html msg
viewMtimeBadge mPosix now =
    Html.viewMaybe
        (\posix ->
            let
                iso =
                    Iso8601.fromTime posix
            in
            Html.node "time"
                [ class "table-record-mtime"
                , attribute "datetime" iso
                , title ("Last modified: " ++ iso)
                ]
                [ Html.text (Time.Distance.inWords posix now) ]
        )
        mPosix


routeCommit : Route.Page -> Maybe String
routeCommit page =
    case page of
        Route.Project { mCommit } ->
            mCommit

        _ ->
            Nothing


viewStatusApiData : String -> Maybe String -> ApiData String -> Maybe Int -> ApiData Status -> Html (Flow Model ())
viewStatusApiData tableName currentRouteCommit logState mRecordId status =
    let
        viewStatusPill s =
            let
                ( colorClass, statusText ) =
                    case s of
                        StatusNotStarted ->
                            ( "status-not-started", "Not Started" )

                        StatusRunning ->
                            ( "status-running", "Running" )

                        StatusSuccess ->
                            ( "status-success", "Success" )

                        StatusFailure mError ->
                            ( "status-failure"
                            , case mError of
                                Just err ->
                                    "Failure: " ++ err

                                Nothing ->
                                    "Failure"
                            )
            in
            case ( s, mRecordId ) of
                ( StatusFailure _, Just stepId ) ->
                    let
                        popoverId =
                            "step-log-popover-" ++ tableName ++ "-" ++ String.fromInt stepId
                    in
                    Html.span []
                        [ Html.button
                            [ class "status-indicator-wrapper status-log-trigger"
                            , title statusText
                            , attribute "popovertarget" popoverId
                            , style "anchor-name" ("--anchor-" ++ popoverId)
                            , Events.onClick (Actions.loadStepLog stepId)
                            ]
                            [ Html.span
                                [ class ("status-indicator " ++ colorClass) ]
                                []
                            ]
                        , Html.div
                            [ class "step-log-popover"
                            , id popoverId
                            , attribute "popover" "auto"
                            , style "position-anchor" ("--anchor-" ++ popoverId)
                            ]
                            [ Html.div [ class "step-log-popover-header" ]
                                [ Html.strong [] [ Html.text ("Build log for step " ++ String.fromInt stepId) ]
                                , Html.span []
                                    [ Html.viewMaybe
                                        (\log ->
                                            Html.button
                                                [ class "icon-btn"
                                                , title "Investigate with agent"
                                                , Events.onClick (Actions.hidePopover popoverId |> Flow.seq (Actions.investigateStepWithAgent stepId log))
                                                ]
                                                [ icon False "smart_toy" ]
                                        )
                                        (ApiData.toMaybe logState)
                                    , Html.button
                                        [ class "icon-btn"
                                        , title "Close"
                                        , Events.onClick (Actions.hidePopover popoverId)
                                        ]
                                        [ icon True "close" ]
                                    ]
                                ]
                            , Html.div [ class "step-log-popover-body" ]
                                [ case logState of
                                    NotAsked ->
                                        Html.text "Loading build log..."

                                    Loading _ ->
                                        Html.text "Loading build log..."

                                    Success log ->
                                        if String.isEmpty log then
                                            Html.text "Build log is empty."

                                        else
                                            Html.div [ class "step-log-pre" ]
                                                [ AnsiLog.view (AnsiLog.update log (AnsiLog.init AnsiLog.Cooked)) ]

                                    Error err ->
                                        Html.text (Http.errorMessage err)
                                ]
                            ]
                        ]

                _ ->
                    Html.span
                        [ class "status-indicator-wrapper"
                        , title statusText
                        ]
                        [ Html.span
                            [ class ("status-indicator " ++ colorClass) ]
                            []
                        ]
    in
    ApiData.foldVisible
        (Html.div [] [])
        (\mPrevStatus ->
            Html.span
                [ class "status-indicator-wrapper"
                , title "Loading"
                ]
                [ mPrevStatus
                    |> Maybe.map viewStatusPill
                    |> Maybe.withDefault (Html.div [] [])
                , iconCustom True "progress_activity" [ class "status-indicator-loading" ]
                ]
        )
        viewStatusPill
        (always <| viewStatusPill (StatusFailure Nothing))
        status


{-| The status indicator of a step record, as a memoizable node: failure rows
carry a build-log popover, which is rendered into its own node per row on
every redraw otherwise. The log state is passed in pre-resolved so that a log
arriving for one step only invalidates that step's row.
-}
viewStepRecordStatus : String -> StepConfigEntry -> Route.Page -> ApiData String -> StepRecord -> Html (Flow Model ())
viewStepRecordStatus name entry page logState record =
    viewStatusApiData
        name
        (routeCommit page)
        logState
        record.id
        (TableSpec.getStatus (Specs.steps name entry) record)


actionsPopoverId : String -> BaseRecord a -> String
actionsPopoverId tableName record =
    "actions-popover-" ++ tableName ++ "-" ++ String.fromInt (Maybe.withDefault -1 record.id)


viewRecordActionsPopover : String -> List (Html (Flow Model ())) -> Html (Flow Model ())
viewRecordActionsPopover popoverId actions =
    Html.div
        [ class "table-record-actions"
        , id popoverId
        , attribute "popover" "auto"
        , style "position-anchor" ("--anchor-" ++ popoverId)
        , Events.on "click" (Decode.succeed (Actions.hidePopover popoverId))
        ]
        actions


{-| The actions of one record. Split out of `viewTable` so that the memoized
step variant below can rebuild them from data alone: a `Html.Lazy` thunk
compares its references with `===`, so nothing here may capture the model.
-}
viewRecordActions : TableSpec (BaseRecord a) -> Bool -> Maybe Int -> BaseRecord a -> List (Html (Flow Model ()))
viewRecordActions spec isReadOnly mProjectId record =
    let
        editable r =
            not isReadOnly && not (TableSpec.getIsLocked spec r)

        isDirectoryOpen =
            TableSpec.getDirectoryView spec record |> Maybe.map .expanded |> Maybe.withDefault False

        sourceFilesNeedLoading =
            case Maybe.map .children (TableSpec.getSrcFilesView spec record) of
                Just NotAsked ->
                    True

                Just (Error _) ->
                    True

                _ ->
                    False

        toggleRecordEditor =
            let
                loadSourceFiles =
                    if sourceFilesNeedLoading then
                        Maybe.unwrap (Flow.pure ())
                            (\recordId -> Actions.toggleSrcEntry recordId (Just True) [] |> Flow.return ())
                            record.id

                    else
                        Flow.pure ()
            in
            Actions.toggleAddOrEditRecordForm spec record.id
                |> Flow.seq loadSourceFiles

        recordActions =
            [ -- Directory button
              { shouldShow = isSuccessStatus << TableSpec.getStatus spec
              , render = \r -> Html.viewMaybe (dirButton isDirectoryOpen []) r.id
              }
            , { shouldShow = Maybe.isJust << .id
              , render =
                    \r ->
                        if editable r then
                            viewIconButtonWithTooltip "edit" True "Edit" toggleRecordEditor

                        else
                            viewIconButtonWithTooltip "data_info_alert" True "Inspect Parameters" (Actions.toggleAddOrEditRecordForm spec r.id)
              }
            , -- Share button (shareable only)
              { shouldShow = \r -> TableSpec.getShareable spec r && Maybe.isJust r.id
              , render =
                    \r ->
                        viewIconButtonWithTooltip
                            "share"
                            True
                            "Share"
                            (Maybe.map2 (\projectId recordId -> Actions.shareEntity projectId recordId Route.Output [] Nothing)
                                mProjectId
                                r.id
                                |> Maybe.withDefault Flow.none
                            )
              }
            , -- Visibility toggle button
              { shouldShow = \r -> not isReadOnly && Maybe.isJust r.id
              , render =
                    \r ->
                        viewIconButtonWithTooltip
                            (if r.hidden then
                                "visibility"

                             else
                                "visibility_off"
                            )
                            True
                            (if r.hidden then
                                "Show"

                             else
                                "Hide"
                            )
                            (Actions.toggleRecordVisibility spec mProjectId Nothing r)
              }
            , -- Clone button (shareable only)
              { shouldShow = \r -> not isReadOnly && TableSpec.getShareable spec r
              , render = \r -> viewIconButtonWithTooltip "content_copy" False "Clone" (TableSpec.getCloneRecord spec r)
              }
            , -- Remove button
              { shouldShow =
                    \r ->
                        let
                            hasDependentInProject =
                                False
                        in
                        editable r
                            && Maybe.isJust r.id
                            && (not (TableSpec.getShareable spec r) || not hasDependentInProject)
              , render = \r -> Html.viewMaybe (viewIconButtonWithTooltip "delete" False "Remove" << Actions.removeRecord spec) r.id
              }
            ]
    in
    List.filterMap (\recordActionBtn -> if recordActionBtn.shouldShow record then Just (recordActionBtn.render record) else Nothing) recordActions


viewRunStop : TableSpec StepRecord -> StepRecord -> List (Html (Flow Model ()))
viewRunStop spec record =
    case record.id of
        Just id ->
            let
                status =
                    TableSpec.getStatus spec record

                isRunning =
                    status
                        |> ApiData.toMaybe
                        |> (==) (Just StatusRunning)

                canRun =
                    case status of
                        Loading _ ->
                            False

                        Success StatusSuccess ->
                            False

                        Success StatusRunning ->
                            False

                        _ ->
                            True
            in
            [ Html.viewIf canRun (viewRunButton "Run" (Actions.runStep spec id))
            , Html.viewIf isRunning (viewStopButton "Stop" (Actions.stopStep spec id))
            ]

        Nothing ->
            []


{-| The record actions of a step table, as a single memoizable node.

Everything it needs arrives as a value: the section name and entry give the
spec, the step config and the names with tables give the quick-create
buttons, and the route page and the project id key give the share and
visibility targets. Rows that are mid-upload change the upload button, so
that comes in as a flag. Nothing else in a row's actions is time- or
model-dependent, so an unchanged row's popover is never rebuilt.

-}
viewStepRecordActions : String -> StepConfigEntry -> StepConfig -> String -> String -> Route.Page -> StepRecord -> Bool -> Html (Flow Model ())
viewStepRecordActions name entry stepConfig presentTypesKey projectIdKey page record uploading =
    let
        spec =
            Specs.steps name entry

        isReadOnly =
            Model.isReadOnlyPage page

        mProjectId =
            String.toInt projectIdKey

        presentTypes =
            String.split "," presentTypesKey

        prefill argType =
            let
                wire allowedTypes toValue =
                    if Maybe.unwrap True (List.member record.type_) allowedTypes then
                        Maybe.map toValue record.id

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

        runActions =
            case entry.stepType of
                Derivation _ _ ->
                    viewRunStop spec record

                Download ->
                    viewRunStop spec record

                FileUpload _ ->
                    []

        uploadActions =
            if isReadOnly || uploading || Maybe.isJust record.review then
                []

            else
                case entry.stepType of
                    FileUpload types ->
                        [ Html.viewMaybe (viewUploadButton << Actions.uploadFiles spec (Maybe.withDefault [] types)) record.id ]

                    Derivation _ _ ->
                        []

                    Download ->
                        []

        quickCreateActions =
            if isReadOnly then
                []

            else
                stepConfig
                    |> Dict.toList
                    |> List.filter (\( targetType, _ ) -> List.member targetType presentTypes)
                    |> List.concatMap
                        (\( targetType, targetEntry ) ->
                            let
                                targetSpec =
                                    Specs.steps targetType targetEntry

                                label =
                                    "Create " ++ TableSpec.getDisplayName targetSpec
                            in
                            case targetEntry.stepType of
                                Derivation args _ ->
                                    args
                                        |> Dict.toList
                                        |> List.filterMap
                                            (\( argName, arg ) ->
                                                prefill arg.type_
                                                    |> Maybe.map (viewQuickCreateButton targetEntry.icon label << Actions.addStepWithArg targetSpec argName)
                                            )

                                FileUpload _ ->
                                    []

                                Download ->
                                    []
                        )
    in
    viewRecordActionsPopover
        (actionsPopoverId name record)
        (uploadActions ++ runActions ++ quickCreateActions ++ viewRecordActions spec isReadOnly mProjectId record)


viewAddOrEditRecordForm : Model -> TableSpec (BaseRecord a) -> Table (BaseRecord a) -> Html (Flow Model ()) -> BaseRecord a -> Html (Flow Model ())
viewAddOrEditRecordForm model spec table extraSection record =
    let
        readOnly =
            Model.isReadOnlyRoute model || TableSpec.getIsLocked spec record

        editing =
            record.id /= Nothing && (table.addMode /= AddFromOtherProject)

        savingInFlight =
            try (records << success << by .id record.id) table
                |> Maybe.unwrap False .isUpdating

        extraFields =
            case TableSpec.getTag spec of
                TagSteps key stepDef ->
                    [ viewStepExtraFormFields model readOnly key stepDef ]

                TagProjects ->
                    viewProjectExtraFormFields model

        noteInput =
            case TableSpec.getTag spec of
                TagSteps tableId _ ->
                    viewStepNoteField model readOnly tableId

                TagProjects ->
                    Html.nothing

        nameInput =
            let
                originalRecord =
                    try (records << success << by .id record.id) table
            in
            textField
                { label = "Name"
                , mHint = Nothing
                , placeholder = TableSpec.getDisplayName spec ++ " name"
                , value = record.name
                , onInput = Actions.editRecordName (TableSpec.getLens spec)
                , hasChanged = not readOnly && fieldChanged .name record.name originalRecord
                , readOnly = readOnly
                , id = TableSpec.getName spec ++ "-name-input"
                }

        formClasses =
            classList
                [ ( "form", True )
                , ( "form-adding", not editing )
                , ( "form-editing", editing )
                , ( "form-read-only", readOnly )
                ]

        radioButton mode label =
            Html.label []
                [ Html.input
                    [ type_ "radio"
                    , name ("addMode" ++ TableSpec.getName spec)
                    , checked (table.addMode == mode)
                    , Events.onClick (Actions.setAddMode (TableSpec.getLens spec) (TableSpec.getDefaultRecord spec) mode)
                    ]
                    []
                , Html.text label
                ]

        modeSelector =
            Html.div [ class "form-mode-selector" ]
                [ radioButton AddNew "Create new"
                , radioButton AddFromOtherProject "Add from other project"
                ]

        viewSelectExisting state =
            let
                mProjectId =
                    try currentProjectId model

                availableItems =
                    List.map (\{ id, name } -> { id = id, name = name, mProjectId = Nothing })
                        (all (allEntities (where_ (\{ id } -> id /= mProjectId) << tables << key (TableSpec.getName spec) << just)) model
                            |> List.filter
                                (\r -> r.id |> Maybe.unwrap True (\id -> not (List.member id (ApiData.withDefault [] table.records |> List.filterMap .id))))
                        )
                        |> List.unique
                        |> List.filter (\item -> not (List.any (\i -> i.id == item.id) state.selected))

                toItemTooltip =
                    Maybe.unwrap [] (\entityId_ -> "projects containing entity:" :: List.map (\p -> "• " ++ p) (List.map .name (all (projectsContainingEntity entityId_) model))) << .id
            in
            Select.view
                { optic = TableSpec.getLens spec << selectExistingSteps
                , selectState = state
                , selected_ = state.selected
                , availableItems = availableItems
                , readOnly = False
                , hasChanged = False
                , label = "Select records"
                , mHint = Nothing
                , placeholder = ""
                , inputIcon = Nothing
                , toInputItemName = .name
                , toInputItemTooltip = toItemTooltip
                , onInputItemClick = \_ -> Nothing
                , toMenuItemName = .name
                , toMenuItemTooltip = toItemTooltip
                , onChange = Flow.pure ()
                , onRemove = \_ -> Flow.pure ()
                , activeAfterSelect = True
                , clearInputAfterSelect = True
                , onSelect = \_ -> Flow.pure ()
                , alignRight = False
                , inputItemStyle = \_ -> []
                }

        headerTitle =
            if readOnly then
                "Inspect Parameters"

            else
                let
                    displayName =
                        TableSpec.getDisplayName spec
                in
                case ( editing, table.addMode ) of
                    ( False, AddNew ) ->
                        "Create new " ++ displayName

                    ( False, AddFromOtherProject ) ->
                        "Add from other project: " ++ displayName

                    ( True, _ ) ->
                        "Edit " ++ displayName

        closeAction =
            let
                endEdit =
                    Actions.endRecordEdit (TableSpec.getLens spec)
            in
            case ( readOnly, record.id ) of
                ( False, Just recordId ) ->
                    Actions.discardSrcFileChanges recordId
                        |> Flow.seq endEdit

                _ ->
                    endEdit
    in
    Html.div [ class "table-form-wrapper" ]
        [ Html.div
            (formClasses
                :: (if readOnly then
                        []

                    else
                        [ upsertOnEnter spec ]
                   )
            )
            [ Html.div [ class "loading-wrapper" ]
                [ Html.header [ class "form-header" ] [ Html.text headerTitle ]
                , Html.viewMaybe
                    (\d -> Html.p [ class "form-intro" ] [ Html.text d ])
                    (if editing || table.addMode == AddNew then
                        TableSpec.getDescription spec

                     else
                        Nothing
                    )
                , Html.div [ class "form-body" ]
                    [ Html.viewIf (not editing && TableSpec.getTag spec /= TagProjects) modeSelector
                    , Html.viewIf (not editing && table.addMode == AddFromOtherProject && TableSpec.getTag spec /= TagProjects) <| Html.Lazy.lazy viewSelectExisting table.selectExistingSteps
                    , Html.viewIf (not editing && table.addMode == AddNew || editing) nameInput
                    , Html.viewIf (not editing && table.addMode == AddNew || editing) noteInput
                    , Html.viewIf ((not editing && table.addMode == AddNew || editing) && not (List.isEmpty extraFields)) <|
                        Html.div [ class "form-group" ] extraFields
                    , extraSection
                    , Html.div [ class "form-actions" ]
                        [ Html.viewIf (not readOnly) <|
                            Html.button [ id "save-button", Events.onClick (TableSpec.getUpsertRecord spec), class "btn", disabled table.isUpdating ]
                                [ Html.text "Save" ]
                        , Html.button [ Events.onClick closeAction, class "btn" ]
                            [ Html.text
                                (if readOnly then
                                    "Close"

                                 else
                                    "Cancel"
                                )
                            ]
                        ]
                    ]
                , Html.viewIf savingInFlight <|
                    Html.div [ class "loading-overlay" ] [ iconCustom True "progress_activity" [ class "loading-icon" ] ]
                ]
            ]
        ]


upsertOnEnter : TableSpec (BaseRecord a) -> Html.Attribute (Flow Model ())
upsertOnEnter spec =
    let
        targetDecoder =
            Decode.map2
                (\tag id -> { tag = tag, id = id })
                (Decode.at [ "target", "tagName" ] Decode.string)
                (Decode.at [ "target", "id" ] Decode.string |> Decode.maybe |> Decode.map (Maybe.withDefault ""))

        allowEnter target =
            target.tag /= "TEXTAREA" && target.id /= "save-button" && target.id /= "select-input" && target.id /= "src-file-name-input" && not (String.endsWith "-list-input" target.id)
    in
    Events.on "keydown" <|
        Keyboard.decodeCombinations
            [ ( Keyboard.enter
              , Decode.field "target" (Decode.whenNotInside "code-input" (TableSpec.getUpsertRecord spec)) |> Decode.when targetDecoder allowEnter
              )
            ]


viewProjectExtraFormFields : Model -> List (Html (Flow Model ()))
viewProjectExtraFormFields model =
    let
        mEdited =
            try (projects << edited << just) model

        mPresets =
            ApiData.toMaybe (Model.getPresets model)

        mStepConfig =
            ApiData.toMaybe (Model.getStepConfig model)
    in
    case ( mEdited, mPresets, mStepConfig ) of
        ( Just edited_, Just presets_, Just stepConfig_ ) ->
            let
                source =
                    edited_.templateSource

                effective =
                    Model.effectiveTemplates presets_ source

                sortedTemplates =
                    Dict.keys stepConfig_ |> List.sort

                templateIdMap =
                    sortedTemplates
                        |> List.indexedMap (\i n -> ( n, i ))
                        |> Dict.fromList

                templateItem name_ =
                    { id = Dict.get name_ templateIdMap
                    , name = name_
                    , mProjectId = Nothing
                    }

                templateLabel name_ =
                    Dict.get name_ stepConfig_
                        |> Maybe.andThen .displayName
                        |> Maybe.withDefault name_

                presetLabel name_ =
                    Dict.get name_ presets_ |> Maybe.unwrap name_ .displayName

                customSentinel =
                    "__custom__"

                sortedPresetNames =
                    Dict.toList presets_
                        |> List.sortBy (Tuple.second >> .sortKey >> Maybe.withDefault 999999)
                        |> List.map Tuple.first

                presetIdMap =
                    customSentinel
                        :: sortedPresetNames
                        |> List.indexedMap (\i n -> ( n, i ))
                        |> Dict.fromList

                presetItem name_ =
                    { id = Dict.get name_ presetIdMap
                    , name = name_
                    , mProjectId = Nothing
                    }

                presetMenuLabel name_ =
                    if name_ == customSentinel then
                        "Custom (no preset)"

                    else
                        presetLabel name_

                availablePresets =
                    customSentinel
                        :: sortedPresetNames
                        |> List.map presetItem

                onPickPreset item =
                    if item.name == customSentinel then
                        Actions.chooseProjectCustom

                    else
                        Actions.chooseProjectPreset item.name

                presetStateLens =
                    projects << edited << just << presetSelect

                rawPresetState =
                    try presetStateLens model |> Maybe.withDefault Select.initSelectState

                presetDisplayState =
                    if rawPresetState.active then
                        rawPresetState

                    else
                        let
                            currentPresetLabel =
                                case source of
                                    FromPreset n ->
                                        presetLabel n

                                    CustomTemplates _ ->
                                        "Custom (no preset)"
                        in
                        { rawPresetState | input = currentPresetLabel }

                presetPicker =
                    Select.view
                        { optic = presetStateLens
                        , selectState = presetDisplayState
                        , selected_ = []
                        , availableItems = availablePresets
                        , readOnly = False
                        , hasChanged = False
                        , label = "Preset"
                        , mHint = Nothing
                        , placeholder = "Pick a preset..."
                        , inputIcon = Nothing
                        , toInputItemName = .name >> presetMenuLabel
                        , toInputItemTooltip = always []
                        , onInputItemClick = \_ -> Nothing
                        , toMenuItemName = .name >> presetMenuLabel
                        , toMenuItemTooltip = always []
                        , onChange = Flow.pure ()
                        , onRemove = \_ -> Flow.pure ()
                        , activeAfterSelect = False
                        , clearInputAfterSelect = False
                        , onSelect = onPickPreset
                        , alignRight = False
                        , inputItemStyle = \_ -> []
                        }

                selectedItems =
                    List.map templateItem effective

                availableItems =
                    sortedTemplates
                        |> List.filter (\t -> not (List.member t effective))
                        |> List.map templateItem

                stateLens =
                    projects << edited << just << templatesSelect

                templatesSelectView =
                    Select.view
                        { optic = stateLens
                        , selectState = try stateLens model |> Maybe.withDefault Select.initSelectState
                        , selected_ = selectedItems
                        , availableItems = availableItems
                        , readOnly = False
                        , hasChanged = False
                        , label = "Templates"
                        , mHint = Nothing
                        , placeholder =
                            if List.isEmpty selectedItems then
                                "Pick a template..."

                            else
                                ""
                        , inputIcon = Nothing
                        , toInputItemName = .name >> templateLabel
                        , toInputItemTooltip = always []
                        , onInputItemClick = \_ -> Nothing
                        , toMenuItemName = .name >> templateLabel
                        , toMenuItemTooltip = always []
                        , onChange = Flow.pure ()
                        , onRemove = .name >> Actions.removeProjectTemplate
                        , activeAfterSelect = True
                        , clearInputAfterSelect = True
                        , onSelect = .name >> Actions.addProjectTemplate
                        , alignRight = False
                        , inputItemStyle = .name >> stringToColor >> style "background-color" >> List.singleton
                        }
            in
            [ presetPicker, templatesSelectView ]

        _ ->
            [ Html.span [ class "shimmer-text shimmer-text--medium-contrast" ] [ Html.text "Loading presets..." ] ]


viewStepExtraFormFields : Model -> Bool -> String -> StepType -> Html (Flow Model ())
viewStepExtraFormFields model readOnly tableId stepDef =
    let
        argsLens =
            currentTableOf tableId << edited << just << args

        mEditedId =
            try (currentTableOf tableId << edited << just) model
                |> Maybe.andThen .id

        allCurrentProjectSteps =
            all (currentProject << success << projectStepRecords << where_ (\step -> Maybe.unwrap True (\editedId -> step.id /= Just editedId) mEditedId)) model

        allSteps mTypes =
            allCurrentProjectSteps
                |> List.filter (\step -> Maybe.unwrap True (List.member step.type_) mTypes)

        allStepsById =
            all (projects << records << success << each << projectStepRecords) model
                |> List.filterMap (\step -> step.id |> Maybe.map (\id -> ( id, step )))
                |> Dict.fromList

        getStep id =
            id |> Maybe.andThen (\i -> Dict.get i allStepsById)

        currentProjectStepIds =
            allCurrentProjectSteps
                |> List.filterMap .id
                |> Set.fromList

        isStepInCurrentProject id =
            id |> Maybe.map (\i -> Set.member i currentProjectStepIds) |> Maybe.withDefault False

        originalRecord =
            try (currentTableOf tableId << edited << just) model
                |> Maybe.andThen .id
                |> Maybe.andThen (\id_ -> try (currentTableOf tableId << records << success << by .id (Just id_)) model)

        stepConfig_ =
            Model.getStepConfig model |> ApiData.toMaybe |> Maybe.withDefault Dict.empty

        typeDisplayName typeName =
            Dict.get typeName stepConfig_
                |> Maybe.andThen .displayName
                |> Maybe.withDefault typeName

        currentRouteCommit =
            case (Model.getRoute model).page of
                Route.Project { mCommit } ->
                    mCommit

                _ ->
                    Nothing

        noticesForField paramName =
            mEditedId
                |> Maybe.map (\stepId -> Model.stepLogKey stepId currentRouteCommit)
                |> Maybe.andThen (\key -> Dict.get key (Model.getNotices model))
                |> Maybe.andThen ApiData.toMaybe
                |> Maybe.withDefault []
                |> List.filter (\notice -> notice.field == Just paramName && notice.severity == Model.Info)

        viewField ( paramName, { type_, description, displayName } ) =
            let
                fieldLabel =
                    Maybe.withDefault paramName displayName

                fieldNotices =
                    noticesForField paramName

                fieldHint =
                    if String.isEmpty description then
                        Nothing

                    else
                        Just description

                paramLens =
                    argsLens << key paramName

                fieldId =
                    paramName
                        ++ (case type_ of
                                TList (TString _ _) ->
                                    "-list-input"

                                _ ->
                                    "-input"
                           )

                fieldHasChanged =
                    not readOnly && paramName /= "downloadedAt" && fieldChanged (try (args << key paramName)) (try paramLens model) originalRecord

                buildListField listLens tagStrings addTag =
                    listField
                        { label = fieldLabel
                        , mHint = fieldHint
                        , tags = tagStrings
                        , onAdd = addTag
                        , onRemoveLast = Flow.modify (over listLens (\xs -> List.take (List.length xs - 1) xs)) |> Flow.seq (focus fieldId)
                        , onRemoveIndex = \idx -> Flow.modify (over listLens (List.removeAt idx)) |> Flow.seq (focus fieldId)
                        , readOnly = readOnly
                        , id = fieldId
                        , hasChanged = fieldHasChanged
                        }

                buildStepSelect { selectedStepIds, onSelectStep, onRemoveStep, activeAfterSelect, mAllowedStepTypes } =
                    let
                        stateLens =
                            currentTableOf tableId
                                << argSelectStates
                                << lens "keyWithDefault" (Dict.get paramName >> Maybe.withDefault Select.initSelectState) (\d v -> Dict.insert paramName v d)

                        selectedItems =
                            selectedStepIds
                                |> List.map
                                    (\stepId ->
                                        { id = Just stepId
                                        , name =
                                            case getStep (Just stepId) of
                                                Nothing ->
                                                    "#" ++ String.fromInt stepId ++ " (not in any project)"

                                                Just step ->
                                                    if isStepInCurrentProject (Just stepId) then
                                                        step.name

                                                    else
                                                        step.name ++ " (not in project)"
                                        , mProjectId = Nothing
                                        }
                                    )

                        selectedIds =
                            List.map .id selectedItems

                        availableItems =
                            allSteps mAllowedStepTypes
                                |> List.filterMap (\step -> step.id |> Maybe.map (\id -> { id = Just id, name = step.name, mProjectId = Nothing }))
                                |> List.filter (\item -> not (List.member item.id selectedIds))

                        toTooltip =
                            .id
                                >> Maybe.unwrap []
                                    (\id ->
                                        case getStep (Just id) of
                                            Just step ->
                                                [ "id: " ++ String.fromInt id ++ " — " ++ typeDisplayName step.type_ ]

                                            Nothing ->
                                                [ "id: " ++ String.fromInt id ]
                                    )

                        toHighlightRoute stepId =
                            try currentProjectId model
                                |> Maybe.map
                                    (\projectId ->
                                        let
                                            mCommit_ =
                                                try (route << Route.page << Route.project << mCommit << just) model
                                        in
                                        Route.fromPage
                                            (Route.Project
                                                { projectId = projectId
                                                , mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }
                                                , mCommit = mCommit_
                                                , mCompare = Nothing
                                                }
                                            )
                                    )
                    in
                    Select.view
                        { optic = stateLens
                        , selectState = try stateLens model |> Maybe.withDefault Select.initSelectState
                        , selected_ = selectedItems
                        , availableItems = availableItems
                        , readOnly = readOnly
                        , hasChanged = fieldHasChanged
                        , label = fieldLabel
                        , mHint = fieldHint
                        , placeholder = ""
                        , inputIcon = Nothing
                        , toInputItemName = .name
                        , toInputItemTooltip = toTooltip
                        , onInputItemClick = .id >> Maybe.andThen toHighlightRoute >> Maybe.map Actions.goToRoute
                        , toMenuItemName =
                            \item ->
                                Maybe.map2 (\id s -> "[" ++ String.fromInt id ++ "] [" ++ typeDisplayName s.type_ ++ "] " ++ item.name) item.id (getStep item.id) |> Maybe.withDefault item.name
                        , toMenuItemTooltip = toTooltip
                        , onChange = Flow.pure ()
                        , onRemove = .id >> Maybe.unwrap (Flow.pure ()) onRemoveStep
                        , activeAfterSelect = activeAfterSelect
                        , clearInputAfterSelect = True
                        , onSelect = .id >> Maybe.unwrap (Flow.pure ()) onSelectStep
                        , alignRight = False
                        , inputItemStyle = \item -> getStep item.id |> Maybe.map (.type_ >> stringToColor >> style "background-color") |> Maybe.toList
                        }

                viewFieldNotice notice =
                    Html.div [ class "field-notice", class "field-notice-info" ]
                        [ iconCustom True "info" [ class "field-notice-icon" ]
                        , Html.div [ class "field-notice-markdown" ] <| Markdown.plain notice.message
                        ]

                withFieldNotices field =
                    case fieldNotices of
                        [] ->
                            field

                        _ ->
                            Html.div [ class "field-with-notices" ]
                                [ field
                                , Html.div [ class "field-notices" ] (List.map viewFieldNotice fieldNotices)
                                ]
            in
            withFieldNotices <|
                case type_ of
                    TStep mAllowedStepTypes _ ->
                        buildStepSelect
                            { selectedStepIds =
                                case try (paramLens << just << tStepId) model of
                                    Just stepId ->
                                        [ stepId ]

                                    Nothing ->
                                        []
                            , onRemoveStep = \_ -> Flow.modify (set paramLens Nothing)
                            , activeAfterSelect = False
                            , onSelectStep = \stepId -> Flow.modify (set paramLens (Just (TStepValue stepId)))
                            , mAllowedStepTypes = mAllowedStepTypes
                            }

                    TEnum values enumDisplayNames ->
                        formField
                            { label = fieldLabel
                            , mHint = fieldHint
                            , id = fieldId
                            }
                            (Html.select
                                [ id fieldId
                                , class "form-input"
                                , classList [ ( "field-changed", fieldHasChanged ) ]
                                , disabled readOnly
                                , Events.onInput (\v -> Flow.modify (set paramLens (Just (TEnumValue v))))
                                ]
                                (List.map
                                    (\v ->
                                        Html.option
                                            [ value v
                                            , selected
                                                (case try (paramLens << just << tEnumValue) model of
                                                    Just current ->
                                                        current == v

                                                    Nothing ->
                                                        False
                                                )
                                            ]
                                            [ Html.text (Dict.get v enumDisplayNames |> Maybe.withDefault v) ]
                                    )
                                    values
                                )
                            )

                    TInt _ _ ->
                        let
                            intField value =
                                textField
                                    { label = fieldLabel
                                    , mHint = fieldHint
                                    , placeholder = fieldLabel
                                    , value = value
                                    , onInput = \s ->
                                        case String.toInt s of
                                            Just n ->
                                                Flow.modify (set paramLens (Just (TIntValue n)))

                                            Nothing ->
                                                Flow.none
                                    , hasChanged = fieldHasChanged
                                    , readOnly = readOnly
                                    , id = paramName ++ "-input"
                                    }
                        in
                        case try (paramLens << just << tIntValue) model of
                            Just n ->
                                intField (String.fromInt n)

                            Nothing ->
                                intField ""

                    TBool ->
                        formField
                            { label = fieldLabel
                            , mHint = fieldHint
                            , id = fieldId
                            }
                            (Html.input
                                [ Html.Attributes.type_ "checkbox"
                                , id fieldId
                                , checked (Maybe.withDefault False (try (paramLens << just << tBoolValue) model))
                                , Events.onCheck (\b -> Flow.modify (set paramLens (Just (TBoolValue b))))
                                , class "form-checkbox"
                                , classList [ ( "field-changed", fieldHasChanged ) ]
                                ]
                                []
                            )

                    TString display _ ->
                        case display of
                            TextField ->
                                textField
                                    { label = fieldLabel
                                    , mHint = fieldHint
                                    , placeholder = fieldLabel
                                    , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                    , onInput = Flow.modify << set paramLens << Just << TStringValue
                                    , hasChanged = fieldHasChanged
                                    , readOnly = readOnly || paramName == "downloadedAt"
                                    , id = paramName ++ "-input"
                                    }

                            TextArea ->
                                textArea
                                    { label = fieldLabel
                                    , mHint = fieldHint
                                    , placeholder = ""
                                    , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                    , onInput = Flow.modify << set paramLens << Just << TStringValue
                                    , hasChanged = fieldHasChanged
                                    , readOnly = readOnly
                                    , id = paramName ++ "-input"
                                    }

                            Command cmdPrefix ->
                                commandField
                                    { label = fieldLabel
                                    , mHint = fieldHint
                                    , placeholder = fieldLabel
                                    , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                    , onInput = Flow.modify << set paramLens << Just << TStringValue
                                    , hasChanged = fieldHasChanged
                                    , readOnly = readOnly
                                    , id = paramName ++ "-input"
                                    , commandPrefix = cmdPrefix
                                    }

                            Code language ->
                                codeField
                                    { label = fieldLabel
                                    , mHint = fieldHint
                                    , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                    , onInput = Flow.modify << set paramLens << Just << TStringValue
                                    , hasChanged = fieldHasChanged
                                    , readOnly = readOnly
                                    , id = paramName ++ "-input"
                                    , language = language
                                    }

                    TList (TStep mAllowedStepTypes _) ->
                        let
                            listLens =
                                paramLens << lens "withDefault" (Maybe.withDefault (TListValue [])) (\_ -> Just) << tListValue
                        in
                        buildStepSelect
                            { selectedStepIds = all (listLens << each) model |> List.filterMap (try tStepId)
                            , onRemoveStep = \stepId -> Flow.modify (over listLens (List.filter (\val -> try tStepId val /= Just stepId)))
                            , activeAfterSelect = True
                            , onSelectStep = \stepId -> Flow.modify (over listLens (flip (++) [ TStepValue stepId ]))
                            , mAllowedStepTypes = mAllowedStepTypes
                            }

                    TList (TString _ mAutocomplete) ->
                        let
                            listLens =
                                paramLens << lens "withDefault" (Maybe.withDefault (TListValue [])) (\_ -> Just) << tListValue

                            strings =
                                all (listLens << each << tStringValue) model

                            addTag val =
                                let
                                    trimmed =
                                        String.trim val
                                in
                                Flow.modify (over listLens (flip (++) [ TStringValue trimmed ]))
                                    |> Flow.seq (focus fieldId)
                                    |> Flow.when (not <| String.isEmpty trimmed)
                        in
                        case mAutocomplete of
                            Just autocompleteKey ->
                                let
                                    autocompleteStateKey =
                                        tableId ++ ":" ++ paramName

                                    autocompleteState =
                                        Dict.get autocompleteStateKey (Model.getAutocomplete model)
                                            |> Maybe.withDefault Model.initAutocompleteState

                                    autocompleteRequest query =
                                        { template = tableId
                                        , autocomplete = autocompleteKey
                                        , context = Dict.empty
                                        , query = query
                                        , limit = 25
                                        }
                                in
                                autocompleteListField
                                    { label = fieldLabel
                                    , mHint = fieldHint
                                    , selectedStrings = strings
                                    , validity = Actions.autocompleteValueValidity autocompleteStateKey model
                                    , suggestions = autocompleteState.suggestions
                                    , activeIndex = autocompleteState.activeIndex
                                    , onQueryChange =
                                        Actions.fetchAutocomplete autocompleteStateKey currentRouteCommit
                                            << autocompleteRequest
                                    , onSuggestionSelect =
                                        \suggestion ->
                                            Actions.clearAutocomplete autocompleteStateKey
                                                |> Flow.seq (addTag suggestion)
                                    , onAddItem =
                                        \val ->
                                            Flow.async (Actions.checkAutocompleteValue autocompleteStateKey currentRouteCommit (autocompleteRequest (String.trim val)))
                                                |> Flow.seq (Actions.clearAutocomplete autocompleteStateKey)
                                                |> Flow.seq (addTag val)
                                    , onRemoveIndex =
                                        \i ->
                                            Flow.modify (over listLens (List.removeAt i))
                                                |> Flow.seq (focus fieldId)
                                    , onActiveIndexChange =
                                        \newIndex ->
                                            Flow.over Lenses.autocomplete
                                                (Dict.insert autocompleteStateKey
                                                    { autocompleteState | activeIndex = newIndex }
                                                )
                                    , readOnly = readOnly
                                    , id = fieldId
                                    , hasChanged = fieldHasChanged
                                    , query = autocompleteState.query
                                    }

                            Nothing ->
                                let
                                    tags =
                                        strings
                                            |> List.map
                                                (\str ->
                                                    { body = Html.text str
                                                    , route = Nothing
                                                    , backgroundColor = Nothing
                                                    }
                                                )
                                in
                                buildListField listLens tags addTag

                    TList (TRecord fieldTypes) ->
                        let
                            listLens =
                                paramLens << lens "withDefault" (Maybe.withDefault (TListValue [])) (\_ -> Just) << tListValue

                            recordValues =
                                all (listLens << each) model

                            getDict rec =
                                case rec of
                                    TRecordValue d ->
                                        d

                                    _ ->
                                        Dict.empty

                            updateField idx fieldName newVal =
                                Flow.modify
                                    (over listLens
                                        (List.updateAt idx
                                            (\rec -> TRecordValue (Dict.insert fieldName newVal (getDict rec)))
                                        )
                                    )

                            viewRecordField idx fieldName fieldArgType =
                                let
                                    currentDict =
                                        List.getAt idx recordValues |> Maybe.map getDict |> Maybe.withDefault Dict.empty

                                    currentVal =
                                        Dict.get fieldName currentDict

                                    fieldId_ =
                                        paramName ++ "-" ++ String.fromInt idx ++ "-" ++ fieldName

                                    recordFieldLabel =
                                        Maybe.withDefault fieldName fieldArgType.displayName
                                in
                                case fieldArgType.type_ of
                                    TString display _ ->
                                        let
                                            strVal =
                                                case currentVal of
                                                    Just (TStringValue s) ->
                                                        s

                                                    _ ->
                                                        ""
                                        in
                                        case display of
                                            TextField ->
                                                textField
                                                    { label = recordFieldLabel
                                                    , mHint = Nothing
                                                    , placeholder = fieldName
                                                    , value = strVal
                                                    , onInput = \s -> updateField idx fieldName (TStringValue s)
                                                    , hasChanged = False
                                                    , readOnly = readOnly
                                                    , id = fieldId_ ++ "-input"
                                                    }

                                            TextArea ->
                                                textArea
                                                    { label = recordFieldLabel
                                                    , mHint = Nothing
                                                    , placeholder = ""
                                                    , value = strVal
                                                    , onInput = \s -> updateField idx fieldName (TStringValue s)
                                                    , hasChanged = False
                                                    , readOnly = readOnly
                                                    , id = fieldId_ ++ "-input"
                                                    }

                                            Command cmdPrefix ->
                                                commandField
                                                    { label = recordFieldLabel
                                                    , mHint = Nothing
                                                    , placeholder = fieldName
                                                    , value = strVal
                                                    , onInput = \s -> updateField idx fieldName (TStringValue s)
                                                    , hasChanged = False
                                                    , readOnly = readOnly
                                                    , id = fieldId_ ++ "-input"
                                                    , commandPrefix = cmdPrefix
                                                    }

                                            Code language ->
                                                codeField
                                                    { label = recordFieldLabel
                                                    , mHint = Nothing
                                                    , value = strVal
                                                    , onInput = \s -> updateField idx fieldName (TStringValue s)
                                                    , hasChanged = False
                                                    , readOnly = readOnly
                                                    , id = fieldId_ ++ "-input"
                                                    , language = language
                                                    }

                                    TInt _ _ ->
                                        let
                                            intVal =
                                                case currentVal of
                                                    Just (TIntValue n) ->
                                                        String.fromInt n

                                                    _ ->
                                                        ""
                                        in
                                        textField
                                            { label = recordFieldLabel
                                            , mHint = Nothing
                                            , placeholder = fieldName
                                            , value = intVal
                                            , onInput = \s ->
                                                case String.toInt s of
                                                    Just n ->
                                                        updateField idx fieldName (TIntValue n)

                                                    Nothing ->
                                                        Flow.none
                                            , hasChanged = False
                                            , readOnly = readOnly
                                            , id = fieldId_ ++ "-input"
                                            }

                                    TBool ->
                                        let
                                            boolVal =
                                                case currentVal of
                                                    Just (TBoolValue b) ->
                                                        b

                                                    _ ->
                                                        False
                                        in
                                        formField
                                            { label = recordFieldLabel
                                            , mHint = Nothing
                                            , id = fieldId_ ++ "-input"
                                            }
                                            (Html.input
                                                [ Html.Attributes.type_ "checkbox"
                                                , id (fieldId_ ++ "-input")
                                                , checked boolVal
                                                , Events.onCheck (\b -> updateField idx fieldName (TBoolValue b))
                                                , class "form-checkbox"
                                                ]
                                                []
                                            )

                                    TEnum enumValues enumDisplayNames ->
                                        formField
                                            { label = recordFieldLabel
                                            , mHint = Nothing
                                            , id = fieldId_ ++ "-input"
                                            }
                                            (Html.select
                                                [ id (fieldId_ ++ "-input")
                                                , class "form-input"
                                                , disabled readOnly
                                                , Events.onInput (\v -> updateField idx fieldName (TEnumValue v))
                                                ]
                                                (List.map
                                                    (\v ->
                                                        Html.option
                                                            [ value v
                                                            , selected
                                                                (case currentVal of
                                                                    Just (TEnumValue current) ->
                                                                        current == v

                                                                    _ ->
                                                                        False
                                                                )
                                                            ]
                                                            [ Html.text (Dict.get v enumDisplayNames |> Maybe.withDefault v) ]
                                                    )
                                                    enumValues
                                                )
                                            )

                                    TList (TString _ mAutocomplete) ->
                                        let
                                            items =
                                                case currentVal of
                                                    Just (TListValue xs) ->
                                                        xs

                                                    _ ->
                                                        []

                                            listId =
                                                fieldId_ ++ "-list-input"

                                            addItem val =
                                                let
                                                    trimmed =
                                                        String.trim val
                                                in
                                                updateField idx fieldName (TListValue (items ++ [ TStringValue trimmed ]))
                                                    |> Flow.seq (focus listId)
                                                    |> Flow.when (not <| String.isEmpty trimmed)
                                        in
                                        case mAutocomplete of
                                            Just autocompleteKey ->
                                                let
                                                    autocompleteStateKey =
                                                        recordAutocompleteStateKey tableId paramName fieldName recordValues idx (TRecordValue currentDict)

                                                    autocompleteState =
                                                        Dict.get autocompleteStateKey (Model.getAutocomplete model)
                                                            |> Maybe.withDefault Model.initAutocompleteState

                                                    recordContext =
                                                        currentDict
                                                            |> Dict.foldl
                                                                (\k v acc ->
                                                                    case v of
                                                                        TEnumValue s ->
                                                                            Dict.insert k s acc

                                                                        TStringValue s ->
                                                                            Dict.insert k s acc

                                                                        _ ->
                                                                            acc
                                                                )
                                                                Dict.empty

                                                    autocompleteRequest query =
                                                        { template = tableId
                                                        , autocomplete = autocompleteKey
                                                        , context = recordContext
                                                        , query = query
                                                        , limit = 25
                                                        }

                                                    packageStrings =
                                                        List.filterMap
                                                            (\v ->
                                                                case v of
                                                                    TStringValue s ->
                                                                        Just s

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                            items
                                                in
                                                autocompleteListField
                                                    { label = recordFieldLabel
                                                    , mHint = Nothing
                                                    , selectedStrings = packageStrings
                                                    , validity = Actions.autocompleteValueValidity autocompleteStateKey model
                                                    , suggestions = autocompleteState.suggestions
                                                    , activeIndex = autocompleteState.activeIndex
                                                    , onQueryChange =
                                                        Actions.fetchAutocomplete autocompleteStateKey currentRouteCommit
                                                            << autocompleteRequest
                                                    , onSuggestionSelect =
                                                        \suggestion ->
                                                            Actions.clearAutocomplete autocompleteStateKey
                                                                |> Flow.seq (addItem suggestion)
                                                    , onAddItem =
                                                        \val ->
                                                            Flow.async (Actions.checkAutocompleteValue autocompleteStateKey currentRouteCommit (autocompleteRequest (String.trim val)))
                                                                |> Flow.seq (Actions.clearAutocomplete autocompleteStateKey)
                                                                |> Flow.seq (addItem val)
                                                    , onRemoveIndex =
                                                        \i ->
                                                            updateField idx fieldName (TListValue (List.removeAt i items))
                                                                |> Flow.seq (focus listId)
                                                    , onActiveIndexChange =
                                                        \newIndex ->
                                                            Flow.over Lenses.autocomplete
                                                                (Dict.insert autocompleteStateKey
                                                                    { autocompleteState | activeIndex = newIndex }
                                                                )
                                                    , readOnly = readOnly
                                                    , id = listId
                                                    , hasChanged = False
                                                    , query = autocompleteState.query
                                                    }

                                            Nothing ->
                                                let
                                                    tags =
                                                        List.filterMap
                                                            (\v ->
                                                                case v of
                                                                    TStringValue s ->
                                                                        Just { body = Html.text s, route = Nothing, backgroundColor = Nothing }

                                                                    _ ->
                                                                        Nothing
                                                            )
                                                            items
                                                in
                                                listField
                                                    { label = recordFieldLabel
                                                    , mHint = Nothing
                                                    , tags = tags
                                                    , onAdd = addItem
                                                    , onRemoveLast =
                                                        updateField idx fieldName (TListValue (List.take (List.length items - 1) items))
                                                            |> Flow.seq (focus listId)
                                                    , onRemoveIndex =
                                                        \i ->
                                                            updateField idx fieldName (TListValue (List.removeAt i items))
                                                                |> Flow.seq (focus listId)
                                                    , readOnly = readOnly
                                                    , id = listId
                                                    , hasChanged = False
                                                    }

                                    _ ->
                                        Html.nothing


                            viewRecord idx _ =
                                Html.div [ class "record-item" ]
                                    [ Html.div [ class "record-item-fields" ]
                                        (List.map
                                            (\( fName, argType_ ) -> viewRecordField idx fName argType_)
                                            (Dict.toList fieldTypes)
                                        )
                                    , Html.viewIf (not readOnly) <|
                                        Html.button
                                            [ Events.onClick (Flow.modify (over listLens (List.removeAt idx)))
                                            , class "remove-record-btn"
                                            , attribute "type" "button"
                                            ]
                                            [ icon True "remove" ]
                                    ]
                        in
                        Html.div [ class "form-field" ]
                            [ Html.label [ class "form-label" ] [ Html.text fieldLabel ]
                            , Html.div [ class "record-list" ]
                                (List.indexedMap viewRecord recordValues
                                    ++ (if readOnly then
                                            []

                                        else
                                            let
                                                defaultRecord =
                                                    TRecordValue
                                                        (Dict.map
                                                            (\_ fieldArgType ->
                                                                case fieldArgType.type_ of
                                                                    TList _ ->
                                                                        TListValue []

                                                                    TBool ->
                                                                        TBoolValue False

                                                                    TEnum (first :: _) _ ->
                                                                        TEnumValue first

                                                                    TEnum [] _ ->
                                                                        TEnumValue ""

                                                                    _ ->
                                                                        TStringValue ""
                                                            )
                                                            fieldTypes
                                                        )
                                            in
                                            [ Html.button
                                                [ Events.onClick (Flow.modify (over listLens (flip (++) [ defaultRecord ])))
                                                , class "add-record-btn"
                                                , attribute "type" "button"
                                                ]
                                                [ Html.text ("Add " ++ fieldLabel) ]
                                            ]
                                       )
                                )
                            ]

                    TList _ ->
                        Html.nothing

                    TUploadHash ->
                        Html.nothing

                    TRecord _ ->
                        Html.nothing
    in
    Html.div [ class "form-group" ] <|
        case stepDef of
            FileUpload _ ->
                []

            Derivation args _ ->
                List.map viewField (Dict.toList args)

            Download ->
                let
                    showDownloadedAt =
                        Maybe.isJust (try (argsLens << key "downloadedAt") model)
                in
                downloadArgs
                    |> Dict.toList
                    |> List.filter (\( paramName, _ ) -> paramName /= "downloadedAt" || showDownloadedAt)
                    |> List.map viewField


viewStepNoteField : Model -> Bool -> String -> Html (Flow Model ())
viewStepNoteField model readOnly tableId =
    let
        noteLens =
            currentTableOf tableId << edited << just << note

        currentNote =
            try noteLens model |> Maybe.withDefault ""

        originalRecord =
            try (currentTableOf tableId << edited << just) model
                |> Maybe.andThen .id
                |> Maybe.andThen (\id_ -> try (currentTableOf tableId << records << success << by .id (Just id_)) model)
    in
    Html.div [ class "form-field" ]
        [ Html.label [ class "form-label", for (tableId ++ "-note-input") ] [ Html.text "Note" ]
        , Html.textarea
            [ value currentNote
            , Events.onInput (Flow.modify << set noteLens)
            , placeholder "Notes about this step..."
            , class "form-input"
            , class "form-input-note"
            , classList [ ( "field-changed", not readOnly && fieldChanged .note currentNote originalRecord ) ]
            , readonly readOnly
            , id (tableId ++ "-note-input")
            ]
            []
        ]


viewLabelWithHint : { label : String, mHint : Maybe String, htmlFor : String } -> Html msg
viewLabelWithHint { label, mHint, htmlFor } =
    case mHint of
        Nothing ->
            Html.label [ class "form-label", for htmlFor ] [ Html.text label ]

        Just hint ->
            Html.div [ class "form-label-group" ]
                [ Html.label [ class "form-label", for htmlFor ] [ Html.text label ]
                , Html.small [ class "form-hint" ] [ Html.text hint ]
                ]


formField : { r | label : String, mHint : Maybe String, id : String } -> Html (Flow Model ()) -> Html (Flow Model ())
formField config inputEl =
    Html.div [ class "form-field" ]
        [ viewLabelWithHint { label = config.label, mHint = config.mHint, htmlFor = config.id }
        , inputEl
        ]


textField :
    { label : String
    , mHint : Maybe String
    , placeholder : String
    , value : String
    , onInput : String -> Flow Model ()
    , hasChanged : Bool
    , readOnly : Bool
    , id : String
    }
    -> Html (Flow Model ())
textField config =
    formField config
        (Html.input
            [ type_ "text"
            , value config.value
            , Events.onInput config.onInput
            , placeholder config.placeholder
            , class "form-input"
            , classList [ ( "field-changed", config.hasChanged ) ]
            , readonly config.readOnly
            , id config.id
            ]
            []
        )


commandField :
    { label : String
    , mHint : Maybe String
    , placeholder : String
    , value : String
    , onInput : String -> Flow Model ()
    , hasChanged : Bool
    , readOnly : Bool
    , id : String
    , commandPrefix : String
    }
    -> Html (Flow Model ())
commandField config =
    formField config
        (Html.div
            [ class "command-input"
            , classList [ ( "field-changed", config.hasChanged ), ( "disabled", config.readOnly ) ]
            ]
            [ Html.span [ class "command-input-prefix" ] [ Html.text config.commandPrefix ]
            , Html.textarea
                [ value config.value
                , placeholder config.placeholder
                , class "command-input-textarea"
                , Events.onInput config.onInput
                , rows 1
                , attribute "data-auto-resize" "true"
                , spellcheck False
                , readonly config.readOnly
                , id config.id
                ]
                []
            ]
        )


textArea :
    { label : String
    , mHint : Maybe String
    , placeholder : String
    , value : String
    , onInput : String -> Flow Model ()
    , hasChanged : Bool
    , readOnly : Bool
    , id : String
    }
    -> Html (Flow Model ())
textArea config =
    formField config
        (Html.textarea
            [ value config.value
            , Events.onInput config.onInput
            , placeholder config.placeholder
            , class "form-input"
            , class "form-input-textarea"
            , classList [ ( "field-changed", config.hasChanged ) ]
            , readonly config.readOnly
            , id config.id
            , rows 1
            , attribute "data-auto-resize" "true"
            ]
            []
        )


codeField :
    { label : String
    , mHint : Maybe String
    , value : String
    , onInput : String -> Flow Model ()
    , hasChanged : Bool
    , readOnly : Bool
    , id : String
    , language : String
    }
    -> Html (Flow Model ())
codeField config =
    formField config
        (Html.node "code-editor"
            [ value config.value
            , Events.onInput config.onInput
            , class "code-input"
            , classList [ ( "field-changed", config.hasChanged ), ( "disabled", config.readOnly ) ]
            , readonly config.readOnly
            , id config.id
            , attribute "language" config.language
            , attribute "aria-label" config.label
            ]
            []
        )


listField :
    { label : String
    , mHint : Maybe String
    , tags :
        List
            { body : Html (Flow Model ())
            , route : Maybe Route
            , backgroundColor : Maybe String
            }
    , onAdd : String -> Flow Model ()
    , onRemoveLast : Flow Model ()
    , onRemoveIndex : Int -> Flow Model ()
    , readOnly : Bool
    , id : String
    , hasChanged : Bool
    }
    -> Html (Flow Model ())
listField config =
    formField config (listFieldTagWrapper config)


autocompleteListField :
    { label : String
    , mHint : Maybe String
    , selectedStrings : List String
    , validity : String -> ApiData Bool
    , suggestions : ApiData (List String)
    , activeIndex : Int
    , onQueryChange : String -> Flow Model ()
    , onSuggestionSelect : String -> Flow Model ()
    , onAddItem : String -> Flow Model ()
    , onRemoveIndex : Int -> Flow Model ()
    , onActiveIndexChange : Int -> Flow Model ()
    , readOnly : Bool
    , id : String
    , hasChanged : Bool
    , query : String
    }
    -> Html (Flow Model ())
autocompleteListField config =
    let
        availableItems =
            case config.suggestions of
                Success items ->
                    items

                _ ->
                    []

        loading =
            case config.suggestions of
                Loading _ ->
                    True

                _ ->
                    False

        error =
            case config.suggestions of
                Error _ ->
                    Just "Could not load suggestions."

                _ ->
                    Nothing
    in
    Combobox.view
        { selected = config.selectedStrings
        , availableItems = availableItems
        , loading = loading
        , error = error
        , toKey = identity
        , toLabel = identity
        , isInvalid = ApiData.unwrap False not << config.validity
        , isPending = ApiData.foldVisible False (always True) (always False) (always False) << config.validity
        , onSelect = config.onSuggestionSelect
        , onRemove = config.onRemoveIndex
        , onCreate = config.onAddItem
        , onInput = config.onQueryChange
        , onActiveIndexChange =
            \newIndex ->
                config.onActiveIndexChange newIndex
                    |> Flow.seq (scrollAutocompleteSuggestion config.id newIndex)
        , inputValue = config.query
        , activeIndex = config.activeIndex
        , allowFreeText = True
        , readOnly = config.readOnly
        , placeholder = ""
        , id = config.id
        , hasChanged = config.hasChanged
        , label = config.label
        , mHint = config.mHint
        }


listFieldTagWrapper :
    { config
        | tags :
            List
                { body : Html (Flow Model ())
                , route : Maybe Route
                , backgroundColor : Maybe String
                }
        , onAdd : String -> Flow Model ()
        , onRemoveLast : Flow Model ()
        , onRemoveIndex : Int -> Flow Model ()
        , readOnly : Bool
        , id : String
        , hasChanged : Bool
    }
    -> Html (Flow Model ())
listFieldTagWrapper config =
    Html.Keyed.node "div"
        [ class "tag-wrapper"
        , class "form-input"
        , classList [ ( "field-changed", config.hasChanged ), ( "disabled", config.readOnly ) ]
        ]
        (List.indexedMap
            (\i t ->
                let
                    colorStyle =
                        Maybe.map (style "background-color") t.backgroundColor
                            |> Maybe.toList

                    chipBody =
                        case t.route of
                            Just route_ ->
                                Html.a
                                    ([ Route.href route_
                                     , class "tag"
                                     , style "text-decoration" "none"
                                     , style "color" "inherit"
                                     ]
                                        ++ colorStyle
                                    )
                                    [ t.body
                                    , Html.viewIf (not config.readOnly) <|
                                        iconCustom True
                                            "close_small"
                                            [ class "remove-selected-icon"
                                            , Events.preventDefaultOn "click" (Decode.succeed ( config.onRemoveIndex i, True ))
                                            ]
                                    ]

                            Nothing ->
                                Html.div (class "tag" :: colorStyle)
                                    [ t.body
                                    , Html.viewIf (not config.readOnly) <|
                                        iconCustom True
                                            "close_small"
                                            [ class "remove-selected-icon"
                                            , Events.onClick (config.onRemoveIndex i)
                                            ]
                                    ]
                in
                ( "tag-" ++ String.fromInt i
                , chipBody
                )
            )
            config.tags
            ++ (if config.readOnly then
                    []

                else
                    [ ( config.id ++ "-" ++ String.fromInt (List.length config.tags)
                      , let
                            handleKey =
                                let
                                    inputVal =
                                        Decode.at [ "target", "value" ] Decode.string

                                    inputEmpty =
                                        inputVal |> Decode.map (String.trim >> String.isEmpty)

                                    baseBindings =
                                        [ ( Keyboard.space
                                          , Decode.ifM (inputEmpty |> Decode.map not) (inputVal |> Decode.map (\v -> ( config.onAdd (String.trim v), True )))
                                          )
                                        , ( Keyboard.enter
                                          , Decode.ifM (inputEmpty |> Decode.map not) (inputVal |> Decode.map (\v -> ( config.onAdd (String.trim v), True )))
                                          )
                                        , ( Keyboard.backspace
                                          , Decode.ifM inputEmpty (Decode.succeed ( config.onRemoveLast, False ))
                                          )
                                        ]
                                in
                                Keyboard.decodeCombinations baseBindings
                        in
                        Html.input
                            [ id config.id
                            , type_ "text"
                            , Events.preventDefaultOn "keydown" handleKey
                            , Events.on "blur"
                                (Decode.at [ "target", "value" ] Decode.string
                                    |> Decode.map
                                        (\v ->
                                            if String.isEmpty (String.trim v) then
                                                Flow.none

                                            else
                                                config.onAdd (String.trim v)
                                        )
                                )
                            , class "list-field-input"
                            , attribute "autocomplete" "off"
                            ]
                            []
                      )
                    ]
               )
        )

fieldChanged : (b -> c) -> c -> Maybe b -> Bool
fieldChanged get currentValue maybeOriginal =
    maybeOriginal
        |> Maybe.map (\orig -> currentValue /= get orig)
        |> Maybe.withDefault False


viewIconButtonWithTooltip : String -> Bool -> String -> Flow Model () -> Html (Flow Model ())
viewIconButtonWithTooltip iconName filled tooltip action =
    Html.button
        [ Events.onClick action
        , class "icon-btn"
        , title tooltip
        ]
        [ icon filled iconName
        , Html.span [ class "icon-btn-text" ] [ Html.text tooltip ]
        ]


viewQuickCreateButton : Maybe String -> String -> Flow Model () -> Html (Flow Model ())
viewQuickCreateButton mIcon tooltip action =
    Html.button
        [ Events.onClick action
        , class "icon-btn quick-create-btn"
        , title tooltip
        ]
        [ Maybe.unwrap
            (icon False "add")
            (\iconName ->
                Html.span [ class "quick-create-glyph" ]
                    [ icon True iconName
                    , iconCustom False "add" [ class "quick-create-add-icon" ]
                    ]
            )
            mIcon
        , Html.span [ class "icon-btn-text" ] [ Html.text tooltip ]
        ]


viewRunButton : String -> Flow Model () -> Html (Flow Model ())
viewRunButton =
    viewIconButtonWithTooltip "play_arrow" True


viewStopButton : String -> Flow Model () -> Html (Flow Model ())
viewStopButton =
    viewIconButtonWithTooltip "stop" True


viewUploadButton : Flow Model () -> Html (Flow Model ())
viewUploadButton =
    viewIconButtonWithTooltip "upload_file" True "Upload files"


viewUploadProgress : Int -> UploadProgress -> Html (Flow Model ())
viewUploadProgress stepId { sent, size } =
    let
        pct =
            if size == 0 then
                0

            else
                toFloat sent / toFloat size * 100
    in
    Html.div
        [ class "upload-progress"
        , title (String.fromInt (round pct) ++ "%")
        ]
        [ Html.div [ class "upload-progress-bar" ]
            [ Html.div
                [ class "upload-progress-fill"
                , style "width" (String.fromInt (round pct) ++ "%")
                ]
                []
            ]
        , viewIconButtonWithTooltip "close" False "Cancel upload" (Actions.cancelUpload stepId)
        ]


dirButton : Bool -> List String -> Int -> Html (Flow Model ())
dirButton isOpen dirPath recordId =
    viewIconButtonWithTooltip
        (if isOpen then
            "folder_open"

         else
            "folder"
        )
        True
        "Browse output files"
        (Actions.toggleOutputEntry recordId Nothing dirPath |> Flow.map (always ()))




recordAutocompleteStateKey : String -> String -> String -> List StepArgValue -> Int -> StepArgValue -> String
recordAutocompleteStateKey tableId paramName fieldName recordValues idx recordValue =
    let
        duplicateOrdinal =
            recordValues
                |> List.take idx
                |> List.filter ((==) recordValue)
                |> List.length
    in
    tableId
        ++ ":"
        ++ paramName
        ++ ":"
        ++ stepArgValueKey recordValue
        ++ ":"
        ++ String.fromInt duplicateOrdinal
        ++ ":"
        ++ fieldName


stepArgValueKey : StepArgValue -> String
stepArgValueKey value =
    let
        keyPart tag body =
            tag ++ String.fromInt (String.length body) ++ ":" ++ body
    in
    case value of
        TStringValue str ->
            keyPart "string" str

        TIntValue n ->
            keyPart "int" (String.fromInt n)

        TBoolValue b ->
            keyPart "bool" (if b then "true" else "false")

        TStepValue stepId ->
            keyPart "step" (String.fromInt stepId)

        TUploadHashValue hash ->
            keyPart "upload" hash

        TListValue values ->
            values
                |> List.map stepArgValueKey
                |> String.concat
                |> keyPart "list"

        TRecordValue fields ->
            fields
                |> Dict.toList
                |> List.map (\( name, fieldValue ) -> keyPart "field" name ++ stepArgValueKey fieldValue)
                |> String.concat
                |> keyPart "record"

        TEnumValue enumValue ->
            keyPart "enum" enumValue


scrollAutocompleteSuggestion : String -> Int -> Flow Model ()
scrollAutocompleteSuggestion comboboxId index =
    Flow.attemptTask
        (Scroll.scrollElementY
            (comboboxId ++ "-suggestions")
            (comboboxId ++ "-suggestion-" ++ String.fromInt index)
            0.5
            0
        )


focus : String -> Flow Model ()
focus =
    Flow.attemptTask << Dom.focus
