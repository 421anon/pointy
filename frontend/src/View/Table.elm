module View.Table exposing (viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewRunButton, viewStopButton, viewTable, viewUploadButton, viewUploadProgress)

import Accessors exposing (all, each, just, key, lens, over, set, try)
import Actions
import Ansi.Log as AnsiLog
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Components.Combobox as Combobox
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
import Markdown
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, Model, Status(..), Table, TableTag(..), TemplateSource(..), UploadProgress, dndSystem, getSortKey)
import Model.Lenses as Lenses exposing (allEntities, argSelectStates, args, currentProject, currentProjectId, currentTableOf, dndAffected, edited, mCommit, note, presetSelect, projectStepRecords, projects, projectsContainingEntity, records, route, selectExistingSteps, tables, templatesSelect)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepType(..), TStringDisplay(..), downloadArgs, tEnumValue, tIntValue, tListValue, tStepId, tStringValue)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route exposing (Route)
import Scroll
import Set
import Time.Distance
import View.Icons exposing (icon, iconCustom)


viewStatusCountBadge : TableSpec (BaseRecord a) -> List (BaseRecord a) -> Html msg
viewStatusCountBadge spec allRecords =
    let
        statusOf r =
            TableSpec.getStatus spec r |> ApiData.toMaybe

        totalCount =
            List.length allRecords

        count pred =
            List.count (statusOf >> pred) allRecords

        format ( n, label ) =
            if n > 0 then
                Just (String.fromInt n ++ " " ++ label)

            else
                Nothing

        statusDetails =
            [ ( count ((==) (Just Model.StatusRunning)), "running" )
            , ( count ((==) (Just Model.StatusSuccess)), "done" )
            , ( count (Maybe.unwrap False (\s -> s /= Model.StatusNotStarted && s /= Model.StatusRunning && s /= Model.StatusSuccess)), "failed" )
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
    , specificRecordActions : BaseRecord a -> List (Html (Flow Model ()))
    , alwaysVisibleRecordActions : BaseRecord a -> List (Html (Flow Model ()))
    , directorySection : BaseRecord a -> Html (Flow Model ())
    , srcFilesSection : BaseRecord a -> Html (Flow Model ())
    , onRecordClick : BaseRecord a -> Maybe (Flow Model ())
    , isOpen : BaseRecord a -> Bool
    , isSrcOpen : BaseRecord a -> Bool
    }
    -> Html (Flow Model ())
viewTable { model, spec, table, specificRecordActions, alwaysVisibleRecordActions, directorySection, srcFilesSection, onRecordClick, isOpen, isSrcOpen } =
    let
        lens =
            TableSpec.getLens spec

        currentRouteCommit =
            case (Model.getRoute model).page of
                Route.Project { mCommit } ->
                    mCommit

                _ ->
                    Nothing

        ( highlightedEntityId, isReadOnly ) =
            case (Model.getRoute model).page of
                Route.Project { mHighlight, mCommit } ->
                    ( mHighlight, Maybe.isJust mCommit )

                _ ->
                    ( Nothing, False )

        tableActionBtn action className content =
            Html.button
                [ Events.onClick action
                , Events.stopPropagationOn "click" (Decode.succeed ( action, True ))
                , class className
                ]
                content

        mProjectId =
            try currentProjectId model

        recordActions =
            [ -- Directory button
              { shouldShow = \record -> TableSpec.getStatus spec record == Success StatusSuccess
              , render = \record -> Html.viewMaybe (dirButton (isOpen record) []) record.id
              }
            , -- Source directory button
              { shouldShow = \record -> TableSpec.getSrcFilesView spec record /= Nothing
              , render = \record -> Html.viewMaybe (dirCodeButton (isSrcOpen record) []) record.id
              }
            , -- Edit button
              { shouldShow = \record -> not isReadOnly && record.id /= Nothing
              , render = \record -> viewIconButtonWithTooltip "edit" True "Edit" <| Actions.toggleAddOrEditRecordForm False spec record.id
              }
            , -- Inspect Parameters button (shareable only)
              { shouldShow = \record -> TableSpec.getShareable spec record && record.id /= Nothing
              , render =
                    \record -> viewIconButtonWithTooltip "data_info_alert" True "Inspect Parameters" (Actions.toggleAddOrEditRecordForm True spec record.id)
              }
            , -- Share button (shareable only)
              { shouldShow = \record -> TableSpec.getShareable spec record && record.id /= Nothing
              , render =
                    \record ->
                        viewIconButtonWithTooltip
                            "share"
                            True
                            "Share"
                            (Maybe.map2 (\projectId recordId -> Actions.shareEntity projectId recordId Route.Output [] Nothing)
                                mProjectId
                                record.id
                                |> Maybe.withDefault Flow.none
                            )
              }
            , -- Visibility toggle button
              { shouldShow = \record -> not isReadOnly && record.id /= Nothing
              , render =
                    \record ->
                        viewIconButtonWithTooltip
                            (if record.hidden then
                                "visibility"

                             else
                                "visibility_off"
                            )
                            True
                            (if record.hidden then
                                "Show"

                             else
                                "Hide"
                            )
                            (Actions.toggleRecordVisibility spec mProjectId Nothing record)
              }
            , -- Clone button (shareable only)
              { shouldShow = \record -> not isReadOnly && TableSpec.getShareable spec record
              , render = \record -> viewIconButtonWithTooltip "content_copy" False "Clone" (TableSpec.getCloneRecord spec record)
              }
            , -- Remove button
              { shouldShow =
                    \record ->
                        let
                            hasDependentInProject =
                                False
                        in
                        not isReadOnly
                            && record.id
                            /= Nothing
                            && (not (TableSpec.getShareable spec record) || not hasDependentInProject)
              , render = \record -> Html.viewMaybe (viewIconButtonWithTooltip "delete" False "Remove" << Actions.removeRecord spec) record.id
              }
            ]

        viewRecord index record =
            let
                isHighlighted =
                    Maybe.map2 (==) (Maybe.map .id highlightedEntityId) record.id
                        |> Maybe.withDefault False

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
                    case ( s, record.id ) of
                        ( StatusFailure _, Just stepId ) ->
                            let
                                logKey =
                                    Model.stepLogKey stepId currentRouteCommit

                                logState =
                                    Dict.get logKey (Model.getStepLogs model) |> Maybe.withDefault NotAsked

                                popoverId =
                                    "step-log-popover-" ++ TableSpec.getName spec ++ "-" ++ String.fromInt stepId
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
                                        , Html.button
                                            [ class "icon-btn"
                                            , title "Close"
                                            , Events.onClick (Actions.hidePopover popoverId)
                                            ]
                                            [ icon True "close" ]
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

                viewStatusApiData aStatus =
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
                        aStatus

                recordNameEditable =
                    let
                        editing =
                            Maybe.unwrap False
                                (\r -> r.id == record.id && record.id /= Nothing)
                                table.edited
                    in
                    if editing && table.nameEditOnly then
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
                            , iconCustom True
                                "edit"
                                [ class "edit-icon"
                                , Events.stopPropagationOn "click" (Decode.succeed ( Actions.startInlineRecordNameEdit spec record, True ))
                                ]
                            ]

                viewUnmovedRecord attrs mkTargetAttrs =
                    let
                        itemId =
                            Maybe.unwrap (TableSpec.getName spec ++ "-new") String.fromInt record.id

                        actionsContainerClass =
                            "table-record-actions-container"

                        mtimeBadge =
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
                                        [ Html.text (Time.Distance.inWords posix (Model.getNow model)) ]
                                )
                                record.lastModifiedAt
                    in
                    Html.div
                        ([ class "table-record", id itemId ] ++ attrs)
                        [ Html.div
                            ([ class "table-record-header"
                             , classList
                                [ ( "hidden", record.hidden )
                                , ( "highlighted", isHighlighted )
                                , ( "no-status", TableSpec.getTag spec == TagProjects && List.isEmpty (TableSpec.getValidationErrors spec record) )
                                ]
                             ]
                                ++ (if record.id /= Nothing && (TableSpec.getTag spec == TagProjects || TableSpec.getStatus spec record == Success StatusSuccess) then
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
                            [ case TableSpec.getValidationErrors spec record of
                                [] ->
                                    case TableSpec.getTag spec of
                                        TagProjects ->
                                            Html.nothing

                                        _ ->
                                            viewStatusApiData (TableSpec.getStatus spec record)

                                errors ->
                                    Html.span
                                        [ class "project-error-indicator"
                                        , title (String.join "\n" errors)
                                        ]
                                        [ iconCustom True "error" [] ]
                            , Html.span [ class "table-record-name" ]
                                [ recordNameEditable
                                , mtimeBadge
                                , Html.viewIf (record.id == Nothing) <|
                                    Html.span [ class "pending-record-indicator", title "Saving..." ]
                                        [ iconCustom True "progress_activity" [ class "pending-record-icon" ]
                                        ]
                                ]
                            , let
                                popoverId =
                                    "actions-popover-" ++ TableSpec.getName spec ++ "-" ++ String.fromInt (Maybe.withDefault -1 record.id)
                              in
                              Html.div
                                [ class actionsContainerClass ]
                                [ Html.button
                                    [ class "icon-btn hamburger-icon-btn-mobile"
                                    , attribute "popovertarget" popoverId
                                    , style "anchor-name" ("--anchor-" ++ popoverId)
                                    ]
                                    [ icon True "more_vert" ]
                                , Html.div
                                    [ class "table-record-actions"
                                    , id popoverId
                                    , attribute "popover" "auto"
                                    , style "position-anchor" ("--anchor-" ++ popoverId)
                                    , Events.on "click" (Decode.succeed (Actions.hidePopover popoverId))
                                    ]
                                    (specificRecordActions record
                                        ++ List.filterMap
                                            (\recordActionBtn ->
                                                if recordActionBtn.shouldShow record then
                                                    Just (recordActionBtn.render record)

                                                else
                                                    Nothing
                                            )
                                            recordActions
                                    )
                                , Html.div [] (alwaysVisibleRecordActions record)
                                , Html.viewIf (not isReadOnly) <|
                                    Html.div (class "table-record-drag-target" :: List.map (map (Actions.dndMsgToIO mProjectId spec)) (mkTargetAttrs itemId))
                                        [ icon True "drag_indicator" ]
                                , mtimeBadge
                                ]
                            ]
                        , Html.viewIf (TableSpec.getSrcFilesView spec record |> Maybe.map .expanded |> Maybe.withDefault False) (srcFilesSection record)
                        , let
                            editing =
                                Maybe.unwrap False
                                    (\r -> r.id == record.id && record.id /= Nothing)
                                    table.edited
                          in
                          Html.viewIf (editing && not table.nameEditOnly)
                            (Html.viewMaybe (viewAddOrEditRecordForm model spec table)
                                table.edited
                            )
                        , Html.viewIf (TableSpec.getDirectoryView spec record |> Maybe.map .expanded |> Maybe.withDefault False) (directorySection record)
                        ]
            in
            Html.Keyed.node "div"
                []
                (case dndSystem.info table.dnd of
                    Just { dragIndex } ->
                        if dragIndex /= index then
                            [ ( "record-" ++ String.fromInt index, viewUnmovedRecord [] (dndSystem.dropEvents index) ) ]

                        else
                            [ ( "placeholder", viewUnmovedRecord [ class "zero-opacity" ] (always []) )
                            , ( "ghost", viewUnmovedRecord (class "dnd-ghost" :: (List.map (map (always Flow.none)) <| dndSystem.ghostStyles table.dnd)) (always []) )
                            ]

                    Nothing ->
                        [ ( "record-" ++ String.fromInt index, viewUnmovedRecord [] (dndSystem.dragEvents index) ) ]
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
                                        List.sortBy getSortKey
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
                        [ Html.viewIf (not isReadOnly && ApiData.unwrap False (List.any .hidden) table.records) <|
                            tableActionBtn (Actions.toggleShowHiddenRecords lens)
                                "btn"
                                [ Html.text
                                    (if table.showHiddenRecords then
                                        "Hide Hidden"

                                     else
                                        "Show Hidden"
                                    )
                                ]
                        , Html.viewIf (not isReadOnly && ApiData.unwrap False (List.any .hidden) table.records) <|
                            tableActionBtn
                                (ApiData.unwrap (Flow.pure ())
                                    (Flow.batchM << List.map (Actions.toggleRecordVisibility spec mProjectId (Just False)))
                                    table.records
                                )
                                "btn"
                                [ Html.text "Unhide All" ]
                        , Html.viewIf (not isReadOnly) <| tableActionBtn (Actions.toggleAddOrEditRecordForm False spec Nothing) "icon-btn" [ icon True "add" ]
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
                    |> Maybe.map (viewAddOrEditRecordForm model spec table)
                    |> Maybe.withDefault Html.nothing
                , Html.viewIf table.isOpen viewRecordsSection
                ]
    in
    viewContent


viewAddOrEditRecordForm : Model -> TableSpec (BaseRecord a) -> Table (BaseRecord a) -> BaseRecord a -> Html (Flow Model ())
viewAddOrEditRecordForm model spec table record =
    let
        editing =
            record.id /= Nothing && (table.addMode /= AddFromOtherProject)

        readOnly =
            table.inspected

        extraFields =
            case TableSpec.getTag spec of
                TagSteps key stepDef ->
                    [ viewStepExtraFormFields model readOnly key stepDef ]

                TagProjects ->
                    viewProjectExtraFormFields model readOnly

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
            let
                displayName =
                    TableSpec.getDisplayName spec
            in
            case ( readOnly, editing, table.addMode ) of
                ( False, False, AddNew ) ->
                    "Create new " ++ displayName

                ( False, False, AddFromOtherProject ) ->
                    "Add from other project: " ++ displayName

                ( False, True, _ ) ->
                    "Edit " ++ displayName

                ( True, _, _ ) ->
                    "Inspect Parameters"

        handleEnter =
            let
                targetDecoder =
                    Decode.map3
                        (\tag id value -> { tag = tag, id = id, value = value })
                        (Decode.at [ "target", "tagName" ] Decode.string)
                        (Decode.at [ "target", "id" ] Decode.string |> Decode.maybe |> Decode.map (Maybe.withDefault ""))
                        (Decode.at [ "target", "value" ] Decode.string |> Decode.maybe |> Decode.map (Maybe.withDefault ""))

                allowEnter target =
                    target.tag /= "TEXTAREA" && target.id /= "save-button" && target.id /= "select-input" && not (String.endsWith "-list-input" target.id)
            in
            Keyboard.decodeCombinations
                [ ( Keyboard.enter
                  , Decode.field "target" (Decode.whenNotInside "code-input" (TableSpec.getUpsertRecord spec)) |> Decode.when targetDecoder allowEnter
                  )
                ]
    in
    Html.div [ class "table-form-wrapper" ]
        [ Html.div [ formClasses, Events.on "keydown" handleEnter ]
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
                , Html.div [ class "form-actions" ]
                    [ Html.viewIf (not readOnly) <| Html.button [ id "save-button", Events.onClick (TableSpec.getUpsertRecord spec), class "btn", disabled table.isUpdating ] [ Html.text "Save" ]
                    , Html.button [ Events.onClick (Actions.endRecordEdit (TableSpec.getLens spec)), class "btn" ] [ Html.text "Cancel" ]
                    ]
                ]
            ]
        ]


viewProjectExtraFormFields : Model -> Bool -> List (Html (Flow Model ()))
viewProjectExtraFormFields model readOnly =
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
                        , readOnly = readOnly
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
                        , readOnly = readOnly
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
                        , Html.div [ class "field-notice-markdown" ] <| Markdown.toHtml Nothing notice.message
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
                    TStep mAllowedStepTypes ->
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

                    TList (TStep mAllowedStepTypes) ->
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
                                    (Html.div [ class "record-item-fields" ]
                                        (List.map
                                            (\( fName, argType_ ) -> viewRecordField idx fName argType_)
                                            (Dict.toList fieldTypes)
                                        )
                                        :: (if readOnly then
                                                []

                                            else
                                                [ Html.button
                                                    [ Events.onClick (Flow.modify (over listLens (List.removeAt idx)))
                                                    , class "remove-record-btn"
                                                    , attribute "type" "button"
                                                    ]
                                                    [ icon True "remove" ]
                                                ]
                                           )
                                    )
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
    formField config (listFieldTagWrapper Nothing Nothing config)


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
    Maybe { value : String, onInput : String -> Flow Model () }
    -> Maybe (List ( Keyboard.Combination, Decode.Decoder ( Flow Model (), Bool ) ))
    ->
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
listFieldTagWrapper mOnInput mExtraKeyBindings config =
    Html.Keyed.node "div"
        [ class "tag-wrapper"
        , class "form-input"
        , classList [ ( "field-changed", config.hasChanged ) ]
        , classList [ ( "disabled", config.readOnly ) ]
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
                                    , if config.readOnly then
                                        Html.nothing

                                      else
                                        iconCustom True
                                            "close_small"
                                            [ class "remove-selected-icon"
                                            , Events.preventDefaultOn "click" (Decode.succeed ( config.onRemoveIndex i, True ))
                                            ]
                                    ]

                            Nothing ->
                                Html.div (class "tag" :: colorStyle)
                                    [ t.body
                                    , if config.readOnly then
                                        Html.nothing

                                      else
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
            ++ [ ( config.id ++ "-" ++ String.fromInt (List.length config.tags)
                 , if config.readOnly then
                    Html.nothing

                   else
                    let
                        handleKey =
                            let
                                inputVal =
                                    Decode.at [ "target", "value" ] Decode.string

                                inputEmpty =
                                    inputVal |> Decode.map (String.trim >> String.isEmpty)

                                autocompleteBindings =
                                    Maybe.withDefault [] mExtraKeyBindings

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
                            Keyboard.decodeCombinations (autocompleteBindings ++ baseBindings)
                    in
                    Html.input
                        ([ id config.id
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
                            ++ (mOnInput
                                    |> Maybe.map (\inputConfig -> [ Events.onInput inputConfig.onInput, value inputConfig.value ])
                                    |> Maybe.withDefault []
                               )
                        )
                        []
                 )
               ]
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


dirCodeButton : Bool -> List String -> Int -> Html (Flow Model ())
dirCodeButton isOpen dirPath recordId =
    viewIconButtonWithTooltip
        (if isOpen then
            "folder_open"

         else
            "folder_code"
        )
        True
        "Browse source files"
        (Actions.toggleSrcEntry recordId Nothing dirPath |> Flow.map (always ()))


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
