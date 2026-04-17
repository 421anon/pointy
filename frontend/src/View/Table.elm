module View.Table exposing (viewIconButtonWithTooltip, viewRunButton, viewStopButton, viewTable, viewUploadButton, viewUploadProgress)

import Accessors exposing (all, each, just, key, lens, over, set, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Basics.Extra exposing (flip)
import Browser.Dom as Dom
import Components.Select as Select
import Dict
import Extra.Accessors exposing (by, where_)
import Extra.Decode as Decode
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Keyboard
import Lib.StringColor exposing (stringToColor)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, Model, Status(..), Table, TableTag(..), UploadProgress, dndSystem, getSortKey)
import Model.Lenses exposing (allEntities, argSelectStates, args, currentProject, currentProjectId, currentTableOf, dndAffected, edited, mCommit, projectStepRecords, projects, projectsContainingEntity, records, route, selectExistingSteps, tables)
import Model.Shadow exposing (StepArgType(..), StepArgValue(..), StepType(..), TStringDisplay(..), tListValue, tStepId, tStringValue)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Route exposing (Route)
import Set
import View.Icons exposing (icon, iconCustom)
import View.Lib exposing (viewLoading)


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

        ( highlightedEntityId, isReadOnly ) =
            case Model.getRoute model of
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
                            (Maybe.map2 (\projectId recordId -> Actions.shareEntity projectId recordId [ "output" ])
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
                    in
                    Html.div
                        ([ class "table-record", id itemId ] ++ attrs)
                        [ Html.div
                            ([ class "table-record-header"
                             , classList
                                [ ( "hidden", record.hidden )
                                , ( "highlighted", isHighlighted )
                                , ( "no-status", TableSpec.getTag spec == TagProjects )
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
                            [ case TableSpec.getTag spec of
                                TagProjects ->
                                    Html.nothing

                                _ ->
                                    viewStatusApiData (TableSpec.getStatus spec record)
                            , Html.span [ class "table-record-name" ]
                                [ recordNameEditable
                                , Html.viewMaybe
                                    (\id_ ->
                                        Html.span [ class "table-record-id", title <| "id: " ++ String.fromInt id_ ]
                                            [ Html.text (String.fromInt id_) ]
                                    )
                                    record.id
                                , Html.viewIf (record.id == Nothing) <|
                                    Html.span [ class "pending-record-indicator", title "Saving..." ]
                                        [ iconCustom True "progress_activity" [ class "pending-record-icon" ]
                                        ]
                                , Html.viewIf (TableSpec.getShareable spec record && TableSpec.getTag spec /= TagProjects && record.id /= Nothing) <|
                                    Html.span
                                        [ class "status-icon shareable-icon"
                                        , title "Shareable – inspect, clone, or share this step state"
                                        ]
                                        [ icon True "share" ]
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

        viewContent =
            Html.div [ class "table", id ("table-" ++ TableSpec.getName spec) ]
                [ Html.div [ Events.onClick (Actions.toggleTable lens), class "table-header" ]
                    [ Html.div [ class "table-header-content" ]
                        [ icon True
                            (if table.isOpen then
                                "expand_more"

                             else
                                "chevron_right"
                            )
                        , Html.span [ class "table-content-header" ] [ Html.text (TableSpec.getName spec) ]
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

                            else if table.addMode == AddExisting then
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
    if table.isUpdating then
        viewLoading viewContent

    else
        viewContent


viewAddOrEditRecordForm : Model -> TableSpec (BaseRecord a) -> Table (BaseRecord a) -> BaseRecord a -> Html (Flow Model ())
viewAddOrEditRecordForm model spec table record =
    let
        editing =
            record.id /= Nothing && (table.addMode /= AddExisting)

        readOnly =
            table.inspected

        extraFields =
            case TableSpec.getTag spec of
                TagSteps key stepDef ->
                    [ viewStepExtraFormFields model readOnly key stepDef ]

                TagProjects ->
                    []

        nameInput =
            let
                originalRecord =
                    try (records << success << by .id record.id) table
            in
            textField
                { label = "Name:"
                , placeholder = TableSpec.getName spec ++ " name"
                , value = record.name
                , onInput = Actions.editRecordName (TableSpec.getLens spec)
                , hasChanged = fieldChanged .name record.name originalRecord
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
                [ radioButton AddNew "Create New"
                , radioButton AddExisting "Add Existing"
                ]

        viewSelectExisting state =
            let
                mProjectId =
                    try currentProjectId model

                availableItems =
                    List.map (\{ id, name } -> { id = id, name = name })
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
                    "Create New " ++ displayName

                ( False, False, AddExisting ) ->
                    "Add Existing " ++ displayName

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
                    target.tag /= "TEXTAREA" && target.id /= "save-button" && target.id /= "select-input" && (not (String.endsWith "-list-input" target.id) || String.isEmpty (String.trim target.value))
            in
            Keyboard.decodeCombinations
                [ ( Keyboard.enter
                  , Decode.succeed (TableSpec.getUpsertRecord spec) |> Decode.when targetDecoder allowEnter
                  )
                ]
    in
    Html.div [ class "table-form-wrapper" ]
        [ Html.div [ formClasses, Events.on "keydown" handleEnter ]
            [ Html.header [ class "form-header" ] [ Html.text headerTitle ]
            , Html.div [ class "form-body" ]
                [ Html.viewIf (not editing && TableSpec.getTag spec /= TagProjects) modeSelector
                , Html.viewIf (not editing && table.addMode == AddExisting && TableSpec.getTag spec /= TagProjects) <| Html.Lazy.lazy viewSelectExisting table.selectExistingSteps
                , Html.viewIf (not editing && table.addMode == AddNew || editing) nameInput
                , Html.viewIf ((not editing && table.addMode == AddNew || editing) && not (List.isEmpty extraFields)) <|
                    Html.div [ class "form-group" ] extraFields
                , Html.div [ class "form-actions" ]
                    [ Html.viewIf (not readOnly) <| Html.button [ id "save-button", Events.onClick (TableSpec.getUpsertRecord spec), class "btn" ] [ Html.text "Save" ]
                    , Html.button [ Events.onClick (Actions.endRecordEdit (TableSpec.getLens spec)), class "btn" ] [ Html.text "Cancel" ]
                    ]
                ]
            ]
        ]


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

        viewField ( paramName, { type_ } ) =
            let
                paramLens =
                    argsLens << key paramName

                fieldId =
                    paramName
                        ++ (case type_ of
                                TList (TString _) ->
                                    "-list-input"

                                _ ->
                                    "-input"
                           )

                fieldHasChanged =
                    fieldChanged (try (args << key paramName)) (try paramLens model) originalRecord

                buildListField listLens tagStrings addTag =
                    listField
                        { label = paramName ++ ":"
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
                                        }
                                    )

                        selectedIds =
                            List.map .id selectedItems

                        availableItems =
                            allSteps mAllowedStepTypes
                                |> List.filterMap (\step -> step.id |> Maybe.map (\id -> { id = Just id, name = step.name }))
                                |> List.filter (\item -> not (List.member item.id selectedIds))

                        toTooltip =
                            .id >> Maybe.unwrap [] (\id -> [ "id: " ++ String.fromInt id ])

                        toHighlightRoute stepId =
                            try currentProjectId model
                                |> Maybe.map
                                    (\projectId ->
                                        let
                                            mCommit_ =
                                                try (route << Route.project << mCommit << just) model
                                        in
                                        Route.Project
                                            { projectId = projectId
                                            , mHighlight = Just { id = stepId, path = [] }
                                            , mCommit = mCommit_
                                            }
                                    )
                    in
                    Select.view
                        { optic = stateLens
                        , selectState = try stateLens model |> Maybe.withDefault Select.initSelectState
                        , selected_ = selectedItems
                        , availableItems = availableItems
                        , readOnly = readOnly
                        , hasChanged = fieldHasChanged
                        , label = paramName ++ ":"
                        , placeholder = ""
                        , inputIcon = Nothing
                        , toInputItemName = .name
                        , toInputItemTooltip = toTooltip
                        , onInputItemClick = .id >> Maybe.andThen toHighlightRoute >> Maybe.map Actions.goToRoute
                        , toMenuItemName =
                            \item ->
                                Maybe.map2 (\id s -> "[" ++ String.fromInt id ++ "] [" ++ s.type_ ++ "] " ++ item.name) item.id (getStep item.id) |> Maybe.withDefault item.name
                        , toMenuItemTooltip = toTooltip
                        , onChange = Flow.pure ()
                        , onRemove = .id >> Maybe.unwrap (Flow.pure ()) onRemoveStep
                        , activeAfterSelect = activeAfterSelect
                        , clearInputAfterSelect = True
                        , onSelect = .id >> Maybe.unwrap (Flow.pure ()) onSelectStep
                        , alignRight = False
                        , inputItemStyle = \item -> getStep item.id |> Maybe.map (.type_ >> stringToColor >> style "background-color") |> Maybe.toList
                        }
            in
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

                TString display ->
                    case display of
                        TextField ->
                            textField
                                { label = paramName ++ ":"
                                , placeholder = paramName
                                , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                , onInput = Flow.modify << set paramLens << Just << TStringValue
                                , hasChanged = fieldChanged (try (args << key paramName)) (try paramLens model) originalRecord
                                , readOnly = readOnly
                                , id = paramName ++ "-input"
                                }

                        TextArea ->
                            textArea
                                { label = paramName ++ ":"
                                , placeholder = ""
                                , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                , onInput = Flow.modify << set paramLens << Just << TStringValue
                                , hasChanged = fieldChanged (try (args << key paramName)) (try paramLens model) originalRecord
                                , readOnly = readOnly
                                , id = paramName ++ "-input"
                                }

                        Command cmdPrefix ->
                            commandField
                                { label = paramName ++ ":"
                                , placeholder = paramName
                                , value = Maybe.withDefault "" <| try (paramLens << just << tStringValue) model
                                , onInput = Flow.modify << set paramLens << Just << TStringValue
                                , hasChanged = fieldChanged (try (args << key paramName)) (try paramLens model) originalRecord
                                , readOnly = readOnly
                                , id = paramName ++ "-input"
                                , commandPrefix = cmdPrefix
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

                TList (TString _) ->
                    let
                        listLens =
                            paramLens << lens "withDefault" (Maybe.withDefault (TListValue [])) (\_ -> Just) << tListValue

                        tags =
                            all (listLens << each << tStringValue) model
                                |> List.map
                                    (\str ->
                                        { body = Html.text str
                                        , route = Nothing
                                        , backgroundColor = Nothing
                                        }
                                    )

                        addTag val =
                            let
                                trimmed =
                                    String.trim val
                            in
                            Flow.modify (over listLens (flip (++) [ TStringValue trimmed ]))
                                |> Flow.seq (focus fieldId)
                                |> Flow.when (not <| String.isEmpty trimmed)
                    in
                    buildListField listLens tags addTag

                TList _ ->
                    Html.nothing

                TUploadHash ->
                    Html.nothing
    in
    Html.div [ class "form-group" ] <|
        case stepDef of
            FileUpload _ ->
                []

            Derivation args _ ->
                List.map viewField (Dict.toList args)


textField :
    { label : String
    , placeholder : String
    , value : String
    , onInput : String -> Flow Model ()
    , hasChanged : Bool
    , readOnly : Bool
    , id : String
    }
    -> Html (Flow Model ())
textField config =
    Html.div [ class "form-field" ]
        [ Html.label [ class "form-label" ] [ Html.text config.label ]
        , Html.input
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
        ]


commandField :
    { label : String
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
    Html.div [ class "form-field" ]
        [ Html.label [ class "form-label" ] [ Html.text config.label ]
        , Html.div
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
        ]


textArea :
    { label : String
    , placeholder : String
    , value : String
    , onInput : String -> Flow Model ()
    , hasChanged : Bool
    , readOnly : Bool
    , id : String
    }
    -> Html (Flow Model ())
textArea config =
    Html.div [ class "form-field" ]
        [ Html.label [ class "form-label" ] [ Html.text config.label ]
        , Html.textarea
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
        ]


listField :
    { label : String
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
    Html.div [ class "form-field" ]
        [ Html.label [ class "form-label" ] [ Html.text config.label ]
        , Html.Keyed.node "div"
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
                                in
                                Keyboard.decodeCombinations
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
                            ]
                            []
                     )
                   ]
            )
        ]


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


focus : String -> Flow Model ()
focus =
    Flow.attemptTask << Dom.focus
