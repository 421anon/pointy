module View.Table exposing (ListingRow, actionsPopoverId, hasBrowsableOutput, stepFormReadOnly, viewAddOrEditRecordForm, viewIconButtonWithTooltip, viewIngestProgress, viewListing, viewProjectExtraFormFields, viewRecordActionsPopover, viewRowActions, viewStepExtraFormFields, viewStepNoteField, viewStepRecordActions, viewStepRecordStatus, viewUploadProgress)

import Accessors exposing (has, just, key, lens, set, try)
import Actions
import Ansi.Log as AnsiLog
import Api.ApiData as ApiData exposing (ApiData(..))
import Browser.Dom as Dom
import Components.Combobox as Combobox
import Components.Markdown as Markdown
import Components.Select as Select
import Dict
import Extra.Accessors exposing (A_Traversal, by, remkT, where_)
import Extra.Decode as Decode
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events as Events
import Html.Extra as Html
import Html.Keyed
import Html.Lazy
import Ingest
import Iso8601
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Keyboard
import Lib.StringColor exposing (stringToColor)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, ChildLink, ListingSort(..), Model, ProjectRecord, Status(..), StepRecord, Table, TemplateSource(..), UploadProgress)
import Model.Lenses as Lenses exposing (argSelectStates, args, edited, isReadOnlyPage, isReadOnlyRoute, note, presetSelect, route, selectExistingSteps, stepRecordById, templatesSelect)
import Model.Lib
import Model.Selection
import Model.Shadow exposing (Field, StepArgValue(..), StepConfig, StepConfigEntry, StepType(..), Widget(..), tBoolValue, tEnumValue, tIntValue, tStepId, tStringValue)
import Model.TableSpec as TableSpec exposing (TableSpec)
import Organize
import Route exposing (Route)
import Scroll
import Set
import Specs
import Time exposing (Posix)
import Time.Distance
import View.Icons exposing (icon, iconCustom)
import View.Lib
import View.Organize


hasBrowsableOutput : ApiData Status -> Bool
hasBrowsableOutput =
    has (ApiData.success << where_ Model.hasBuiltOutput)


stepFormReadOnly : Model -> TableSpec (BaseRecord a) -> BaseRecord a -> Bool
stepFormReadOnly model spec record =
    isReadOnlyRoute model || TableSpec.getIsLocked spec record


type alias ListingRow =
    { link : Model.ChildLink
    , name : String
    , displayName : String
    , typeName : String
    , typeIcon : Maybe String
    , statusPill : Html (Flow Model ())
    , validationErrors : List String
    , alwaysVisibleActions : List (Html (Flow Model ()))
    , actionsPopover : Html (Flow Model ())
    , mTime : Maybe Posix
    , cTime : Maybe Posix
    , statusSortKey : Int
    , isUpdating : Bool
    , openRow : Maybe (Flow Model ())
    , editName : Maybe (Flow Model ())
    , inlineRename : Maybe InlineRename
    , expanders : List (Html (Flow Model ()))
    , form : Html (Flow Model ())
    }


type alias InlineRename =
    { value : String
    , onInput : String -> Flow Model ()
    , onSubmit : Flow Model ()
    , onCancel : Flow Model ()
    }


type alias ListingRowContext =
    { scope : Model.ListingScope
    , editable : Bool
    , selected : Set.Set ( String, Int )
    }


type alias ListingGroup =
    { title : String
    , icon : Maybe String
    , rows : List ListingRow
    }


listingSortOptions : List ( Model.ListingSort, String )
listingSortOptions =
    [ ( SortManual, "Manual" )
    , ( SortName, "Name" )
    , ( SortType, "Type" )
    , ( SortCreated, "Created" )
    , ( SortModified, "Modified" )
    , ( SortStatus, "Status" )
    ]


compareRows : Model.ListingSort -> ListingRow -> ListingRow -> Order
compareRows sort a b =
    case sort of
        SortManual ->
            Model.compareChildLinks a.link b.link

        SortName ->
            compare (String.toLower a.name) (String.toLower b.name)

        SortType ->
            compare ( String.toLower a.typeName, String.toLower a.name ) ( String.toLower b.typeName, String.toLower b.name )

        SortCreated ->
            compare (timeMillis a.cTime) (timeMillis b.cTime)

        SortModified ->
            compare (timeMillis a.mTime) (timeMillis b.mTime)

        SortStatus ->
            compare a.statusSortKey b.statusSortKey


timeMillis : Maybe Posix -> Int
timeMillis mTime =
    Maybe.map Time.posixToMillis mTime |> Maybe.withDefault 0


isFolderRow : ListingRow -> Bool
isFolderRow =
    .link >> .kind >> (==) Model.ProjectChild


sortRows : Model.ListingPreferences -> List ListingRow -> List ListingRow
sortRows prefs rows =
    let
        sorted =
            List.sortWith (compareRows prefs.sort) rows

        ordered =
            if prefs.descending then
                List.reverse sorted

            else
                sorted

        ( folders, steps_ ) =
            List.partition isFolderRow ordered
    in
    if prefs.foldersFirst then
        folders ++ steps_

    else
        ordered


groupRows : Model.ListingPreferences -> StepConfig -> List ListingRow -> List ListingGroup
groupRows prefs stepConfig rows =
    let
        sorted =
            sortRows prefs rows

        ( folders, steps_ ) =
            List.partition isFolderRow sorted

        typeOrder typeName =
            ( Dict.get typeName stepConfig |> Maybe.andThen .sortKey |> Maybe.withDefault 2147483647
            , typeName
            )

        typeNames =
            List.map .typeName steps_ |> List.unique |> List.sortBy typeOrder

        groupFor typeName =
            { title = Dict.get typeName stepConfig |> Maybe.andThen .displayName |> Maybe.withDefault typeName
            , icon = Dict.get typeName stepConfig |> Maybe.andThen .icon
            , rows = List.filter (\row -> row.typeName == typeName) steps_
            }
    in
    { title = "Folders", icon = Just "folder", rows = folders }
        :: List.map groupFor typeNames


visibleRows : Model.ListingPreferences -> List ListingRow -> List ListingRow
visibleRows prefs rows =
    if prefs.showHidden then
        rows

    else
        List.filter (\row -> not row.link.hidden) rows


viewListing :
    { model : Model
    , scope : Model.ListingScope
    , stepConfig : StepConfig
    , rows : List ListingRow
    , header : List (Html (Flow Model ()))
    }
    -> Html (Flow Model ())
viewListing { model, scope, stepConfig, rows, header } =
    let
        prefs =
            Model.getListingPreferences model

        editable =
            Model.Selection.listingEditable model

        selected =
            if editable then
                case Model.getListingSelection model of
                    Just selection ->
                        if selection.scope == scope then
                            Set.fromList (List.map (\ref -> ( Model.childKindName ref.kind, ref.id )) selection.refs)

                        else
                            Set.empty

                    Nothing ->
                        Set.empty

            else
                Set.empty

        rowContext =
            { scope = scope, editable = editable, selected = selected }

        groups =
            (if prefs.groupByType then
                groupRows prefs stepConfig rows

             else
                [ { title = "", icon = Nothing, rows = sortRows prefs rows } ]
            )
                |> List.filter (\group -> not (List.isEmpty (visibleRows prefs group.rows)))
    in
    Html.div
        ([ class "listing", id "project-listing" ]
            ++ (if editable then
                    View.Organize.selectionRefsAttr model

                else
                    []
               )
        )
        [ Html.viewIf editable (View.Organize.viewActionBar model)
        , Html.div [ class "listing-header" ]
            [ Html.div [ class "listing-header-title" ]
                [ iconCustom True "folder_open" [ class "listing-header-icon" ]
                , Html.span [ class "listing-content-header" ] [ Html.text "Contents" ]
                , Html.span [ class "listing-header-count" ]
                    [ Html.text ("(" ++ String.fromInt (List.length rows) ++ ")") ]
                ]
            , Html.div [ class "listing-header-controls" ]
                (viewListingSort prefs
                    :: viewListingToggle "folder" "Folders first" prefs.foldersFirst Actions.toggleListingFoldersFirst
                    :: viewListingToggle "visibility" "Show hidden" prefs.showHidden Actions.toggleListingShowHidden
                    :: viewListingToggle "widgets" "Group by type" prefs.groupByType Actions.toggleListingGroupByType
                    :: header
                    ++ viewClipboardButtons model
                )
            ]
        , Html.div
            ([ class "listing-groups" ]
                ++ (if editable then
                        [ Events.custom "contextmenu"
                            (Decode.map2
                                (\x y -> { message = Organize.openEmptyMenu x y, stopPropagation = True, preventDefault = True })
                                (Decode.field "clientX" Decode.int)
                                (Decode.field "clientY" Decode.int)
                            )
                        ]

                    else
                        []
                   )
            )
            (List.map (viewListingGroup model rowContext prefs) groups)
        ]


viewClipboardButtons : Model -> List (Html (Flow Model ()))
viewClipboardButtons model =
    [ Model.OrganizePasteAction, Model.OrganizeClearClipboardAction ]
        |> List.filter (Model.Selection.actionVisible model)
        |> List.map
            (\action ->
                let
                    spec =
                        Model.Selection.actionSpec model action
                in
                viewIconButtonWithTooltip spec.icon True spec.label (Organize.runAction action)
            )


viewListingSort : Model.ListingPreferences -> Html (Flow Model ())
viewListingSort prefs =
    let
        popoverId =
            "listing-sort-popover"

        currentLabel =
            listingSortOptions
                |> List.filterMap
                    (\( field, label ) ->
                        if prefs.sort == field then
                            Just label

                        else
                            Nothing
                    )
                |> List.head
                |> Maybe.withDefault "Manual"

        optionButton ( field, label ) =
            Html.button
                [ class "listing-sort-option"
                , classList [ ( "active", prefs.sort == field ) ]
                , Events.onClick (Actions.setListingSort field)
                ]
                [ Html.text label
                , Html.viewIf (prefs.sort == field)
                    (iconCustom False
                        (if prefs.descending then
                            "arrow_downward"

                         else
                            "arrow_upward"
                        )
                        [ class "listing-sort-direction" ]
                    )
                ]
    in
    View.Organize.viewMenuPopover
        { popoverId = popoverId
        , wrapperClass = "listing-sort"
        , triggerAttrs =
            [ class "listing-control-btn"
            , title "Sort"
            , attribute "aria-label" "Sort"
            ]
        , triggerContent = [ icon True "sort", Html.span [ class "listing-control-label" ] [ Html.text currentLabel ] ]
        , content = List.map optionButton listingSortOptions
        }


viewListingToggle : String -> String -> Bool -> Flow Model () -> Html (Flow Model ())
viewListingToggle iconName tooltip active action =
    Html.button
        [ class "listing-control-btn"
        , classList [ ( "active", active ) ]
        , title tooltip
        , attribute "aria-label" tooltip
        , attribute "aria-pressed" (View.Lib.boolText active)
        , Events.onClick action
        ]
        [ icon True iconName ]


viewListingGroup : Model -> ListingRowContext -> Model.ListingPreferences -> ListingGroup -> Html (Flow Model ())
viewListingGroup model rowContext prefs group =
    let
        visible =
            visibleRows prefs group.rows

        orderedRefs =
            List.map (Model.childRefOf << .link) visible

        gaps =
            Model.Selection.reorderGaps model rowContext.scope orderedRefs
    in
    Html.div [ class "listing-group" ]
        [ Html.viewIf (not (String.isEmpty group.title))
            (Html.div [ class "listing-group-header" ]
                [ Html.viewMaybe (\groupIcon -> iconCustom False groupIcon [ class "listing-group-icon" ]) group.icon
                , Html.text group.title
                , Html.span [ class "listing-group-count" ]
                    [ Html.text ("(" ++ String.fromInt (List.length visible) ++ ")") ]
                ]
            )
        , Html.Keyed.node "div"
            [ class "listing-rows" ]
            (List.indexedMap (\index -> viewRowKeyed model rowContext orderedRefs (View.Organize.dropEdgeAttrs gaps index)) visible)
        ]


viewRowKeyed : Model -> ListingRowContext -> List Model.ChildRef -> List (Html.Attribute (Flow Model ())) -> ListingRow -> ( String, Html (Flow Model ()) )
viewRowKeyed model rowContext orderedRefs edgeAttrs row =
    ( Model.rowDomId row.link.kind row.link.id, viewRow model rowContext orderedRefs edgeAttrs row )


viewRow : Model -> ListingRowContext -> List Model.ChildRef -> List (Html.Attribute (Flow Model ())) -> ListingRow -> Html (Flow Model ())
viewRow model rowContext orderedRefs edgeAttrs row =
    let
        scope =
            rowContext.scope

        kindName =
            Model.childKindName row.link.kind

        ref =
            Model.childRefOf row.link

        isFolder =
            row.link.kind == Model.ProjectChild

        rowId =
            Model.rowDomId row.link.kind row.link.id

        popoverId =
            actionsPopoverId row.link

        isSelected =
            Set.member ( kindName, ref.id ) rowContext.selected

        isHighlighted =
            case (Model.getRoute model).page of
                Route.Project { mHighlight } ->
                    Maybe.map .id mHighlight == Just row.link.id && not isFolder

                _ ->
                    False

        nameView =
            case row.inlineRename of
                Just rename ->
                    Html.input
                        [ type_ "text"
                        , value rename.value
                        , Events.onInput rename.onInput
                        , class "form-input"
                        , Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                        , Events.onBlur rename.onSubmit
                        , Events.on "keydown" <|
                            Keyboard.decodeCombinations
                                [ ( Keyboard.enter, Decode.succeed rename.onSubmit )
                                , ( Keyboard.escape, Decode.succeed rename.onCancel )
                                ]
                        ]
                        []

                Nothing ->
                    Html.span [ class "record-name-container" ]
                        [ Html.text row.name
                        , Html.span [ class "listing-row-id", title ("id: " ++ String.fromInt row.link.id) ]
                            [ Html.text (String.fromInt row.link.id) ]
                        , Html.viewMaybe
                            (\editAction ->
                                iconCustom True
                                    "edit"
                                    [ class "edit-icon"
                                    , Events.stopPropagationOn "click" (Decode.succeed ( editAction, True ))
                                    ]
                            )
                            row.editName
                        ]

        statusView =
            case row.validationErrors of
                [] ->
                    if isFolder then
                        Html.span [ class "listing-row-status" ] []

                    else
                        row.statusPill

                errors ->
                    Html.span
                        [ class "project-error-indicator"
                        , title (String.join "\n" errors)
                        ]
                        [ iconCustom True "error" [] ]

        clickAttrs =
            rowClickAttrs rowContext.editable scope orderedRefs ref row.openRow

        dragAttrs =
            if rowContext.editable then
                View.Organize.rowDragAttrs scope row.link
                    ++ edgeAttrs
                    ++ (if isFolder then
                            View.Organize.dropTargetAttrs model row.link.id

                        else
                            []
                       )

            else
                []

        dragHandleAttrs =
            if rowContext.editable then
                View.Organize.rowDragHandleAttrs

            else
                []
    in
    Html.div
        ([ class "listing-row"
         , classList
            [ ( "listing-row-folder", isFolder )
            , ( "listing-row-selected", isSelected )
            , ( "listing-row-cut", Model.Selection.isCut model scope ref )
            , ( "drag-source", Model.Selection.isDragged model scope ref )
            ]
         , id rowId
         ]
            ++ dragAttrs
            ++ (if rowContext.editable then
                    rowContextMenuAttrs scope ref

                else
                    []
               )
        )
        (Html.div
            ([ class "listing-row-header"
             , classList
                [ ( "highlighted", isHighlighted )
                , ( "listing-row-header--read-only", not rowContext.editable )
                , ( "listing-row-header--openable", Maybe.isJust row.openRow )
                ]
             ]
                ++ dragHandleAttrs
                ++ clickAttrs
            )
            [ Html.viewIf rowContext.editable (View.Organize.viewRowCheckbox rowContext.selected scope row.link)
            , statusView
            , Html.span [ class "listing-row-name" ]
                [ Html.viewMaybe
                    (\typeIcon -> iconCustom False typeIcon [ class "listing-row-type-icon", title row.displayName ])
                    row.typeIcon
                , nameView
                , Html.span [] row.alwaysVisibleActions
                , Html.viewIf isFolder row.statusPill
                , Html.Lazy.lazy2 viewMtimeBadge row.mTime (Model.getNow model)
                , Html.viewIf row.isUpdating <|
                    Html.span [ class "pending-record-indicator", title "Saving..." ]
                        [ iconCustom True "progress_activity" [ class "pending-record-icon" ] ]
                ]
            , Html.div [ class "listing-row-actions" ]
                [ Html.button
                    [ class "icon-btn hamburger-icon-btn-mobile"
                    , attribute "popovertarget" popoverId
                    , style "anchor-name" ("--anchor-" ++ popoverId)
                    ]
                    [ icon True "more_vert" ]
                , row.actionsPopover
                , Html.Lazy.lazy2 viewMtimeBadge row.mTime (Model.getNow model)
                ]
            ]
            :: row.form
            :: row.expanders
        )


rowClickAttrs : Bool -> Model.ListingScope -> List Model.ChildRef -> Model.ChildRef -> Maybe (Flow Model ()) -> List (Html.Attribute (Flow Model ()))
rowClickAttrs editable scope orderedRefs ref mOpenRow =
    [ Events.on "click"
        (Decode.field "target" (Decode.whenNotInside "listing-row-actions" ())
            |> Decode.andThen (\_ -> clickModsDecoder)
            |> Decode.andThen
                (\mods ->
                    if editable && (mods.ctrl || mods.shift) then
                        Decode.succeed (Organize.clickRow scope orderedRefs mods.shift ref)

                    else
                        Maybe.unwrap (Decode.fail "row has no open action") Decode.succeed mOpenRow
                )
        )
    ]


clickModsDecoder : Decode.Decoder { ctrl : Bool, shift : Bool }
clickModsDecoder =
    Decode.map2 (\ctrl shift -> { ctrl = ctrl, shift = shift })
        (Decode.map2 (||) (Decode.field "ctrlKey" Decode.bool) (Decode.field "metaKey" Decode.bool))
        (Decode.field "shiftKey" Decode.bool)


rowContextMenuAttrs : Model.ListingScope -> Model.ChildRef -> List (Html.Attribute (Flow Model ()))
rowContextMenuAttrs scope ref =
    [ Events.custom "contextmenu"
        (Decode.map2
            (\x y -> { message = Organize.openRowMenu scope x y ref, stopPropagation = True, preventDefault = True })
            (Decode.field "clientX" Decode.int)
            (Decode.field "clientY" Decode.int)
        )
    ]


viewMtimeBadge : Maybe Posix -> Posix -> Html msg
viewMtimeBadge mPosix now =
    Html.viewMaybe
        (\posix ->
            let
                iso =
                    Iso8601.fromTime posix
            in
            Html.node "time"
                [ class "listing-row-mtime"
                , attribute "datetime" iso
                , title ("Last modified: " ++ iso)
                ]
                [ Html.text (Time.Distance.inWords posix now) ]
        )
        mPosix


viewStatusApiData : String -> ApiData String -> Maybe Int -> ApiData Status -> Html (Flow Model ())
viewStatusApiData tableName logState mRecordId status =
    let
        viewStatusPill s =
            let
                presentation =
                    View.Lib.statusPresentation s

                colorClass =
                    presentation.className

                statusText =
                    case s of
                        StatusFailure (Just err) ->
                            "Failure: " ++ err

                        _ ->
                            presentation.label

                showsLog =
                    case s of
                        StatusFailure _ ->
                            True

                        StatusCertificationFailed _ ->
                            True

                        _ ->
                            False
            in
            case ( showsLog, mRecordId ) of
                ( True, Just stepId ) ->
                    let
                        popoverId =
                            "step-log-popover-" ++ tableName ++ "-" ++ String.fromInt stepId
                    in
                    Html.span [ class "status-log", Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True )) ]
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


viewStepRecordStatus : String -> StepConfigEntry -> ApiData String -> Bool -> StepRecord -> Html (Flow Model ())
viewStepRecordStatus name entry logState ingesting record =
    viewStatusApiData
        name
        logState
        record.id
        (if ingesting then
            Success StatusRunning

         else
            TableSpec.getStatus (Specs.steps name entry) record
        )


actionsPopoverId : ChildLink -> String
actionsPopoverId link =
    "listing-actions-" ++ Model.childKindName link.kind ++ "-" ++ String.fromInt link.id


viewRecordActionsPopover : String -> List (Html (Flow Model ())) -> Html (Flow Model ())
viewRecordActionsPopover popoverId actions =
    Html.div
        [ class "listing-row-actions-popover"
        , id popoverId
        , attribute "popover" "auto"
        , style "position-anchor" ("--anchor-" ++ popoverId)
        , Events.on "click" (Decode.succeed (Actions.hidePopover popoverId))
        ]
        actions


viewRowActions : Int -> ChildLink -> TableSpec (BaseRecord a) -> Bool -> BaseRecord a -> List (Html (Flow Model ()))
viewRowActions parentId link spec isReadOnly record =
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

        toggleRecordForm =
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
            [ { shouldShow = hasBrowsableOutput << TableSpec.getStatus spec
              , render = \r -> Html.viewMaybe (dirButton isDirectoryOpen []) r.id
              }
            , { shouldShow = \r -> not isReadOnly && TableSpec.getIsLocked spec r
              , render = always (viewInactiveIconButtonWithTooltip "edit" "Remove review to edit")
              }
            , { shouldShow = Maybe.isJust << .id
              , render =
                    \r ->
                        if editable r then
                            viewIconButtonWithTooltip "edit" True "Edit" toggleRecordForm

                        else
                            viewIconButtonWithTooltip "data_info_alert" True "Inspect Parameters" toggleRecordForm
              }
            , { shouldShow = \r -> TableSpec.getShareable spec r && Maybe.isJust r.id
              , render =
                    \r ->
                        viewIconButtonWithTooltip
                            "share"
                            True
                            "Share"
                            (Maybe.unwrap Flow.none (\recordId -> Actions.shareEntity recordId Route.Output [] Nothing) r.id)
              }
            , { shouldShow = \r -> not isReadOnly && Maybe.isJust r.id
              , render =
                    \r ->
                        viewIconButtonWithTooltip
                            (if link.hidden then
                                "visibility"

                             else
                                "visibility_off"
                            )
                            True
                            (if link.hidden then
                                "Show"

                             else
                                "Hide"
                            )
                            (Organize.setChildHidden parentId (Model.childRefOf link) (not link.hidden))
              }
            , { shouldShow = \r -> not isReadOnly && TableSpec.getShareable spec r
              , render = \r -> viewIconButtonWithTooltip "content_copy" False "Clone" (TableSpec.getCloneRecord spec r)
              }
            , { shouldShow = \r -> editable r && Maybe.isJust r.id
              , render = \_ -> viewIconButtonWithTooltip "delete" False "Remove" (Organize.unlinkChild parentId (Model.childRefOf link))
              }
            ]
    in
    List.filterMap
        (\recordActionBtn ->
            if recordActionBtn.shouldShow record then
                Just (recordActionBtn.render record)

            else
                Nothing
        )
        recordActions


viewRunStop : TableSpec StepRecord -> Bool -> StepRecord -> List (Html (Flow Model ()))
viewRunStop spec stopping record =
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
            , Html.viewIf (isRunning && not stopping) (viewStopButton "Stop" (Actions.stopStep spec id))
            , Html.viewIf (isRunning && stopping) viewStoppingIndicator
            ]

        Nothing ->
            []


viewStepRecordActions : Int -> ChildLink -> String -> StepConfigEntry -> StepConfig -> List String -> Route.Page -> StepRecord -> { uploading : Bool, scratchAvailable : Bool, stopping : Bool } -> Html (Flow Model ())
viewStepRecordActions parentId link name entry stepConfig presentTypes page record flags =
    let
        spec =
            Specs.steps name entry

        isReadOnly =
            isReadOnlyPage page

        prefill widget_ =
            let
                wire proven toValue =
                    if List.member record.type_ (Maybe.withDefault [] proven) then
                        Maybe.map toValue record.id

                    else
                        Nothing
            in
            case widget_ of
                WStep artifact ->
                    if artifact.create then
                        wire artifact.proven TStepValue

                    else
                        Nothing

                WSteps artifact ->
                    if artifact.create then
                        wire artifact.proven (TListValue << List.singleton << TStepValue)

                    else
                        Nothing

                _ ->
                    Nothing

        runActions =
            case entry.stepType of
                Derivation _ _ ->
                    viewRunStop spec flags.stopping record

                Download _ ->
                    viewRunStop spec flags.stopping record

                FileUpload _ ->
                    []

        uploadActions =
            if isReadOnly || flags.uploading || Maybe.isJust record.review then
                []

            else
                case entry.stepType of
                    FileUpload types ->
                        [ Html.viewMaybe (viewUploadButton << Ingest.uploadFiles (Maybe.withDefault [] types)) record.id ]
                            ++ (if flags.scratchAvailable then
                                    [ Html.viewMaybe (viewScratchButton << Ingest.openScratchPicker) record.id ]

                                else
                                    []
                               )

                    Derivation _ _ ->
                        []

                    Download _ ->
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
                                Derivation fields _ ->
                                    let
                                        eligible =
                                            fields
                                                |> List.filterMap
                                                    (\f ->
                                                        prefill f.widget
                                                            |> Maybe.map (\value -> ( f, value ))
                                                    )

                                        buttonLabel f =
                                            if List.length eligible > 1 then
                                                label ++ " (" ++ Maybe.withDefault f.name f.label ++ ")"

                                            else
                                                label
                                    in
                                    eligible
                                        |> List.map
                                            (\( f, value ) ->
                                                viewQuickCreateButton targetEntry.icon (buttonLabel f) (Actions.addStepWithArg targetSpec f.name value)
                                            )

                                FileUpload _ ->
                                    []

                                Download _ ->
                                    []
                        )
    in
    viewRecordActionsPopover
        (actionsPopoverId link)
        (uploadActions ++ runActions ++ quickCreateActions ++ viewRowActions parentId link spec isReadOnly record)


viewAddOrEditRecordForm : Model -> Int -> TableSpec (BaseRecord a) -> Table (BaseRecord a) -> { extraFields : List (Html (Flow Model ())), noteInput : Html (Flow Model ()) } -> Html (Flow Model ()) -> BaseRecord a -> Html (Flow Model ())
viewAddOrEditRecordForm model parentId spec table fields extraSection record =
    let
        readOnly =
            stepFormReadOnly model spec record

        editing =
            record.id /= Nothing && (table.addMode /= LinkExisting)

        savingInFlight =
            table.isUpdating

        extraFields =
            fields.extraFields

        noteInput =
            fields.noteInput

        nameInput =
            let
                originalRecord =
                    record.id |> Maybe.andThen (\id -> TableSpec.getFindRecord spec id model)
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
                    , Events.onClick (Actions.setAddMode (TableSpec.getLens spec) (TableSpec.getDefaultRecord spec model) mode)
                    ]
                    []
                , Html.text label
                ]

        modeSelector =
            Html.div [ class "form-mode-selector" ]
                [ radioButton AddNew "Create new"
                , radioButton LinkExisting "Link existing"
                ]

        viewSelectExisting state =
            let
                availableItems =
                    Model.Lib.linkCandidates model parentId
                        |> List.map
                            (\( ref, label ) ->
                                { id = Just ref.id, name = label, mProjectId = Nothing, ref = Just ref }
                            )
                        |> List.filter (\item -> not (List.any (\chosen -> chosen.ref == item.ref) state.selected))

                toItemTooltip item =
                    item.ref
                        |> Maybe.unwrap []
                            (\ref ->
                                case Model.Lib.entityOtherParents model ref.kind ref.id Nothing of
                                    [] ->
                                        [ "unfiled" ]

                                    parents ->
                                        List.map (\linkedParentId -> "in " ++ Model.Lib.canonicalNamePath model linkedParentId) parents
                            )
            in
            Select.view
                { optic = TableSpec.getLens spec << selectExistingSteps
                , selectState = state
                , selected_ = state.selected
                , availableItems = availableItems
                , readOnly = False
                , hasChanged = False
                , label = "Link existing"
                , mHint = Just "Pick steps and folders to link into this folder"
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

                    ( False, LinkExisting ) ->
                        "Link existing"

                    ( True, _ ) ->
                        "Edit " ++ displayName

        closeAction =
            let
                endEdit =
                    Actions.endRecordEdit (TableSpec.getLens spec)
            in
            case ( readOnly, record.id, TableSpec.getChildKind spec ) of
                ( False, Just recordId, Model.StepChild ) ->
                    Actions.discardSrcFileChanges recordId
                        |> Flow.seq endEdit

                _ ->
                    endEdit
    in
    Html.div [ class "table-form-wrapper", id (TableSpec.formId spec) ]
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
                    [ Html.viewIf (not editing) modeSelector
                    , Html.viewIf (not editing && table.addMode == LinkExisting) <| Html.Lazy.lazy viewSelectExisting table.selectExistingSteps
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


viewProjectExtraFormFields : Model -> A_Traversal Model (Table ProjectRecord) -> List (Html (Flow Model ()))
viewProjectExtraFormFields model tableLens =
    let
        mEdited =
            try (remkT tableLens << edited << just) model

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
                    , ref = Nothing
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
                    , ref = Nothing
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
                        Actions.chooseProjectCustom tableLens

                    else
                        Actions.chooseProjectPreset tableLens item.name

                presetStateLens =
                    remkT tableLens << edited << just << presetSelect

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
                    remkT tableLens << edited << just << templatesSelect

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
                        , onRemove = .name >> Actions.removeProjectTemplate tableLens
                        , activeAfterSelect = True
                        , clearInputAfterSelect = True
                        , onSelect = .name >> Actions.addProjectTemplate tableLens
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
            Lenses.stepFormsAt tableId << edited << just << args

        mEditedId =
            try (Lenses.stepFormsAt tableId << edited << just) model
                |> Maybe.andThen .id

        otherSteps =
            Dict.values (Model.getSteps model)
                |> List.filter (\step -> Maybe.unwrap True (\editedId -> step.id /= Just editedId) mEditedId)

        allSteps mTypes =
            otherSteps
                |> List.filter (\step -> Maybe.unwrap True (List.member step.type_) mTypes)

        allStepsById =
            Model.getSteps model

        getStep id =
            id |> Maybe.andThen (\i -> Dict.get i allStepsById)

        originalRecord =
            try (Lenses.stepFormsAt tableId << edited << just) model
                |> Maybe.andThen .id
                |> Maybe.andThen (\id_ -> try (stepRecordById id_) model)

        stepConfig_ =
            Model.getStepConfig model |> ApiData.toMaybe |> Maybe.withDefault Dict.empty

        typeDisplayName typeName =
            Dict.get typeName stepConfig_
                |> Maybe.andThen .displayName
                |> Maybe.withDefault typeName

        currentRouteCommit =
            Route.viewedCommit (Model.getRoute model).page

        noticesForField paramName =
            mEditedId
                |> Maybe.map (\stepId -> Model.stepLogKey stepId currentRouteCommit)
                |> Maybe.andThen (\key -> Dict.get key (Model.getNotices model))
                |> Maybe.andThen ApiData.toMaybe
                |> Maybe.withDefault []
                |> List.filter (\notice -> notice.field == Just paramName && notice.severity == Model.Info)

        buildStepSelect cfg { selectedStepIds, onSelectStep, onRemoveStep, activeAfterSelect, mAllowedStepTypes } =
            let
                stateLens =
                    Lenses.stepFormsAt tableId
                        << argSelectStates
                        << lens "keyWithDefault" (Dict.get cfg.stateKey >> Maybe.withDefault Select.initSelectState) (\d v -> Dict.insert cfg.stateKey v d)

                selectState =
                    try stateLens model |> Maybe.withDefault Select.initSelectState

                selectedItems =
                    selectedStepIds
                        |> List.map
                            (\stepId ->
                                { id = Just stepId
                                , name =
                                    case getStep (Just stepId) of
                                        Nothing ->
                                            "#" ++ String.fromInt stepId ++ " (missing)"

                                        Just step ->
                                            step.name
                                , mProjectId = Nothing
                                , ref = Nothing
                                }
                            )

                selectedIds =
                    List.map .id selectedItems

                availableItems =
                    if selectState.active || not (String.isEmpty selectState.input) then
                        let
                            projectStepIds =
                                Model.Lib.currentProjectStepIds model
                        in
                        allSteps mAllowedStepTypes
                            |> List.filterMap (\step -> step.id |> Maybe.filter (\id -> Set.member id projectStepIds) |> Maybe.map (\id -> { id = Just id, name = step.name, mProjectId = Nothing, ref = Nothing }))
                            |> List.filter (\item -> not (List.member item.id selectedIds))

                    else
                        []

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
                    try (route << Route.page << Route.project) model
                        |> Maybe.map
                            (\params ->
                                Route.fromPage
                                    (Route.Project
                                        { params
                                            | mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }
                                            , mCompare = Nothing
                                        }
                                    )
                            )
            in
            Select.view
                { optic = stateLens
                , selectState = selectState
                , selected_ = selectedItems
                , availableItems = availableItems
                , readOnly = cfg.readOnly
                , hasChanged = cfg.changed
                , label = cfg.label
                , mHint = cfg.hint
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

        viewValue cfg rawGet rawSet =
            let
                get =
                    rawGet model

                items =
                    case get of
                        Just (TListValue xs) ->
                            xs

                        _ ->
                            []

                currentDict =
                    case get of
                        Just (TRecordValue d) ->
                            d

                        _ ->
                            Dict.empty

                setItems =
                    rawSet << Just << TListValue

                addItem value =
                    setItems (items ++ [ value ]) |> Flow.seq (focus cfg.id)

                removeItem idx =
                    setItems (List.removeAt idx items) |> Flow.seq (focus cfg.id)

                addString rawValue =
                    let
                        trimmed =
                            String.trim rawValue
                    in
                    addItem (TStringValue trimmed)
                        |> Flow.when (not <| String.isEmpty trimmed)

                stringValues =
                    List.filterMap
                        (\v ->
                            case v of
                                TStringValue s ->
                                    Just s

                                _ ->
                                    Nothing
                        )
                        items

                asTags values =
                    List.map (\s -> { body = Html.text s, route = Nothing, backgroundColor = Nothing }) values

                valueOfRecord idx =
                    case List.getAt idx items of
                        Just (TRecordValue d) ->
                            d

                        _ ->
                            Dict.empty

                defaultValue widget_ =
                    case widget_ of
                        WCheckbox ->
                            TBoolValue False

                        WSelect (( first, _ ) :: _) ->
                            TEnumValue first

                        WSelect [] ->
                            TEnumValue ""

                        WRecord recordFields ->
                            TRecordValue (Dict.fromList (List.map (\f -> ( f.name, defaultValue f.widget )) recordFields))

                        WList _ ->
                            TListValue []

                        WTokens _ ->
                            TListValue []

                        WSteps _ ->
                            TListValue []

                        _ ->
                            TStringValue ""

                stringValue =
                    Maybe.withDefault "" (Maybe.andThen (try tStringValue) get)

                textEditor editor =
                    editor
                        { label = cfg.label
                        , mHint = cfg.hint
                        , placeholder = cfg.label
                        , value = stringValue
                        , onInput = \s -> rawSet (Just (TStringValue s))
                        , hasChanged = cfg.changed
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        }

                viewAutocompleteList hook =
                    let
                        stateKey =
                            cfg.stateKey

                        autocompleteState =
                            Dict.get stateKey (Model.getAutocomplete model)
                                |> Maybe.withDefault Model.initAutocompleteState

                        autocompleteRequest query =
                            { template = tableId
                            , autocomplete = hook
                            , context = cfg.context
                            , query = query
                            , limit = 25
                            }
                    in
                    autocompleteListField
                        { label = cfg.label
                        , mHint = cfg.hint
                        , selectedStrings = stringValues
                        , validity = Actions.autocompleteValueValidity stateKey model
                        , suggestions = autocompleteState.suggestions
                        , activeIndex = autocompleteState.activeIndex
                        , onQueryChange =
                            Actions.fetchAutocomplete stateKey currentRouteCommit
                                << autocompleteRequest
                        , onSuggestionSelect =
                            \suggestion ->
                                Actions.clearAutocomplete stateKey
                                    |> Flow.seq (addString suggestion)
                        , onAddItem =
                            \val ->
                                Flow.async (Actions.checkAutocompleteValue stateKey currentRouteCommit (autocompleteRequest (String.trim val)))
                                    |> Flow.seq (Actions.clearAutocomplete stateKey)
                                    |> Flow.seq (addString val)
                        , onRemoveIndex = removeItem
                        , onActiveIndexChange =
                            \newIndex ->
                                Flow.over Lenses.autocomplete
                                    (Dict.insert stateKey
                                        { autocompleteState | activeIndex = newIndex }
                                    )
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        , hasChanged = cfg.changed
                        , query = autocompleteState.query
                        }

                viewTokenList =
                    listField
                        { label = cfg.label
                        , mHint = cfg.hint
                        , tags = asTags stringValues
                        , onAdd = addString
                        , onRemoveLast = removeItem (List.length items - 1)
                        , onRemoveIndex = removeItem
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        , hasChanged = cfg.changed
                        }

                viewRows element =
                    Html.div [ class "form-field" ]
                        [ Html.label [ class "form-label" ] [ Html.text cfg.label ]
                        , Html.div [ class "record-list" ]
                            (List.indexedMap
                                (\idx _ ->
                                    Html.div [ class "record-item" ]
                                        [ Html.div [ class "record-item-fields" ]
                                            [ viewValue
                                                { label = cfg.label
                                                , hint = Nothing
                                                , stateKey = cfg.stateKey ++ "#" ++ String.fromInt idx
                                                , id = cfg.id ++ "-" ++ String.fromInt idx
                                                , changed = False
                                                , readOnly = cfg.readOnly
                                                , context = contextOfRecord (valueOfRecord idx)
                                                , widget = element
                                                }
                                                (\_ -> List.getAt idx items)
                                                (\mValue ->
                                                    setItems
                                                        (List.updateAt idx (\old -> Maybe.withDefault old mValue) items)
                                                )
                                            ]
                                        , Html.viewIf (not cfg.readOnly) <|
                                            Html.button
                                                [ Events.onClick (removeItem idx)
                                                , class "remove-record-btn"
                                                , attribute "type" "button"
                                                ]
                                                [ icon True "remove" ]
                                        ]
                                )
                                items
                                ++ (if cfg.readOnly then
                                        []

                                    else
                                        [ Html.button
                                            [ Events.onClick (addItem (defaultValue element))
                                            , class "add-record-btn"
                                            , attribute "type" "button"
                                            ]
                                            [ Html.text ("Add " ++ cfg.label) ]
                                        ]
                                   )
                            )
                        ]

                contextOfRecord recordDict =
                    Dict.foldl
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
                        recordDict
            in
            case cfg.widget of
                WText _ ->
                    textEditor textField

                WDatetime ->
                    textEditor textField

                WTextarea ->
                    textArea
                        { label = cfg.label
                        , mHint = cfg.hint
                        , placeholder = ""
                        , value = stringValue
                        , onInput = rawSet << Just << TStringValue
                        , hasChanged = cfg.changed
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        }

                WCode language ->
                    codeField
                        { label = cfg.label
                        , mHint = cfg.hint
                        , value = stringValue
                        , onInput = rawSet << Just << TStringValue
                        , hasChanged = cfg.changed
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        , language = language
                        }

                WCommand prefix ->
                    commandField
                        { label = cfg.label
                        , mHint = cfg.hint
                        , placeholder = cfg.label
                        , value = stringValue
                        , onInput = rawSet << Just << TStringValue
                        , hasChanged = cfg.changed
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        , commandPrefix = prefix
                        }

                WNumber ->
                    textField
                        { label = cfg.label
                        , mHint = cfg.hint
                        , placeholder = cfg.label
                        , value = Maybe.withDefault "" (Maybe.map String.fromInt (Maybe.andThen (try tIntValue) get))
                        , onInput = String.toInt >> Maybe.map (TIntValue >> Just >> rawSet) >> Maybe.withDefault Flow.none
                        , hasChanged = cfg.changed
                        , readOnly = cfg.readOnly
                        , id = cfg.id
                        }

                WCheckbox ->
                    formField
                        { label = cfg.label, mHint = cfg.hint, id = cfg.id }
                        (Html.input
                            [ Html.Attributes.type_ "checkbox"
                            , id cfg.id
                            , checked (Maybe.withDefault False (Maybe.andThen (try tBoolValue) get))
                            , Events.onCheck (TBoolValue >> Just >> rawSet)
                            , class "form-checkbox"
                            , classList [ ( "field-changed", cfg.changed ) ]
                            ]
                            []
                        )

                WSelect options ->
                    formField
                        { label = cfg.label, mHint = cfg.hint, id = cfg.id }
                        (Html.select
                            [ id cfg.id
                            , class "form-input"
                            , classList [ ( "field-changed", cfg.changed ) ]
                            , disabled cfg.readOnly
                            , Events.onInput (TEnumValue >> Just >> rawSet)
                            ]
                            (List.map
                                (\( value_, label_ ) ->
                                    Html.option
                                        [ value value_
                                        , selected (Maybe.andThen (try tEnumValue) get == Just value_)
                                        ]
                                        [ Html.text label_ ]
                                )
                                options
                            )
                        )

                WTokens mHook ->
                    mHook |> Maybe.map viewAutocompleteList |> Maybe.withDefault viewTokenList

                WList element ->
                    viewRows element

                WStep artifact_ ->
                    buildStepSelect cfg
                        { selectedStepIds = Maybe.toList (Maybe.andThen (try tStepId) get)
                        , onRemoveStep = always (rawSet Nothing)
                        , onSelectStep = rawSet << Just << TStepValue
                        , activeAfterSelect = False
                        , mAllowedStepTypes = artifact_.accepts
                        }

                WSteps artifact_ ->
                    buildStepSelect cfg
                        { selectedStepIds = List.filterMap (try tStepId) items
                        , onRemoveStep = \stepId -> setItems (List.filter (\stepValue -> try tStepId stepValue /= Just stepId) items)
                        , onSelectStep = \stepId -> setItems (items ++ [ TStepValue stepId ])
                        , activeAfterSelect = True
                        , mAllowedStepTypes = artifact_.accepts
                        }

                WRecord recordFields ->
                    Html.div [ class "record-item-fields" ]
                        (List.map
                            (\f ->
                                viewValue
                                    { label = Maybe.withDefault f.name f.label
                                    , hint = Nothing
                                    , stateKey = cfg.stateKey ++ "." ++ f.name
                                    , id = cfg.id ++ "-" ++ f.name
                                    , changed = False
                                    , readOnly = cfg.readOnly
                                    , context = cfg.context
                                    , widget = f.widget
                                    }
                                    (\_ -> Dict.get f.name currentDict)
                                    (\mValue ->
                                        rawSet <|
                                            Just <|
                                                TRecordValue <|
                                                    case mValue of
                                                        Just fieldValue ->
                                                            Dict.insert f.name fieldValue currentDict

                                                        Nothing ->
                                                            Dict.remove f.name currentDict
                                    )
                            )
                            recordFields
                        )

        viewField : Field -> Html (Flow Model ())
        viewField field =
            let
                fieldLabel =
                    Maybe.withDefault field.name field.label

                fieldNotices =
                    noticesForField field.name

                fieldHint =
                    if String.isEmpty field.help then
                        Nothing

                    else
                        Just field.help

                fieldId =
                    field.name
                        ++ (case field.widget of
                                WTokens _ ->
                                    "-list-input"

                                _ ->
                                    "-input"
                           )

                fieldHasChanged =
                    not readOnly
                        && not field.readOnly
                        && fieldChanged (try (args << key field.name)) (try (argsLens << key field.name) model) originalRecord

                viewFieldNotice notice =
                    Html.div [ class "field-notice", class "field-notice-info" ]
                        [ iconCustom True "info" [ class "field-notice-icon" ]
                        , Html.div [ class "field-notice-markdown" ] <| Markdown.plain notice.message
                        ]

                withFieldNotices html =
                    case fieldNotices of
                        [] ->
                            html

                        _ ->
                            Html.div [ class "field-with-notices" ]
                                [ html
                                , Html.div [ class "field-notices" ] (List.map viewFieldNotice fieldNotices)
                                ]
            in
            withFieldNotices <|
                viewValue
                    { label = fieldLabel
                    , hint = fieldHint
                    , stateKey = tableId ++ ":" ++ field.name
                    , id = fieldId
                    , changed = fieldHasChanged
                    , readOnly = readOnly || field.readOnly
                    , context = Dict.empty
                    , widget = field.widget
                    }
                    (\_ -> try (argsLens << key field.name << just) model)
                    (\mValue -> Flow.modify (set (argsLens << key field.name) mValue))

        visibleFields fields =
            List.filter
                (\f -> not f.readOnly || Maybe.isJust (try (argsLens << key f.name) model))
                fields
    in
    Html.div [ class "form-group" ] <|
        case stepDef of
            FileUpload _ ->
                []

            Derivation fields _ ->
                List.map viewField (visibleFields fields)

            Download fields ->
                List.map viewField (visibleFields fields)


viewStepNoteField : Model -> Bool -> String -> Html (Flow Model ())
viewStepNoteField model readOnly tableId =
    let
        noteLens =
            Lenses.stepFormsAt tableId << edited << just << note

        currentNote =
            try noteLens model |> Maybe.withDefault ""

        originalRecord =
            try (Lenses.stepFormsAt tableId << edited << just) model
                |> Maybe.andThen .id
                |> Maybe.andThen (\id_ -> try (stepRecordById id_) model)
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


viewInactiveIconButtonWithTooltip : String -> String -> Html (Flow Model ())
viewInactiveIconButtonWithTooltip iconName tooltip =
    Html.button
        [ class "icon-btn icon-btn-inactive"
        , attribute "aria-disabled" "true"
        , attribute "aria-label" tooltip
        , title tooltip
        ]
        [ icon False iconName
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


viewStoppingIndicator : Html (Flow Model ())
viewStoppingIndicator =
    Html.button
        [ class "icon-btn icon-btn-inactive"
        , attribute "aria-disabled" "true"
        , attribute "aria-label" "Stopping"
        , title "Stopping"
        ]
        [ iconCustom True "progress_activity" [ class "icon-btn-spinner" ]
        , Html.span [ class "icon-btn-text" ] [ Html.text "Stopping" ]
        ]


viewUploadButton : Flow Model () -> Html (Flow Model ())
viewUploadButton =
    viewIconButtonWithTooltip "upload_file" True "Upload files"


viewScratchButton : Flow Model () -> Html (Flow Model ())
viewScratchButton =
    viewIconButtonWithTooltip "folder_copy" True "From scratch"


viewUploadProgress : Int -> UploadProgress -> Html (Flow Model ())
viewUploadProgress stepId { sent, size } =
    viewProgressBar { done = Just sent, total = Just size } (Just (Ingest.cancelUpload stepId))


viewIngestProgress : { done : Maybe Int, total : Maybe Int } -> Html (Flow Model ())
viewIngestProgress progress =
    viewProgressBar progress Nothing


viewProgressBar : { done : Maybe Int, total : Maybe Int } -> Maybe (Flow Model ()) -> Html (Flow Model ())
viewProgressBar { done, total } mCancel =
    let
        pct =
            Maybe.map2
                (\d t ->
                    if t == 0 then
                        0

                    else
                        toFloat d / toFloat t * 100
                )
                done
                total

        fillAttrs =
            case pct of
                Just value ->
                    [ style "width" (String.fromInt (round value) ++ "%") ]

                Nothing ->
                    [ class "upload-progress-fill--indeterminate" ]

        progressTitle =
            case pct of
                Just value ->
                    String.fromInt (round value) ++ "%"

                Nothing ->
                    "In progress"
    in
    Html.div
        [ class "upload-progress"
        , title progressTitle
        ]
        [ Html.div [ class "upload-progress-bar" ]
            [ Html.div (class "upload-progress-fill" :: fillAttrs) [] ]
        , Html.viewMaybe (viewIconButtonWithTooltip "close" False "Cancel upload") mCancel
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
            keyPart "bool"
                (if b then
                    "true"

                 else
                    "false"
                )

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
