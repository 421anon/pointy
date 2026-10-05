module Model.Selection exposing
    ( actionDefinitions
    , actionSpec
    , actionVisible
    , clear
    , currentListingScope
    , displayOrder
    , dropActionToken
    , dropAllowed
    , dropEdgeAllowed
    , folderLinks
    , hasSelection
    , isCut
    , isSelected
    , listingEditable
    , moveValid
    , onListingRoute
    , organizeTargets
    , pruneSelection
    , rangeSelect
    , reorderDropAllowed
    , reorderForEdgeDrop
    , resolveDropAction
    , resolveInto
    , selectAllState
    , selectOne
    , selectionRefs
    , shouldHide
    , storedOrder
    , toggle
    , visibleRefs
    )

import Accessors exposing (get, has, set, try)
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildKind(..), ChildLink, ChildRef, ListingScope, ListingSelection, Model, OrganizeAction(..), OrganizeDialogMode(..), OrganizeDrag, OrganizeDropAction(..))
import Model.Lenses exposing (currentProjectPath, isReadOnlyRoute, listingPreferences, listingSelection, organizeClipboard, organizeDrag, projectsDict, route, steps)
import Model.Lib as Lib
import Route


onListingRoute : Model -> Bool
onListingRoute model =
    has (route << Route.page << Route.project) model


listingEditable : Model -> Bool
listingEditable model =
    not (isReadOnlyRoute model) && onListingRoute model


folderLinks : Model -> ListingScope -> List ChildLink
folderLinks model parentId =
    Dict.get parentId (projectsDict model) |> Maybe.map .children |> Maybe.withDefault []


childLinkIn : Model -> ListingScope -> ChildRef -> Maybe ChildLink
childLinkIn model scope ref =
    List.find (Model.sameEntity ref) (folderLinks model scope)


isSelected : Maybe ListingSelection -> ListingScope -> ChildRef -> Bool
isSelected mSelection scope ref =
    case mSelection of
        Just selection ->
            selection.scope == scope && List.any (Model.sameEntity ref) selection.refs

        Nothing ->
            False


isCut : Model -> ListingScope -> ChildRef -> Bool
isCut model scope ref =
    case get organizeClipboard model of
        Just { mode, sourceScope, refs } ->
            mode == Model.ClipboardCut && sourceScope == scope && not (isReadOnlyRoute model) && List.any (Model.sameEntity ref) refs

        Nothing ->
            False


selectionRefs : Model -> List ChildRef
selectionRefs model =
    get listingSelection model |> Maybe.map .refs |> Maybe.withDefault []


hasSelection : Model -> Bool
hasSelection model =
    not (List.isEmpty (selectionRefs model))


clear : Model -> Model
clear =
    set listingSelection Nothing


currentListingScope : Model -> Maybe ListingScope
currentListingScope model =
    try currentProjectPath model |> Maybe.map Route.pathProjectId


selectOne : ListingScope -> ChildRef -> Model -> Model
selectOne scope ref model =
    set listingSelection (Just { scope = scope, refs = [ ref ], anchor = Just ref }) model


toggle : ListingScope -> ChildRef -> Model -> Model
toggle scope ref model =
    case get listingSelection model of
        Just selection ->
            if selection.scope /= scope then
                selectOne scope ref model

            else if List.any (Model.sameEntity ref) selection.refs then
                case keepReanchored (not << Model.sameEntity ref) selection of
                    Just kept ->
                        set listingSelection (Just kept) model

                    Nothing ->
                        clear model

            else
                set listingSelection (Just { selection | refs = selection.refs ++ [ ref ], anchor = Just ref }) model

        Nothing ->
            selectOne scope ref model


rangeSelect : ListingScope -> List ChildRef -> ChildRef -> Model -> Model
rangeSelect scope orderedRefs ref model =
    case get listingSelection model of
        Just selection ->
            if selection.scope /= scope then
                selectOne scope ref model

            else
                let
                    anchorRef =
                        selection.anchor |> Maybe.withDefault ref

                    bounds =
                        Maybe.map2 Tuple.pair
                            (List.findIndex (Model.sameEntity anchorRef) orderedRefs)
                            (List.findIndex (Model.sameEntity ref) orderedRefs)
                in
                case bounds of
                    Just ( start, end ) ->
                        let
                            range =
                                List.drop (min start end) orderedRefs |> List.take (abs (end - start) + 1)
                        in
                        set listingSelection (Just { selection | refs = range, anchor = Just anchorRef }) model

                    Nothing ->
                        selectOne scope ref model

        Nothing ->
            selectOne scope ref model


selectAllState : ListingScope -> List ChildRef -> Maybe ListingSelection
selectAllState scope refs =
    if List.isEmpty refs then
        Nothing

    else
        Just { scope = scope, refs = refs, anchor = List.head refs }


keepReanchored : (ChildRef -> Bool) -> ListingSelection -> Maybe ListingSelection
keepReanchored keep selection =
    let
        kept =
            List.filter keep selection.refs

        anchor =
            selection.anchor |> Maybe.filter (\a -> List.any (Model.sameEntity a) kept)
    in
    if List.isEmpty kept then
        Nothing

    else
        Just { selection | refs = kept, anchor = anchor }


pruneSelection : Model -> Model
pruneSelection model =
    case get listingSelection model of
        Nothing ->
            model

        Just selection ->
            let
                visible =
                    visibleRefs model selection.scope
            in
            case keepReanchored (\ref -> List.any (Model.sameEntity ref) visible) selection of
                Just kept ->
                    set listingSelection (Just kept) model

                Nothing ->
                    clear model


visibleRefs : Model -> ListingScope -> List ChildRef
visibleRefs model scope =
    let
        prefs =
            get listingPreferences model
    in
    folderLinks model scope
        |> List.filter (\link -> prefs.showHidden || not link.hidden)
        |> List.map Model.childRefOf


reorderDropAllowed : Model -> Bool
reorderDropAllowed model =
    let
        prefs =
            get listingPreferences model
    in
    prefs.sort == Model.SortManual && not prefs.groupByType && not (isReadOnlyRoute model)


dropEdgeAllowed : Model -> Maybe OrganizeDropAction
dropEdgeAllowed model =
    if reorderDropAllowed model then
        Just OrganizeDropMove

    else
        Nothing


moveValid : Model -> ListingScope -> Int -> ChildRef -> Bool
moveValid model sourceScope targetId ref =
    sourceScope
        /= targetId
        && not (Lib.linkCreatesCycle (projectsDict model) targetId ref)


resolveInto : Model -> OrganizeDrag -> Int -> List OrganizeDropAction
resolveInto model drag targetId =
    if not (listingEditable model) then
        []

    else
        let
            movePossible =
                List.any (moveValid model drag.sourceScope targetId) drag.refs

            linkPossible =
                List.any (Lib.linkValid model targetId) drag.refs
        in
        List.filter Tuple.first
            [ ( movePossible, OrganizeDropMove )
            , ( linkPossible, OrganizeDropLink )
            ]
            |> List.map Tuple.second


resolveDropAction : Bool -> List OrganizeDropAction -> Maybe OrganizeDropAction
resolveDropAction linkRequested allowed =
    let
        preference =
            if linkRequested then
                [ OrganizeDropLink, OrganizeDropMove ]

            else
                [ OrganizeDropMove, OrganizeDropLink ]
    in
    List.find (\action -> List.member action allowed) preference


dropActionToken : OrganizeDropAction -> String
dropActionToken action =
    case action of
        OrganizeDropMove ->
            "move"

        OrganizeDropLink ->
            "link"


dropAllowed : Model -> Int -> List OrganizeDropAction
dropAllowed model folderId =
    case get organizeDrag model of
        Nothing ->
            []

        Just drag ->
            resolveInto model drag folderId


displayOrder : Model.ListingPreferences -> List ChildRef -> List ChildRef
displayOrder prefs refs =
    let
        ordered =
            if prefs.descending then
                List.reverse refs

            else
                refs

        ( folders, others ) =
            List.partition (\ref -> ref.kind == ProjectChild) ordered
    in
    if prefs.foldersFirst then
        folders ++ others

    else
        ordered


storedOrder : Model.ListingPreferences -> List ChildRef -> List ChildRef
storedOrder prefs refs =
    if prefs.descending then
        List.reverse refs

    else
        refs


reorderForEdgeDrop : List ChildRef -> List ChildRef -> ChildRef -> Bool -> List ChildRef
reorderForEdgeDrop visual payload ref before =
    let
        present =
            List.filter (\r -> List.any (Model.sameEntity r) visual) payload

        withoutPayload =
            List.filter (\r -> not (List.any (Model.sameEntity r) present)) visual

        insertAt =
            case List.findIndex (Model.sameEntity ref) visual of
                Just index ->
                    let
                        removedBefore =
                            List.take index visual
                                |> List.filter (\r -> List.any (Model.sameEntity r) present)
                                |> List.length
                    in
                    (if before then
                        index - removedBefore

                     else
                        index - removedBefore + 1
                    )
                        |> clamp 0 (List.length withoutPayload)

                Nothing ->
                    List.length withoutPayload
    in
    if List.isEmpty present then
        visual

    else
        List.take insertAt withoutPayload ++ present ++ List.drop insertAt withoutPayload


organizeTargets : Model -> OrganizeDialogMode -> ListingScope -> List ChildRef -> List ( Int, String )
organizeTargets model mode sourceScope refs =
    let
        projects_ =
            projectsDict model

        projectRefs =
            List.filter (\ref -> ref.kind == ProjectChild) refs

        excluded id =
            (mode == OrganizeMove && sourceScope == id)
                || List.any (\ref -> ref.id == id || Model.isAncestorProject projects_ ref.id id) projectRefs
    in
    Dict.toList projects_
        |> List.filter (\( id, _ ) -> not (excluded id))
        |> List.map (\( id, _ ) -> ( id, Lib.canonicalNamePath model id ))
        |> List.sortBy Tuple.second


shouldHide : Model -> Bool
shouldHide model =
    case get listingSelection model of
        Just selection ->
            let
                isHidden ref =
                    childLinkIn model selection.scope ref |> Maybe.map .hidden |> Maybe.withDefault False
            in
            List.any (not << isHidden) selection.refs

        Nothing ->
            False


selectionHasLocked : Model -> Bool
selectionHasLocked model =
    let
        lockedStep step =
            step.review /= Nothing
    in
    selectionRefs model
        |> List.any
            (\ref ->
                ref.kind
                    == StepChild
                    && (Dict.get ref.id (get steps model) |> Maybe.unwrap False lockedStep)
            )


actionDefinitions : List OrganizeAction
actionDefinitions =
    [ OrganizeMoveAction
    , OrganizeLinkAction
    , OrganizeGroupAction
    , OrganizeCutAction
    , OrganizeCopyAction
    , OrganizeHideAction
    , OrganizeRemoveAction
    , OrganizeDuplicateAction
    , OrganizeDeleteAction
    , OrganizeClearAction
    , OrganizePasteAction
    , OrganizePasteDuplicateAction
    , OrganizeNewFolderAction
    ]


actionSpec :
    Model
    -> OrganizeAction
    -> { label : String, icon : String, inBar : Bool }
actionSpec model action =
    case action of
        OrganizeMoveAction ->
            { label = "Move to...", icon = "drive_file_move", inBar = True }

        OrganizeLinkAction ->
            { label = "Link to...", icon = "drive_file_move", inBar = True }

        OrganizeGroupAction ->
            { label = "Group into new folder"
            , icon = "create_new_folder"
            , inBar = True
            }

        OrganizeCutAction ->
            { label = "Cut", icon = "content_cut", inBar = True }

        OrganizeCopyAction ->
            { label = "Copy", icon = "content_copy", inBar = True }

        OrganizeHideAction ->
            { label =
                if shouldHide model then
                    "Hide"

                else
                    "Unhide"
            , icon = "visibility_off"
            , inBar = True
            }

        OrganizeRemoveAction ->
            { label = "Remove from here", icon = "remove", inBar = True }

        OrganizeDuplicateAction ->
            { label = "Duplicate", icon = "copy_all", inBar = True }

        OrganizeDeleteAction ->
            { label = "Delete permanently", icon = "delete", inBar = True }

        OrganizeClearAction ->
            { label = "Clear", icon = "close", inBar = True }

        OrganizePasteAction ->
            { label = "Paste", icon = "content_paste", inBar = False }

        OrganizePasteDuplicateAction ->
            { label = "Paste as duplicate"
            , icon = "content_paste_go"
            , inBar = False
            }

        OrganizeNewFolderAction ->
            { label = "New folder", icon = "create_new_folder", inBar = False }


actionVisible : Model -> OrganizeAction -> Bool
actionVisible model action =
    let
        selection =
            get listingSelection model

        hasSel =
            hasSelection model

        hasClipboard =
            Maybe.isJust (get organizeClipboard model)

        selectionInFolder =
            Maybe.isJust selection

        hasFolder =
            Maybe.isJust (currentListingScope model)

        editable =
            listingEditable model
    in
    case action of
        OrganizeGroupAction ->
            editable && hasSel && hasFolder

        OrganizeDuplicateAction ->
            editable && hasSel && hasFolder

        OrganizeHideAction ->
            editable && hasSel && selectionInFolder

        OrganizeRemoveAction ->
            editable && hasSel && selectionInFolder

        OrganizeDeleteAction ->
            editable && hasSel && not (selectionHasLocked model)

        OrganizeClearAction ->
            hasSel

        OrganizePasteAction ->
            editable && hasClipboard && hasFolder

        OrganizePasteDuplicateAction ->
            editable && hasClipboard && hasFolder

        OrganizeNewFolderAction ->
            editable && hasFolder

        _ ->
            editable && hasSel
