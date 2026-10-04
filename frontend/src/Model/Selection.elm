module Model.Selection exposing
    ( actionDefinitions
    , actionIcon
    , actionInBar
    , actionLabel
    , actionVisible
    , clear
    , currentListingScope
    , dropActionToken
    , dropAllowed
    , dropEdgeAllowed
    , displayOrder
    , folderLinks
    , hasSelection
    , isSelected
    , linkValid
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
import Model.Core as Model exposing (ChildKind(..), ChildLink, ChildRef, ListingScope(..), ListingSelection, Model, OrganizeAction(..), OrganizeDialogMode(..), OrganizeDrag, OrganizeDropAction(..))
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
folderLinks model scope =
    case scope of
        ProjectListing parentId ->
            Dict.get parentId (projectsDict model) |> Maybe.map .children |> Maybe.withDefault []

        UnfiledListing ->
            []


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


selectionRefs : Model -> List ChildRef
selectionRefs model =
    get listingSelection model |> Maybe.map .refs |> Maybe.withDefault []


hasSelection : Model -> Bool
hasSelection model =
    not (List.isEmpty (selectionRefs model))


clear : Model -> Model
clear =
    set listingSelection Nothing


currentListingScope : Model -> ListingScope
currentListingScope model =
    try currentProjectPath model
        |> Maybe.map (Route.pathProjectId >> ProjectListing)
        |> Maybe.withDefault UnfiledListing


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


reorderDropAllowed : Model -> ListingScope -> Bool
reorderDropAllowed model scope =
    let
        prefs =
            get listingPreferences model
    in
    case scope of
        UnfiledListing ->
            False

        ProjectListing _ ->
            prefs.sort == Model.SortManual && not prefs.groupByType && not (isReadOnlyRoute model)


dropEdgeAllowed : Model -> ListingScope -> Maybe OrganizeDropAction
dropEdgeAllowed model scope =
    if reorderDropAllowed model scope then
        Just OrganizeDropMove

    else
        Nothing


moveValid : Model -> ListingScope -> Int -> ChildRef -> Bool
moveValid model sourceScope targetId ref =
    sourceScope
        /= ProjectListing targetId
        && not (Lib.linkCreatesCycle (projectsDict model) targetId ref)


linkValid : Model -> Int -> ChildRef -> Bool
linkValid =
    Lib.linkValid


resolveInto : Model -> OrganizeDrag -> Int -> List OrganizeDropAction
resolveInto model drag targetId =
    if not (listingEditable model) then
        []

    else
        let
            movePossible =
                List.any (moveValid model drag.sourceScope targetId) drag.refs

            linkPossible =
                List.any (linkValid model targetId) drag.refs
        in
        List.filterMap identity
            [ if movePossible then
                Just OrganizeDropMove

              else
                Nothing
            , if linkPossible then
                Just OrganizeDropLink

              else
                Nothing
            ]


resolveDropAction : Bool -> List OrganizeDropAction -> Maybe OrganizeDropAction
resolveDropAction linkRequested allowed =
    let
        prefer action =
            if List.member action allowed then
                Just action

            else
                Nothing
    in
    if linkRequested then
        prefer OrganizeDropLink
            |> Maybe.orElse (prefer OrganizeDropMove)

    else
        prefer OrganizeDropMove
            |> Maybe.orElse (prefer OrganizeDropLink)


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

        mIndex =
            List.findIndex (Model.sameEntity ref) visual

        removedBefore =
            case mIndex of
                Just index ->
                    List.take index visual
                        |> List.filter (\r -> List.any (Model.sameEntity r) present)
                        |> List.length

                Nothing ->
                    0

        withoutPayload =
            List.filter (\r -> not (List.any (Model.sameEntity r) present)) visual

        insertAt =
            case mIndex of
                Just index ->
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
            (mode == OrganizeMove && sourceScope == ProjectListing id)
                || List.any (\ref -> ref.id == id || Model.isAncestorProject projects_ ref.id id) projectRefs
    in
    Dict.toList projects_
        |> List.filter (\( id, _ ) -> not (excluded id))
        |> List.map (\( id, _ ) -> ( id, Lib.canonicalNamePath model id ))
        |> List.sortBy Tuple.second


shouldHide : Model -> Bool
shouldHide model =
    let
        scope =
            get listingSelection model |> Maybe.map .scope |> Maybe.withDefault UnfiledListing

        isHidden ref =
            childLinkIn model scope ref |> Maybe.map .hidden |> Maybe.withDefault False
    in
    selectionRefs model |> List.any (not << isHidden)


hideUnhideLabel : Model -> String
hideUnhideLabel model =
    if shouldHide model then
        "Hide"

    else
        "Unhide"


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


actionInBar : OrganizeAction -> Bool
actionInBar action =
    case action of
        OrganizePasteAction ->
            False

        OrganizePasteDuplicateAction ->
            False

        OrganizeNewFolderAction ->
            False

        _ ->
            True


actionLabel : Model -> OrganizeAction -> String
actionLabel model action =
    case action of
        OrganizeMoveAction ->
            "Move to..."

        OrganizeLinkAction ->
            "Link to..."

        OrganizeGroupAction ->
            "Group into new folder"

        OrganizeCutAction ->
            "Cut"

        OrganizeCopyAction ->
            "Copy"

        OrganizeHideAction ->
            hideUnhideLabel model

        OrganizeRemoveAction ->
            "Remove from here"

        OrganizeDuplicateAction ->
            "Duplicate"

        OrganizeDeleteAction ->
            "Delete permanently"

        OrganizeClearAction ->
            "Clear"

        OrganizePasteAction ->
            "Paste"

        OrganizePasteDuplicateAction ->
            "Paste as duplicate"

        OrganizeNewFolderAction ->
            "New folder"


actionIcon : OrganizeAction -> String
actionIcon action =
    case action of
        OrganizeMoveAction ->
            "drive_file_move"

        OrganizeLinkAction ->
            "drive_file_move"

        OrganizeGroupAction ->
            "create_new_folder"

        OrganizeCutAction ->
            "content_cut"

        OrganizeCopyAction ->
            "content_copy"

        OrganizeHideAction ->
            "visibility_off"

        OrganizeRemoveAction ->
            "remove"

        OrganizeDuplicateAction ->
            "copy_all"

        OrganizeDeleteAction ->
            "delete"

        OrganizeClearAction ->
            "close"

        OrganizePasteAction ->
            "content_paste"

        OrganizePasteDuplicateAction ->
            "content_paste_go"

        OrganizeNewFolderAction ->
            "create_new_folder"


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
            selection |> Maybe.map (\s -> s.scope /= UnfiledListing) |> Maybe.withDefault False

        hasFolder =
            currentListingScope model /= UnfiledListing

        editable =
            listingEditable model
    in
    case action of
        OrganizeMoveAction ->
            editable && hasSel

        OrganizeLinkAction ->
            editable && hasSel

        OrganizeGroupAction ->
            editable && hasSel && hasFolder

        OrganizeCutAction ->
            editable && hasSel

        OrganizeCopyAction ->
            editable && hasSel

        OrganizeHideAction ->
            editable && hasSel && selectionInFolder

        OrganizeRemoveAction ->
            editable && hasSel && selectionInFolder

        OrganizeDuplicateAction ->
            editable && hasSel && hasFolder

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
