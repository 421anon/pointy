module Model.Selection exposing
    ( ActionSpec
    , actionSpec
    , actionTarget
    , actionVisible
    , clear
    , currentListingScope
    , displayOrder
    , dropActionToken
    , dropAllowed
    , edgeAllowed
    , edgeDropAllowed
    , folderLinks
    , hasSelection
    , isCut
    , isDragged
    , isSelected
    , listingEditable
    , moveValid
    , onListingRoute
    , organizeTargets
    , pruneSelection
    , rangeSelect
    , reorderForEdgeDrop
    , reorderGaps
    , resolveDropAction
    , resolveInto
    , rowActions
    , selectAllState
    , selectOne
    , selectionActions
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


isDragged : Model -> ListingScope -> ChildRef -> Bool
isDragged model scope ref =
    case get organizeDrag model of
        Just drag ->
            drag.sourceScope == scope && List.any (Model.sameEntity ref) drag.refs

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


reorderGaps : Model -> ListingScope -> List ChildRef -> Maybe ( Int, Int )
reorderGaps model scope displayed =
    get organizeDrag model
        |> Maybe.filter (\drag -> drag.sourceScope == scope && reorderDropAllowed model)
        |> Maybe.map (\drag -> landingGaps (get listingPreferences model) displayed drag.refs)


landingGaps : Model.ListingPreferences -> List ChildRef -> List ChildRef -> ( Int, Int )
landingGaps prefs displayed payload =
    let
        end =
            List.length displayed
    in
    if prefs.foldersFirst then
        let
            isFolder ref =
                ref.kind == ProjectChild

            staying =
                List.indexedMap Tuple.pair displayed
                    |> List.filter (\( _, ref ) -> not (List.any (Model.sameEntity ref) payload))
        in
        ( if List.any (not << isFolder) payload then
            staying
                |> List.filter (\( _, ref ) -> isFolder ref)
                |> List.last
                |> Maybe.unwrap 0 (\( index, _ ) -> index + 1)

          else
            0
        , if List.any isFolder payload then
            staying
                |> List.find (\( _, ref ) -> not (isFolder ref))
                |> Maybe.unwrap end Tuple.first

          else
            end
        )

    else
        ( 0, end )


edgeAllowed : Maybe ( Int, Int ) -> Int -> Bool -> Bool
edgeAllowed gaps index before =
    case gaps of
        Just ( first, last ) ->
            let
                gap =
                    if before then
                        index

                    else
                        index + 1
            in
            first <= gap && gap <= last

        Nothing ->
            False


edgeDropAllowed : Model -> ListingScope -> ChildRef -> Bool -> Bool
edgeDropAllowed model scope ref before =
    let
        displayed =
            displayOrder (get listingPreferences model) (visibleRefs model scope)
    in
    List.findIndex (Model.sameEntity ref) displayed
        |> Maybe.unwrap False (\index -> edgeAllowed (reorderGaps model scope displayed) index before)


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
            List.filter (\r -> List.any (Model.sameEntity r) payload) visual

        stays r =
            not (List.any (Model.sameEntity r) present)

        withoutPayload =
            List.filter stays visual

        insertAt =
            case List.findIndex (Model.sameEntity ref) visual of
                Just index ->
                    List.take
                        (if before then
                            index

                         else
                            index + 1
                        )
                        visual
                        |> List.count stays

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


shouldHide : Model -> ListingScope -> List ChildRef -> Bool
shouldHide model scope refs =
    let
        isHidden ref =
            childLinkIn model scope ref |> Maybe.map .hidden |> Maybe.withDefault False
    in
    List.any (not << isHidden) refs


anyLocked : Model -> List ChildRef -> Bool
anyLocked model refs =
    let
        lockedStep step =
            step.review /= Nothing
    in
    List.any
        (\ref ->
            ref.kind
                == StepChild
                && (Dict.get ref.id (get steps model) |> Maybe.unwrap False lockedStep)
        )
        refs


actionTarget : Model -> Maybe ( ListingScope, List ChildRef )
actionTarget model =
    case get listingSelection model of
        Just selection ->
            Just ( selection.scope, selection.refs )

        Nothing ->
            Maybe.map (\scope -> ( scope, [] )) (currentListingScope model)


selectionActions : Model -> List ( OrganizeAction, ActionSpec )
selectionActions model =
    actionTarget model
        |> Maybe.unwrap [] (\( scope, refs ) -> availableActions model scope refs)


rowActions : Model -> ListingScope -> ChildRef -> List ( OrganizeAction, ActionSpec )
rowActions model scope ref =
    availableActions model scope [ ref ]
        |> List.filter (Tuple.second >> .inRow)


availableActions : Model -> ListingScope -> List ChildRef -> List ( OrganizeAction, ActionSpec )
availableActions model scope refs =
    actionDefinitions
        |> List.filter (actionVisible model refs)
        |> List.map (\action -> ( action, actionSpec model scope refs action ))


actionDefinitions : List OrganizeAction
actionDefinitions =
    [ OrganizeMoveAction
    , OrganizeLinkAction
    , OrganizeGroupAction
    , OrganizeCutAction
    , OrganizeCopyAction
    , OrganizePasteAction
    , OrganizeClearClipboardAction
    , OrganizeHideAction
    , OrganizeRemoveAction
    , OrganizeDuplicateAction
    , OrganizeDeleteAction
    , OrganizeClearAction
    , OrganizeNewFolderAction
    ]


type alias ActionSpec =
    { label : String
    , icon : String
    , inBar : Bool
    , inRow : Bool
    }


actionSpec : Model -> ListingScope -> List ChildRef -> OrganizeAction -> ActionSpec
actionSpec model scope refs action =
    case action of
        OrganizeMoveAction ->
            { label = "Move to...", icon = "drive_file_move", inBar = True, inRow = False }

        OrganizeLinkAction ->
            { label = "Link to...", icon = "drive_file_move", inBar = True, inRow = False }

        OrganizeGroupAction ->
            { label = "Group into new folder", icon = "create_new_folder", inBar = True, inRow = False }

        OrganizeCutAction ->
            { label = "Cut", icon = "content_cut", inBar = True, inRow = False }

        OrganizeCopyAction ->
            { label = "Copy", icon = "content_copy", inBar = True, inRow = False }

        OrganizeHideAction ->
            if shouldHide model scope refs then
                { label = "Hide", icon = "visibility_off", inBar = True, inRow = True }

            else
                { label = "Unhide", icon = "visibility", inBar = True, inRow = True }

        OrganizeRemoveAction ->
            { label = "Remove from here", icon = "remove", inBar = True, inRow = True }

        OrganizeDuplicateAction ->
            { label = "Duplicate", icon = "copy_all", inBar = True, inRow = True }

        OrganizeDeleteAction ->
            { label = "Delete permanently", icon = "delete", inBar = True, inRow = False }

        OrganizeClearAction ->
            { label = "Clear", icon = "close", inBar = True, inRow = False }

        OrganizePasteAction ->
            { label = "Paste", icon = "content_paste", inBar = False, inRow = False }

        OrganizeClearClipboardAction ->
            { label = "Clear clipboard", icon = "content_paste_off", inBar = not (List.isEmpty refs), inRow = False }

        OrganizeNewFolderAction ->
            { label = "New folder", icon = "create_new_folder", inBar = False, inRow = False }


actionVisible : Model -> List ChildRef -> OrganizeAction -> Bool
actionVisible model refs action =
    let
        hasRefs =
            not (List.isEmpty refs)

        hasClipboard =
            Maybe.isJust (get organizeClipboard model)

        editable =
            listingEditable model
    in
    case action of
        OrganizeDeleteAction ->
            editable && hasRefs && not (anyLocked model refs)

        OrganizeClearAction ->
            hasRefs

        OrganizePasteAction ->
            editable && hasClipboard

        OrganizeClearClipboardAction ->
            editable && hasClipboard

        OrganizeNewFolderAction ->
            editable

        _ ->
            editable && hasRefs
