module Organize exposing (..)

import Accessors exposing (get, just, over)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (success)
import Dict exposing (Dict)
import Dict.Accessors
import Extra.Http as Http
import Flow exposing (Flow)
import Json.Decode as Decode
import Keyboard
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildKind(..), ChildRef, ClipboardMode(..), ListingScope, Model, OrganizeAction(..), OrganizeDialog, OrganizeDialogMode(..), OrganizeDragEvent(..), OrganizeDropAction(..), OrganizeDropTarget(..), ProjectRecord, TemplateSource(..), TreeOp(..), blankProject)
import Model.Lenses exposing (listingPreferences, listingSelection, organizeClipboard, organizeContextMenu, organizeDialog, organizeDrag, projectRecordById, projects, projectsDict, stepConfig, stepRecordById, store)
import Model.Lib as Lib
import Model.Selection as Selection
import Model.Shadow exposing (StepConfig)
import Specs


dialogId : String
dialogId =
    "organize-dialog"


selectedScopeAndRefs : Model -> Maybe ( ListingScope, List ChildRef )
selectedScopeAndRefs model =
    case get listingSelection model of
        Just selection ->
            Just ( selection.scope, selection.refs )

        Nothing ->
            Maybe.map (\scope -> ( scope, [] )) (Selection.currentListingScope model)


clickRow : ListingScope -> List ChildRef -> Bool -> ChildRef -> Flow Model ()
clickRow scope orderedRefs shift ref =
    Flow.modify
        (if shift then
            Selection.rangeSelect scope orderedRefs ref

         else
            Selection.toggle scope ref
        )


toggleCheckbox : ListingScope -> ChildRef -> Flow Model ()
toggleCheckbox scope ref =
    Flow.modify (Selection.toggle scope ref)


selectAll : Flow Model ()
selectAll =
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.when (Selection.listingEditable model) <|
                    case Selection.currentListingScope model of
                        Just scope ->
                            Flow.setAll listingSelection (Selection.selectAllState scope (Selection.visibleRefs model scope))

                        Nothing ->
                            Flow.pure ()
            )


clearSelection : Flow Model ()
clearSelection =
    Flow.setAll listingSelection Nothing


openRowMenu : ListingScope -> Int -> Int -> ChildRef -> Flow Model ()
openRowMenu scope x y ref =
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.when (not (Selection.isSelected (get listingSelection model) scope ref))
                    (Flow.modify (Selection.selectOne scope ref))
                    |> Flow.seq (Flow.setAll organizeContextMenu (Just { x = x, y = y, ref = Just ref }))
            )


openEmptyMenu : Int -> Int -> Flow Model ()
openEmptyMenu x y =
    Flow.setAll organizeContextMenu (Just { x = x, y = y, ref = Nothing })


closeMenu : Flow Model ()
closeMenu =
    Flow.setAll organizeContextMenu Nothing


runAction : OrganizeAction -> Flow Model ()
runAction action =
    closeMenu
        |> Flow.seq
            (Flow.get
                |> Flow.andThen
                    (\model ->
                        Flow.when (Selection.listingEditable model) <|
                            case selectedScopeAndRefs model of
                                Just ( scope, refs ) ->
                                    runActionOn scope refs action

                                Nothing ->
                                    Flow.pure ()
                    )
            )


runActionOn : ListingScope -> List ChildRef -> OrganizeAction -> Flow Model ()
runActionOn scope refs action =
    case action of
        OrganizeMoveAction ->
            openOrganizeDialogFor OrganizeMove scope refs

        OrganizeLinkAction ->
            openOrganizeDialogFor OrganizeLinkTo scope refs

        OrganizeGroupAction ->
            openOrganizeDialogFor OrganizeGroup scope refs

        OrganizeCutAction ->
            cutSelection

        OrganizeCopyAction ->
            copySelection

        OrganizeHideAction ->
            hideSelection scope refs

        OrganizeRemoveAction ->
            removeSelection scope refs

        OrganizeDuplicateAction ->
            duplicateInto scope refs

        OrganizeDeleteAction ->
            openOrganizeDialogFor OrganizeDelete scope refs

        OrganizeClearAction ->
            clearSelection

        OrganizePasteAction ->
            pasteClipboard

        OrganizeClearClipboardAction ->
            Flow.setAll organizeClipboard Nothing

        OrganizeNewFolderAction ->
            openOrganizeDialogFor OrganizeGroup scope []


openOrganizeDialogFor : OrganizeDialogMode -> ListingScope -> List ChildRef -> Flow Model ()
openOrganizeDialogFor mode scope refs =
    Flow.setAll organizeDialog
        (Just
            { mode = mode
            , sourceScope = scope
            , refs = refs
            , query = ""
            , targetId = Nothing
            , name =
                case mode of
                    OrganizeGroup ->
                        "New folder"

                    _ ->
                        ""
            }
        )
        |> Flow.seq (Actions.openDialog dialogId)


setDialogQuery : String -> Flow Model ()
setDialogQuery query =
    Flow.over (organizeDialog << just) (\dialog -> { dialog | query = query })


setDialogTarget : Int -> Flow Model ()
setDialogTarget targetId =
    Flow.over (organizeDialog << just) (\dialog -> { dialog | targetId = Just targetId })


setDialogName : String -> Flow Model ()
setDialogName name =
    Flow.over (organizeDialog << just) (\dialog -> { dialog | name = name })


confirmDialog : Flow Model ()
confirmDialog =
    Flow.get
        |> Flow.andThen
            (\model ->
                case get organizeDialog model of
                    Nothing ->
                        Flow.pure ()

                    Just dialog ->
                        Flow.when (Selection.listingEditable model) <|
                            let
                                apply =
                                    case dialog.mode of
                                        OrganizeMove ->
                                            dialog.targetId
                                                |> Maybe.unwrap (Flow.pure ())
                                                    (\targetId -> moveRefsInto dialog.sourceScope targetId dialog.refs)

                                        OrganizeLinkTo ->
                                            dialog.targetId
                                                |> Maybe.unwrap (Flow.pure ())
                                                    (\targetId -> linkRefsInto targetId dialog.refs)

                                        OrganizeGroup ->
                                            let
                                                name =
                                                    String.trim dialog.name
                                            in
                                            Flow.when (not (String.isEmpty name))
                                                (groupIntoNewFolder name dialog.sourceScope dialog.refs)

                                        OrganizeDelete ->
                                            confirmDeleteRefs dialog.refs
                            in
                            closeDialog |> Flow.seq apply
            )


closeDialog : Flow Model ()
closeDialog =
    Flow.setAll organizeDialog Nothing
        |> Flow.seq (Actions.closeDialog dialogId)


confirmDeleteRefs : List ChildRef -> Flow Model ()
confirmDeleteRefs refs =
    Flow.when (not (List.isEmpty refs))
        (organizeWithUndo "Delete permanently" (List.map DeleteOp refs))


moveRefsInto : ListingScope -> Int -> List ChildRef -> Flow Model ()
moveRefsInto sourceScope targetId refs =
    organizeOrToast "Move" "Nothing to move." (\model -> List.concatMap (moveOps model sourceScope targetId) refs)


linkRefsInto : Int -> List ChildRef -> Flow Model ()
linkRefsInto targetId refs =
    organizeOrToast "Link" "Nothing to link." (\model -> List.concatMap (linkOps model targetId) refs)


organizeOrToast : String -> String -> (Model -> List TreeOp) -> Flow Model ()
organizeOrToast label emptyMessage buildOps =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    ops =
                        buildOps model
                in
                if List.isEmpty ops then
                    Actions.addToast False emptyMessage

                else
                    organizeWithUndo label ops
            )


removeSelection : ListingScope -> List ChildRef -> Flow Model ()
removeSelection parentId refs =
    Flow.when (not (List.isEmpty refs))
        (organizeWithUndo "Remove from here" (List.map (UnlinkOp parentId) refs))


unlinkChild : Int -> ChildRef -> Flow Model ()
unlinkChild parentId ref =
    organizeWithUndo "Remove from here" [ UnlinkOp parentId ref ]


setChildHidden : Int -> ChildRef -> Bool -> Flow Model ()
setChildHidden parentId ref hidden =
    organizeWithUndo
        (if hidden then
            "Hide"

         else
            "Unhide"
        )
        [ HideOp parentId ref hidden ]


unhideAll : Int -> List ChildRef -> Flow Model ()
unhideAll parentId refs =
    Flow.when (not (List.isEmpty refs))
        (organizeWithUndo "Unhide all" (List.map (\ref -> HideOp parentId ref False) refs))


hideSelection : ListingScope -> List ChildRef -> Flow Model ()
hideSelection parentId refs =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    hidden =
                        Selection.shouldHide model
                in
                Flow.when (not (List.isEmpty refs))
                    (organizeWithUndo
                        (if hidden then
                            "Hide"

                         else
                            "Unhide"
                        )
                        (List.map (\ref -> HideOp parentId ref hidden) refs)
                    )
            )


cutSelection : Flow Model ()
cutSelection =
    Flow.get
        |> Flow.andThen (\model -> Flow.when (Selection.listingEditable model) (setClipboard ClipboardCut "Cut"))


copySelection : Flow Model ()
copySelection =
    Flow.get
        |> Flow.andThen (\model -> Flow.when (Selection.listingEditable model) (setClipboard ClipboardCopy "Copy"))


setClipboard : ClipboardMode -> String -> Flow Model ()
setClipboard mode label =
    Flow.get
        |> Flow.andThen
            (\model ->
                case selectedScopeAndRefs model of
                    Just ( scope, refs ) ->
                        Flow.when (not (List.isEmpty refs))
                            (Flow.setAll organizeClipboard (Just { mode = mode, sourceScope = scope, refs = refs })
                                |> Flow.seq (Actions.addToast True (label ++ " " ++ String.fromInt (List.length refs) ++ " item(s)"))
                            )

                    Nothing ->
                        Flow.pure ()
            )


pasteClipboard : Flow Model ()
pasteClipboard =
    Flow.get
        |> Flow.andThen
            (\model ->
                case get organizeClipboard model of
                    Just clipboard ->
                        Flow.when (Selection.listingEditable model) <|
                            case Selection.currentListingScope model of
                                Just targetId ->
                                    pasteInto model targetId clipboard

                                Nothing ->
                                    Flow.pure ()

                    Nothing ->
                        Flow.pure ()
            )


pasteInto : Model -> Int -> Model.OrganizeClipboard -> Flow Model ()
pasteInto model targetId clipboard =
    if clipboard.mode == ClipboardCut && clipboard.sourceScope == targetId then
        Flow.setAll organizeClipboard Nothing

    else
        let
            ops =
                case clipboard.mode of
                    ClipboardCopy ->
                        List.concatMap (linkOps model targetId) clipboard.refs

                    ClipboardCut ->
                        List.concatMap (moveOps model clipboard.sourceScope targetId) clipboard.refs
        in
        if List.isEmpty ops then
            Actions.addToast False "Nothing to paste."

        else
            organizeWithUndo "Paste" ops
                |> Flow.seq
                    (Flow.when (clipboard.mode == ClipboardCut)
                        (Flow.setAll organizeClipboard Nothing)
                    )


moveOps : Model -> ListingScope -> Int -> ChildRef -> List TreeOp
moveOps model sourceScope targetId ref =
    if Selection.moveValid model sourceScope targetId ref then
        [ UnlinkOp sourceScope ref, LinkOp targetId ref ]

    else
        []


linkOps : Model -> Int -> ChildRef -> List TreeOp
linkOps model targetId ref =
    if Lib.linkValid model targetId ref then
        [ LinkOp targetId ref ]

    else
        []


duplicateInto : Int -> List ChildRef -> Flow Model ()
duplicateInto targetId refs =
    let
        stepRefs =
            List.filter (\ref -> ref.kind == StepChild) refs

        folderRefs =
            List.filter (\ref -> ref.kind == ProjectChild) refs
    in
    Flow.batchM
        (List.map (\ref -> duplicateStep ref.id) stepRefs
            ++ List.map (\ref -> duplicateFolder targetId ref.id) folderRefs
        )
        |> Flow.seq (Flow.when (not (List.isEmpty folderRefs)) (Flow.async Actions.loadProjects))


duplicateStep : Int -> Flow Model ()
duplicateStep stepId =
    Flow.forAll (stepRecordById stepId)
        (\step ->
            Flow.forAll (stepConfig << success << Dict.Accessors.at step.type_ << just)
                (\entry -> Actions.cloneStep (Specs.steps step.type_ entry) step)
        )


insertNewFolder : Int -> Int -> ProjectRecord -> Flow Model ()
insertNewFolder parentId folderId folder =
    let
        link =
            LinkOp parentId (ChildRef ProjectChild folderId)
    in
    Flow.modify
        (over projects (ApiData.map (Dict.insert folderId folder))
            >> over store (Model.applyTreeOps [ link ])
        )


duplicateFolder : Int -> Int -> Flow Model ()
duplicateFolder targetId projectId =
    Flow.forAll (projectRecordById projectId)
        (\project ->
            Flow.forAll (stepConfig << success)
                (\config ->
                    let
                        record =
                            { project
                                | id = Nothing
                                , clientId = Nothing
                                , name = project.name ++ " (Copy)"
                                , isUpdating = False
                                , validationErrors = []
                            }
                    in
                    createFolder config targetId record <|
                        \newId newFolder ->
                            let
                                ops =
                                    List.map (\child -> LinkOp newId (Model.childRefOf child)) (Model.projectChildren project)
                            in
                            insertNewFolder targetId newId newFolder
                                |> Flow.seq (Flow.when (not (List.isEmpty ops)) (organizeWithUndo "Duplicate" ops))
                )
        )


groupIntoNewFolder : String -> Int -> List ChildRef -> Flow Model ()
groupIntoNewFolder name sourceFolderId refs =
    Flow.forAll (stepConfig << success)
        (\config ->
            Flow.get
                |> Flow.andThen
                    (\model ->
                        let
                            projects_ =
                                projectsDict model

                            templateSource =
                                Dict.get sourceFolderId projects_
                                    |> Maybe.map .templateSource
                                    |> Maybe.withDefault (CustomTemplates [])

                            record =
                                { blankProject | name = name, templateSource = templateSource }
                        in
                        createFolder config sourceFolderId record <|
                            \newId newFolder ->
                                Flow.get
                                    |> Flow.andThen
                                        (\freshModel ->
                                            let
                                                ops =
                                                    List.concatMap (moveOps freshModel sourceFolderId newId) refs
                                            in
                                            insertNewFolder sourceFolderId newId newFolder
                                                |> Flow.seq (Flow.when (not (List.isEmpty ops)) (organizeWithUndo "Group into new folder" ops))
                                                |> Flow.seq (Flow.async Actions.loadProjects)
                                        )
                    )
        )


createFolder : StepConfig -> Int -> ProjectRecord -> (Int -> ProjectRecord -> Flow Model ()) -> Flow Model ()
createFolder config parentId record onCreated =
    Api.createProject config parentId record
        |> Flow.andThen
            (\result ->
                case result of
                    Ok newFolder ->
                        case newFolder.id of
                            Just newId ->
                                onCreated newId newFolder

                            Nothing ->
                                Flow.pure ()

                    Err err ->
                        Actions.addToast False (Http.errorMessage err)
            )


organizeWithUndo : String -> List TreeOp -> Flow Model ()
organizeWithUndo label ops =
    Actions.organize label ops
        |> Flow.andThen
            (\mUndoId ->
                Actions.addToastAction True
                    label
                    (Maybe.map (\undoId -> { label = "Undo", run = Actions.undoOrganizeEntry undoId }) mUndoId)
            )


dropIntoFolder : Int -> Bool -> Flow Model ()
dropIntoFolder targetId linkModifier =
    Flow.get
        |> Flow.andThen
            (\model ->
                case get organizeDrag model of
                    Just drag ->
                        case Selection.resolveDropAction linkModifier (Selection.resolveInto model drag targetId) of
                            Just OrganizeDropMove ->
                                moveRefsInto drag.sourceScope targetId drag.refs

                            Just OrganizeDropLink ->
                                linkRefsInto targetId drag.refs

                            _ ->
                                Flow.pure ()

                    Nothing ->
                        Flow.pure ()
            )


dropReorder : ListingScope -> ChildRef -> Bool -> Flow Model ()
dropReorder scope ref before =
    Flow.get
        |> Flow.andThen
            (\model ->
                case get organizeDrag model of
                    Just drag ->
                        let
                            prefs =
                                get listingPreferences model

                            rendered =
                                Selection.displayOrder prefs
                                    (List.map Model.childRefOf (Model.sortChildLinks (Selection.folderLinks model scope)))

                            desired =
                                Selection.reorderForEdgeDrop rendered drag.refs ref before

                            newOrder =
                                Selection.storedOrder prefs desired
                        in
                        Flow.when (Selection.edgeDropAllowed model scope ref before) <|
                            if Selection.displayOrder prefs newOrder == rendered then
                                Flow.pure ()

                            else
                                organizeWithUndo "Reorder"
                                    [ OrderOp scope newOrder ]

                    Nothing ->
                        Flow.pure ()
            )


onOrganizeDragEvent : Decode.Value -> Flow Model ()
onOrganizeDragEvent value =
    case Decode.decodeValue dragEventDecoder value of
        Ok (OrganizeDragStart { sourceScope, refs }) ->
            Flow.setAll organizeDrag (Just { sourceScope = sourceScope, refs = refs })
                |> Flow.seq (Flow.setAll listingSelection (Selection.selectAllState sourceScope refs))
                |> Flow.seq closeMenu

        Ok OrganizeDragEnd ->
            Flow.setAll organizeDrag Nothing

        Ok (OrganizeDragDrop { target, linkModifier }) ->
            (case target of
                OrganizeDropFolder { folderId } ->
                    dropIntoFolder folderId linkModifier

                OrganizeDropEdge { parentScope, ref, before } ->
                    dropReorder parentScope ref before
            )
                |> Flow.seq (Flow.setAll organizeDrag Nothing)

        Err _ ->
            Flow.pure ()


childKindDecoder : Decode.Decoder ChildKind
childKindDecoder =
    Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "step" ->
                        Decode.succeed StepChild

                    "project" ->
                        Decode.succeed ProjectChild

                    _ ->
                        Decode.fail ("Unknown child kind: " ++ kind)
            )


childRefDecoder : Decode.Decoder ChildRef
childRefDecoder =
    Decode.map2 ChildRef
        (Decode.field "kind" childKindDecoder)
        (Decode.field "id" Decode.int)


listingScopeDecoder : Decode.Decoder ListingScope
listingScopeDecoder =
    Decode.int


dropTargetDecoder : Decode.Decoder OrganizeDropTarget
dropTargetDecoder =
    Decode.field "kind" Decode.string
        |> Decode.andThen
            (\kind ->
                case kind of
                    "folder" ->
                        Decode.map (\folderId -> OrganizeDropFolder { folderId = folderId })
                            (Decode.field "folderId" Decode.int)

                    "edge" ->
                        Decode.map3 (\parentScope ref before -> OrganizeDropEdge { parentScope = parentScope, ref = ref, before = before })
                            (Decode.field "parentId" listingScopeDecoder)
                            (Decode.field "ref" childRefDecoder)
                            (Decode.field "before" Decode.bool)

                    _ ->
                        Decode.fail ("Unknown drop target: " ++ kind)
            )


dragEventDecoder : Decode.Decoder OrganizeDragEvent
dragEventDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\eventType ->
                case eventType of
                    "start" ->
                        Decode.map2 (\sourceScope refs -> OrganizeDragStart { sourceScope = sourceScope, refs = refs })
                            (Decode.field "sourceFolderId" listingScopeDecoder)
                            (Decode.field "refs" (Decode.list childRefDecoder))

                    "end" ->
                        Decode.succeed OrganizeDragEnd

                    "drop" ->
                        Decode.map2 (\target linkModifier -> OrganizeDragDrop { target = target, linkModifier = linkModifier })
                            (Decode.field "target" dropTargetDecoder)
                            (Decode.field "linkModifier" Decode.bool)

                    _ ->
                        Decode.fail ("Unknown drag event: " ++ eventType)
            )


editableTargetDecoder : Decode.Decoder Bool
editableTargetDecoder =
    Decode.map3
        (\tagName contentEditable className ->
            List.member tagName [ "INPUT", "TEXTAREA", "SELECT" ]
                || contentEditable
                || String.contains "cm-content" className
        )
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
        (Decode.oneOf [ Decode.at [ "target", "isContentEditable" ] Decode.bool, Decode.succeed False ])
        (Decode.oneOf [ Decode.at [ "target", "className" ] Decode.string, Decode.succeed "" ])


keyBindings : List ( Keyboard.Combination, Decode.Decoder (Flow Model ()) )
keyBindings =
    [ ( Keyboard.escape, Decode.succeed escapePressed )
    , ( Keyboard.delete, Decode.succeed removeShortcut )
    , ( Keyboard.backspace, Decode.succeed removeShortcut )
    ]
        ++ List.concatMap
            (\( key, msg ) ->
                Keyboard.primary key
                    |> List.map (\combination -> ( combination, Decode.succeed msg ))
            )
            [ ( Keyboard.KeyA, selectAll )
            , ( Keyboard.KeyX, cutSelection )
            , ( Keyboard.KeyC, copySelection )
            , ( Keyboard.KeyV, pasteClipboard )
            , ( Keyboard.KeyZ, undoShortcut )
            ]


shortcutDecoder : Decode.Decoder (Flow Model ())
shortcutDecoder =
    editableTargetDecoder
        |> Decode.andThen
            (\editable ->
                if editable then
                    Decode.fail "The shortcut target is editable."

                else
                    Keyboard.decodeCombinations keyBindings
            )


escapePressed : Flow Model ()
escapePressed =
    Flow.get
        |> Flow.andThen
            (\model ->
                if Maybe.isJust (get organizeDialog model) then
                    closeDialog

                else if Maybe.isJust (get organizeContextMenu model) then
                    closeMenu

                else
                    clearSelection
            )


removeShortcut : Flow Model ()
removeShortcut =
    Flow.get
        |> Flow.andThen
            (\model ->
                case selectedScopeAndRefs model of
                    Just ( scope, refs ) ->
                        Flow.when (Selection.listingEditable model) (removeSelection scope refs)

                    Nothing ->
                        Flow.pure ()
            )


undoShortcut : Flow Model ()
undoShortcut =
    Flow.get
        |> Flow.andThen (\model -> Flow.when (Selection.listingEditable model) Actions.undoOrganize)
