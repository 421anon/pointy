module View.Organize exposing
    ( dropEdgeAttrs
    , dropTargetAttrs
    , rowDragAttrs
    , rowDragHandleAttrs
    , selectionRefsAttr
    , viewActionBar
    , viewContextMenu
    , viewMenuPopover
    , viewOrganizeDialog
    , viewRowCheckbox
    )

import Accessors exposing (get)
import Actions
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, id, style, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildLink, ChildRef, Model, OrganizeAction(..), OrganizeDialog, OrganizeDialogMode(..))
import Model.Lenses exposing (listingSelection, organizeContextMenu, organizeDialog)
import Model.Selection as Selection
import Organize
import Set exposing (Set)
import View.Icons exposing (iconCustom)


refToken : ChildRef -> String
refToken ref =
    Model.childKindName ref.kind ++ ":" ++ String.fromInt ref.id


dropTargetAttrs : Model -> Int -> List (Html.Attribute msg)
dropTargetAttrs model folderId =
    [ attribute "data-drop-folder" (String.fromInt folderId)
    , attribute "data-drop-allowed" (dropAllowedToken model folderId)
    ]


dropAllowedToken : Model -> Int -> String
dropAllowedToken model folderId =
    Selection.dropAllowed model folderId
        |> List.map Selection.dropActionToken
        |> String.join " "


dropEdgeAttrs : Maybe ( Int, Int ) -> Int -> List (Html.Attribute msg)
dropEdgeAttrs gaps index =
    [ ( True, "before" ), ( False, "after" ) ]
        |> List.filter (\( before, _ ) -> Selection.edgeAllowed gaps index before)
        |> List.map Tuple.second
        |> String.join " "
        |> attribute "data-drop-edges"
        |> List.singleton


rowDragAttrs : Model.ListingScope -> ChildLink -> List (Html.Attribute msg)
rowDragAttrs scope link =
    [ attribute "data-drag-ref" (refToken (Model.childRefOf link))
    , attribute "data-drag-folder"
        (String.fromInt scope)
    ]


rowDragHandleAttrs : List (Html.Attribute msg)
rowDragHandleAttrs =
    [ attribute "draggable" "true"
    , attribute "data-drag-handle" ""
    ]


selectionRefsAttr : Model -> List (Html.Attribute msg)
selectionRefsAttr model =
    let
        refs =
            get listingSelection model
                |> Maybe.map .refs
                |> Maybe.withDefault []
    in
    [ attribute "data-selected-refs" (String.join " " (List.map refToken refs)) ]


viewMenuPopover :
    { popoverId : String
    , wrapperClass : String
    , triggerAttrs : List (Html.Attribute (Flow Model ()))
    , triggerContent : List (Html (Flow Model ()))
    , content : List (Html (Flow Model ()))
    }
    -> Html (Flow Model ())
viewMenuPopover config =
    Html.span [ class config.wrapperClass ]
        [ Html.button
            (config.triggerAttrs
                ++ [ attribute "popovertarget" config.popoverId
                   , style "anchor-name" ("--anchor-" ++ config.popoverId)
                   ]
            )
            config.triggerContent
        , Html.div
            [ class "listing-popover"
            , id config.popoverId
            , attribute "popover" "auto"
            , style "position-anchor" ("--anchor-" ++ config.popoverId)
            , Events.on "click" (Decode.succeed (Actions.hidePopover config.popoverId))
            ]
            config.content
        ]


viewActionBar : Model -> Html (Flow Model ())
viewActionBar model =
    let
        actions =
            barActions model
    in
    Html.viewIf (Selection.listingEditable model && Selection.hasSelection model && not (List.isEmpty actions)) <|
        Html.div [ class "listing-action-bar-anchor" ]
            [ Html.div [ class "listing-action-bar" ]
                [ Html.span [ class "listing-action-bar-count" ]
                    [ Html.text (String.fromInt (List.length (Selection.selectionRefs model)) ++ " selected") ]
                , Html.div [ class "listing-action-bar-actions" ]
                    (List.map viewActionButton actions)
                ]
            ]


barActions : Model -> List ( OrganizeAction, Selection.ActionSpec )
barActions model =
    List.filter (Tuple.second >> .inBar) (Selection.selectionActions model)


viewActionButton : ( OrganizeAction, Selection.ActionSpec ) -> Html (Flow Model ())
viewActionButton ( action, spec ) =
    Html.button
        [ class "listing-action-bar-btn"
        , classList [ ( "danger", action == OrganizeDeleteAction ) ]
        , title spec.label
        , Events.onClick (Organize.runAction action)
        ]
        [ iconCustom False spec.icon [ class "listing-action-bar-icon" ]
        , Html.span [ class "listing-action-bar-label" ] [ Html.text spec.label ]
        ]


viewContextMenu : Model -> Html (Flow Model ())
viewContextMenu model =
    Html.viewIf (Selection.listingEditable model) <|
        Html.viewMaybe
            (\menu ->
                let
                    actions =
                        case menu.ref of
                            Just _ ->
                                barActions model

                            Nothing ->
                                List.filter (Tuple.second >> .inBar >> not) (Selection.selectionActions model)
                in
                Html.viewIf (not (List.isEmpty actions)) <|
                    Html.div
                        [ class "listing-context-menu"
                        , style "left" (String.fromInt menu.x ++ "px")
                        , style "top" (String.fromInt menu.y ++ "px")
                        ]
                        (List.map viewActionButton actions)
            )
            (get organizeContextMenu model)


viewRowCheckbox : Set ( String, Int ) -> Model.ListingScope -> ChildLink -> Html (Flow Model ())
viewRowCheckbox selected scope link =
    let
        ref =
            Model.childRefOf link
    in
    Html.input
        [ type_ "checkbox"
        , class "listing-row-checkbox"
        , checked (Set.member ( Model.childKindName ref.kind, ref.id ) selected)
        , title "Select"
        , attribute "aria-label" "Select"
        , Events.stopPropagationOn "click" (Decode.succeed ( Organize.toggleCheckbox scope ref, True ))
        ]
        []


viewOrganizeDialog : Model -> Html (Flow Model ())
viewOrganizeDialog model =
    Html.node "dialog"
        [ id Organize.dialogId
        , class "dialog organize-dialog"
        ]
        [ Html.viewMaybe (viewOrganizeDialogContent model) (get organizeDialog model) ]


viewOrganizeDialogContent : Model -> OrganizeDialog -> Html (Flow Model ())
viewOrganizeDialogContent model dialog =
    let
        ( dialogTitle, confirmLabel ) =
            case dialog.mode of
                OrganizeMove ->
                    ( "Move to", "Move" )

                OrganizeLinkTo ->
                    ( "Link to", "Link" )

                OrganizeGroup ->
                    if List.isEmpty dialog.refs then
                        ( "New folder", "Create" )

                    else
                        ( "Group into new folder", "Create" )

                OrganizeDelete ->
                    ( "Delete permanently", "Delete" )

        showsTargets =
            dialog.mode == OrganizeMove || dialog.mode == OrganizeLinkTo

        isDelete =
            dialog.mode == OrganizeDelete

        canConfirm =
            case dialog.mode of
                OrganizeGroup ->
                    not (String.isEmpty (String.trim dialog.name))

                OrganizeDelete ->
                    not (List.isEmpty dialog.refs)

                _ ->
                    Maybe.isJust dialog.targetId
    in
    Html.div [ class "dialog-content organize-dialog-content" ]
        [ Html.span [ class "dialog-title" ] [ Html.text dialogTitle ]
        , Html.viewIf (not (List.isEmpty dialog.refs)) <|
            Html.span [ class "dialog-subtitle" ]
                [ Html.text (String.fromInt (List.length dialog.refs) ++ " selected") ]
        , Html.viewIf isDelete <|
            Html.p [ class "organize-dialog-warning" ]
                [ Html.text "This permanently deletes the selected items from the repository. This cannot be undone." ]
        , Html.viewIf (dialog.mode == OrganizeGroup)
            (Html.div [ class "organize-dialog-field" ]
                [ Html.text "Folder name"
                , Html.input
                    [ class "form-input"
                    , value dialog.name
                    , Events.onInput Organize.setDialogName
                    , attribute "autofocus" "autofocus"
                    ]
                    []
                ]
            )
        , Html.viewIf showsTargets
            (Html.div [ class "organize-dialog-field" ]
                [ Html.input
                    [ class "form-input"
                    , attribute "type" "search"
                    , attribute "placeholder" "Search folders"
                    , value dialog.query
                    , Events.onInput Organize.setDialogQuery
                    ]
                    []
                , Html.div [ class "organize-dialog-targets" ]
                    (List.map (viewTargetOption dialog) (filteredTargets model dialog))
                ]
            )
        , Html.div [ class "dialog-actions" ]
            [ Html.button
                [ class "btn"
                , Events.onClick Organize.closeDialog
                ]
                [ Html.text "Cancel" ]
            , Html.button
                [ class "btn"
                , classList [ ( "btn-danger", isDelete ) ]
                , disabled (not canConfirm)
                , Events.onClick Organize.confirmDialog
                ]
                [ Html.text confirmLabel ]
            ]
        ]


filteredTargets : Model -> OrganizeDialog -> List ( Int, String )
filteredTargets model dialog =
    let
        query =
            String.toLower (String.trim dialog.query)
    in
    Selection.organizeTargets model dialog.mode dialog.sourceScope dialog.refs
        |> List.filter
            (\( _, path ) ->
                String.isEmpty query || String.contains query (String.toLower path)
            )


viewTargetOption : OrganizeDialog -> ( Int, String ) -> Html (Flow Model ())
viewTargetOption dialog ( targetId, path ) =
    Html.button
        [ class "listing-menu-item organize-dialog-target"
        , classList [ ( "active", dialog.targetId == Just targetId ) ]
        , title path
        , Events.onClick (Organize.setDialogTarget targetId)
        ]
        [ iconCustom False "folder" [ class "organize-dialog-target-icon" ]
        , Html.span [ class "organize-dialog-target-name" ] [ Html.text path ]
        ]
