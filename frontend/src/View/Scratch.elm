module View.Scratch exposing (viewScratchPicker)

import Actions
import Api.ApiData as ApiData
import Filesize
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class, disabled, id, title)
import Html.Events as Events
import Html.Extra as Html
import Ingest
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model, ScratchEntry, ScratchState)
import View.Icons exposing (icon)


viewScratchPicker : Model -> Html (Flow Model ())
viewScratchPicker model =
    Html.node "dialog"
        [ id "scratch-picker-dialog"
        , class "dialog scratch-picker-dialog"
        , Events.on "close" (Decode.succeed Ingest.scratchPickerClosed)
        , Events.onClick (Actions.closeDialog "scratch-picker-dialog")
        ]
        [ viewContent model ]


viewContent : Model -> Html (Flow Model ())
viewContent model =
    let
        state =
            Model.getScratchState model
    in
    case state.pickerStepId of
        Nothing ->
            Html.nothing

        Just stepId ->
            Html.div
                [ class "dialog-content scratch-picker-content"
                , Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                ]
                [ Html.div [ class "scratch-picker-header" ]
                    [ Html.span [ class "dialog-title" ] [ Html.text "Wrap a scratch directory" ]
                    , Html.button
                        [ class "icon-btn scratch-picker-close"
                        , title "Close"
                        , Events.onClick (Actions.closeDialog "scratch-picker-dialog")
                        ]
                        [ icon True "close" ]
                    ]
                , viewBreadcrumb state
                , Html.viewMaybe (\error -> Html.div [ class "scratch-picker-error" ] [ Html.text error ]) state.error
                , viewListing state
                , viewFooter stepId state
                ]


viewBreadcrumb : ScratchState -> Html (Flow Model ())
viewBreadcrumb state =
    let
        currentPath =
            ApiData.unwrap "" .path state.listing

        segments =
            String.split "/" currentPath
                |> List.filter (not << String.isEmpty)

        rootLabel =
            ApiData.unwrap "" (Maybe.withDefault "scratch") state.root
                |> String.split "/"
                |> List.reverse
                |> List.head
                |> Maybe.withDefault "scratch"

        trail =
            List.map2
                (\index segment -> { path = String.join "/" (List.take (index + 1) segments), label = segment })
                (List.range 0 (List.length segments - 1))
                segments
    in
    Html.div [ class "scratch-picker-breadcrumb" ]
        (viewCrumb "" rootLabel
            :: List.map (\crumb -> viewCrumb crumb.path crumb.label) trail
        )


viewCrumb : String -> String -> Html (Flow Model ())
viewCrumb path label =
    Html.button
        [ class "scratch-picker-crumb"
        , Events.onClick (Ingest.loadScratchListing path)
        ]
        [ Html.text label ]


viewListing : ScratchState -> Html (Flow Model ())
viewListing state =
    case state.listing of
        ApiData.NotAsked ->
            Html.nothing

        ApiData.Loading _ ->
            Html.div [ class "scratch-picker-status" ] [ Html.text "Loading..." ]

        ApiData.Error _ ->
            Html.nothing

        ApiData.Success listing ->
            if List.isEmpty listing.entries then
                Html.div [ class "scratch-picker-status" ] [ Html.text "This directory is empty." ]

            else
                Html.div [ class "scratch-picker-list" ] (List.map (viewEntry listing.path) listing.entries)


viewEntry : String -> ScratchEntry -> Html (Flow Model ())
viewEntry currentPath entry =
    if entry.directory then
        Html.button
            [ class "scratch-picker-entry scratch-picker-entry--directory"
            , Events.onClick (Ingest.loadScratchListing (joinPath currentPath entry.name))
            ]
            [ icon True "folder"
            , Html.span [ class "scratch-picker-entry-name" ] [ Html.text entry.name ]
            ]

    else
        Html.div [ class "scratch-picker-entry" ]
            [ icon False "draft"
            , Html.span [ class "scratch-picker-entry-name" ] [ Html.text entry.name ]
            , Maybe.unwrap Html.nothing
                (\size -> Html.span [ class "scratch-picker-entry-size" ] [ Html.text (Filesize.formatBase2 size) ])
                entry.size
            ]


joinPath : String -> String -> String
joinPath parent name =
    if String.isEmpty parent then
        name

    else
        parent ++ "/" ++ name


viewFooter : Int -> ScratchState -> Html (Flow Model ())
viewFooter stepId state =
    let
        currentPath =
            ApiData.unwrap "" .path state.listing
    in
    Html.div [ class "scratch-picker-footer" ]
        [ Html.button
            [ class "btn"
            , disabled (String.isEmpty currentPath)
            , Events.onClick (Ingest.wrapScratchDirectory stepId currentPath)
            ]
            [ Html.text "Wrap this directory" ]
        ]
