module View.FileBrowser exposing (viewDirectorySection, viewSrcFilesSection)

import Accessors exposing (Prism, has, just, prism, snd, try)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData, success)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Extra.Accessors exposing (where_)
import Filesize
import Flow exposing (Flow)
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class, classList, href, id, rel, src, style, target)
import Html.Attributes.Extra exposing (attributeIf)
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (DirectoryItem(..), Model, Status(..), StepRecord, getUserRepoInfo)
import Model.Lenses exposing (currentProjectId, fileZoomAt, gutterDrag, mHighlight, mimeType, route)
import Model.Shadow as Shadow exposing (StepType, WithSrcFiles(..))
import Model.TableSpec exposing (StepSpec)
import Route
import View.Icons exposing (icon)
import View.Lib exposing (viewLoading)


type DirContext
    = OutputDir Int String
    | SrcDir Int


srcDir : Prism pr DirContext Int x y
srcDir =
    prism ">SrcDir"
        SrcDir
        (\ctx ->
            case ctx of
                SrcDir id ->
                    Ok id

                other ->
                    Err other
        )


renderDirectoryContents : Model -> StepSpec -> Maybe Int -> Maybe DirContext -> Bool -> List String -> String -> ApiData (Dict String DirectoryItem) -> Html (Flow Model ())
renderDirectoryContents model spec mRecordId mDirCtx isLocked directoryPath cssClass children =
    let
        viewContents childrenDict =
            Html.div [ class "directory-tree" ]
                (Dict.toList childrenDict |> List.map (\( itemName, item ) -> viewDirectoryItemWithPath model spec mRecordId mDirCtx isLocked directoryPath itemName item))
    in
    Html.div [ class cssClass ]
        [ ApiData.foldVisible
            (Html.div [] [ Html.text "Directory is empty" ])
            (Maybe.map (viewLoading << viewContents)
                >> Maybe.withDefault (Html.div [] [ Html.span [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading directory contents..." ] ])
            )
            (\childrenDict ->
                if Dict.isEmpty childrenDict then
                    Html.div [] [ Html.text "Directory is empty" ]

                else
                    viewContents childrenDict
            )
            (always <| Html.div [] [ Html.text "Failed to load" ])
            children
        ]


viewDirectorySection : Model -> StepSpec -> StepRecord -> Html (Flow Model ())
viewDirectorySection model spec step =
    case step.id of
        Nothing ->
            Html.nothing

        Just stepId ->
            ApiData.toMaybe step.runState
                |> Html.viewMaybe
                    (\rs ->
                        Html.div [ class "output-files-section" ]
                            [ Html.h3 [] [ Html.text "Output Files" ]
                            , renderDirectoryContents model
                                spec
                                (Just stepId)
                                (Just (OutputDir stepId rs.commit))
                                (rs.status == ApiData.Success StatusSuccess)
                                []
                                "directory-view"
                                rs.directoryView.children
                            ]
                    )


viewSrcFilesSection : Model -> StepType -> StepSpec -> StepRecord -> Html (Flow Model ())
viewSrcFilesSection model stepType spec step =
    let
        hasSrcFiles =
            has (Shadow.derivation << snd << where_ ((==) WithSrcFiles)) stepType

        viewInstructions =
            getUserRepoInfo model
                |> ApiData.toMaybe
                |> Html.viewMaybe
                    (\info ->
                        let
                            dirName =
                                "srcFiles/" ++ (step.id |> Maybe.map String.fromInt |> Maybe.withDefault "unknown")

                            msg =
                                if has success step.srcFiles.children then
                                    "Edit the files in the user repository (" ++ info.url ++ ") on branch: " ++ info.branch ++ " in " ++ dirName

                                else
                                    "Create the directory " ++ dirName ++ " in the user repository (" ++ info.url ++ ") on branch: " ++ info.branch
                        in
                        Html.div [ class "src-files-repo-note" ] [ Html.text msg ]
                    )
    in
    Html.viewIf hasSrcFiles <|
        Html.div [ class "src-files-section" ]
            [ Html.h3 [] [ Html.text "Source Files" ]
            , viewInstructions
            , renderDirectoryContents model
                spec
                step.id
                (Maybe.map SrcDir step.id)
                False
                []
                "directory-view"
                step.srcFiles.children
            ]


viewDirectoryItemWithPath :
    Model
    -> StepSpec
    -> Maybe Int
    -> Maybe DirContext
    -> Bool
    -> List String
    -> String
    -> DirectoryItem
    -> Html (Flow Model ())
viewDirectoryItemWithPath model spec mRecordId mDirCtx isLocked directoryPath itemName item =
    let
        path =
            directoryPath ++ [ itemName ]

        mGutter =
            case ( mRecordId, mDirCtx ) of
                ( Just recordId, Just (OutputDir _ _) ) ->
                    Just { recordId = recordId, target = Route.Output }

                ( Just recordId, Just (SrcDir _) ) ->
                    Just { recordId = recordId, target = Route.Source }

                _ ->
                    Nothing

        anchor =
            mGutter
                |> Maybe.map (\{ recordId, target } -> Route.highlightAnchor target recordId path)
                |> Maybe.withDefault (String.join "/" <| Maybe.unwrap "" String.fromInt mRecordId :: path)

        mSelectedRange =
            mGutter
                |> Maybe.andThen
                    (\{ recordId, target } ->
                        try (route << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) model
                            |> Maybe.andThen .range
                    )

        shareButton =
            let
                shareAction =
                    case ( try currentProjectId model, mGutter ) of
                        ( Just projectId, Just { recordId, target } ) ->
                            Actions.shareEntity projectId recordId target path mSelectedRange

                        _ ->
                            Flow.none

                shareTooltip =
                    Maybe.unwrap "Share" (\range -> "Share lines " ++ Route.formatLineRange range) mSelectedRange
            in
            Html.button
                [ class "dir-item-icon-btn"
                , Html.Attributes.title shareTooltip
                , Html.Events.stopPropagationOn "click" (Decode.succeed ( shareAction, True ))
                ]
                [ icon True "share" ]
    in
    case item of
        File file ->
            let
                isImage =
                    has (mimeType << just << where_ (String.startsWith "image/")) file

                isHtml =
                    has (mimeType << just << where_ (String.startsWith "text/html")) file

                externalHtmlUrl =
                    if isHtml then
                        case mDirCtx of
                            Just (OutputDir stepId_ commit_) ->
                                Just (Api.stepFileRawUrl stepId_ (Just commit_) path)

                            _ ->
                                Nothing

                    else
                        Nothing

                fileIcon =
                    if isImage then
                        "image"

                    else
                        "description"
            in
            Html.div [ class "directory-file-container" ]
                [ Html.div [ class "directory-file", id anchor ]
                    [ icon True fileIcon
                    , Html.div [ class "file-name-container" ]
                        [ Html.span [ class "file-name" ] [ Html.text itemName ]
                        , Html.nothing
                        , Html.span [ class "directory-item-meta" ] [ Html.text (Filesize.formatBase2 file.size) ]
                        ]
                    , Html.div [ class "file-actions" ]
                        [ Html.viewIf (file.viewable || isImage) <|
                            Html.button
                                [ class "dir-item-icon-btn"
                                , Html.Events.onClick
                                    (case mDirCtx of
                                        Just (OutputDir _ _) ->
                                            Maybe.unwrap (Flow.pure ()) (flip Actions.toggleFile path) mRecordId

                                        Just (SrcDir _) ->
                                            Maybe.unwrap (Flow.pure ()) (flip Actions.toggleSrcFile path) mRecordId

                                        Nothing ->
                                            Flow.pure ()
                                    )
                                ]
                                [ icon True "visibility" ]
                        , externalHtmlUrl
                            |> Html.viewMaybe
                                (\url ->
                                    Html.a
                                        [ class "dir-item-icon-btn"
                                        , href url
                                        , target "_blank"
                                        , rel "noopener noreferrer"
                                        ]
                                        [ icon True "open_in_new" ]
                                )
                        , Html.viewIf ((file.viewable || isImage) && (not (has (just << srcDir) mDirCtx) || Maybe.isJust mSelectedRange)) shareButton
                        , Html.button
                            [ class "dir-item-icon-btn"
                            , Html.Events.onClick
                                (case mDirCtx of
                                    Just (OutputDir stepId_ commit_) ->
                                        Actions.downloadFile stepId_ commit_ path

                                    Just (SrcDir id) ->
                                        Actions.downloadSrcFile id path

                                    Nothing ->
                                        Flow.pure ()
                                )
                            ]
                            [ icon True "download" ]
                        ]
                    ]
                , Html.viewIf file.view.isViewing <|
                    Html.div [ class "file-content-viewer" ]
                        [ if isImage then
                            case mDirCtx of
                                Just (OutputDir stepId_ commit_) ->
                                    Html.img
                                        [ src (Api.stepFileRawUrl stepId_ (Just commit_) path)
                                        , class "file-image-viewer"
                                        ]
                                        []

                                _ ->
                                    Html.nothing

                          else if isHtml then
                            case mDirCtx of
                                Just (OutputDir stepId_ commit_) ->
                                    let
                                        iframeId =
                                            "iframe-" ++ anchor

                                        zoomAction factor =
                                            mRecordId
                                                |> Maybe.map (\recordId -> Actions.zoomHtmlFileBy (fileZoomAt recordId path) iframeId factor)
                                                |> Maybe.withDefault Flow.none
                                    in
                                    Html.div [ class "iframe-zoom-wrapper" ]
                                        [ Html.node "iframe"
                                            [ src (Api.stepFileRawUrl stepId_ (Just commit_) path)
                                            , Html.Attributes.attribute "sandbox" "allow-same-origin allow-scripts"
                                            , class "file-html-viewer"
                                            , id iframeId
                                            ]
                                            []
                                        , Html.button
                                            [ class "iframe-zoom-btn zoom-in"
                                            , Html.Events.stopPropagationOn "click" (Decode.succeed ( zoomAction 1.16, True ))
                                            ]
                                            [ icon True "zoom_in" ]
                                        , Html.button
                                            [ class "iframe-zoom-btn zoom-out"
                                            , Html.Events.stopPropagationOn "click" (Decode.succeed ( zoomAction (1 / 1.16), True ))
                                            ]
                                            [ icon True "zoom_out" ]
                                        ]

                                _ ->
                                    Html.nothing

                          else
                            let
                                renderLine n line =
                                    let
                                        lineNum =
                                            n + 1

                                        gutterAttrs =
                                            Maybe.unwrap []
                                                (\{ recordId, target } ->
                                                    [ Html.Events.on "pointerdown" (Decode.succeed (Actions.startGutterDrag target recordId path lineNum))
                                                    , attributeIf (has (gutterDrag << just) model) <|
                                                        Html.Events.on "pointerenter" (Decode.succeed (Actions.extendGutterDrag target recordId path lineNum))
                                                    ]
                                                )
                                                mGutter
                                    in
                                    Html.div
                                        [ classList
                                            [ ( "file-line", True )
                                            , ( "highlighted", Maybe.unwrap False (\{ from, to } -> lineNum >= from && lineNum <= to) mSelectedRange )
                                            ]
                                        , id ("line-" ++ anchor ++ "-" ++ String.fromInt lineNum)
                                        ]
                                        [ Html.span
                                            (classList
                                                [ ( "file-line-number", True )
                                                , ( "is-gutter", Maybe.isJust mGutter )
                                                ]
                                                :: gutterAttrs
                                            )
                                            [ Html.text (String.fromInt lineNum) ]
                                        , Html.span [ class "file-line-content" ] [ Html.text line ]
                                        ]

                                gridAction =
                                    case ( mRecordId, mDirCtx ) of
                                        ( Just recordId, Just (OutputDir _ _) ) ->
                                            Just (Actions.wrapDelimitedGridFlow recordId path)

                                        _ ->
                                            Nothing

                                viewPlainContent text =
                                    let
                                        lines =
                                            String.split "\n" text
                                    in
                                    Html.div
                                        [ class "file-content"
                                        , id ("viewer-" ++ anchor)
                                        , style "max-height" (calculateViewerHeight (List.length lines))
                                        ]
                                        (List.indexedMap renderLine lines)

                                viewContent text =
                                    case ( file.delimitedGrid, gridAction, mSelectedRange ) of
                                        ( Just delimitedGrid, Just updateGrid, Nothing ) ->
                                            Grid.view delimitedGrid.grid |> Html.map updateGrid

                                        _ ->
                                            viewPlainContent text
                            in
                            Html.div [ class "file-viewer" ]
                                [ ApiData.foldVisible
                                    Html.nothing
                                    (Maybe.map (viewLoading << viewContent)
                                        >> Maybe.withDefault (viewLoading <| Html.div [ class "file-content-loading" ] [])
                                    )
                                    viewContent
                                    (always <| Html.span [ class "file-error" ] [ Html.text "Failed to load file" ])
                                    file.content
                                ]
                        ]
                ]

        Folder folder ->
            let
                isSrcDir =
                    has (just << srcDir) mDirCtx
            in
            Html.div [ class "directory-folder", id anchor ]
                [ Html.map (Flow.map (always ())) <|
                    Html.div
                        [ class "folder-header"
                        , Html.Events.stopPropagationOn "click" <|
                            Decode.succeed
                                ( mRecordId
                                    |> Maybe.unwrap (Flow.pure ())
                                        (\recordId ->
                                            case mDirCtx of
                                                Just (OutputDir _ _) ->
                                                    Actions.toggleOutputEntry recordId Nothing (directoryPath ++ [ itemName ])
                                                        |> Flow.return ()

                                                Just (SrcDir _) ->
                                                    Actions.toggleSrcEntry recordId Nothing (directoryPath ++ [ itemName ])
                                                        |> Flow.return ()

                                                Nothing ->
                                                    Flow.pure ()
                                        )
                                , True
                                )
                        ]
                        [ Html.button [ class "folder-header-btn" ]
                            [ icon True
                                (if folder.expanded then
                                    "folder_open"

                                 else
                                    "folder"
                                )
                            , Html.span [ class "folder-name" ] [ Html.text itemName ]
                            , Html.span [ class "folder-expand-icon" ]
                                [ icon True
                                    (if folder.expanded then
                                        "expand_less"

                                     else
                                        "expand_more"
                                    )
                                ]
                            ]
                        , Html.viewIf (isLocked && not isSrcDir) shareButton
                        ]
                , Html.viewIf folder.expanded <|
                    renderDirectoryContents model spec mRecordId mDirCtx isLocked path "folder-contents" folder.children
                ]


calculateViewerHeight : Int -> String
calculateViewerHeight lineCount =
    let
        lineHeight =
            17

        padding =
            16

        calculatedHeight =
            lineCount * lineHeight + padding

        cappedHeight =
            min calculatedHeight 600

        finalHeight =
            max cappedHeight 100
    in
    String.fromInt finalHeight ++ "px"
