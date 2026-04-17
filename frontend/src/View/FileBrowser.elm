module View.FileBrowser exposing (viewDirectorySection, viewSrcFilesSection)

import Accessors exposing (Prism, has, just, prism, snd, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData, success)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Extra.Accessors exposing (where_)
import Filesize
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class, href, id, readonly, rel, src, style, target, value)
import Html.Events
import Html.Extra as Html
import Json.Decode as Decode
import Maybe.Extra as Maybe
import Model.Core exposing (DirectoryItem(..), Model, Status(..), StepRecord, getUserRepoInfo)
import Model.Lenses exposing (currentProjectId, fileZoomAt, mimeType)
import Model.Shadow as Shadow exposing (StepType, WithSrcFiles(..))
import Model.TableSpec exposing (StepSpec)
import View.Icons exposing (icon)
import View.Lib exposing (viewLoading)


type DirContext
    = OutputDir String
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


renderDirectoryContents : StepSpec -> Maybe Int -> Maybe DirContext -> Bool -> List String -> String -> ApiData (Dict String DirectoryItem) -> Html (Flow Model ())
renderDirectoryContents spec mRecordId mDirCtx isLocked directoryPath cssClass children =
    let
        viewContents childrenDict =
            Html.div [ class "directory-tree" ]
                (Dict.toList childrenDict |> List.map (\( itemName, item ) -> viewDirectoryItemWithPath spec mRecordId mDirCtx isLocked directoryPath itemName item))
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


viewDirectorySection : StepSpec -> StepRecord -> Html (Flow Model ())
viewDirectorySection spec step =
    ApiData.toMaybe step.runState
        |> Html.viewMaybe
            (\rs ->
                Html.div [ class "output-files-section" ]
                    [ Html.h3 [] [ Html.text "Output Files" ]
                    , renderDirectoryContents spec
                        step.id
                        (if String.isEmpty rs.outPath then
                            Nothing

                         else
                            Just (OutputDir rs.outPath)
                        )
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
            , renderDirectoryContents spec
                step.id
                (Maybe.map SrcDir step.id)
                False
                []
                "directory-view"
                step.srcFiles.children
            ]


viewDirectoryItemWithPath :
    StepSpec
    -> Maybe Int
    -> Maybe DirContext
    -> Bool
    -> List String
    -> String
    -> DirectoryItem
    -> Html (Flow Model ())
viewDirectoryItemWithPath spec mRecordId mDirCtx isLocked directoryPath itemName item =
    let
        path =
            directoryPath ++ [ itemName ]

        anchor =
            String.join "/" <| Maybe.unwrap "" String.fromInt mRecordId :: path

        shareButton =
            let
                shareAction =
                    Flow.get
                        |> Flow.andThen
                            (\m ->
                                Maybe.map2 (\projectId recordId -> Actions.shareEntity projectId recordId path)
                                    (try currentProjectId m)
                                    mRecordId
                                    |> Maybe.withDefault Flow.none
                            )
            in
            Html.button
                [ class "dir-item-icon-btn"
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
                            Just (OutputDir outPath_) ->
                                Just ("/backend/store-files" ++ outPath_ ++ "/" ++ String.join "/" path)

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
                                        Just (OutputDir _) ->
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
                        , Html.viewIf ((file.viewable || isImage) && not (has (just << srcDir) mDirCtx)) shareButton
                        , Html.button
                            [ class "dir-item-icon-btn"
                            , Html.Events.onClick
                                (case mDirCtx of
                                    Just (OutputDir op) ->
                                        Actions.downloadFile op (String.join "/" path)

                                    Just (SrcDir id) ->
                                        Actions.downloadSrcFile id (String.join "/" path)

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
                                Just (OutputDir outPath_) ->
                                    Html.img
                                        [ src ("/backend/store-files" ++ outPath_ ++ "/" ++ String.join "/" path)
                                        , class "file-image-viewer"
                                        ]
                                        []

                                _ ->
                                    Html.nothing

                          else if isHtml then
                            case mDirCtx of
                                Just (OutputDir outPath_) ->
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
                                            [ src ("/backend/store-files" ++ outPath_ ++ "/" ++ String.join "/" path)
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
                                viewContent content =
                                    Html.textarea
                                        [ readonly True
                                        , class "file-content"
                                        , value content
                                        , style "height" (calculateTextareaHeight (Just content))
                                        ]
                                        []
                            in
                            ApiData.foldVisible
                                Html.nothing
                                (Maybe.map (viewLoading << viewContent)
                                    >> Maybe.withDefault (viewLoading <| Html.div [ class "file-content-loading" ] [])
                                )
                                viewContent
                                (always <| Html.span [ class "file-error" ] [ Html.text "Failed to load file" ])
                                file.content
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
                                                Just (OutputDir _) ->
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
                    renderDirectoryContents spec mRecordId mDirCtx isLocked path "folder-contents" folder.children
                ]


calculateTextareaHeight : Maybe String -> String
calculateTextareaHeight maybeContent =
    case maybeContent of
        Nothing ->
            "100px"

        Just content ->
            let
                lineCount =
                    content
                        |> String.split "\n"
                        |> List.length

                -- Each line is approximately 17px (12px font + 1.4 line-height)
                lineHeight =
                    17

                padding =
                    16

                -- var(--spacing-sm) * 2
                calculatedHeight =
                    lineCount * lineHeight + padding

                -- Cap at 300px by default, but allow CSS resize to go higher
                cappedHeight =
                    min calculatedHeight 300

                -- Ensure minimum of 100px
                finalHeight =
                    max cappedHeight 100
            in
            String.fromInt finalHeight ++ "px"
