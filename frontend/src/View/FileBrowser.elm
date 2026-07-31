module View.FileBrowser exposing (viewDirectorySection, viewSrcFilesSection)

import Accessors exposing (Prism, has, just, prism, snd, try, values)
import Actions
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Basics.Extra exposing (flip)
import Dict exposing (Dict)
import Extra.Accessors exposing (where_)
import Filesize
import Flow exposing (Flow)
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, readonly, rel, src, style, target, type_)
import Html.Events
import Html.Extra as Html
import Html.Lazy
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core exposing (CompareSelection, CompareSource(..), DirectoryItem(..), FileChunk, Model, ScrollMetrics, SeekDirection(..), SeekWindow, Status(..), StepRecord, plainLineHeight, windowLineCount, windowStartLine)
import Model.Lenses exposing (compareSelecting, compareState, currentProject, currentProjectId, fileZoomAt, gutterDrag, mCommit, mHighlight, mimeType, recordById, route, srcFileWriting, tables)
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


srcWritePending : Model -> Maybe DirContext -> Bool
srcWritePending model mDirCtx =
    Maybe.andThen (try srcDir) mDirCtx
        |> Maybe.andThen (\recordId -> try (currentProject << success << tables << values << recordById recordId << srcFileWriting) model)
        |> Maybe.withDefault False


compareSelectionFor : Int -> String -> Maybe String -> List String -> DirContext -> CompareSelection
compareSelectionFor projectId fileName mime path ctx =
    case ctx of
        OutputDir recordId commit_ ->
            { projectId = projectId, recordId = recordId, path = path, fileName = fileName, mimeType = mime, source = FromOutput commit_ }

        SrcDir recordId ->
            { projectId = projectId, recordId = recordId, path = path, fileName = fileName, mimeType = mime, source = FromSrc }


viewCompareButton : Model -> Maybe CompareSelection -> Html (Flow Model ())
viewCompareButton model =
    Maybe.unwrap Html.nothing <|
        \sel ->
            let
                isPicking =
                    has (compareState << compareSelecting) model

                isPicked =
                    has (compareState << compareSelecting << where_ ((==) sel)) model

                ( tooltip, action, iconName ) =
                    if isPicked then
                        ( "Already picked", Flow.none, "check_circle" )

                    else if isPicking then
                        let
                            pickedLabel =
                                try (compareState << compareSelecting) model
                                    |> Maybe.unwrap "selected file" .fileName
                        in
                        ( "Pick to compare with " ++ pickedLabel, Actions.selectCompareFile sel, "compare_arrows" )

                    else
                        ( "Compare", Actions.startCompare sel, "compare_arrows" )
            in
            Html.button
                [ classList
                    [ ( "dir-item-icon-btn", True )
                    , ( "compare-btn-picked", isPicked )
                    , ( "compare-btn-ready", isPicking && not isPicked )
                    ]
                , Html.Attributes.title tooltip
                , Html.Events.onClick action
                ]
                [ icon True iconName ]


renderDirectoryContents : Model -> StepSpec -> Maybe Int -> Maybe DirContext -> Bool -> List String -> String -> ApiData (Dict String DirectoryItem) -> Html (Flow Model ())
renderDirectoryContents model spec mRecordId mDirCtx isLocked directoryPath cssClass children =
    let
        viewContents childrenDict =
            Html.div [ class "directory-tree" ]
                (Dict.toList childrenDict |> List.map (\( itemName, item ) -> viewDirectoryItemWithPath model spec mRecordId mDirCtx isLocked directoryPath itemName item))
    in
    Html.div [ class cssClass ]
        [ ApiData.foldVisible
            (Html.div [] [])
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

        isLocked =
            has (route << Route.page << Route.project << mCommit << just) model

        mEditableId =
            Maybe.filter (always (not isLocked)) step.id

        writePending =
            step.srcFileWriting

        createButton =
            Html.viewMaybe
                (\recordId ->
                    let
                        ( tooltip, symbol, action ) =
                            if Maybe.isJust step.srcFileDraft then
                                ( "Discard new file", "close", Actions.setSrcFileDraft recordId Nothing )

                            else
                                ( "New file", "add", Actions.openSrcFileDraft recordId )
                    in
                    Html.button
                        [ class "dir-item-icon-btn"
                        , Html.Attributes.title tooltip
                        , disabled writePending
                        , Html.Events.onClick action
                        ]
                        [ icon True symbol ]
                )
                mEditableId

        createForm =
            case ( mEditableId, step.srcFileDraft ) of
                ( Just recordId, Just draft ) ->
                    Html.form
                        [ class "src-file-create src-file-editor"
                        , Html.Events.preventDefaultOn "submit"
                            (Decode.succeed ( Actions.createSrcFile recordId draft.name draft.content, True ))
                        ]
                        [ Html.div [ class "src-file-name-row" ]
                            [ Html.input
                                [ id "src-file-name-input"
                                , Html.Attributes.value draft.name
                                , Html.Events.onInput (\value -> Actions.setSrcFileDraft recordId (Just { draft | name = value }))
                                , placeholder "File name"
                                , disabled writePending
                                , class "form-input"
                                ]
                                []
                            ]
                        , Html.node "code-editor"
                            [ Html.Attributes.value draft.content
                            , Html.Events.onInput (\value -> Actions.setSrcFileDraft recordId (Just { draft | content = value }))
                            , Html.Attributes.attribute "filename" draft.name
                            , Html.Attributes.attribute "aria-label" "New file contents"
                            , classList [ ( "disabled", writePending ) ]
                            , readonly writePending
                            , class "code-input"
                            ]
                            []
                        , Html.div [ class "src-file-actions" ]
                            [ Html.button
                                [ type_ "submit"
                                , class "btn"
                                , disabled (writePending || String.trim draft.name == "")
                                ]
                                [ Html.text "Create" ]
                            , Html.button
                                [ type_ "button"
                                , class "btn"
                                , disabled writePending
                                , Html.Events.onClick (Actions.setSrcFileDraft recordId Nothing)
                                ]
                                [ Html.text "Cancel" ]
                            ]
                        ]

                _ ->
                    Html.nothing

        section =
            Html.div [ class "src-files-section" ]
                [ Html.div [ class "src-files-header" ]
                    [ Html.h3 [] [ Html.text "Source Files" ]
                    , createButton
                    ]
                , createForm
                , renderDirectoryContents model
                    spec
                    step.id
                    (Maybe.map SrcDir step.id)
                    isLocked
                    []
                    "directory-view"
                    (case step.srcFiles.children of
                        Loading (Just value) ->
                            Success value

                        other ->
                            other
                    )
                ]
    in
    Html.viewIf hasSrcFiles section


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
                        try (route << Route.page << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) model
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

        downloadAction =
            case mDirCtx of
                Just (OutputDir stepId_ commit_) ->
                    Actions.downloadFile stepId_ commit_ path

                Just (SrcDir id) ->
                    Actions.downloadSrcFile id path

                Nothing ->
                    Flow.pure ()
    in
    case item of
        File file ->
            let
                isImage =
                    has (mimeType << just << where_ (String.startsWith "image/")) file

                canView =
                    file.viewable || file.seekable || isImage

                isHtml =
                    has (mimeType << just << where_ (String.startsWith "text/html")) file

                isPdb =
                    has (mimeType << just << where_ ((==) "chemical/x-pdb")) file

                mCompareSelection =
                    if file.viewable || isImage then
                        Maybe.map2 (\pid -> compareSelectionFor pid itemName file.mimeType path)
                            (try currentProjectId model)
                            mDirCtx

                    else
                        Nothing

                externalArtifactUrl =
                    case ( try currentProjectId model, mDirCtx ) of
                        ( Just projectId, Just (OutputDir stepId_ commit_) ) ->
                            Just <|
                                Route.toString <|
                                    Route.fromPage <|
                                        Route.Artifact
                                            { projectId = projectId
                                            , stepId = stepId_
                                            , commit = commit_
                                            , path = path
                                            }

                        _ ->
                            Nothing

                fileIcon =
                    if isPdb then
                        "biotech"

                    else if isImage then
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
                        [ Html.viewIf canView <|
                            Html.button
                                [ class "dir-item-icon-btn"
                                , disabled (srcWritePending model mDirCtx)
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
                        , externalArtifactUrl
                            |> Html.viewMaybe
                                (\url ->
                                    Html.a
                                        [ class "dir-item-icon-btn"
                                        , Html.Attributes.title "View external"
                                        , href url
                                        , target "_blank"
                                        , rel "noopener noreferrer"
                                        ]
                                        [ icon True "open_in_new" ]
                                )
                        , Html.viewIf (canView && (not (has (just << srcDir) mDirCtx) || Maybe.isJust mSelectedRange)) shareButton
                        , Html.button
                            [ class "dir-item-icon-btn"
                            , Html.Events.onClick downloadAction
                            ]
                            [ icon True "download" ]
                        , viewCompareButton model mCompareSelection
                        , case ( mDirCtx, isLocked ) of
                            ( Just (SrcDir recordId), False ) ->
                                Html.button
                                    [ class "dir-item-icon-btn"
                                    , Html.Attributes.title "Delete"
                                    , disabled (srcWritePending model mDirCtx)
                                    , Html.Events.onClick (Actions.deleteSrcFile recordId path)
                                    ]
                                    [ icon True "delete" ]

                            _ ->
                                Html.nothing
                        ]
                    ]
                , Html.viewIfLazy file.view.isViewing <|
                    \() ->
                        Html.div [ class "file-content-viewer" ]
                            [ if isPdb then
                                case mDirCtx of
                                    Just (OutputDir stepId_ commit_) ->
                                        Html.node "molstar-viewer"
                                            [ class "file-molstar-viewer"
                                            , Html.Attributes.attribute "src" (Api.stepFileBundleUrl stepId_ commit_ path)
                                            ]
                                            []

                                    _ ->
                                        Html.nothing

                              else if isImage then
                                case mDirCtx of
                                    Just (OutputDir stepId_ commit_) ->
                                        Html.img
                                            [ src (Api.stepFileBundleUrl stepId_ commit_ path)
                                            , class "file-image-viewer"
                                            ]
                                            []

                                    _ ->
                                        Html.nothing

                              else if isHtml && not file.seekable then
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
                                                [ src (Api.stepFileBundleUrl stepId_ commit_ path)
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

                              else if file.seekable then
                                let
                                    viewSeekWindow window_ =
                                        let
                                            selectedFrom =
                                                Maybe.unwrap 0 .from mSelectedRange

                                            selectedTo =
                                                Maybe.unwrap 0 .to mSelectedRange

                                            viewSeekPlainContent_ () =
                                                Html.Lazy.lazy7 viewSeekPlainContent
                                                    (gutterKey mGutter)
                                                    (has (gutterDrag << just) model)
                                                    selectedFrom
                                                    selectedTo
                                                    anchor
                                                    file.view.plainScrollTop
                                                    window_
                                        in
                                        viewSeekPlainContent_ ()
                                in
                                Html.div [ class "file-viewer" ]
                                    [ ApiData.foldVisible
                                        Html.nothing
                                        (Maybe.map (viewLoading << viewSeekWindow)
                                            >> Maybe.withDefault (viewLoading <| Html.div [ class "file-content-loading" ] [])
                                        )
                                        viewSeekWindow
                                        (always <| Html.span [ class "file-error" ] [ Html.text "Failed to load file" ])
                                        file.seekWindow
                                    ]

                              else
                                let
                                    gridAction =
                                        case ( mRecordId, mDirCtx ) of
                                            ( Just recordId, Just (OutputDir _ _) ) ->
                                                Just (Actions.wrapDelimitedGridFlow recordId path)

                                            _ ->
                                                Nothing

                                    mEditRecordId =
                                        mDirCtx
                                            |> Maybe.andThen (try srcDir)
                                            |> Maybe.filter (always (not isLocked && Maybe.isNothing mSelectedRange))

                                    viewContent text =
                                        let
                                            selectedFrom =
                                                Maybe.unwrap 0 .from mSelectedRange

                                            selectedTo =
                                                Maybe.unwrap 0 .to mSelectedRange

                                            viewPlainContent_ () =
                                                Html.Lazy.lazy8 viewPlainContent
                                                    (gutterKey mGutter)
                                                    (has (gutterDrag << just) model)
                                                    selectedFrom
                                                    selectedTo
                                                    anchor
                                                    file.view.plainScrollTop
                                                    text
                                                    ( 1, file.plainLineCount )
                                        in
                                        case ( file.delimitedGrid, gridAction ) of
                                            ( Just delimitedGrid, Just updateGrid ) ->
                                                Grid.view updateGrid viewPlainContent_ delimitedGrid.grid

                                            _ ->
                                                viewPlainContent_ ()

                                    viewEditor recordId text =
                                        let
                                            changed =
                                                Maybe.unwrap False ((/=) text) file.editedContent

                                            writePending =
                                                srcWritePending model mDirCtx
                                        in
                                        Html.div [ class "src-file-editor" ]
                                            [ Html.node "code-editor"
                                                [ Html.Attributes.value (Maybe.withDefault text file.editedContent)
                                                , Html.Events.onInput (Actions.updateSrcFileContent recordId path)
                                                , Html.Attributes.attribute "filename" itemName
                                                , Html.Attributes.attribute "aria-label" ("Edit " ++ itemName)
                                                , class "code-input"
                                                , classList [ ( "field-changed", changed ), ( "disabled", writePending ) ]
                                                , readonly writePending
                                                ]
                                                []
                                            , Html.viewIf changed <|
                                                Html.div [ class "src-file-actions" ]
                                                    [ Html.button
                                                        [ class "btn"
                                                        , disabled writePending
                                                        , Html.Events.onClick (Actions.saveSrcFile recordId path)
                                                        ]
                                                        [ Html.text "Save" ]
                                                    ]
                                            ]
                                in
                                Html.div [ class "file-viewer" ]
                                    [ ApiData.foldVisible
                                        Html.nothing
                                        (Maybe.map (viewLoading << viewContent)
                                            >> Maybe.withDefault (viewLoading <| Html.div [ class "file-content-loading" ] [])
                                        )
                                        (\text -> Maybe.unwrap (viewContent text) (flip viewEditor text) mEditRecordId)
                                        (always <| Html.span [ class "file-error" ] [ Html.text "Failed to load file" ])
                                        file.content
                                    ]
                            ]
                ]

        Folder folder ->
            let
                isSrcDir =
                    has (just << srcDir) mDirCtx

                isZip =
                    folder.mimeType == Just "application/zip"
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
                                                    Actions.toggleOutputEntry recordId Nothing path
                                                        |> Flow.return ()

                                                Just (SrcDir _) ->
                                                    Actions.toggleSrcEntry recordId Nothing path
                                                        |> Flow.return ()

                                                Nothing ->
                                                    Flow.pure ()
                                        )
                                , True
                                )
                        ]
                        [ Html.button [ class "folder-header-btn" ]
                            [ icon True
                                (if isZip then
                                    "folder_zip"

                                 else if folder.expanded then
                                    "folder_open"

                                 else
                                    "folder"
                                )
                            , Html.span [ class "folder-name" ] [ Html.text itemName ]
                            , folder.size
                                |> Html.viewMaybe
                                    (\size -> Html.span [ class "directory-item-meta" ] [ Html.text (Filesize.formatBase2 size) ])
                            , Html.span [ class "folder-expand-icon" ]
                                [ icon True
                                    (if folder.expanded then
                                        "expand_less"

                                     else
                                        "expand_more"
                                    )
                                ]
                            ]
                        , Html.viewIf isZip <|
                            Html.button
                                [ class "dir-item-icon-btn"
                                , Html.Events.stopPropagationOn "click" (Decode.succeed ( downloadAction, True ))
                                ]
                                [ icon True "download" ]
                        , Html.viewIf (isLocked && not isSrcDir) shareButton
                        ]
                , Html.viewIf folder.expanded <|
                    renderDirectoryContents model spec mRecordId mDirCtx isLocked path "folder-contents" folder.children
                ]


gutterKey : Maybe { a | recordId : Int, target : Route.HighlightTarget } -> Int
gutterKey mGutter =
    case mGutter of
        Just { recordId, target } ->
            case target of
                Route.Output ->
                    recordId

                Route.Source ->
                    -recordId

        Nothing ->
            0


gutterFromKey : Int -> Maybe { recordId : Int, target : Route.HighlightTarget }
gutterFromKey key =
    if key > 0 then
        Just { recordId = key, target = Route.Output }

    else if key < 0 then
        Just { recordId = -key, target = Route.Source }

    else
        Nothing


pathFromAnchor : Int -> String -> List String
pathFromAnchor gutterKeyValue anchor =
    if gutterKeyValue == 0 then
        []

    else
        case String.split "/" anchor of
            _ :: _ :: path ->
                path

            _ ->
                []


lineNumberDecoder : Decode.Decoder Int
lineNumberDecoder =
    Decode.at [ "currentTarget", "dataset", "line" ] Decode.string
        |> Decode.andThen
            (\lineStr ->
                case String.toInt lineStr of
                    Just line ->
                        Decode.succeed line

                    Nothing ->
                        Decode.fail ("Invalid line number: " ++ lineStr)
            )


scrollMetricsDecoder : Decode.Decoder ScrollMetrics
scrollMetricsDecoder =
    Decode.map3 ScrollMetrics
        (Decode.at [ "target", "scrollTop" ] Decode.float)
        (Decode.at [ "target", "clientHeight" ] Decode.float)
        (Decode.at [ "target", "scrollHeight" ] Decode.float)


plainViewerPadding : Int
plainViewerPadding =
    16


plainViewerMaxHeight : Int
plainViewerMaxHeight =
    600


plainViewerMinHeight : Int
plainViewerMinHeight =
    100


plainViewerOverscan : Int
plainViewerOverscan =
    600


visibleLineIndexes : Float -> Int -> List Int
visibleLineIndexes scrollTop lineCount =
    let
        containerHeight =
            min (lineCount * plainLineHeight + plainViewerPadding) plainViewerMaxHeight

        visibleCount =
            (containerHeight + 2 * plainViewerOverscan) // plainLineHeight + 1

        firstIndex =
            max 0 ((floor scrollTop - plainViewerOverscan) // plainLineHeight)

        boundedFirstIndex =
            min firstIndex (max 0 (lineCount - 1))

        lastIndexExclusive =
            min lineCount (boundedFirstIndex + visibleCount)
    in
    if lastIndexExclusive <= boundedFirstIndex then
        []

    else
        List.range boundedFirstIndex (lastIndexExclusive - 1)


type PlainContentMode
    = EagerContent
    | SeekContent (Maybe SeekDirection)


viewPlainContent : Int -> Bool -> Int -> Int -> String -> Float -> String -> ( Int, Int ) -> Html (Flow Model ())
viewPlainContent gutterKeyValue hasGutterDrag selectedFrom selectedTo anchor scrollTop text lineRange =
    viewPlainContentBody EagerContent gutterKeyValue hasGutterDrag selectedFrom selectedTo anchor scrollTop (Html.Lazy.lazy viewFileText text) lineRange


viewSeekPlainContent : Int -> Bool -> Int -> Int -> String -> Float -> SeekWindow -> Html (Flow Model ())
viewSeekPlainContent gutterKeyValue hasGutterDrag selectedFrom selectedTo anchor scrollTop window_ =
    viewPlainContentBody
        (SeekContent window_.loading)
        gutterKeyValue
        hasGutterDrag
        selectedFrom
        selectedTo
        anchor
        scrollTop
        (Html.Lazy.lazy2 viewFileChunks window_.loading window_.chunks)
        ( windowStartLine window_, windowLineCount window_ )


viewPlainContentBody : PlainContentMode -> Int -> Bool -> Int -> Int -> String -> Float -> Html (Flow Model ()) -> ( Int, Int ) -> Html (Flow Model ())
viewPlainContentBody mode gutterKeyValue hasGutterDrag selectedFrom selectedTo anchor scrollTop fileText ( startLine, lineCount_ ) =
    let
        path =
            pathFromAnchor gutterKeyValue anchor

        mGutter =
            gutterFromKey gutterKeyValue

        hasGutter =
            Maybe.isJust mGutter

        ( loadingBefore, loadingAfter ) =
            case mode of
                SeekContent (Just Before) ->
                    ( True, False )

                SeekContent (Just After) ->
                    ( False, True )

                _ ->
                    ( False, False )

        hiddenFirstLines =
            if loadingBefore then
                1

            else
                0

        hiddenLastLines =
            if loadingAfter then
                1

            else
                0

        lineCount =
            max 1 lineCount_

        lineIndexes =
            visibleLineIndexes scrollTop lineCount

        firstVisibleIndex =
            List.head lineIndexes |> Maybe.withDefault 0

        topOffset =
            firstVisibleIndex * plainLineHeight

        totalHeight =
            lineCount * plainLineHeight

        gutterWidth =
            "calc(" ++ String.fromInt (String.length (String.fromInt (startLine + lineCount - 1))) ++ "ch + var(--spacing-sm) + var(--spacing-xs))"

        gutterEventAttrs =
            case mGutter of
                Just { recordId, target } ->
                    let
                        lineAction action =
                            Decode.map (action target recordId path) lineNumberDecoder

                        pointerDown =
                            Html.Events.on "pointerdown" (lineAction Actions.startGutterDrag)
                    in
                    if hasGutterDrag then
                        let
                            pointerEnter =
                                Html.Events.on "pointerenter" (lineAction Actions.extendGutterDrag)
                        in
                        [ pointerDown, pointerEnter ]

                    else
                        [ pointerDown ]

                Nothing ->
                    []

        scrollAttrs =
            case ( mode, mGutter ) of
                ( SeekContent _, Just { recordId, target } ) ->
                    [ Html.Events.on "scroll" (Decode.map (Actions.onSeekScroll target recordId path) scrollMetricsDecoder) ]

                ( EagerContent, Just { recordId, target } ) ->
                    [ Html.Events.on "scroll" (Decode.map (\metrics -> Actions.setPlainFileScrollTop target recordId path metrics.scrollTop) scrollMetricsDecoder) ]

                _ ->
                    []

        renderLineNumber lineIndex =
            let
                isLoadingLine =
                    (loadingBefore && lineIndex == 0) || (loadingAfter && lineIndex == lineCount - 1)
            in
            if isLoadingLine then
                let
                    attrs =
                        [ class "file-line-number"
                        , style "height" (String.fromInt plainLineHeight ++ "px")
                        , style "line-height" (String.fromInt plainLineHeight ++ "px")
                        ]
                in
                Html.div attrs []

            else
                let
                    lineNum =
                        startLine + lineIndex
                in
                Html.div
                    (classList
                        [ ( "file-line-number", True )
                        , ( "is-gutter", hasGutter )
                        , ( "highlighted", selectedFrom > 0 && lineNum >= selectedFrom && lineNum <= selectedTo )
                        ]
                        :: Html.Attributes.attribute "data-line" (String.fromInt lineNum)
                        :: style "height" (String.fromInt plainLineHeight ++ "px")
                        :: style "line-height" (String.fromInt plainLineHeight ++ "px")
                        :: gutterEventAttrs
                    )
                    [ Html.text (String.fromInt lineNum) ]

        overlayFrom =
            max (startLine + hiddenFirstLines) selectedFrom

        overlayTo =
            min selectedTo (startLine + lineCount - 1 - hiddenLastLines)

        highlightOverlay =
            Html.viewIf (selectedFrom > 0 && overlayFrom <= overlayTo) <|
                Html.div
                    [ class "file-highlight-overlay"
                    , style "top" (String.fromInt ((overlayFrom - startLine) * plainLineHeight) ++ "px")
                    , style "height" (String.fromInt ((overlayTo - overlayFrom + 1) * plainLineHeight) ++ "px")
                    ]
                    []
    in
    Html.div
        ([ class "file-content"
         , id ("viewer-" ++ anchor)
         , style "max-height" (calculateViewerHeight lineCount)
         ]
            ++ scrollAttrs
        )
        [ Html.div
            [ class "file-body"
            , style "height" (String.fromInt totalHeight ++ "px")
            ]
            [ Html.div
                [ class "file-gutter"
                , style "width" gutterWidth
                ]
                [ Html.div
                    [ class "file-gutter-window"
                    , style "top" (String.fromInt topOffset ++ "px")
                    ]
                    (List.map renderLineNumber lineIndexes)
                ]
            , highlightOverlay
            , fileText
            ]
        ]


viewFileText : String -> Html (Flow Model ())
viewFileText text =
    Html.div [ class "file-text" ] [ Html.text text ]


viewFileChunks : Maybe SeekDirection -> List FileChunk -> Html (Flow Model ())
viewFileChunks loading chunks =
    let
        dropFirstLine texts =
            case texts of
                [] ->
                    []

                first :: rest ->
                    case List.head (String.indexes "\n" first) of
                        Just newline ->
                            String.dropLeft (newline + 1) first :: rest

                        Nothing ->
                            "" :: dropFirstLine rest

        dropLastLine texts =
            let
                drop reversed =
                    case reversed of
                        [] ->
                            []

                        last :: rest ->
                            case List.last (String.indexes "\n" last) of
                                Just newline ->
                                    String.left newline last :: rest

                                Nothing ->
                                    "" :: drop rest
            in
            texts |> List.reverse |> drop |> List.reverse

        loadingLine =
            Html.div [ class "seek-loading-line" ]
                [ Html.span [ class "seek-loading-icon" ] [ icon True "progress_activity" ] ]

        chunkTexts =
            List.map .content chunks

        children =
            case loading of
                Just Before ->
                    loadingLine :: List.map Html.text (dropFirstLine chunkTexts)

                Just After ->
                    List.map Html.text (dropLastLine chunkTexts) ++ [ loadingLine ]

                Nothing ->
                    List.map Html.text chunkTexts
    in
    Html.div [ class "file-text" ] children


calculateViewerHeight : Int -> String
calculateViewerHeight lineCount =
    let
        calculatedHeight =
            lineCount * plainLineHeight + plainViewerPadding

        cappedHeight =
            min calculatedHeight plainViewerMaxHeight

        finalHeight =
            max cappedHeight plainViewerMinHeight
    in
    String.fromInt finalHeight ++ "px"
