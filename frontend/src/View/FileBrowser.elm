module View.FileBrowser exposing (viewDirectorySection, viewHtmlFrame, viewSrcFilesSection)

import Accessors exposing (Prism, has, just, prism, snd, try)
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
import Html.Attributes exposing (class, classList, disabled, href, id, placeholder, readonly, rel, src, target, type_)
import Html.Events
import Html.Extra as Html
import Html.Lazy
import Json.Decode as Decode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core exposing (CompareSelection, CompareSource(..), DirectoryItem(..), FileChunk, Model, SeekDirection(..), SeekWindow, StepRecord, hasBuiltOutput, stepRevision, windowLineCount, windowStartLine)
import Model.Lenses exposing (compareSelecting, compareState, currentProjectPath, fileZoomAt, gutterDrag, mHighlight, mimeType, route, srcFileWriting, stepRecordById)
import Model.Shadow as Shadow exposing (StepType, WithSrcFiles(..))
import Model.TableSpec exposing (StepSpec)
import Route
import View.Icons exposing (icon)
import View.Lib exposing (viewLoading)
import View.PlainText as PlainText


type DirContext
    = OutputDir Int String
    | SrcDir Int


viewHtmlFrame : { id : String, src : String, zoom : Float -> Flow Model (), fitContent : Bool } -> Html (Flow Model ())
viewHtmlFrame frame =
    Html.div
        [ classList
            [ ( "iframe-zoom-wrapper", True )
            , ( "iframe-zoom-wrapper-fit", frame.fitContent )
            ]
        ]
        [ Html.node "iframe"
            [ src frame.src
            , Html.Attributes.attribute "sandbox" "allow-same-origin allow-scripts allow-top-navigation-by-user-activation allow-popups allow-popups-to-escape-sandbox"
            , class "file-html-viewer"
            , id frame.id
            ]
            []
        , Html.button
            [ class "iframe-zoom-btn zoom-in"
            , Html.Events.stopPropagationOn "click" (Decode.succeed ( frame.zoom 1.16, True ))
            ]
            [ icon True "zoom_in" ]
        , Html.button
            [ class "iframe-zoom-btn zoom-out"
            , Html.Events.stopPropagationOn "click" (Decode.succeed ( frame.zoom (1 / 1.16), True ))
            ]
            [ icon True "zoom_out" ]
        ]


isBrowserNative : String -> Bool
isBrowserNative mime =
    List.member mime
        [ "text/html"
        , "text/plain"
        , "text/css"
        , "text/xml"
        , "application/xhtml+xml"
        , "application/xml"
        , "application/json"
        , "application/javascript"
        , "image/png"
        , "image/jpeg"
        , "image/gif"
        , "image/webp"
        , "image/avif"
        , "image/bmp"
        , "image/svg+xml"
        , "video/mp4"
        , "video/webm"
        , "video/ogg"
        , "audio/mpeg"
        , "audio/ogg"
        , "audio/wav"
        , "audio/x-wav"
        , "audio/webm"
        ]


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
        |> Maybe.andThen (\recordId -> try (stepRecordById recordId << srcFileWriting) model)
        |> Maybe.withDefault False


recordRevision : Model -> Int -> Maybe String
recordRevision model recordId =
    try (stepRecordById recordId) model
        |> Maybe.andThen (stepRevision model)


compareSelectionFor : Model -> List Int -> String -> Maybe String -> List String -> DirContext -> CompareSelection
compareSelectionFor model projectPath_ fileName mime path ctx =
    case ctx of
        OutputDir recordId commit_ ->
            { projectPath = projectPath_, recordId = recordId, path = path, fileName = fileName, mimeType = mime, source = FromOutput commit_ }

        SrcDir recordId ->
            { projectPath = projectPath_, recordId = recordId, path = path, fileName = fileName, mimeType = mime, source = FromSrc (recordRevision model recordId) }


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
                                (Just (OutputDir stepId rs.outputsCommit))
                                (has (success << where_ hasBuiltOutput) rs.status)
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
            Maybe.isJust step.review || has (route << Route.page << Route.pageCommit << just) model

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
                step.id

        createForm =
            case ( step.id, step.srcFileDraft ) of
                ( Just recordId, Just draft ) ->
                    Html.form
                        [ class "src-file-create src-file-editor"
                        , Html.Events.preventDefaultOn "submit"
                            (Decode.succeed ( Actions.stageSrcFile recordId draft.name draft.content, True ))
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
                    , Html.viewIf (not isLocked) createButton
                    ]
                , Html.viewIf (not isLocked) createForm
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
                    case mGutter of
                        Just { recordId, target } ->
                            Actions.shareEntity recordId target path mSelectedRange

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
                    Actions.downloadSrcFile id (recordRevision model id) path

                Nothing ->
                    Flow.pure ()
    in
    case item of
        File file ->
            let
                isImage =
                    has (mimeType << just << where_ (String.startsWith "image/")) file

                canView =
                    not file.isDeleted && (file.viewable || file.seekable || isImage)

                isHtml =
                    has (mimeType << just << where_ (String.startsWith "text/html")) file

                isPdb =
                    has (mimeType << just << where_ ((==) "chemical/x-pdb")) file

                mCompareSelection =
                    if not file.isDeleted && (file.viewable || isImage) then
                        Maybe.map2 (\projectPath_ -> compareSelectionFor model projectPath_ itemName file.mimeType path)
                            (try currentProjectPath model)
                            mDirCtx

                    else
                        Nothing

                opensAsArtifact =
                    if Model.Core.isDelimitedPath path then
                        file.viewable

                    else
                        has (mimeType << just << where_ isBrowserNative) file

                externalArtifactUrl =
                    case ( try currentProjectPath model, mDirCtx, opensAsArtifact ) of
                        ( Just projectPath_, Just (OutputDir stepId_ commit_), True ) ->
                            Just <|
                                Route.toString <|
                                    Route.fromPage <|
                                        Route.Artifact
                                            { projectPath = projectPath_
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

                isEditableSrcFile =
                    has (just << srcDir) mDirCtx && not isLocked

                fileActionLabel =
                    if file.view.isViewing then
                        "Close " ++ itemName

                    else if isEditableSrcFile then
                        "Edit " ++ itemName

                    else
                        "View " ++ itemName
            in
            Html.div [ class "directory-file-container" ]
                [ Html.div [ class "directory-file", id anchor ]
                    [ icon True fileIcon
                    , Html.div [ class "file-name-container" ]
                        [ Html.span [ class "file-name" ] [ Html.text itemName ]
                        , if isEditableSrcFile && file.isNew then
                            Html.span [ class "src-file-change-status is-new" ] [ Html.text "New" ]

                          else if isEditableSrcFile && file.isDeleted then
                            Html.span [ class "src-file-change-status is-deleted" ] [ Html.text "Deleted" ]

                          else
                            Html.viewIf (isEditableSrcFile && Maybe.isJust file.editedContent) <|
                                Html.span [ class "src-file-change-status" ] [ Html.text "Changed" ]
                        , Html.span [ class "directory-item-meta" ] [ Html.text (Filesize.formatBase2 file.size) ]
                        ]
                    , Html.div [ class "file-actions" ]
                        [ Html.viewIf canView <|
                            Html.button
                                [ class "dir-item-icon-btn"
                                , Html.Attributes.title fileActionLabel
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
                                [ icon True
                                    (if isEditableSrcFile then
                                        "edit"

                                     else
                                        "visibility"
                                    )
                                ]
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
                                if file.isDeleted then
                                    Html.button
                                        [ class "dir-item-icon-btn"
                                        , Html.Attributes.title "Restore"
                                        , disabled (srcWritePending model mDirCtx)
                                        , Html.Events.onClick (Actions.restoreSrcFile recordId path)
                                        ]
                                        [ icon True "restore_from_trash" ]

                                else
                                    Html.button
                                        [ class "dir-item-icon-btn"
                                        , Html.Attributes.title "Delete"
                                        , disabled (srcWritePending model mDirCtx)
                                        , Html.Events.onClick (Actions.stageSrcFileDeletion recordId path)
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
                                let
                                    mHtmlSrc =
                                        case mDirCtx of
                                            Just (OutputDir stepId_ commit_) ->
                                                Just (Api.stepFileBundleUrl stepId_ commit_ path)

                                            Just (SrcDir recordId) ->
                                                Just (Api.srcFileRawUrl recordId (recordRevision model recordId) path)

                                            Nothing ->
                                                Nothing

                                    iframeId =
                                        "iframe-" ++ anchor

                                    zoomAction factor =
                                        mRecordId
                                            |> Maybe.map (\recordId -> Actions.zoomIframeBy (fileZoomAt recordId path) iframeId factor)
                                            |> Maybe.withDefault Flow.none
                                in
                                Html.viewMaybe
                                    (\htmlSrc -> viewHtmlFrame { id = iframeId, src = htmlSrc, zoom = zoomAction, fitContent = False })
                                    mHtmlSrc

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
                                                Just (Actions.updateDelimitedGrid recordId path)

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
                                                Grid.view updateGrid viewPlainContent_ Html.nothing delimitedGrid.grid

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


viewPlainContent : Int -> Bool -> Int -> Int -> String -> Float -> String -> ( Int, Int ) -> Html (Flow Model ())
viewPlainContent gutterKeyValue hasGutterDrag selectedFrom selectedTo anchor scrollTop text ( startLine, lineCount ) =
    let
        path =
            pathFromAnchor gutterKeyValue anchor
    in
    PlainText.view
        { id = "viewer-" ++ anchor
        , layout = PlainText.FitLines
        , scrollTop = scrollTop
        , onScroll =
            gutterFromKey gutterKeyValue
                |> Maybe.map (\{ recordId, target } metrics -> Actions.setPlainFileScrollTop target recordId path metrics.scrollTop)
        , lineSelection = gutterLineSelection gutterKeyValue hasGutterDrag selectedFrom selectedTo path
        , loading = Nothing
        , startLine = startLine
        , lineCount = lineCount
        }
        (Html.Lazy.lazy PlainText.viewText text)


viewSeekPlainContent : Int -> Bool -> Int -> Int -> String -> Float -> SeekWindow -> Html (Flow Model ())
viewSeekPlainContent gutterKeyValue hasGutterDrag selectedFrom selectedTo anchor scrollTop window_ =
    let
        path =
            pathFromAnchor gutterKeyValue anchor
    in
    PlainText.view
        { id = "viewer-" ++ anchor
        , layout = PlainText.FitLines
        , scrollTop = scrollTop
        , onScroll =
            gutterFromKey gutterKeyValue
                |> Maybe.map (\{ recordId, target } -> Actions.onSeekScroll target recordId path)
        , lineSelection = gutterLineSelection gutterKeyValue hasGutterDrag selectedFrom selectedTo path
        , loading = window_.loading
        , startLine = windowStartLine window_
        , lineCount = windowLineCount window_
        }
        (Html.Lazy.lazy2 viewFileChunks window_.loading window_.chunks)


gutterLineSelection : Int -> Bool -> Int -> Int -> List String -> Maybe (PlainText.LineSelection (Flow Model ()))
gutterLineSelection gutterKeyValue hasGutterDrag selectedFrom selectedTo path =
    gutterFromKey gutterKeyValue
        |> Maybe.map
            (\{ recordId, target } ->
                { from = selectedFrom
                , to = selectedTo
                , onPointerDown = Actions.startGutterDrag target recordId path
                , onPointerEnter =
                    if hasGutterDrag then
                        Just (Actions.extendGutterDrag target recordId path)

                    else
                        Nothing
                }
            )


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
