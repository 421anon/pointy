module Actions exposing (..)

import Accessors exposing (An_Optic, all, each, get, has, just, keyI, set, try, values)
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Api.Decode as ApiDecode
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Channels
import Components.Select exposing (selected)
import Dict
import DnDList
import Extra.Accessors exposing (A_Traversal, by, orElseT, remkT, where_)
import Extra.FlowError as FlowError exposing (FlowError)
import Extra.Http as Http
import Extra.List as List
import File.Select as Select
import Flow exposing (Flow)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, Model, ProjectRecord, Status(..), StepRecord, StepStatusEvent(..), Table, TableTag(..), dndSystem)
import Model.Lenses exposing (..)
import Model.Core as Model
import Model.Lib exposing (sortProjects)
import Model.TableSpec as TableSpec exposing (StepSpec, TableSpec, getTag)
import Ports
import Process
import Route exposing (Route(..))
import Scroll
import Set
import Task
import Toast exposing (Toast)
import Url


toggleTable : A_Traversal Model (Table a) -> Flow Model ()
toggleTable lens =
    Flow.over (remkT lens << isOpen) not


toggleShowHiddenRecords : A_Traversal Model (Table a) -> Flow Model ()
toggleShowHiddenRecords lens =
    Flow.over (remkT lens << showHiddenRecords) not


toggleAddOrEditRecordForm : Bool -> TableSpec (BaseRecord a) -> Maybe Int -> Flow Model ()
toggleAddOrEditRecordForm inspected spec mRecordId =
    let
        updateTable t =
            let
                mRecordToEdit =
                    try (success << by .id mRecordId) t.records

                formIsOpen =
                    t.edited /= Nothing && not t.nameEditOnly

                notEditingExistingRecord =
                    (t.edited |> Maybe.andThen .id) == Nothing

                togglingCurrentRecord =
                    mRecordToEdit == t.edited

                clickedNewRecord =
                    mRecordToEdit == Nothing

                switchingMode =
                    togglingCurrentRecord && inspected /= t.inspected

                newedited =
                    if formIsOpen && not switchingMode && (togglingCurrentRecord || (clickedNewRecord && notEditingExistingRecord)) then
                        Nothing

                    else
                        Just (Maybe.withDefault (TableSpec.getDefaultRecord spec) mRecordToEdit)
            in
            { t | nameEditOnly = False, inspected = inspected, edited = newedited }

        scrollAction =
            Flow.attemptTask (Scroll.scrollY (Maybe.unwrap ("table-" ++ TableSpec.getName spec) String.fromInt mRecordId) 0 0)

        focusAction =
            Flow.attemptTask (Dom.focus (TableSpec.getName spec ++ "-name-input"))
    in
    Flow.over (TableSpec.getLens spec) updateTable
        |> Flow.seq Flow.get
        |> Flow.map (try (TableSpec.getLens spec << edited << just))
        |> Flow.andThen (\mEdited -> Flow.when (Maybe.isJust mEdited) (Flow.batchM [ scrollAction, focusAction ]))


startInlineRecordNameEdit : TableSpec a -> a -> Flow Model ()
startInlineRecordNameEdit spec record =
    Flow.over (TableSpec.getLens spec) (\t -> { t | edited = Just record, nameEditOnly = True })


stopInlineRecordNameEdit : TableSpec a -> Flow Model ()
stopInlineRecordNameEdit spec =
    Flow.over (TableSpec.getLens spec) (\t -> { t | edited = Nothing, nameEditOnly = False })


editRecordName : A_Traversal s (Table (BaseRecord a)) -> String -> Flow s ()
editRecordName lens value =
    Flow.over (remkT lens << edited << just) (\record -> { record | name = value })


createProject : ProjectRecord -> FlowError Http.Error Model ProjectRecord
createProject record =
    Flow.forAll (stepConfig << success)
        (\stepConfig_ ->
            Flow.forAll nextClientId
                (\cid ->
                    Flow.over nextClientId ((+) 1)
                        |> Flow.seq (Flow.over (projects << records << success) (\rs -> rs ++ [ { record | id = Nothing, clientId = Just cid, isUpdating = True } ]))
                        |> Flow.seq (endRecordEdit projects)
                        |> Flow.seq
                            (callApi void (Api.createProject stepConfig_ record)
                                |> FlowError.andThen
                                    (\newRecord ->
                                        Flow.over (projects << records << success)
                                            (List.map
                                                (\r ->
                                                    if r.clientId == Just cid then
                                                        { newRecord | clientId = Nothing }

                                                    else
                                                        r
                                                )
                                            )
                                            |> Flow.seq refetchCommitHash
                                            |> Flow.return newRecord
                                    )
                                |> FlowError.catchError
                                    (\e ->
                                        Flow.over (projects << records << success) (List.filter (\r -> r.clientId /= Just cid))
                                            |> Flow.seq (FlowError.throwError e)
                                    )
                            )
                )
        )


createStep : StepSpec -> StepRecord -> FlowError Http.Error Model StepRecord
createStep spec record =
    Flow.forAll currentProjectId
        (\projectId ->
            case getTag spec of
                TagSteps _ stepType ->
                    let
                        tableLens =
                            projects << records << success << by .id (Just projectId) << tableInProject (TableSpec.getName spec)
                    in
                    Flow.forAll nextClientId
                        (\cid ->
                            Flow.over nextClientId ((+) 1)
                                |> Flow.seq (Flow.over (tableLens << records << success) (\rs -> rs ++ [ { record | id = Nothing, clientId = Just cid, isUpdating = True } ]))
                                |> Flow.seq (endRecordEdit tableLens)
                                |> Flow.seq
                                    (callApi void (Api.createStep (Just projectId) stepType record)
                                        |> FlowError.andThen
                                            (\newRecord ->
                                                Flow.pure newRecord.id
                                                    |> Flow.assertJust
                                                    |> Flow.seq
                                                        (Flow.over (tableLens << records << success)
                                                            (List.map
                                                                (\r ->
                                                                    if r.clientId == Just cid then
                                                                        { newRecord | clientId = Nothing }

                                                                    else
                                                                        r
                                                                )
                                                            )
                                                        )
                                                    |> Flow.seq refetchCommitHash
                                                    |> Flow.return newRecord
                                            )
                                        |> FlowError.catchError
                                            (\e ->
                                                Flow.over (tableLens << records << success) (List.filter (\r -> r.clientId /= Just cid))
                                                    |> Flow.seq (FlowError.throwError e)
                                            )
                                    )
                        )

                _ ->
                    FlowError.throwError (Http.BadBody "Invalid step table specification")
        )


toggleRecordVisibility : TableSpec (BaseRecord a) -> Maybe Int -> Maybe Bool -> BaseRecord a -> Flow Model ()
toggleRecordVisibility spec mProjectId mHidden record =
    let
        hiddenRecord =
            { record | hidden = Maybe.withDefault (not record.hidden) mHidden }

        recordLens =
            TableSpec.getLens spec << records << success << by .id hiddenRecord.id
    in
    Flow.setAll recordLens hiddenRecord
        |> Flow.seq
            (case ( mProjectId, getTag spec, hiddenRecord.id ) of
                ( Just projectId, _, _ ) ->
                    saveProject projectId

                ( Nothing, TagProjects, Just id ) ->
                    saveProject id

                ( Nothing, TagSteps _ _, _ ) ->
                    callApi void (Api.saveRecord spec hiddenRecord)

                _ ->
                    Flow.pure (Ok ())
            )
        |> FlowError.foldResult
            (\_ -> refetchCommitHash)
            (\_ -> Flow.setAll recordLens record)
        |> Flow.return ()


loadProjects : Flow Model ()
loadProjects =
    Flow.forAll (stepConfig << success)
        (\stepConfig_ ->
            Flow.get
                |> Flow.andThen
                    (\model ->
                        let
                            mCommit_ =
                                try (route << Route.project << mCommit << just) model
                        in
                        callApiMerge Model.updateProjectRecordList (projects << records) (Api.fetchProjects mCommit_ stepConfig_ |> Flow.map (Result.map sortProjects))
                    )
        )
        |> Flow.return ()


loadUserRepoInfo : Flow Model ()
loadUserRepoInfo =
    callApi userRepoInfo Api.fetchUserRepoInfo |> Flow.return ()


loadStepConfig : Flow Model ()
loadStepConfig =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.project << mCommit << just) model
                in
                callApi stepConfig (Api.fetchStepConfig mCommit_)
                    |> Flow.seq
                        (case mCommit_ of
                            Nothing ->
                                refetchCommitHash

                            Just c ->
                                Flow.setAll commitHash (ApiData.Success c)
                        )
            )
        |> Flow.return ()


refetchCommitHash : Flow Model ()
refetchCommitHash =
    callApi commitHash Api.fetchCommitHash |> Flow.map (always ())


removeRecord : TableSpec (BaseRecord a) -> Int -> Flow Model ()
removeRecord spec recordId_ =
    case getTag spec of
        TagProjects ->
            Flow.forAll (TableSpec.getLens spec << records << success << by .id (Just recordId_))
                (\recordToDelete ->
                    Flow.over (TableSpec.getLens spec << records << success)
                        (List.filter (\r -> r.id /= Just recordId_))
                        |> Flow.seq
                            (callApi void (Api.deleteProject recordId_)
                                |> FlowError.andThen (\_ -> refetchCommitHash)
                                |> FlowError.foldResult
                                    (always (Flow.pure ()))
                                    (\_ -> Flow.over (TableSpec.getLens spec << records << success) (\rs -> rs ++ [ recordToDelete ]))
                            )
                )

        TagSteps _ _ ->
            Flow.forAll currentProjectId
                (\projectId ->
                    let
                        tableLens =
                            projects << records << success << by .id (Just projectId) << tableInProject (TableSpec.getName spec) << records << success
                    in
                    Flow.forAll (tableLens << by .id (Just recordId_))
                        (\recordToDelete ->
                            Flow.over tableLens (List.filter (\r -> r.id /= Just recordId_))
                                |> Flow.seq
                                    (Flow.setting (projectStep (Just projectId) (Just recordId_) << isUpdating)
                                        (callApi void (Api.unassignRecordFromProject projectId recordId_))
                                        |> FlowError.andThen (\_ -> refetchCommitHash)
                                        |> FlowError.foldResult
                                            (always (Flow.pure ()))
                                            (\_ -> Flow.over tableLens (\rs -> rs ++ [ recordToDelete ]))
                                    )
                        )
                )


batchAssignRecordsToProject : List Int -> Int -> Flow Model ()
batchAssignRecordsToProject recordIds projectId =
    callApi void (Api.batchAssignRecordsToProject projectId recordIds)
        |> Flow.seq refetchCommitHash
        |> Flow.return ()


upsertProject : TableSpec ProjectRecord -> Flow Model ()
upsertProject spec =
    let
        lens =
            TableSpec.getLens spec
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.pure (Maybe.map2 Tuple.pair (try (lens << edited << just) model) (try (lens << addMode) model))
                    |> Flow.assertJust
                    |> Flow.assertCondition (\( edited_, addMode_ ) -> String.trim edited_.name /= "" || addMode_ == AddExisting)
                    |> Flow.andThen
                        (\( edited_, addMode_ ) ->
                            case ( edited_.id, addMode_ ) of
                                ( Nothing, AddNew ) ->
                                    createProject edited_ |> Flow.return ()

                                ( Nothing, AddExisting ) ->
                                    Flow.pure ()

                                ( Just _, _ ) ->
                                    let
                                        clearUpdating =
                                            Flow.over (lens << records << success << by .id edited_.id << isUpdating) (always False)
                                                |> Flow.seq refetchCommitHash
                                    in
                                    Flow.over (lens << records << success) (List.updateIf (\r -> r.id == edited_.id) (always { edited_ | isUpdating = True }))
                                        |> Flow.seq (endRecordEdit lens)
                                        |> Flow.seq
                                            (callApi void (Api.saveRecord spec edited_)
                                                |> FlowError.foldResult (always clearUpdating) (always clearUpdating)
                                            )
                        )
            )


upsertStep : StepSpec -> Flow Model ()
upsertStep spec =
    let
        lens =
            TableSpec.getLens spec
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.pure (Maybe.map2 Tuple.pair (try (lens << edited << just) model) (try (lens << addMode) model))
                    |> Flow.assertJust
                    |> Flow.assertCondition (\( edited_, addMode_ ) -> String.trim edited_.name /= "" || addMode_ == AddExisting)
                    |> Flow.andThen
                        (\( edited_, addMode_ ) ->
                            Flow.forAll currentProjectId
                                (\projectId ->
                                    case ( edited_.id, addMode_ ) of
                                        ( Nothing, AddNew ) ->
                                            createStep spec edited_ |> Flow.return ()

                                        ( Nothing, AddExisting ) ->
                                            Flow.setting (TableSpec.getLens spec << isUpdating)
                                                (batchAssignRecordsToProject (all (lens << selectExistingSteps << selected << each << recordId << just) model) projectId)
                                                |> Flow.seq (Flow.setAll (lens << selectExistingSteps << selected) [])
                                                |> Flow.seq (endRecordEdit lens)
                                                |> Flow.seq loadProjects

                                        ( Just stepId, _ ) ->
                                            Flow.try (lens << recordById stepId << args)
                                                (\originalArgs ->
                                                    let
                                                        argsChanged =
                                                            originalArgs /= Just edited_.args

                                                        clearUpdating =
                                                            Flow.over (lens << records << success << by .id (Just stepId) << isUpdating) (always False)
                                                                |> Flow.seq refetchCommitHash
                                                    in
                                                    Flow.when argsChanged (Flow.setAll (lens << recordById stepId << runState) (ApiData.loading Nothing))
                                                        |> Flow.seq
                                                            (Flow.over (lens << records << success)
                                                                (List.updateIf
                                                                    (\r -> r.id == Just stepId)
                                                                    (\r ->
                                                                        { edited_
                                                                            | isUpdating = True
                                                                            , runState =
                                                                                if argsChanged then
                                                                                    ApiData.loading Nothing

                                                                                else
                                                                                    r.runState
                                                                        }
                                                                    )
                                                                )
                                                                |> Flow.seq (endRecordEdit lens)
                                                                |> Flow.seq
                                                                    (callApi void (Api.saveRecord spec edited_)
                                                                        |> FlowError.foldResult (always clearUpdating) (always clearUpdating)
                                                                    )
                                                            )
                                                )
                                )
                        )
            )


endRecordEdit : A_Traversal s (Table (BaseRecord a)) -> Flow s ()
endRecordEdit lens =
    Flow.over (remkT lens) (\t -> { t | edited = Nothing, addMode = AddNew })


onUrlRequest : Browser.UrlRequest -> Flow Model ()
onUrlRequest urlRequest =
    case urlRequest of
        Browser.Internal url ->
            Flow.get
                |> Flow.andThen (\model -> Flow.lift (Nav.pushUrl (Model.getKey model) (Url.toString url)))

        Browser.External href ->
            Flow.lift (Nav.load href)


goToRoute : Route -> Flow Model ()
goToRoute route =
    Flow.get |> Flow.andThen (\model -> Flow.lift (Nav.pushUrl (Model.getKey model) (Route.toString route)))


runStep : StepSpec -> Int -> Flow Model ()
runStep spec id =
    let
        table =
            TableSpec.getLens spec

        setStatus status =
            Flow.setAll (statusAt table id) status
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.project << mCommit << just) model
                in
                Flow.when (model |> has (table << edited << just << recordId << just << where_ ((==) id))) (TableSpec.getUpsertRecord spec)
                    |> Flow.seq (toggleSrcEntry id (Just False) [])
                    |> Flow.seq (toggleOutputEntry id (Just False) [])
                    |> Flow.seq (setStatus (ApiData.loading <| Just StatusRunning))
                    |> Flow.seq
                        (registerStepStatusHook id
                            (addToast True
                                (case try (table << recordById id << name) model of
                                    Just stepName ->
                                        "Step '" ++ stepName ++ "' completed"

                                    Nothing ->
                                        "Step completed"
                                )
                            )
                        )
                    |> Flow.seq (callApi void (Api.runStep id mCommit_))
            )
        |> FlowError.foldResult
            (\_ -> Flow.pure ())
            (\_ -> setStatus (ApiData.loading <| Just (StatusFailure Nothing)))


fetchLastSuccessesFor : Int -> Flow Model ()
fetchLastSuccessesFor stepId =
    Flow.forAll currentProjectId
        (\projectId ->
            Flow.get
                |> Flow.andThen
                    (\model ->
                        case Dict.get stepId (Model.getLastSuccesses model) of
                            Just (ApiData.Loading _) ->
                                Flow.pure ()

                            Just (ApiData.Success _) ->
                                Flow.pure ()

                            _ ->
                                let
                                    mCommit_ =
                                        try (route << Route.project << mCommit << just) model
                                in
                                Flow.over lastSuccesses (Dict.insert stepId (ApiData.loading Nothing))
                                    |> Flow.seq (Api.fetchStepLastSuccesses projectId stepId mCommit_)
                                    |> Flow.andThen
                                        (\result ->
                                            Flow.over lastSuccesses (Dict.insert stepId (ApiData.fromResult result))
                                        )
                    )
        )


navigateToSuccessCommit : Int -> String -> Flow Model ()
navigateToSuccessCommit stepId commit =
    Flow.forAll currentProjectId
        (\projectId ->
            goToRoute (Project { projectId = projectId, mHighlight = Just { id = stepId, path = [] }, mCommit = Just commit })
        )


stopStep : StepSpec -> Int -> Flow Model ()
stopStep spec id =
    let
        table =
            TableSpec.getLens spec

        setStatus status =
            Flow.setAll (statusAt table id) status
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.project << mCommit << just) model
                in
                setStatus (ApiData.loading <| Just StatusRunning)
                    |> Flow.seq (callApi void (Api.stopStep id mCommit_))
            )
        |> FlowError.foldResult
            (\_ -> Flow.pure ())
            (\_ -> setStatus (ApiData.loading <| Just (StatusFailure Nothing)))


setAddMode : A_Traversal s (Table (BaseRecord a)) -> BaseRecord a -> AddMode -> Flow s ()
setAddMode lens defaultRecord mode =
    Flow.over lens (\t -> { t | addMode = mode, edited = Just defaultRecord })


cloneStep : StepSpec -> StepRecord -> Flow Model ()
cloneStep spec record =
    let
        generateUniqueCloneName baseName existingNames =
            let
                original =
                    case String.indexes " (Clone" baseName |> List.head of
                        Just i ->
                            String.left i baseName

                        Nothing ->
                            baseName

                findFree index =
                    let
                        candidate =
                            if index == 1 then
                                original ++ " (Clone)"

                            else
                                original ++ " (Clone " ++ String.fromInt index ++ ")"
                    in
                    if Set.member candidate (Set.fromList existingNames) then
                        findFree (index + 1)

                    else
                        candidate
            in
            findFree 1
    in
    Flow.getAll (TableSpec.getLens spec << records << success << each << name)
        (\existingNames ->
            createStep spec (set name (generateUniqueCloneName record.name existingNames) record)
                |> FlowError.andThen
                    (\newRecord ->
                        Flow.assertJust (Flow.pure newRecord.id)
                            |> Flow.andThen (\_ -> loadProjects)
                    )
        )
        |> Flow.return ()


shareEntity : Int -> Int -> List String -> Flow Model ()
shareEntity projectId entityId pathSegments =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    route_ =
                        Route.Project
                            { projectId = projectId
                            , mHighlight = Just { id = entityId, path = pathSegments }
                            , mCommit = try (commitHash << success) model
                            }
                in
                callJs "copyToClipboard" Encode.string (Decode.succeed ()) (Model.getOrigin model ++ Route.toString route_)
            )
        |> Flow.seq (addToast True "Share link copied to clipboard")


{-| Call an API. Adds an error toast and convenience updates over a lens.
-}
callApi : An_Optic pr ls Model (ApiData a) -> FlowError Http.Error Model a -> FlowError Http.Error Model a
callApi =
    callApiMerge always


callApiMerge : (a -> a -> a) -> An_Optic pr ls Model (ApiData a) -> FlowError Http.Error Model a -> FlowError Http.Error Model a
callApiMerge merge lens apiCall =
    Flow.over (remkT lens) ApiData.toLoading
        |> Flow.seq apiCall
        |> Flow.andThen
            (\result ->
                Flow.over (remkT lens) (ApiData.update merge (ApiData.fromResult result))
                    |> Flow.return result
                    |> FlowError.catchError
                        (\e ->
                            Flow.batchM
                                [ addToast False (Http.errorMessage e) |> Flow.seq Flow.none
                                , FlowError.throwError e
                                ]
                        )
            )


downloadFile : String -> String -> Flow Model ()
downloadFile outPath_ path =
    Flow.lift (Nav.load ("/backend/store/download?outPath=" ++ outPath_ ++ "&path=" ++ path))


downloadSrcFile : Int -> String -> Flow Model ()
downloadSrcFile id path =
    Flow.lift (Nav.load ("/backend/src-files/download?id=" ++ String.fromInt id ++ "&path=" ++ path))


toggleFile : Int -> List String -> Flow Model ()
toggleFile recordId path =
    toggleOutputEntry recordId Nothing path |> Flow.return ()


toggleSrcFile : Int -> List String -> Flow Model ()
toggleSrcFile recordId path =
    toggleSrcEntry recordId Nothing path |> Flow.return ()


zoomHtmlFileBy : A_Traversal (Table StepRecord) Float -> String -> Float -> Flow Model ()
zoomHtmlFileBy tableZoomLens iframeId factor =
    let
        zoomLens =
            currentProject << success << tables << values << tableZoomLens
    in
    Flow.forAll zoomLens
        (\currentZoom ->
            let
                newZoom =
                    clamp 0.5 2.0 (currentZoom * factor)
            in
            Flow.over zoomLens (always newZoom)
                |> Flow.seq (callJs "zoomIframe" (\r -> Encode.object [ ( "id", Encode.string r.id ), ( "zoom", Encode.float r.zoom ) ]) (Decode.succeed ()) { id = iframeId, zoom = newZoom })
        )


toggleOutputEntry :
    Int
    -> Maybe Bool
    -> List String
    -> Flow Model Bool
toggleOutputEntry recordId mOpen path =
    let
        allStepTables =
            currentProject << success << tables << values

        isExpanded =
            (allStepTables << folderExpandedAt recordId path) |> orElseT (allStepTables << fileIsViewingAt recordId path)

        stepOutPath =
            allStepTables << recordById recordId << runState << success << outPath

        folderAction =
            Flow.forAll stepOutPath
                (\outPath_ ->
                    Flow.forAll (allStepTables << directoryItemAtPath recordId path << folder)
                        (\_ ->
                            callApi (allStepTables << childrenAt recordId path)
                                (Api.fetchDirectoryContents ApiDecode.directoryItemGeneric outPath_ path)
                                |> Flow.return ()
                        )
                )

        fileAction =
            Flow.forAll stepOutPath
                (\outPath_ ->
                    Flow.forAll (allStepTables << directoryItemAtPath recordId path << file)
                        (\file_ ->
                            Flow.when
                                (not
                                    (has (mimeType << just << where_ (String.startsWith "image/")) file_
                                        || has (mimeType << just << where_ (String.startsWith "text/html")) file_
                                    )
                                )
                                (callApi (allStepTables << fileContentAt recordId path)
                                    (Api.fetchFileContents outPath_ path)
                                    |> Flow.return ()
                                )
                        )
                )
    in
    Flow.forAll isExpanded
        (\wasExpanded ->
            let
                newlyExpanded =
                    Maybe.withDefault (not wasExpanded) mOpen
            in
            Flow.setAll (allStepTables << childrenAt recordId path) NotAsked
                |> Flow.seq (Flow.setAll isExpanded newlyExpanded)
                |> Flow.seq (Flow.when newlyExpanded <| Flow.batchM [ folderAction, fileAction ])
                |> Flow.return newlyExpanded
        )


toggleSrcEntry :
    Int
    -> Maybe Bool
    -> List String
    -> Flow Model Bool
toggleSrcEntry recordId mOpen path =
    let
        allStepTables =
            currentProject << success << tables << values

        isExpanded =
            (allStepTables << srcFilesFolderExpandedAt recordId path) |> orElseT (allStepTables << srcFilesFileIsViewingAt recordId path)

        folderAction =
            Flow.forAll (allStepTables << srcFilesItemAtPath recordId path << folder)
                (\_ ->
                    callApi (allStepTables << srcFilesChildrenAt recordId path)
                        (Api.fetchSrcDirectoryContents ApiDecode.directoryItemGeneric recordId path)
                        |> Flow.return ()
                )

        fileAction =
            Flow.forAll (allStepTables << srcFilesItemAtPath recordId path << file)
                (\file_ ->
                    Flow.when
                        (not
                            (has (mimeType << just << where_ (String.startsWith "image/")) file_
                                || has (mimeType << just << where_ (String.startsWith "text/html")) file_
                            )
                        )
                        (callApi (allStepTables << srcFilesFileContentAt recordId path)
                            (Api.fetchSrcFileContents recordId path)
                            |> Flow.return ()
                        )
                )
    in
    Flow.forAll isExpanded
        (\wasExpanded ->
            let
                newlyExpanded =
                    Maybe.withDefault (not wasExpanded) mOpen
            in
            Flow.setAll (allStepTables << srcFilesChildrenAt recordId path) NotAsked
                |> Flow.seq (Flow.setAll isExpanded newlyExpanded)
                |> Flow.seq (Flow.when newlyExpanded <| Flow.batchM [ folderAction, fileAction ])
                |> Flow.return newlyExpanded
        )


registerStepStatusHook : Int -> Flow Model () -> Flow Model ()
registerStepStatusHook stepId hook =
    Flow.over (stepStatusHooks << keyI stepId)
        (Just << Flow.seq hook << Maybe.withDefault (Flow.pure ()))


runAndClearStepStatusHook : Int -> Flow Model ()
runAndClearStepStatusHook stepId =
    Flow.forAll (stepStatusHooks << keyI stepId << just)
        ((|>) (Flow.setAll (stepStatusHooks << keyI stepId) Nothing) << Flow.seq)


deepOpenEntryOrDefer : Int -> List String -> Flow Model ()
deepOpenEntryOrDefer id path =
    Flow.try
        (projects << records << success << each << tables << values << records << success << by .id (Just id) << runState << success << status << success << where_ ((==) StatusSuccess))
        (\mStatus ->
            case mStatus of
                Just _ ->
                    deepOpenEntry id path

                Nothing ->
                    registerStepStatusHook id (deepOpenEntry id path)
                        |> Flow.seq (Flow.attemptTask (Scroll.scrollY (String.fromInt id) 0 0))
        )


deepOpenEntry : Int -> List String -> Flow Model ()
deepOpenEntry stepId path =
    Flow.forAll (currentProject << success << tables << values << recordById stepId << runState << success << status << success << where_ ((==) StatusSuccess))
        (\_ ->
            List.prefixes path
                |> List.map
                    (\pathPart ->
                        toggleOutputEntry stepId (Just True) pathPart
                            |> Flow.seq (Flow.attemptTask (Scroll.scrollY (String.join "/" <| String.fromInt stepId :: pathPart) 0 0))
                    )
                |> List.foldl Flow.seq (Flow.attemptTask (Scroll.scrollY (String.fromInt stepId) 0 0))
        )


addToast : Bool -> String -> Flow Model ()
addToast isSuccess message =
    Flow.get
        |> Flow.map (get nextToastId)
        |> Flow.andThen
            (\nextId ->
                Flow.over toasts ((::) <| Toast message nextId isSuccess)
                    |> Flow.seq (Flow.over nextToastId (\_ -> nextId + 1))
                    |> Flow.seq (Flow.lift (Task.perform identity (Process.sleep 3500)))
                    |> Flow.seq (Flow.over toasts (List.removeWhen <| (==) nextId << .id))
            )


dndMsgToIO : Maybe Int -> TableSpec (BaseRecord a) -> DnDList.Msg -> Flow Model ()
dndMsgToIO maybeProjectId tableSpec msg =
    let
        lens =
            TableSpec.getLens tableSpec
    in
    Flow.get
        |> Flow.map (try (remkT lens << dnd))
        |> Flow.assertJust
        |> Flow.andThen (\dnd -> Flow.get |> Flow.map (try (remkT lens << records << success)) |> Flow.assertJust |> Flow.map (\items -> ( dnd, items )))
        |> Flow.map (\( dnd_, items ) -> ( dnd_, dndSystem.update msg dnd_ items ))
        |> Flow.andThen
            (\( oldDnd, ( newDnd, newItems ) ) ->
                Flow.setAll (remkT lens << dnd) newDnd
                    |> Flow.seq (Flow.setAll (remkT lens << records << success) newItems)
                    |> Flow.seq
                        (if Maybe.isJust (dndSystem.info oldDnd) && Maybe.isNothing (dndSystem.info newDnd) then
                            updateSortKeys maybeProjectId tableSpec newItems

                         else
                            Flow.pure ()
                        )
                    |> Flow.seq (Flow.lift (dndSystem.commands newDnd) |> Flow.andThen (dndMsgToIO maybeProjectId tableSpec))
            )
        |> Flow.return ()


dndSub : Model -> Maybe Int -> TableSpec (BaseRecord a) -> Sub (Flow Model ())
dndSub model maybeProjectId tableSpec =
    (List.map dndSystem.subscriptions <|
        all (remkT (TableSpec.getLens tableSpec) << dnd) model
    )
        |> Sub.batch
        |> Sub.map (dndMsgToIO maybeProjectId tableSpec)


updateSortKeys : Maybe Int -> TableSpec (BaseRecord a) -> List (BaseRecord a) -> Flow Model ()
updateSortKeys mProjectId tableSpec records_ =
    let
        allUpdatedRecords =
            List.indexedMap (\i -> set sortKey (Just i)) records_
    in
    Flow.setAll (TableSpec.getLens tableSpec << records << success) allUpdatedRecords
        |> Flow.seq
            (case ( mProjectId, getTag tableSpec ) of
                ( Just projectId, _ ) ->
                    saveProject projectId

                ( Nothing, TagProjects ) ->
                    let
                        changedRecords =
                            List.map2 Tuple.pair records_ allUpdatedRecords
                                |> List.filterMap
                                    (\( old, new ) ->
                                        if old.sortKey /= new.sortKey then
                                            Just new

                                        else
                                            Nothing
                                    )
                    in
                    Flow.batchM (List.filterMap (\rec -> Maybe.map saveProject rec.id) changedRecords)
                        |> Flow.seq refetchCommitHash
                        |> Flow.return (Ok ())

                ( Nothing, TagSteps _ _ ) ->
                    let
                        changedRecords =
                            List.map2 Tuple.pair records_ allUpdatedRecords
                                |> List.filterMap
                                    (\( old, new ) ->
                                        if old.sortKey /= new.sortKey then
                                            Just new

                                        else
                                            Nothing
                                    )
                    in
                    Flow.batchM (List.map (Api.saveRecord tableSpec >> callApi void) changedRecords)
                        |> Flow.seq refetchCommitHash
                        |> Flow.return (Ok ())
            )
        |> Flow.return ()


onSelectSearch : Int -> Flow Model ()
onSelectSearch stepId =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.project << mCommit << just) model
                in
                try (projectsContainingEntity stepId) model
                    |> Maybe.andThen .id
                    |> Maybe.unwrap (Flow.pure ()) (\pId -> goToRoute (Project { projectId = pId, mHighlight = Just { id = stepId, path = [] }, mCommit = mCommit_ }))
            )


uploadFiles : StepSpec -> List String -> Int -> Flow Model ()
uploadFiles spec types stepId =
    Flow.lift (Select.files types (\file files -> Api.uploadFiles stepId (file :: files)))
        |> Flow.andThen
            (\cmd ->
                Flow.over uploadProgress (Dict.insert stepId { sent = 0, size = 0 })
                    |> Flow.seq
                        (callApi void cmd
                            |> FlowError.foldResult
                                (\_ ->
                                    Flow.over uploadProgress (Dict.remove stepId)
                                        |> Flow.seq refetchCommitHash
                                        |> Flow.seq (runStep spec stepId)
                                )
                                (\_ -> Flow.over uploadProgress (Dict.remove stepId))
                        )
            )


onUploadProgress : Int -> Http.Progress -> Flow Model ()
onUploadProgress stepId progress =
    case progress of
        Http.Sending p ->
            Flow.over uploadProgress (Dict.insert stepId { sent = p.sent, size = p.size })

        Http.Receiving _ ->
            Flow.pure ()


callJs : String -> (a -> Encode.Value) -> Decode.Decoder b -> a -> Flow Model b
callJs =
    Flow.ffi Ports.ffiOut Ports.ffiIn


hidePopover : String -> Flow Model ()
hidePopover popoverId =
    callJs "hidePopover" Encode.string (Decode.succeed ()) popoverId


openDialog : String -> Flow Model ()
openDialog id =
    callJs "openDialog" Encode.string (Decode.succeed ()) id


toggleTheme : Flow Model ()
toggleTheme =
    callJs "toggleTheme" (\_ -> Encode.null) (Decode.succeed ()) ()


cancelUpload : Int -> Flow Model ()
cancelUpload stepId =
    Flow.batchM
        [ Flow.lift (Http.cancel ("upload-" ++ String.fromInt stepId))
        , Flow.over uploadProgress (Dict.remove stepId)
        ]


saveProject : Int -> FlowError Http.Error Model ()
saveProject projectId =
    Flow.get
        |> Flow.map (try (projects << records << success << by .id (Just projectId)))
        |> Flow.assertJust
        |> Flow.andThen (Api.saveProject projectId >> callApi void)
        |> FlowError.andThen (\_ -> refetchCommitHash |> Flow.return ())


closeStepStatusStream : Flow Model ()
closeStepStatusStream =
    callJs "closeStepStatusStream" (\_ -> Encode.null) (Decode.succeed ()) ()


listenAndProcessStepStatus : Int -> Maybe String -> Flow Model Decode.Value
listenAndProcessStepStatus projectId commit =
    Flow.subscribe onStepStatusIn (Channels.stepStatus projectId commit)


onStepStatusIn : Decode.Value -> Flow Model ()
onStepStatusIn value =
    case Decode.decodeValue ApiDecode.stepStatusEvent value of
        Ok (SSESnapshot { steps }) ->
            Flow.batchM (List.map (\s -> updateStepStatus s.stepId s.status s.outPath) steps)

        Ok SSEHeartbeat ->
            Flow.pure ()

        Ok (SSEError err) ->
            addToast False ("SSE Error: " ++ err)

        Err err ->
            addToast False ("SSE Decode Error: " ++ Decode.errorToString err)


updateStepStatus : Int -> Status -> String -> Flow Model ()
updateStepStatus stepId newStatus outPath =
    let
        stepRunState =
            projects << records << success << each << tables << values << records << success << by .id (Just stepId) << runState

        defaultRunState =
            { outPath = outPath, status = NotAsked, directoryView = { children = NotAsked, expanded = False } }
    in
    Flow.over stepRunState
        (\rs ->
            let
                current =
                    ApiData.withDefault defaultRunState rs
            in
            Success { current | status = Success newStatus, outPath = outPath }
        )
        |> Flow.seq (Flow.when (newStatus == StatusSuccess) (runAndClearStepStatusHook stepId))
