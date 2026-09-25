module Actions exposing (..)

import Accessors exposing (An_Optic, all, each, get, has, just, keyI, over, set, try, values)
import Api.Agent as AgentApi
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Api.Decode as ApiDecode
import Basics.Extra exposing (flip)
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Channels
import Components.Select exposing (selected)
import Debounce
import Dict exposing (Dict)
import Dict.Accessors
import DnDList
import Extra.Accessors exposing (A_Traversal, by, orElseT, remkT, where_)
import Extra.FlowError as FlowError exposing (FlowError)
import Extra.Http as Http
import Extra.List as List
import Flow exposing (Flow)
import Grid
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, CompareActiveData, CompareFile, CompareMode(..), CompareSelection, CompareSource(..), CompareState(..), Model, ProjectRecord, SeekWindow, Status(..), StepRecord, StepStatusEvent(..), Table, TableTag(..), TemplateSource(..), dndSystem)
import Model.Lenses exposing (..)
import Model.Lib exposing (sortProjects)
import Model.Shadow exposing (StepArgValue)
import Model.TableSpec as TableSpec exposing (StepSpec, TableSpec, getTag)
import Ports
import Route exposing (Route)
import Scroll
import Set exposing (Set)
import Task
import Time
import Toast exposing (Toast)


toggleTable : A_Traversal Model (Table a) -> Flow Model ()
toggleTable lens =
    Flow.over (remkT lens << isOpen) not


toggleShowHiddenRecords : A_Traversal Model (Table a) -> Flow Model ()
toggleShowHiddenRecords lens =
    Flow.over (remkT lens << showHiddenRecords) not


toggleAddOrEditRecordForm : TableSpec (BaseRecord a) -> Maybe Int -> Flow Model ()
toggleAddOrEditRecordForm spec mRecordId =
    let
        updateTable readOnly t =
            let
                stashed =
                    case ( readOnly, t.edited ) of
                        ( False, Just r ) ->
                            set (draftAt r.id) (Just r) t

                        _ ->
                            t

                mRecordToEdit =
                    mRecordId
                        |> Maybe.andThen (\recordId -> try (success << by .id (Just recordId)) t.records)

                inspecting =
                    readOnly || Maybe.unwrap False (TableSpec.getIsLocked spec) mRecordToEdit

                formIsOpen =
                    t.edited /= Nothing && not t.nameEditOnly

                notEditingExistingRecord =
                    (t.edited |> Maybe.andThen .id) == Nothing

                togglingCurrentRecord =
                    (t.edited |> Maybe.map .id) == Just mRecordId

                clickedNewRecord =
                    mRecordToEdit == Nothing

                newEdited =
                    if formIsOpen && (togglingCurrentRecord || (clickedNewRecord && notEditingExistingRecord)) then
                        Nothing

                    else if inspecting then
                        mRecordToEdit

                    else
                        get (draftAt mRecordId) stashed
                            |> Maybe.orElse mRecordToEdit
                            |> Maybe.withDefault (TableSpec.getDefaultRecord spec)
                            |> Just
            in
            { stashed | nameEditOnly = False, edited = newEdited }

        scrollAction =
            Flow.attemptTask (Scroll.scrollY (Maybe.unwrap ("table-" ++ TableSpec.getName spec) String.fromInt mRecordId) 0 0)

        focusAction =
            Flow.attemptTask (Dom.focus (TableSpec.getName spec ++ "-name-input"))
    in
    Flow.get
        |> Flow.andThen (\model -> Flow.over (TableSpec.getLens spec) (updateTable (isReadOnlyRoute model)))
        |> Flow.seq Flow.get
        |> Flow.map (try (TableSpec.getLens spec << edited << just))
        |> Flow.andThen
            (\mEdited ->
                Flow.when (Maybe.isJust mEdited) (scrollAction |> Flow.seq focusAction)
                    |> Flow.seq
                        (case ( getTag spec, mEdited |> Maybe.andThen .id ) of
                            ( TagSteps _ _, Just stepId ) ->
                                loadNotices stepId

                            _ ->
                                Flow.pure ()
                        )
            )


startInlineRecordNameEdit : TableSpec a -> a -> Flow Model ()
startInlineRecordNameEdit spec record =
    Flow.over (TableSpec.getLens spec) (\t -> { t | edited = Just record, nameEditOnly = True })


stopInlineRecordNameEdit : TableSpec a -> Flow Model ()
stopInlineRecordNameEdit spec =
    Flow.over (TableSpec.getLens spec) (\t -> { t | edited = Nothing, nameEditOnly = False })


editRecordName : A_Traversal s (Table (BaseRecord a)) -> String -> Flow s ()
editRecordName lens value =
    Flow.over (remkT lens << edited << just) (\record -> { record | name = value })


optimisticCreate :
    A_Traversal Model (Table (BaseRecord a))
    -> BaseRecord a
    -> FlowError Http.Error Model (BaseRecord a)
    -> FlowError Http.Error Model (BaseRecord a)
optimisticCreate tableLens record apiCall =
    let
        recordsLens =
            remkT tableLens << records << success
    in
    Flow.forAll nextClientId
        (\cid ->
            Flow.over nextClientId ((+) 1)
                |> Flow.seq (Flow.over recordsLens (\rs -> rs ++ [ { record | id = Nothing, clientId = Just cid, isUpdating = True } ]))
                |> Flow.seq (endRecordEdit tableLens)
                |> Flow.seq
                    (callApi void apiCall
                        |> FlowError.andThen
                            (\newRecord ->
                                Flow.pure newRecord.id
                                    |> Flow.assertJust
                                    |> Flow.seq
                                        (Flow.forAll now
                                            (\posix ->
                                                Flow.over recordsLens
                                                    (List.map
                                                        (\r ->
                                                            if r.clientId == Just cid then
                                                                { newRecord | clientId = Nothing, lastModifiedAt = Just posix }

                                                            else
                                                                r
                                                        )
                                                    )
                                            )
                                        )
                                    |> Flow.seq refetchCommitHash
                                    |> Flow.return newRecord
                            )
                        |> FlowError.catchError
                            (\e ->
                                Flow.over recordsLens (List.filter (\r -> r.clientId /= Just cid))
                                    |> Flow.seq (FlowError.throwError e)
                            )
                    )
        )


createProject : ProjectRecord -> FlowError Http.Error Model ProjectRecord
createProject record =
    Flow.forAll (stepConfig << success)
        (\stepConfig_ ->
            Flow.forAll (presets << success)
                (\presets_ ->
                    optimisticCreate
                        projects
                        record
                        (Api.createProject presets_ stepConfig_ record)
                )
        )


createStep : Maybe Int -> StepSpec -> StepRecord -> FlowError Http.Error Model StepRecord
createStep mSourceId spec record =
    Flow.forAll currentProjectId
        (\projectId ->
            case getTag spec of
                TagSteps _ stepType ->
                    let
                        tableLens =
                            projects << records << success << by .id (Just projectId) << tableInProject (TableSpec.getName spec)
                    in
                    optimisticCreate
                        tableLens
                        record
                        (Api.createStep (Just projectId) mSourceId stepType record)

                _ ->
                    FlowError.throwError (Http.BadBody "Invalid step table specification")
        )


persistRecordChange : Maybe Int -> TableSpec (BaseRecord a) -> BaseRecord a -> FlowError Http.Error Model ()
persistRecordChange mProjectId spec record =
    case ( mProjectId, getTag spec, record.id ) of
        ( Just projectId, _, _ ) ->
            saveProject projectId

        ( Nothing, TagProjects, Just id ) ->
            saveProject id

        ( Nothing, TagSteps _ _, _ ) ->
            callApi void (Api.saveRecord spec record)

        _ ->
            Flow.pure (Ok ())


toggleRecordVisibility : TableSpec (BaseRecord a) -> Maybe Int -> Maybe Bool -> BaseRecord a -> Flow Model ()
toggleRecordVisibility spec mProjectId mHidden record =
    let
        hiddenRecord =
            { record | hidden = Maybe.withDefault (not record.hidden) mHidden }

        recordLens =
            TableSpec.getLens spec << records << success << by .id hiddenRecord.id
    in
    Flow.setAll recordLens hiddenRecord
        |> Flow.seq (persistRecordChange mProjectId spec hiddenRecord)
        |> FlowError.foldResult
            (\_ -> refetchCommitHash)
            (\_ -> Flow.setAll recordLens record)
        |> Flow.return ()


loadProjects : Flow Model ()
loadProjects =
    Flow.get
        |> Flow.andThen
            (\model ->
                case ( try (stepConfig << success) model, try (presets << success) model ) of
                    ( Just stepConfig_, Just presets_ ) ->
                        let
                            mCommit_ =
                                try (route << Route.page << Route.project << mCommit << just) model
                        in
                        callApiMerge Model.updateProjectRecordList (projects << records) (Api.fetchProjects mCommit_ presets_ stepConfig_ |> Flow.map (Result.map sortProjects))
                            |> ignoreResult
                            |> Flow.seq (Flow.async replayStepStatusBuffer)
                            |> Flow.seq (Flow.async loadProjectReviews)

                    _ ->
                        Flow.pure ()
            )


replayStepStatusBuffer : Flow Model ()
replayStepStatusBuffer =
    Flow.get
        |> Flow.andThen
            (\model ->
                applyStepStatuses (get stepStatusBuffer model)
                    |> Flow.seq (Flow.setAll stepStatusBuffer Dict.empty)
            )


applyStepStatus : String -> ApiData Status -> ApiData Model.StepRunState -> ApiData Model.StepRunState
applyStepStatus snapshotCommit status_ rs =
    let
        collapsedDirectoryView =
            { children = NotAsked, expanded = False, extras = NotAsked, size = Nothing, mimeType = Nothing }

        current =
            ApiData.toMaybe rs
                |> Maybe.withDefault { commit = snapshotCommit, status = NotAsked, directoryView = collapsedDirectoryView }

        directoryView_ =
            if current.commit == snapshotCommit then
                current.directoryView

            else
                collapsedDirectoryView

        updated =
            { current | commit = snapshotCommit, status = status_, directoryView = directoryView_ }
    in
    if rs == Success updated then
        rs

    else
        Success updated


applyStatusSnapshot : String -> Status -> ApiData Model.StepRunState -> ApiData Model.StepRunState
applyStatusSnapshot snapshotCommit newStatus rs =
    let
        pendingRun =
            has
                (success
                    << where_ (.commit >> (==) snapshotCommit)
                    << status
                    << where_ ((==) (Loading (Just StatusRunning)))
                )
                rs
    in
    if pendingRun && newStatus == StatusNotStarted then
        rs

    else
        applyStepStatus snapshotCommit (Success newStatus) rs


setLocalStepStatus : An_Optic pr ls Model (Table StepRecord) -> Int -> ApiData Status -> Flow Model ()
setLocalStepStatus table stepId status_ =
    Flow.get
        |> Flow.andThen
            (\model ->
                stepRevisionById stepId model
                    |> Maybe.unwrap (Flow.pure ())
                        (\revision ->
                            Flow.over (remkT table << recordById stepId << runState) (applyStepStatus revision status_)
                        )
            )


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
                        try (route << Route.page << Route.project << mCommit << just) model
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


loadPresets : Flow Model ()
loadPresets =
    Flow.try (route << Route.page << Route.project << mCommit << just)
        (callApi presets << Api.fetchPresets)
        |> Flow.return ()


reloadWorkspaceData : Flow Model ()
reloadWorkspaceData =
    Flow.over (projects << records) ApiData.toLoading
        |> Flow.seq (Flow.over commitHash ApiData.toLoading)
        |> Flow.seq loadStepConfig
        |> Flow.seq loadPresets
        |> Flow.seq loadProjects


chooseProjectPreset : String -> Flow Model ()
chooseProjectPreset =
    Flow.setAll (projects << edited << just << templateSource) << FromPreset


chooseProjectCustom : Flow Model ()
chooseProjectCustom =
    Flow.forAll (presets << success)
        (\presets_ ->
            Flow.over (projects << edited << just << templateSource)
                (CustomTemplates << Model.effectiveTemplates presets_)
        )


addProjectTemplate : String -> Flow Model ()
addProjectTemplate template =
    Flow.forAll (presets << success)
        (\presets_ ->
            Flow.over (projects << edited << just << templateSource)
                (\source ->
                    let
                        current =
                            Model.effectiveTemplates presets_ source
                    in
                    if List.member template current then
                        source

                    else
                        CustomTemplates (current ++ [ template ])
                )
        )


removeProjectTemplate : String -> Flow Model ()
removeProjectTemplate template =
    Flow.forAll (presets << success)
        (\presets_ ->
            Flow.over (projects << edited << just << templateSource)
                (CustomTemplates
                    << List.filter ((/=) template)
                    << Model.effectiveTemplates presets_
                )
        )


refetchCommitHash : Flow Model ()
refetchCommitHash =
    callApi commitHash Api.fetchCommitHash |> Flow.map (always ())


loadProjectReviews : Flow Model ()
loadProjectReviews =
    let
        reviewTarget : Model -> Maybe ( Int, String )
        reviewTarget model =
            Maybe.map2 Tuple.pair
                (try currentProjectId model)
                (Model.viewedRevision model)
    in
    Flow.get
        |> Flow.map reviewTarget
        |> Flow.assertJust
        |> Flow.andThen
            (\(( projectId_, commit_ ) as target) ->
                let
                    stepRecords =
                        projects << records << success << by .id (Just projectId_) << projectStepRecords
                in
                Flow.over (stepRecords << review << just << comparison) ApiData.toLoading
                    |> Flow.seq (Api.fetchProjectReviews projectId_ commit_)
                    |> Flow.andThen
                        (\result ->
                            Flow.get
                                |> Flow.andThen
                                    (\model ->
                                        if reviewTarget model == Just target then
                                            Flow.over stepRecords (mergeReviewReport commit_ result)

                                        else
                                            Flow.over (stepRecords << review << just << comparison) ApiData.stopLoading
                                                |> Flow.seq (Flow.async loadProjectReviews)
                                    )
                        )
            )


mergeReviewReport : String -> Result Http.Error (Dict Int Model.ReviewReport) -> StepRecord -> StepRecord
mergeReviewReport commit_ result record =
    case result of
        Ok reports ->
            record.id
                |> Maybe.andThen (\stepId -> Dict.get stepId reports)
                |> Maybe.unwrap (over (review << just << comparison) ApiData.stopLoading record)
                    (applyReviewReport commit_ record)

        Err err ->
            set (review << just << comparison) (Error err) record


applyReviewReport : String -> StepRecord -> Model.ReviewReport -> StepRecord
applyReviewReport commit_ record report =
    { record
        | review = report.review
        , runState =
            case ( report.review, record.review ) of
                ( Just reviewed, _ ) ->
                    reviewedRunState reviewed.revision report.reviewedStatus record.runState

                ( Nothing, Just _ ) ->
                    applyStepStatus commit_ NotAsked record.runState

                ( Nothing, Nothing ) ->
                    record.runState
    }


reviewedRunState : String -> Maybe Model.Status -> ApiData Model.StepRunState -> ApiData Model.StepRunState
reviewedRunState revision mStatus runState_ =
    case ( ApiData.toMaybe runState_ |> Maybe.filter (.commit >> (==) revision), mStatus ) of
        ( Just current, Just status_ ) ->
            if current.status == Loading (Just Model.StatusRunning) && status_ /= Model.StatusRunning then
                Success current

            else
                Success { current | status = Success status_ }

        ( Just current, Nothing ) ->
            Success current

        ( Nothing, _ ) ->
            applyStepStatus revision (Success (Maybe.withDefault Model.StatusNotStarted mStatus)) NotAsked


refreshReviews : Flow Model ()
refreshReviews =
    refetchCommitHash |> Flow.seq (Flow.async loadProjectReviews)


whenStepIdle : Int -> Flow Model () -> Flow Model ()
whenStepIdle stepId io =
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.when (try (stepRecordById stepId << isUpdating) model == Just False)
                    (Flow.setting (stepRecordById stepId << isUpdating) io)
            )


settleReview : Int -> Maybe Model.Review -> String -> Result Http.Error Bool -> Flow Model ()
settleReview stepId mReview message result =
    case result of
        Ok True ->
            Flow.async (addToast False "The viewed output differs from the reviewed output. Remove the review first to record the newer revision.")
                |> Flow.seq refreshReviews

        Ok False ->
            Flow.setAll (stepRecordById stepId << review) mReview
                |> Flow.seq refreshReviews
                |> Flow.seq (Flow.when (Maybe.isNothing mReview) (refreshViewedStepStatus stepId))
                |> Flow.seq (Flow.setAll reviewDraft Nothing)
                |> Flow.seq (Flow.async (addToast True message))

        Err err ->
            Flow.async (addToast False (Http.errorMessage err))


refreshViewedStepStatus : Int -> Flow Model ()
refreshViewedStepStatus stepId =
    Flow.over (stepRecordById stepId << runState << success << status) ApiData.toLoading
        |> Flow.seq (Flow.forAll currentProjectId (Flow.try viewedRevision << requestProjectStatus))


reviewStep : Model.ReviewDraft -> Flow Model ()
reviewStep draft =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.page << Route.project << mCommit << just) model
                in
                whenStepIdle draft.stepId
                    (Api.reviewStep draft mCommit_
                        |> Flow.andThen (settleReview draft.stepId (Maybe.map (\revision -> { revision = revision, reviewedBy = draft.reviewedBy, comments = draft.comments, comparison = NotAsked }) (Model.viewedRevision model)) "Step reviewed.")
                    )
            )


setReviewDraft : Model.ReviewDraft -> Flow Model ()
setReviewDraft draft =
    Flow.setAll reviewDraft (Just draft)


toggleDiffPanel : Int -> Flow Model ()
toggleDiffPanel stepId =
    Flow.over openDiff
        (\shown ->
            if Maybe.map Tuple.first shown == Just stepId then
                Nothing

            else
                Just ( stepId, 1 )
        )


removeReview : Int -> Flow Model ()
removeReview stepId =
    whenStepIdle stepId
        (Api.removeReview stepId
            |> Flow.map (Result.map (always False))
            |> Flow.andThen (settleReview stepId Nothing "Review removed.")
        )


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
    Flow.forAll (presets << success)
        (\presets_ ->
            Flow.forAll (stepConfig << success)
                (\stepConfig_ ->
                    Flow.get
                        |> Flow.andThen
                            (\model ->
                                Flow.pure (Maybe.map2 Tuple.pair (try (lens << edited << just) model) (try (lens << addMode) model))
                                    |> Flow.assertJust
                                    |> Flow.assertCondition (\( edited_, addMode_ ) -> String.trim edited_.name /= "" || addMode_ == AddFromOtherProject)
                                    |> Flow.andThen
                                        (\( edited_, addMode_ ) ->
                                            case ( edited_.id, addMode_ ) of
                                                ( Nothing, AddNew ) ->
                                                    createProject edited_ |> Flow.return ()

                                                ( Nothing, AddFromOtherProject ) ->
                                                    Flow.pure ()

                                                ( Just _, _ ) ->
                                                    saveExistingRecord lens edited_ (always (Model.repartitionProjectSteps presets_ stepConfig_ edited_)) spec
                                        )
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
                    |> Flow.assertCondition (\( edited_, addMode_ ) -> String.trim edited_.name /= "" || addMode_ == AddFromOtherProject)
                    |> Flow.andThen
                        (\( edited_, addMode_ ) ->
                            Flow.forAll currentProjectId
                                (\projectId ->
                                    case ( edited_.id, addMode_ ) of
                                        ( Nothing, AddNew ) ->
                                            createStep Nothing spec edited_ |> Flow.return ()

                                        ( Nothing, AddFromOtherProject ) ->
                                            Flow.setting (TableSpec.getLens spec << isUpdating)
                                                (batchAssignRecordsToProject (all (lens << selectExistingSteps << selected << each << recordId << just) model) projectId)
                                                |> Flow.seq (Flow.setAll (lens << selectExistingSteps << selected) [])
                                                |> Flow.seq (endRecordEdit lens)
                                                |> Flow.seq loadProjects

                                        ( Just stepId, _ ) ->
                                            let
                                                srcFilePaths =
                                                    try (lens << recordById stepId << srcFiles) model
                                                        |> Maybe.unwrap [] (Model.srcFileChangePaths [])
                                            in
                                            Flow.try (lens << recordById stepId << args)
                                                (\originalArgs ->
                                                    let
                                                        argsChanged =
                                                            originalArgs /= Just edited_.args

                                                        mergeFn r =
                                                            { edited_
                                                                | runState =
                                                                    if argsChanged then
                                                                        ApiData.loading Nothing

                                                                    else
                                                                        r.runState
                                                                , srcFiles = Model.closeDirectoryFileViews r.srcFiles
                                                                , srcFileDraft = r.srcFileDraft
                                                                , srcFileWriting = r.srcFileWriting
                                                            }

                                                        saveSrcFiles =
                                                            saveSrcFileChanges stepId srcFilePaths
                                                                |> Flow.return ()
                                                    in
                                                    saveExistingRecordWith saveSrcFiles lens edited_ mergeFn spec
                                                )
                                )
                        )
            )


endRecordEdit : A_Traversal Model (Table (BaseRecord a)) -> Flow Model ()
endRecordEdit lens =
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.over (remkT lens)
                    (\t ->
                        let
                            cleared =
                                case ( isReadOnlyRoute model, t.edited ) of
                                    ( False, Just r ) ->
                                        set (draftAt r.id) Nothing t

                                    _ ->
                                        t
                        in
                        { cleared | edited = Nothing, addMode = AddNew }
                    )
            )


saveExistingRecord : A_Traversal Model (Table (BaseRecord a)) -> BaseRecord a -> (BaseRecord a -> BaseRecord a) -> TableSpec (BaseRecord a) -> Flow Model ()
saveExistingRecord =
    saveExistingRecordWith (Flow.pure ())


saveExistingRecordWith : Flow Model () -> A_Traversal Model (Table (BaseRecord a)) -> BaseRecord a -> (BaseRecord a -> BaseRecord a) -> TableSpec (BaseRecord a) -> Flow Model ()
saveExistingRecordWith beforeRequest lens record mergeFn spec =
    let
        clearUpdating =
            Flow.forAll now
                (\posix ->
                    Flow.over (remkT lens << records << success << by .id record.id)
                        (\r -> { r | isUpdating = False, lastModifiedAt = Just posix })
                )
                |> Flow.seq refreshReviews
    in
    Flow.over (remkT lens << records << success)
        (List.updateIf (\r -> r.id == record.id)
            (\r ->
                let
                    m =
                        mergeFn r
                in
                { m | isUpdating = True }
            )
        )
        |> Flow.seq (endRecordEdit lens)
        |> Flow.seq beforeRequest
        |> Flow.seq
            (callApi void (Api.saveRecord spec record)
                |> FlowError.foldResult (always clearUpdating) (always clearUpdating)
            )


onUrlRequest : Browser.UrlRequest -> Flow Model ()
onUrlRequest urlRequest =
    case urlRequest of
        Browser.Internal url ->
            Flow.forAll route
                (\currentRoute ->
                    let
                        targetRoute =
                            Route.fromUrl url
                    in
                    Flow.forAll key
                        (\k -> Flow.lift (Nav.pushUrl k (Route.toString (inheritChat currentRoute targetRoute))))
                )

        Browser.External href ->
            Flow.lift (Nav.load href)


goToRoute : Route -> Flow Model ()
goToRoute targetRoute =
    Flow.forAll route
        (\currentRoute ->
            Flow.forAll key
                (\k -> Flow.lift (Nav.pushUrl k (Route.toString (inheritChat currentRoute targetRoute))))
        )


replaceRoute : Route -> Flow Model ()
replaceRoute targetRoute =
    Flow.forAll route
        (\currentRoute ->
            Flow.forAll key
                (\k -> Flow.lift (Nav.replaceUrl k (Route.toString (inheritChat currentRoute targetRoute))))
        )





inheritChat : Route -> Route -> Route
inheritChat currentRoute targetRoute =
    case targetRoute.chat of
        Just _ ->
            targetRoute

        Nothing ->
            { targetRoute | chat = currentRoute.chat }


resetPageScroll : Flow Model ()
resetPageScroll =
    Flow.attemptTask (Dom.setViewport 0 0)


clearStepLog : Int -> Maybe String -> Flow Model ()
clearStepLog id commit =
    Flow.over stepLogs (Dict.remove (Model.stepLogKey id commit))


autocompleteDebounceDelay : Float
autocompleteDebounceDelay =
    350


autocompleteDebounceConfig : Debounce.Config (Flow Model ())
autocompleteDebounceConfig =
    { strategy = Debounce.later autocompleteDebounceDelay
    , transform = autocompleteDebounceMsg
    }


autocompleteStateWithJob : Model.AutocompleteJob -> Maybe Model.AutocompleteState -> Model.AutocompleteState
autocompleteStateWithJob job maybeState =
    if String.isEmpty job.query then
        Model.initAutocompleteState

    else
        { query = job.query
        , suggestions = ApiData.loading (maybeState |> Maybe.andThen (.suggestions >> ApiData.toMaybe))
        , activeIndex = 0
        , activeRequest = Just job
        }


clearAutocomplete : String -> Flow Model ()
clearAutocomplete fieldKey =
    Flow.over autocomplete (Dict.insert fieldKey Model.initAutocompleteState)


fetchAutocomplete : String -> Maybe String -> Api.AutocompleteRequest -> Flow Model ()
fetchAutocomplete fieldKey commit autocompleteRequest =
    if String.isEmpty autocompleteRequest.query then
        clearAutocomplete fieldKey

    else
        Flow.get
            |> Flow.andThen
                (\model ->
                    let
                        currentState =
                            Dict.get fieldKey (Model.getAutocomplete model)

                        job : Model.AutocompleteJob
                        job =
                            { fieldKey = fieldKey
                            , commit = commit
                            , template = autocompleteRequest.template
                            , autocomplete = autocompleteRequest.autocomplete
                            , context = autocompleteRequest.context
                            , query = autocompleteRequest.query
                            , limit = autocompleteRequest.limit
                            }

                        loadingState =
                            autocompleteStateWithJob job currentState

                        ( newDebounce, debounceCmd ) =
                            Debounce.push autocompleteDebounceConfig job (Model.getAutocompleteDebounce model)
                    in
                    Flow.over autocomplete (Dict.insert fieldKey loadingState)
                        |> Flow.seq (Flow.over autocompleteDebounce (always newDebounce))
                        |> Flow.seq (Flow.lift debounceCmd |> Flow.andThen identity)
                )


autocompleteDebounceMsg : Debounce.Msg -> Flow Model ()
autocompleteDebounceMsg msg =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    send job =
                        Task.perform (\_ -> runAutocompleteJob job) (Task.succeed ())

                    ( newDebounce, debounceCmd ) =
                        Debounce.update autocompleteDebounceConfig (Debounce.takeLast send) msg (Model.getAutocompleteDebounce model)
                in
                Flow.over autocompleteDebounce (always newDebounce)
                    |> Flow.seq (Flow.lift debounceCmd |> Flow.andThen identity)
            )


autocompleteJobsMatch : Model.AutocompleteJob -> Model.AutocompleteJob -> Bool
autocompleteJobsMatch expected actual =
    expected.fieldKey
        == actual.fieldKey
        && expected.commit
        == actual.commit
        && expected.template
        == actual.template
        && expected.autocomplete
        == actual.autocomplete
        && expected.context
        == actual.context
        && expected.query
        == actual.query
        && expected.limit
        == actual.limit


autocompleteStateMatchesJob : Model.AutocompleteJob -> Model.AutocompleteState -> Bool
autocompleteStateMatchesJob job state =
    case state.activeRequest of
        Just activeRequest ->
            autocompleteJobsMatch job activeRequest

        Nothing ->
            False


runAutocompleteJob : Model.AutocompleteJob -> Flow Model ()
runAutocompleteJob job =
    Flow.get
        |> Flow.andThen
            (\model ->
                case Dict.get job.fieldKey (Model.getAutocomplete model) of
                    Just state ->
                        if autocompleteStateMatchesJob job state then
                            Api.fetchAutocomplete
                                job.commit
                                { template = job.template
                                , autocomplete = job.autocomplete
                                , context = job.context
                                , query = job.query
                                , limit = job.limit
                                }
                                |> Flow.andThen (applyAutocompleteResult job)

                        else
                            Flow.pure ()

                    Nothing ->
                        Flow.pure ()
            )


applyAutocompleteResult : Model.AutocompleteJob -> Result Http.Error (List String) -> Flow Model ()
applyAutocompleteResult job result =
    Flow.get
        |> Flow.andThen
            (\latestModel ->
                case Dict.get job.fieldKey (Model.getAutocomplete latestModel) of
                    Just state ->
                        if autocompleteStateMatchesJob job state then
                            Flow.over autocomplete
                                (Dict.insert job.fieldKey
                                    { query = job.query
                                    , suggestions = ApiData.fromResult result
                                    , activeIndex = 0
                                    , activeRequest = Just job
                                    }
                                )

                        else
                            Flow.pure ()

                    Nothing ->
                        Flow.pure ()
            )


autocompleteValueKey : String -> String -> String
autocompleteValueKey fieldKey value =
    fieldKey ++ ":value:" ++ value


autocompleteValueValidity : String -> Model -> String -> ApiData Bool
autocompleteValueValidity fieldKey model value =
    try (autocomplete << Accessors.key (autocompleteValueKey fieldKey value) << just << suggestions) model
        |> Maybe.withDefault NotAsked
        |> ApiData.map (List.member value)


checkAutocompleteValue : String -> Maybe String -> Api.AutocompleteRequest -> Flow Model ()
checkAutocompleteValue fieldKey commit request =
    fetchAutocomplete (autocompleteValueKey fieldKey request.query) commit request


loadNotices : Int -> Flow Model ()
loadNotices id =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.page << Route.project << mCommit << just) model

                    key =
                        Model.stepLogKey id mCommit_
                in
                Flow.over notices (Dict.insert key (ApiData.loading Nothing))
                    |> Flow.seq
                        (Api.fetchNotices id mCommit_
                            |> Flow.andThen
                                (\result ->
                                    Flow.over notices (Dict.insert key (ApiData.fromResult result))
                                        |> Flow.seq
                                            (case result of
                                                Ok _ ->
                                                    Flow.pure ()

                                                Err error ->
                                                    addToast False (Http.errorMessage error)
                                            )
                                )
                        )
            )


loadStepLog : Int -> Flow Model ()
loadStepLog id =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    revision =
                        stepRevisionById id model

                    key =
                        Model.stepLogKey id revision
                in
                Flow.over stepLogs (Dict.insert key (ApiData.loading Nothing))
                    |> Flow.seq
                        (Api.fetchStepLog id revision
                            |> Flow.andThen
                                (\result ->
                                    Flow.over stepLogs (Dict.insert key (ApiData.fromResult result))
                                        |> Flow.seq
                                            (case result of
                                                Ok _ ->
                                                    Flow.pure ()

                                                Err error ->
                                                    addToast False (Http.errorMessage error)
                                            )
                                )
                        )
            )


runStep : StepSpec -> Int -> Flow Model ()
runStep spec id =
    let
        table =
            TableSpec.getLens spec

        setStatus =
            setLocalStepStatus table id
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    revision =
                        stepRevisionById id model
                in
                Flow.when (model |> has (table << edited << just << recordId << just << where_ ((==) id))) (TableSpec.getUpsertRecord spec)
                    |> Flow.seq (Flow.async (toggleSrcEntry id (Just False) []))
                    |> Flow.seq (Flow.async (toggleOutputEntry id (Just False) []))
                    |> Flow.seq (clearStepLog id revision)
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
                    |> Flow.seq (callApi void (Api.runStep id revision))
            )
        |> FlowError.foldResult
            (\_ -> Flow.pure ())
            (\_ -> setStatus (Success (StatusFailure Nothing)))


buildViewedRevision : StepSpec -> Int -> Flow Model ()
buildViewedRevision spec id =
    Flow.get
        |> Flow.andThen
            (\model ->
                Flow.fromMaybe (Model.viewedRevision model)
                    (\revision ->
                        Flow.when (model |> has (TableSpec.getLens spec << edited << just << recordId << just << where_ ((==) id))) (TableSpec.getUpsertRecord spec)
                            |> Flow.seq (Flow.over pendingBuilds (Dict.insert id revision))
                            |> Flow.seq (clearStepLog id (Just revision))
                            |> Flow.seq (callApi void (Api.runStep id (Just revision)))
                    )
            )
        |> FlowError.foldResult
            (\_ -> Flow.pure ())
            (\error ->
                Flow.over pendingBuilds (Dict.remove id)
                    |> Flow.seq (addToast False (Http.errorMessage error))
            )


stopStep : StepSpec -> Int -> Flow Model ()
stopStep spec id =
    Flow.get
        |> Flow.andThen
            (\model ->
                setLocalStepStatus (TableSpec.getLens spec) id (Success StatusRunning)
                    |> Flow.seq (callApi void (Api.stopStep id (stepRevisionById id model)))
            )
        |> FlowError.foldResult
            (\_ -> Flow.pure ())
            (\_ -> Flow.pure ())


setAddMode : A_Traversal s (Table (BaseRecord a)) -> BaseRecord a -> AddMode -> Flow s ()
setAddMode lens defaultRecord mode =
    Flow.over lens (\t -> { t | addMode = mode, edited = Just defaultRecord })


addStepWithArg : StepSpec -> String -> StepArgValue -> Flow Model ()
addStepWithArg spec argName value =
    let
        editedDraft =
            TableSpec.getLens spec << where_ (not << .nameEditOnly) << edited << just
    in
    Flow.unlessHas (editedDraft << where_ (.id >> Maybe.isNothing))
        (toggleAddOrEditRecordForm spec Nothing)
        |> Flow.seq (Flow.setAll (editedDraft << args << Accessors.key argName) (Just value))


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
            createStep record.id spec (set name (generateUniqueCloneName record.name existingNames) record)
                |> FlowError.andThen
                    (\newRecord ->
                        Flow.assertJust (Flow.pure newRecord.id)
                            |> Flow.andThen (\_ -> loadProjects)
                    )
        )
        |> Flow.return ()


shareEntity : Int -> Int -> Route.HighlightTarget -> List String -> Maybe Route.LineRange -> Flow Model ()
shareEntity projectId entityId target pathSegments mRange =
    Flow.try (orElseT (route << Route.page << Route.project << mCommit << just) (commitHash << success))
        (\mCommit_ ->
            Flow.forAll origin
                (\origin_ ->
                    let
                        route_ =
                            Route.fromPage
                                (Route.Project
                                    { projectId = projectId
                                    , mHighlight = Just { id = entityId, target = target, path = pathSegments, range = mRange }
                                    , mCommit = mCommit_
                                    , mCompare = Nothing
                                    }
                                )
                    in
                    callJs "copyToClipboard" Encode.string (Decode.succeed ()) (origin_ ++ Route.toString route_)
                )
        )
        |> Flow.seq (addToast True "Share link copied to clipboard")


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


ignoreResult : FlowError Http.Error Model a -> Flow Model ()
ignoreResult =
    FlowError.foldResult (\_ -> Flow.pure ()) (\_ -> Flow.pure ())


downloadFile : Int -> String -> List String -> Flow Model ()
downloadFile stepId commit filePath =
    Flow.lift (Nav.load (Api.stepFileDownloadUrl stepId (Just commit) filePath))


downloadSrcFile : Int -> Maybe String -> List String -> Flow Model ()
downloadSrcFile id revision filePath =
    Flow.lift (Nav.load (Api.srcFileDownloadUrl id revision filePath))


startCompare : CompareSelection -> Flow Model ()
startCompare sel =
    Flow.setAll compareState (CompareSelecting sel)


cancelCompare : Flow Model ()
cancelCompare =
    Flow.setAll compareState CompareIdle
        |> Flow.seq clearCompareRoute


clearCompareRoute : Flow Model ()
clearCompareRoute =
    overRouteReplace <|
        \currentRoute ->
            case currentRoute.page of
                Route.Project params ->
                    { currentRoute | page = Route.Project { params | mCompare = Nothing } }

                _ ->
                    currentRoute


selectCompareFile : CompareSelection -> Flow Model ()
selectCompareFile right =
    Flow.forAll (compareState << compareSelecting) <|
        \left ->
            Flow.forAll route <|
                \currentRoute ->
                    let
                        comparison =
                            { left = compareSelectionToTarget left
                            , right = compareSelectionToTarget right
                            }

                        nextRoute =
                            case currentRoute.page of
                                Route.Project params ->
                                    { currentRoute | page = Route.Project { params | projectId = left.projectId, mCompare = Just comparison } }

                                _ ->
                                    Route.fromPage
                                        (Route.Project
                                            { projectId = left.projectId
                                            , mHighlight = Nothing
                                            , mCommit = Nothing
                                            , mCompare = Just comparison
                                            }
                                        )
                    in
                    goToRoute nextRoute


syncCompareFromRoute : Route -> Flow Model ()
syncCompareFromRoute route_ =
    case route_.page of
        Route.Project { projectId, mCompare } ->
            case Maybe.andThen (compareSelectionsFromRoute projectId) mCompare of
                Just ( left, right ) ->
                    activateCompare left right

                Nothing ->
                    clearActiveCompareIfNeeded

        _ ->
            clearActiveCompareIfNeeded


compareSelectionsFromRoute : Int -> Route.Comparison -> Maybe ( CompareSelection, CompareSelection )
compareSelectionsFromRoute projectId comparison =
    Maybe.map2 Tuple.pair
        (compareSelectionFromTarget projectId comparison.left)
        (compareSelectionFromTarget projectId comparison.right)


compareSelectionFromTarget : Int -> Route.CompareTarget -> Maybe CompareSelection
compareSelectionFromTarget projectId target =
    let
        base =
            { projectId = projectId
            , recordId = target.id
            , path = target.path
            , fileName = List.last target.path |> Maybe.withDefault ""
            , mimeType = target.mimeType
            , source = FromSrc target.commit
            }
    in
    case target.target of
        Route.Output ->
            target.commit
                |> Maybe.map (\commit_ -> { base | source = FromOutput commit_ })

        Route.Source ->
            Just base


compareSelectionToTarget : CompareSelection -> Route.CompareTarget
compareSelectionToTarget sel =
    case sel.source of
        FromOutput commit_ ->
            { id = sel.recordId
            , target = Route.Output
            , path = sel.path
            , commit = Just commit_
            , mimeType = sel.mimeType
            }

        FromSrc commit_ ->
            { id = sel.recordId
            , target = Route.Source
            , path = sel.path
            , commit = commit_
            , mimeType = sel.mimeType
            }


clearActiveCompareIfNeeded : Flow Model ()
clearActiveCompareIfNeeded =
    Flow.get
        |> Flow.andThen
            (\model ->
                if has (compareState << compareActive) model then
                    Flow.setAll compareState CompareIdle
                        |> Flow.seq (closeDialog "compare-dialog")

                else
                    Flow.pure ()
            )


activateCompare : CompareSelection -> CompareSelection -> Flow Model ()
activateCompare left right =
    let
        matchesActiveCompare d =
            d.left == left && d.right == right
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                if has (compareState << compareActive << where_ matchesActiveCompare) model then
                    Flow.pure ()

                else
                    Flow.setAll compareState
                        (CompareActive
                            { left = left
                            , right = right
                            , leftContent = NotAsked
                            , rightContent = NotAsked
                            , leftInspect = False
                            , rightInspect = False
                            }
                        )
                        |> Flow.seq (openDialog "compare-dialog")
                        |> Flow.seq (fetchCompareSide matchesActiveCompare compareLeftContent left)
                        |> Flow.seq (fetchCompareSide matchesActiveCompare compareRightContent right)
            )


fetchCompareSide : (CompareActiveData -> Bool) -> An_Optic pr ls CompareActiveData (ApiData CompareFile) -> CompareSelection -> Flow Model ()
fetchCompareSide matchesActiveCompare contentLens sel =
    case Model.compareSelectionMode sel of
        CompareText ->
            callApi (compareState << compareActive << where_ matchesActiveCompare << remkT contentLens)
                (fetchCompareContent sel
                    |> Flow.map (Result.map (\s -> CompareFile s (Model.delimitedGridFromFile sel.path sel.mimeType s Nothing)))
                )
                |> Flow.return ()

        _ ->
            Flow.pure ()


fetchCompareContent : CompareSelection -> FlowError Http.Error Model String
fetchCompareContent sel =
    case sel.source of
        FromOutput commit_ ->
            Api.fetchFileContents sel.recordId (Just commit_) sel.path

        FromSrc commit_ ->
            Api.fetchSrcFileContents sel.recordId commit_ sel.path


shouldSkipFileContents : { r | mimeType : Maybe String } -> Bool
shouldSkipFileContents file_ =
    has (mimeType << just << where_ (String.startsWith "image/")) file_
        || has (mimeType << just << where_ (String.startsWith "text/html")) file_
        || has (mimeType << just << where_ ((==) "chemical/x-pdb")) file_


decodeTableMeta : Decode.Value -> Maybe Model.TableMeta
decodeTableMeta jsonValue =
    let
        colType =
            Decode.string
                |> Decode.map
                    (\t ->
                        case t of
                            "int" ->
                                Grid.Int

                            "float" ->
                                Grid.Float

                            _ ->
                                Grid.Text
                    )

        decoder =
            Decode.map Model.TableMeta
                (Decode.field "columns"
                    (Decode.list
                        (Decode.map2 Model.ColumnMeta
                            (Decode.field "type" colType)
                            (Decode.field "nullable" Decode.bool)
                        )
                    )
                )
    in
    Decode.decodeValue decoder jsonValue |> Result.toMaybe


toggleFile : Int -> List String -> Flow Model ()
toggleFile recordId path =
    toggleOutputEntry recordId Nothing path
        |> Flow.andThen
            (\isOpen ->
                Flow.when (not isOpen) (clearHighlightedFileOnClose Route.Output recordId path)
            )


toggleSrcFile : Int -> List String -> Flow Model ()
toggleSrcFile recordId path =
    toggleSrcEntry recordId Nothing path
        |> Flow.andThen
            (\isOpen ->
                Flow.when (not isOpen) (clearHighlightedFileOnClose Route.Source recordId path)
            )


wrapDelimitedGridFlow : Int -> List String -> Flow Grid.State () -> Flow Model ()
wrapDelimitedGridFlow recordId path =
    Flow.via (currentProject << success << tables << values << fileDelimitedGridAt recordId path << just << gridState)


setPlainFileScrollTop : Route.HighlightTarget -> Int -> List String -> Float -> Flow Model ()
setPlainFileScrollTop target recordId path scrollTop =
    let
        allStepTables =
            currentProject << success << tables << values
    in
    Flow.setAll (allStepTables << plainScrollTopAt target recordId path) scrollTop


plainLineScrollTop : Int -> Float
plainLineScrollTop line =
    toFloat (max 0 ((line - 1) * Model.plainLineHeight))


scrollPlainFileToLine : Route.HighlightTarget -> Int -> List String -> Int -> Flow Model ()
scrollPlainFileToLine target recordId path line =
    let
        scrollTop =
            plainLineScrollTop line
    in
    setPlainFileScrollTop target recordId path scrollTop
        |> Flow.seq (Flow.attemptTask (Dom.setViewportOf ("viewer-" ++ Route.highlightAnchor target recordId path) 0 scrollTop))


seekChunkSizeInBytes : Int
seekChunkSizeInBytes =
    2 * 1024 * 1024


applySeekChunk : Route.HighlightTarget -> Int -> List String -> Model.FileChunk -> Flow Model ()
applySeekChunk target recordId path chunk =
    let
        allStepTables =
            currentProject << success << tables << values

        apiDataTraversal =
            allStepTables << seekWindowAt target recordId path

        viewTraversal =
            allStepTables << plainScrollTopAt target recordId path
    in
    Flow.forAll apiDataTraversal
        (\currentApiData ->
            let
                oldWindow =
                    ApiData.toMaybe currentApiData |> Maybe.withDefault Model.emptySeekWindow

                merged =
                    Model.insertChunk oldWindow chunk

                deltaLines =
                    Model.windowStartLine oldWindow - Model.windowStartLine merged
            in
            Flow.setAll apiDataTraversal (ApiData.Success merged)
                |> Flow.seq
                    (if deltaLines /= 0 then
                        Flow.forAll viewTraversal
                            (\oldScrollTop ->
                                let
                                    adjustment =
                                        toFloat (deltaLines * Model.plainLineHeight)

                                    newScrollTop =
                                        max 0 (oldScrollTop + adjustment)
                                in
                                setPlainFileScrollTop target recordId path newScrollTop
                                    |> Flow.seq (Flow.attemptTask (Dom.setViewportOf ("viewer-" ++ Route.highlightAnchor target recordId path) 0 newScrollTop))
                            )

                     else
                        Flow.pure ()
                    )
        )


seekAndMerge : Route.HighlightTarget -> Int -> List String -> Api.SeekAnchor -> Int -> Flow Model ()
seekAndMerge target recordId path anchor bytes_ =
    let
        allStepTables =
            currentProject << success << tables << values

        apiDataTraversal =
            allStepTables << seekWindowAt target recordId path

        apiCall =
            case target of
                Route.Output ->
                    Flow.forAll (stepShownRevision recordId)
                        (\commit_ ->
                            Api.fetchFileSeek recordId (Just commit_) path anchor bytes_
                        )

                Route.Source ->
                    Flow.forAll (stepShownRevision recordId)
                        (\commit_ ->
                            Api.fetchSrcFileSeek recordId (Just commit_) path anchor bytes_
                        )
    in
    Flow.forAll apiDataTraversal
        (\current ->
            apiCall
                |> FlowError.foldResult
                    (applySeekChunk target recordId path)
                    (\e ->
                        Flow.setAll apiDataTraversal
                            (ApiData.toMaybe current
                                |> Maybe.unwrap (ApiData.Error e) (\w -> ApiData.Success { w | loading = Nothing })
                            )
                            |> Flow.seq (addToast False (Http.errorMessage e))
                    )
        )


scrollPlainFileToHighlightedRange : Route.HighlightTarget -> Int -> List String -> Flow Model ()
scrollPlainFileToHighlightedRange target recordId path =
    Flow.forAll route
        (\route_ ->
            try (Route.page << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) route_
                |> Maybe.andThen .range
                |> Maybe.unwrap (Flow.pure ()) (.from >> scrollPlainFileToLine target recordId path)
        )


setPlainFileLineCount : Route.HighlightTarget -> Int -> List String -> String -> Flow Model ()
setPlainFileLineCount target recordId path content =
    let
        allStepTables =
            currentProject << success << tables << values
    in
    Flow.setAll (allStepTables << plainLineCountAt target recordId path) (Model.countLines content)
        |> Flow.seq (scrollPlainFileToHighlightedRange target recordId path)


highlightStartLine : Route.HighlightTarget -> Int -> List String -> Route -> Int
highlightStartLine target recordId path route_ =
    try (Route.page << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) route_
        |> Maybe.andThen .range
        |> Maybe.map .from
        |> Maybe.withDefault 1


seekTargetPaddingInLines : Int
seekTargetPaddingInLines =
    10


requestSeekWindow : Route.HighlightTarget -> Int -> List String -> Api.SeekAnchor -> Flow Model ()
requestSeekWindow target recordId path anchor =
    let
        allStepTables =
            currentProject << success << tables << values

        ( paddedAnchor, targetScrollTop ) =
            case anchor of
                Api.AtLine line ->
                    let
                        paddedLine =
                            max 1 (line - seekTargetPaddingInLines)
                    in
                    ( Api.AtLine paddedLine, plainLineScrollTop (line - paddedLine + 1) )

                Api.AtOffset _ ->
                    ( anchor, 0 )
    in
    Flow.setAll (allStepTables << seekWindowAt target recordId path) (ApiData.Loading Nothing)
        |> Flow.seq (seekAndMerge target recordId path paddedAnchor seekChunkSizeInBytes)
        |> Flow.seq (setPlainFileScrollTop target recordId path targetScrollTop)
        |> Flow.seq (Flow.attemptTask (Dom.setViewportOf ("viewer-" ++ Route.highlightAnchor target recordId path) 0 targetScrollTop))
        |> Flow.seq (prefetchAdjacentChunk target recordId path Model.Before)
        |> Flow.seq (prefetchAdjacentChunk target recordId path Model.After)


scrollSeekableFileToLine : Route.HighlightTarget -> Int -> List String -> Int -> Flow Model ()
scrollSeekableFileToLine target recordId path line =
    let
        allStepTables =
            currentProject << success << tables << values

        scroll window_ =
            let
                startLine =
                    Model.windowStartLine window_

                localLine =
                    max 1 (line - startLine + 1)

                scrollTop =
                    plainLineScrollTop localLine
            in
            setPlainFileScrollTop target recordId path scrollTop
                |> Flow.seq (Flow.attemptTask (Dom.setViewportOf ("viewer-" ++ Route.highlightAnchor target recordId path) 0 scrollTop))
    in
    Flow.forAll (allStepTables << seekWindowAt target recordId path << success) scroll


requestAdjacentChunk : Route.HighlightTarget -> Int -> List String -> Model.SeekDirection -> SeekWindow -> Flow Model ()
requestAdjacentChunk target recordId path direction window_ =
    let
        allStepTables =
            currentProject << success << tables << values

        request byteOffset bytes_ =
            Flow.setAll (allStepTables << seekWindowAt target recordId path)
                (ApiData.Success { window_ | loading = Just direction })
                |> Flow.seq (seekAndMerge target recordId path (Api.AtOffset byteOffset) bytes_)
    in
    if Maybe.isJust window_.loading then
        Flow.pure ()

    else
        case direction of
            Model.Before ->
                Model.windowStartOffset window_
                    |> Maybe.filter (\byteOffset -> byteOffset > 0)
                    |> Maybe.unwrap (Flow.pure ()) (\byteOffset -> request byteOffset -seekChunkSizeInBytes)

            Model.After ->
                if Model.windowEof window_ then
                    Flow.pure ()

                else
                    Model.windowEndOffset window_
                        |> Maybe.unwrap (Flow.pure ()) (\byteOffset -> request byteOffset seekChunkSizeInBytes)


prefetchAdjacentChunk : Route.HighlightTarget -> Int -> List String -> Model.SeekDirection -> Flow Model ()
prefetchAdjacentChunk target recordId path direction =
    Flow.forAll
        (currentProject << success << tables << values << seekWindowAt target recordId path << success)
        (requestAdjacentChunk target recordId path direction)


seekPrefetchDistanceInLines : Int
seekPrefetchDistanceInLines =
    20


onSeekScroll : Route.HighlightTarget -> Int -> List String -> Model.ScrollMetrics -> Flow Model ()
onSeekScroll target recordId path metrics =
    let
        nearBottom =
            metrics.scrollTop + metrics.clientHeight >= metrics.scrollHeight - toFloat (seekPrefetchDistanceInLines * Model.plainLineHeight)

        nearTop =
            metrics.scrollTop <= toFloat (seekPrefetchDistanceInLines * Model.plainLineHeight)

        extendWindow window_ =
            if nearBottom && not (Model.windowEof window_) then
                requestAdjacentChunk target recordId path Model.After window_

            else if nearTop then
                requestAdjacentChunk target recordId path Model.Before window_

            else
                Flow.pure ()
    in
    setPlainFileScrollTop target recordId path metrics.scrollTop
        |> Flow.seq (Flow.forAll (currentProject << success << tables << values << seekWindowAt target recordId path << success) extendWindow)


openHighlightedGridInPlainMode : Route.HighlightTarget -> Int -> List String -> Route -> Model.DelimitedGrid -> Model.DelimitedGrid
openHighlightedGridInPlainMode target recordId path route_ delimitedGrid =
    try (Route.page << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) route_
        |> Maybe.andThen .range
        |> Maybe.unwrap delimitedGrid (\_ -> { delimitedGrid | grid = Grid.showPlain delimitedGrid.grid })


zoomIframeBy : A_Traversal Model Float -> String -> Float -> Flow Model ()
zoomIframeBy zoomLens iframeId factor =
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

        stepCommit =
            stepShownRevision recordId

        extrasLensFor p =
            if List.isEmpty p then
                allStepTables << rootExtrasAt recordId

            else
                allStepTables << extrasAt recordId p

        folderAction =
            Flow.forAll stepCommit <|
                \commit_ ->
                    Flow.forAll (allStepTables << directoryItemAtPath recordId path << folder)
                        (\_ ->
                            callApi (allStepTables << childrenAt recordId path)
                                (Api.fetchDirectoryContents ApiDecode.directoryItemGeneric recordId (Just commit_) path)
                                |> Flow.seq
                                    (callApi (extrasLensFor path)
                                        (Api.fetchExtras recordId (Just commit_) path)
                                    )
                                |> Flow.return ()
                        )

        fileAction =
            Flow.forAll stepCommit <|
                \commit_ ->
                    Flow.forAll (allStepTables << directoryItemAtPath recordId path << file)
                        (\file_ ->
                            let
                                parentPath =
                                    List.take (List.length path - 1) path

                                parentExtrasLens =
                                    extrasLensFor parentPath

                                materializeFileContent content =
                                    setPlainFileLineCount Route.Output recordId path content
                                        |> Flow.seq
                                            (Flow.forAll parentExtrasLens
                                                (\extrasData ->
                                                    let
                                                        mTableMeta =
                                                            ApiData.toMaybe extrasData
                                                                |> Maybe.andThen
                                                                    (\extrasDict ->
                                                                        List.last path
                                                                            |> Maybe.andThen (\fileName -> Dict.get fileName extrasDict)
                                                                            |> Maybe.andThen decodeTableMeta
                                                                    )

                                                        mGrid =
                                                            Model.delimitedGridFromFile path file_.mimeType content mTableMeta
                                                    in
                                                    Flow.forAll route
                                                        (\route_ ->
                                                            Flow.setAll
                                                                (allStepTables << fileDelimitedGridAt recordId path)
                                                                (Maybe.map (openHighlightedGridInPlainMode Route.Output recordId path route_) mGrid)
                                                        )
                                                )
                                            )
                            in
                            if file_.seekable then
                                Flow.forAll route <|
                                    \route_ ->
                                        let
                                            initialLine =
                                                highlightStartLine Route.Output recordId path route_
                                        in
                                        requestSeekWindow Route.Output recordId path (Api.AtLine initialLine)

                            else
                                let
                                    ensureExtras =
                                        Flow.forAll parentExtrasLens
                                            (\extras ->
                                                case extras of
                                                    ApiData.Success _ ->
                                                        Flow.pure (Ok ())

                                                    ApiData.Error _ ->
                                                        Flow.pure (Ok ())

                                                    _ ->
                                                        callApi parentExtrasLens
                                                            (Api.fetchExtras recordId (Just commit_) parentPath)
                                                            |> Flow.map (Result.map (always ()))
                                            )
                                in
                                Flow.when (file_.viewable && not (shouldSkipFileContents file_))
                                    (ensureExtras
                                        |> Flow.andThen
                                            (\_ ->
                                                callApi (allStepTables << fileContentAt recordId path)
                                                    (Api.fetchFileContents recordId (Just commit_) path)
                                            )
                                        |> FlowError.andThen materializeFileContent
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
            Flow.forAll (stepShownRevision recordId)
                (\revision ->
                    callApi (allStepTables << srcFilesChildrenAt recordId path)
                        (Api.fetchSrcDirectoryContents ApiDecode.directoryItemGeneric recordId (Just revision) path)
                        |> Flow.return ()
                )

        fetchContent =
            Flow.forAll (stepShownRevision recordId)
                (\revision ->
                    callApi (allStepTables << srcFilesFileContentAt recordId path)
                        (Api.fetchSrcFileContents recordId (Just revision) path)
                        |> FlowError.andThen (setPlainFileLineCount Route.Source recordId path)
                        |> Flow.return ()
                )

        fileAction =
            Flow.forAll (allStepTables << srcFilesItemAtPath recordId path << file)
                (\file_ ->
                    if file_.seekable then
                        Flow.forAll route <|
                            \route_ ->
                                let
                                    initialLine =
                                        highlightStartLine Route.Source recordId path route_
                                in
                                requestSeekWindow Route.Source recordId path (Api.AtLine initialLine)

                    else
                        Flow.when (file_.viewable && not (shouldSkipFileContents file_))
                            (case file_.content of
                                NotAsked ->
                                    fetchContent

                                Error _ ->
                                    fetchContent

                                _ ->
                                    Flow.pure ()
                            )
                )
    in
    Flow.forAll isExpanded
        (\wasExpanded ->
            let
                newlyExpanded =
                    Maybe.withDefault (not wasExpanded) mOpen
            in
            Flow.ifHas (allStepTables << recordById recordId << where_ (\r -> mOpen == Nothing && r.srcFileWriting))
                (\_ -> Flow.pure wasExpanded)
                (Flow.setAll (allStepTables << srcFilesChildrenAt recordId path) NotAsked
                    |> Flow.seq (Flow.setAll isExpanded newlyExpanded)
                    |> Flow.seq (Flow.when newlyExpanded <| Flow.batchM [ folderAction, fileAction ])
                    |> Flow.return newlyExpanded
                )
        )


updateSrcFileContent : Int -> List String -> String -> Flow Model ()
updateSrcFileContent recordId path content =
    let
        allStepTables =
            currentProject << success << tables << values
    in
    Flow.forAll (allStepTables << srcFilesFileContentAt recordId path << success) <|
        \savedContent ->
            Flow.setAll (allStepTables << srcFilesFileEditedContentAt recordId path)
                (if content == savedContent then
                    Nothing

                 else
                    Just content
                )


saveSrcFileChange : Int -> List String -> Flow Model ()
saveSrcFileChange recordId path =
    let
        allStepTables =
            currentProject << success << tables << values

        fileTraversal =
            allStepTables << srcFilesItemAtPath recordId path << file

        saveContent isNew content =
            predictSrcFileChange recordId
                path
                (Flow.over fileTraversal
                    (\file_ ->
                        { file_
                            | content = Success content
                            , size = String.length content
                            , plainLineCount = Model.countLines content
                            , editedContent = Nothing
                            , isNew = False
                        }
                    )
                )
                (if isNew then
                    Api.createSrcFile recordId path content

                 else
                    Api.saveSrcFile recordId path content
                )
    in
    Flow.forAll fileTraversal <|
        \file_ ->
            if file_.isDeleted then
                predictSrcFileChange recordId
                    path
                    (setSrcFileEntry recordId path Nothing)
                    (Api.deleteSrcFile recordId path)

            else
                let
                    contentToSave =
                        if file_.isNew then
                            Maybe.orElse file_.editedContent (ApiData.toMaybe file_.content)

                        else
                            file_.editedContent
                in
                Maybe.unwrap (Flow.pure ()) (saveContent file_.isNew) contentToSave


saveSrcFileChanges : Int -> List (List String) -> Flow Model Bool
saveSrcFileChanges recordId paths =
    case paths of
        [] ->
            Flow.pure True

        path :: remaining ->
            let
                allStepTables =
                    currentProject << success << tables << values

                unsavedFile =
                    allStepTables
                        << srcFilesItemAtPath recordId path
                        << file
                        << where_ Model.hasFileChanges
            in
            Flow.ifHas (allStepTables << recordById recordId << where_ .srcFileWriting)
                (\_ -> Flow.pure False)
                (saveSrcFileChange recordId path
                    |> Flow.seq
                        (Flow.ifHas unsavedFile
                            (\_ -> Flow.pure False)
                            (saveSrcFileChanges recordId remaining)
                        )
                )


discardSrcFileChanges : Int -> Flow Model ()
discardSrcFileChanges recordId =
    let
        stepRecord =
            currentProject << success << tables << values << recordById recordId
    in
    Flow.over (stepRecord << srcFiles) Model.discardDirectoryFileChanges
        |> Flow.seq (Flow.setAll (stepRecord << srcFileDraft) Nothing)


setSrcFileDraft : Int -> Maybe Model.SrcFileDraft -> Flow Model ()
setSrcFileDraft recordId draft =
    Flow.setAll (currentProject << success << tables << values << recordById recordId << srcFileDraft) draft


openSrcFileDraft : Int -> Flow Model ()
openSrcFileDraft recordId =
    setSrcFileDraft recordId (Just { name = "", content = "" })
        |> Flow.seq (Flow.attemptTask (Dom.focus "src-file-name-input"))


predictSrcFileChange : Int -> List String -> Flow Model () -> FlowError Http.Error Model a -> Flow Model ()
predictSrcFileChange recordId path prediction apiCall =
    let
        allStepTables =
            currentProject << success << tables << values

        stepRecord =
            allStepTables << recordById recordId

        dirPath =
            Maybe.withDefault [] (List.init path)
    in
    Flow.forAll (stepRecord << where_ (not << .srcFileWriting)) <|
        \snapshot ->
            Flow.bracket_
                (Flow.setAll (stepRecord << srcFileWriting) True)
                (Flow.setAll (stepRecord << srcFileWriting) False)
                (prediction
                    |> Flow.seq (callApi void apiCall)
                    |> FlowError.andThen
                        (\_ ->
                            Flow.forAll (stepShownRevision recordId)
                                (\revision ->
                                    callApiMerge Model.updateDirectoryChildren
                                        (allStepTables << srcFilesChildrenAt recordId dirPath)
                                        (Api.fetchSrcDirectoryContents ApiDecode.directoryItemGeneric recordId (Just revision) dirPath)
                                        |> Flow.seq refreshReviews
                                )
                        )
                    |> FlowError.foldResult (\_ -> Flow.pure ())
                        (\_ ->
                            Flow.setAll (stepRecord << srcFiles) snapshot.srcFiles
                                |> Flow.seq (Flow.setAll (stepRecord << srcFileDraft) snapshot.srcFileDraft)
                        )
                )


setSrcFileEntry : Int -> List String -> Maybe Model.DirectoryItem -> Flow Model ()
setSrcFileEntry recordId path entry =
    Flow.fromMaybe (List.unconsLast path) <|
        \( name, dirPath ) ->
            Flow.over
                (currentProject << success << tables << values << srcFilesChildrenAt recordId dirPath << success)
                (Dict.update name (always entry))


stageSrcFile : Int -> String -> String -> Flow Model ()
stageSrcFile recordId rawName content =
    case String.trim rawName of
        "" ->
            Flow.none

        fileName ->
            let
                path =
                    [ fileName ]

                predictedSrcFile =
                    Model.File
                        { content = Success content
                        , size = String.length content
                        , viewable = True
                        , seekable = False
                        , seekWindow = NotAsked
                        , mimeType = Nothing
                        , view = { isViewing = False, zoom = 1.0, plainScrollTop = 0 }
                        , delimitedGrid = Nothing
                        , plainLineCount = Model.countLines content
                        , editedContent = Nothing
                        , isNew = True
                        , isDeleted = False
                        }

                stagedItem =
                    currentProject << success << tables << values << srcFilesItemAtPath recordId path

                rootChildren =
                    currentProject << success << tables << values << srcFilesChildrenAt recordId [] << success
            in
            Flow.ifHas rootChildren
                (\_ ->
                    Flow.ifHas stagedItem
                        (\_ -> Flow.pure ())
                        (setSrcFileEntry recordId path (Just predictedSrcFile)
                            |> Flow.seq (setSrcFileDraft recordId Nothing)
                        )
                )
                (Flow.pure ())


stageSrcFileDeletion : Int -> List String -> Flow Model ()
stageSrcFileDeletion recordId path =
    let
        allStepTables =
            currentProject << success << tables << values

        fileTraversal =
            allStepTables << srcFilesItemAtPath recordId path << file
    in
    Flow.forAll fileTraversal <|
        \file_ ->
            if file_.isNew then
                setSrcFileEntry recordId path Nothing

            else
                Flow.over fileTraversal (\current -> { current | isDeleted = True, view = Model.closeFileView current.view })


restoreSrcFile : Int -> List String -> Flow Model ()
restoreSrcFile recordId path =
    Flow.over
        (currentProject << success << tables << values << srcFilesItemAtPath recordId path << file)
        (\file_ -> { file_ | isDeleted = False })


registerStepStatusHook : Int -> Flow Model () -> Flow Model ()
registerStepStatusHook stepId hook =
    Flow.over (stepStatusHooks << keyI stepId)
        (Just << Flow.seq hook << Maybe.withDefault (Flow.pure ()))


runAndClearStepStatusHook : Int -> Flow Model ()
runAndClearStepStatusHook stepId =
    Flow.forAll (stepStatusHooks << keyI stepId << just)
        ((|>) (Flow.setAll (stepStatusHooks << keyI stepId) Nothing) << Flow.seq)


openHighlightedEntry : Route.Highlight -> Flow Model ()
openHighlightedEntry highlight =
    case highlight.target of
        Route.Output ->
            deepOpenOutputEntryOrDefer highlight.id highlight.path highlight.range

        Route.Source ->
            deepOpenSourceEntry highlight.id highlight.path highlight.range


deepOpenOutputEntryOrDefer : Int -> List String -> Maybe Route.LineRange -> Flow Model ()
deepOpenOutputEntryOrDefer id path mRange =
    Flow.try
        (projects << records << success << each << tables << values << records << success << by .id (Just id) << runState << success << status << success << where_ ((==) StatusSuccess))
        (\mStatus ->
            case mStatus of
                Just _ ->
                    deepOpenOutputEntry id path mRange

                Nothing ->
                    registerStepStatusHook id (deepOpenOutputEntry id path mRange)
                        |> Flow.seq (Flow.attemptTask (Scroll.scrollY (String.fromInt id) 0 0))
        )


deepOpenOutputEntry : Int -> List String -> Maybe Route.LineRange -> Flow Model ()
deepOpenOutputEntry stepId path mRange =
    Flow.forAll (currentProject << success << tables << values << recordById stepId << runState << success << status << success << where_ ((==) StatusSuccess))
        (\_ ->
            deepOpenEntryWith Route.Output toggleOutputEntry stepId path mRange
        )


deepOpenSourceEntry : Int -> List String -> Maybe Route.LineRange -> Flow Model ()
deepOpenSourceEntry stepId path mRange =
    deepOpenEntryWith Route.Source toggleSrcEntry stepId path mRange


deepOpenEntryWith : Route.HighlightTarget -> (Int -> Maybe Bool -> List String -> Flow Model Bool) -> Int -> List String -> Maybe Route.LineRange -> Flow Model ()
deepOpenEntryWith target toggleEntry stepId path mRange =
    let
        scrollToRange =
            case mRange of
                Just range ->
                    let
                        allStepTables =
                            currentProject << success << tables << values
                    in
                    Flow.forAll (allStepTables << directoryItemForTargetAt target stepId path << file)
                        (\file_ ->
                            if file_.seekable then
                                scrollSeekableFileToLine target stepId path range.from

                            else
                                scrollPlainFileToLine target stepId path range.from
                        )

                Nothing ->
                    Flow.pure ()
    in
    List.prefixes path
        |> List.map
            (\pathPart ->
                toggleEntry stepId (Just True) pathPart
                    |> Flow.seq (Flow.attemptTask (Scroll.scrollY (Route.highlightAnchor target stepId pathPart) 0 0))
            )
        |> List.foldl Flow.seq (Flow.attemptTask (Scroll.scrollY (String.fromInt stepId) 0 0))
        |> Flow.seq scrollToRange


startGutterDrag : Route.HighlightTarget -> Int -> List String -> Int -> Flow Model ()
startGutterDrag target recordId path line =
    Flow.forAll route
        (\route_ ->
            let
                clearOnClick =
                    try (Route.page << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) route_
                        |> Maybe.andThen .range
                        |> Maybe.map (\range -> range.from == line && range.to == line)
                        |> Maybe.withDefault False

                drag =
                    { target = target
                    , recordId = recordId
                    , path = path
                    , anchor = line
                    , current = line
                    , moved = False
                    , clearOnClick = clearOnClick
                    }
            in
            Flow.setAll gutterDrag (Just drag)
                |> Flow.seq
                    (Flow.when (not clearOnClick)
                        (updateGutterRange target recordId path { from = line, to = line })
                    )
        )


extendGutterDrag : Route.HighlightTarget -> Int -> List String -> Int -> Flow Model ()
extendGutterDrag target recordId path line =
    Flow.forAll (gutterDrag << just << where_ (\d -> d.target == target && d.recordId == recordId && d.path == path))
        (\drag ->
            let
                nextDrag =
                    { drag | current = line, moved = drag.moved || line /= drag.current }
            in
            Flow.setAll gutterDrag (Just nextDrag)
                |> Flow.seq (updateGutterRange target recordId path { from = min drag.anchor line, to = max drag.anchor line })
        )


endGutterDrag : Flow Model ()
endGutterDrag =
    Flow.forAll (gutterDrag << just)
        (\drag ->
            Flow.setAll gutterDrag Nothing
                |> Flow.seq
                    (Flow.when (drag.clearOnClick && not drag.moved)
                        (clearHighlightedRange drag.target drag.recordId drag.path)
                    )
        )


clearHighlightedRange : Route.HighlightTarget -> Int -> List String -> Flow Model ()
clearHighlightedRange target recordId path =
    overRouteReplace
        (over
            (Route.page << Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path))
            (\highlight -> { highlight | range = Nothing })
        )


clearHighlightedFileOnClose : Route.HighlightTarget -> Int -> List String -> Flow Model ()
clearHighlightedFileOnClose target recordId path =
    let
        shouldClear highlight =
            Route.highlightMatches target recordId path highlight && Maybe.isJust highlight.range
    in
    overRouteReplace
        (over (Route.page << Route.project << mHighlight) (Maybe.filter (not << shouldClear)))


updateGutterRange : Route.HighlightTarget -> Int -> List String -> Route.LineRange -> Flow Model ()
updateGutterRange target recordId path range =
    overRouteReplace
        (set (Route.page << Route.project << mHighlight)
            (Just { id = recordId, target = target, path = path, range = Just range })
        )


overRouteReplace : (Route -> Route) -> Flow Model ()
overRouteReplace fn =
    Flow.forAll route
        (\current ->
            let
                next =
                    fn current
            in
            Flow.when (next /= current) (replaceRoute next)
        )


addToast : Bool -> String -> Flow Model ()
addToast isSuccess message =
    Flow.forAll nextToastId
        (\nextId ->
            Flow.setAll (toasts << each << needsIntro) False
                |> Flow.seq (Flow.over toasts ((::) <| Toast message nextId isSuccess True))
                |> Flow.seq (Flow.over nextToastId (\_ -> nextId + 1))
        )


dismissToast : Int -> Flow Model ()
dismissToast toastId =
    Flow.over toasts (List.removeWhen <| (==) toastId << .id)


resetNeedsIntro : Int -> Flow Model ()
resetNeedsIntro toastId =
    Flow.setAll (toasts << by .id toastId << needsIntro) False


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


computeChangedSortRecords : List (BaseRecord a) -> List (BaseRecord a) -> List (BaseRecord a)
computeChangedSortRecords oldRecords newRecords =
    List.map2 Tuple.pair oldRecords newRecords
        |> List.filterMap
            (\( old, new ) ->
                if old.sortKey /= new.sortKey then
                    Just new

                else
                    Nothing
            )


updateSortKeys : Maybe Int -> TableSpec (BaseRecord a) -> List (BaseRecord a) -> Flow Model ()
updateSortKeys mProjectId tableSpec records_ =
    let
        allUpdatedRecords =
            List.indexedMap (\i -> set sortKey (Just i)) records_
    in
    Flow.setAll (TableSpec.getLens tableSpec << records << success) allUpdatedRecords
        |> Flow.seq
            (case mProjectId of
                Just projectId ->
                    saveProject projectId

                Nothing ->
                    let
                        changedRecords =
                            computeChangedSortRecords records_ allUpdatedRecords
                    in
                    if getTag tableSpec == TagProjects then
                        changedRecords
                            |> List.filterMap (\record -> Maybe.map (\id -> ( id, TableSpec.getEncodeRecord tableSpec record )) record.id)
                            |> (\updated ->
                                    if List.isEmpty updated then
                                        Flow.pure (Ok ())

                                    else
                                        callApi void (Api.saveProjectsBatch updated)
                               )
                            |> Flow.seq refetchCommitHash
                            |> Flow.return (Ok ())

                    else
                        Flow.batchM (List.map (persistRecordChange Nothing tableSpec) changedRecords)
                            |> Flow.seq refetchCommitHash
                            |> Flow.return (Ok ())
            )
        |> Flow.return ()


onSelectSearch : Maybe Int -> Int -> Flow Model ()
onSelectSearch mProjectId stepId =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.page << Route.project << mCommit << just) model

                    pickedProjectId =
                        mProjectId |> Maybe.orElse (try (projectsContainingEntity stepId << recordId << just) model)
                in
                pickedProjectId
                    |> Maybe.unwrap (Flow.pure ()) (\pId -> goToRoute (Route.fromPage (Route.Project { projectId = pId, mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }, mCommit = mCommit_, mCompare = Nothing })))
            )


callJs : String -> (a -> Encode.Value) -> Decode.Decoder b -> a -> Flow Model b
callJs =
    Flow.ffi Ports.ffiOut Ports.ffiIn


hidePopover : String -> Flow Model ()
hidePopover popoverId =
    callJs "hidePopover" Encode.string (Decode.succeed ()) popoverId


openDialog : String -> Flow Model ()
openDialog id =
    callJs "openDialog" Encode.string (Decode.succeed ()) id


closeDialog : String -> Flow Model ()
closeDialog id =
    callJs "closeDialog" Encode.string (Decode.succeed ()) id


toggleTheme : Flow Model ()
toggleTheme =
    callJs "toggleTheme" (\_ -> Encode.null) (Decode.succeed ()) ()


saveProject : Int -> FlowError Http.Error Model ()
saveProject projectId =
    Flow.get
        |> Flow.map (try (projects << records << success << by .id (Just projectId)))
        |> Flow.assertJust
        |> Flow.andThen (Api.saveProject projectId >> callApi void)
        |> FlowError.andThen (\_ -> refetchCommitHash |> Flow.return ())


agentChatId : String
agentChatId =
    "agent-chat"


agentChatEndId : String
agentChatEndId =
    "agent-chat-end"


agentSessionNameInputId : String
agentSessionNameInputId =
    "agent-session-name-input"


agentTurnId : String -> String
agentTurnId turnId =
    "agent-turn-" ++ turnId


shareLink : Route -> Flow Model ()
shareLink linkRoute =
    Flow.forAll origin
        (\origin_ ->
            callJs "copyToClipboard" Encode.string (Decode.succeed ()) (origin_ ++ Route.toString linkRoute)
        )
        |> Flow.seq (addToast True "Share link copied to clipboard")


shareAgentChat : String -> Flow Model ()
shareAgentChat sessionId =
    Flow.forAll route
        (\currentRoute ->
            shareLink { currentRoute | chat = Just { sessionId = sessionId, mTurnId = Nothing } }
        )


agentSessionListed : String -> Model.AgentState -> Bool
agentSessionListed sessionId =
    has (sessionAt sessionId)


applyAgentChatFromUrl : Bool -> Flow Model ()
applyAgentChatFromUrl openPanel =
    pruneBlankAgentChats
        |> Flow.seq (selectAgentChatFromUrl openPanel)


pruneBlankAgentChats : Flow Model ()
pruneBlankAgentChats =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    agentState =
                        Model.getAgent model

                    openChatId =
                        Maybe.map .sessionId (get (route << Route.chat) model)

                    abandoned summary =
                        Just summary.session.sessionId /= openChatId && agentSessionBlank summary agentState

                    chatBeingCreated =
                        agentState.request == Just Model.CreatingAgentSession
                in
                Flow.unless chatBeingCreated
                    (ApiData.withDefault [] agentState.sessions
                        |> List.filter abandoned
                        |> Flow.traverse dropBlankAgentChat
                        |> Flow.return ()
                    )
            )


dropBlankAgentChat : Model.AgentSessionSummary -> Flow Model ()
dropBlankAgentChat summary =
    Flow.over agent (dropAgentSession summary.session.sessionId)
        |> Flow.seq (Flow.async (AgentApi.delete_ summary.session.sessionId))


selectAgentChatFromUrl : Bool -> Flow Model ()
selectAgentChatFromUrl openPanel =
    Flow.forAll (route << Route.chat)
        (\mChat ->
            case mChat of
                Nothing ->
                    Flow.over agent
                        (\s ->
                            if s.selectedSessionId == Nothing then
                                s

                            else
                                { s
                                    | selectedSessionId = Nothing
                                    , highlightTurnId = Nothing
                                }
                        )

                Just chat ->
                    Flow.forAll agent
                        (\agentState ->
                            if agentState.selectedSessionId == Just chat.sessionId then
                                Flow.over agent (\s -> { s | highlightTurnId = chat.mTurnId })
                                    |> Flow.seq (scrollToAgentTurn chat.mTurnId)

                            else
                                Flow.over agent (\s -> { s | isPanelOpen = s.isPanelOpen || openPanel })
                                    |> Flow.seq (Flow.when (ApiData.toMaybe agentState.sessions == Nothing) loadAgentSessions)
                                    |> Flow.seq (selectAgentChatIfStillRouted chat)
                        )
        )


selectAgentChatIfStillRouted : Route.ChatRef -> Flow Model ()
selectAgentChatIfStillRouted chat =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    stillRouted =
                        Maybe.map .sessionId (get (route << Route.chat) model) == Just chat.sessionId
                in
                Flow.when (stillRouted && agentSessionListed chat.sessionId (Model.getAgent model))
                    (selectAgentSessionAt chat.sessionId chat.mTurnId)
            )


scrollToAgentTurn : Maybe String -> Flow Model ()
scrollToAgentTurn =
    Maybe.unwrap (Flow.pure ())
        (\turnId -> Flow.attemptTask (Scroll.scrollElementY agentChatId (agentTurnId turnId) 0 0))


scrollAgentChatToBottom : Flow Model ()
scrollAgentChatToBottom =
    Flow.attemptTask (Scroll.scrollElementY agentChatId agentChatEndId 1 1)


agentChatAtBottom : Flow Model Bool
agentChatAtBottom =
    let
        slackInPixels =
            80
    in
    Flow.attemptTaskWith
        (\result ->
            case result of
                Ok { scene, viewport } ->
                    Flow.pure (scene.height - (viewport.y + viewport.height) < slackInPixels)

                Err _ ->
                    Flow.pure False
        )
        (Dom.getViewportOf agentChatId)


setAgentSessions : ApiData (List Model.AgentSessionSummary) -> Flow Model ()
setAgentSessions data =
    Flow.over agent (\agentState -> set sessions (withRenames agentState.sessionRenames data) agentState)


withRenames : Dict String ( String, Model.SessionTimestamp ) -> ApiData (List Model.AgentSessionSummary) -> ApiData (List Model.AgentSessionSummary)
withRenames renames =
    ApiData.map (List.map (applySessionRenameOverride renames))


setAgentSessionsLoading : Flow Model ()
setAgentSessionsLoading =
    Flow.get
        |> Flow.map (Model.getAgent >> .sessions >> ApiData.toMaybe)
        |> Flow.andThen (\previous -> setAgentSessions (Loading previous))


whenAgentSessionAllowed : String -> Flow Model () -> Flow Model ()
whenAgentSessionAllowed sessionId action =
    Flow.forAll agent
        (\agentState ->
            Flow.when (not (agentSessionBlocked sessionId agentState)) action
        )


withAgentRequest : Model.AgentRequest -> Flow Model () -> Flow Model ()
withAgentRequest =
    withAgentRequestUnless Model.agentMutationPending


withAgentSessionRequest : String -> Model.AgentRequest -> Flow Model () -> Flow Model ()
withAgentSessionRequest sessionId =
    withAgentRequestUnless (agentSessionBlocked sessionId)


withAgentRequestUnless : (Model.AgentState -> Bool) -> Model.AgentRequest -> Flow Model () -> Flow Model ()
withAgentRequestUnless blocked request action =
    Flow.forAll agent
        (\agentState ->
            Flow.when (not (blocked agentState))
                (Flow.over agent (\s -> { s | request = Just request })
                    |> Flow.seq action
                    |> Flow.seq (clearRequestIfMatches request)
                )
        )


clearRequestIfMatches : Model.AgentRequest -> Flow Model ()
clearRequestIfMatches expected =
    Flow.over agent
        (\agentState ->
            if agentState.request == Just expected then
                { agentState | request = Nothing }

            else
                agentState
        )


dropAgentSession : String -> Model.AgentState -> Model.AgentState
dropAgentSession sessionId =
    set (sessionViewDataAt sessionId) Nothing
        >> over sessions (ApiData.map (List.filter (not << Model.isSession sessionId)))


setSessionView : String -> ApiData Model.AgentSessionView -> Model.AgentState -> Model.AgentState
setSessionView sessionId =
    set (sessionViewDataAt sessionId) << Just


markSessionViewLoading : String -> Model.AgentState -> Model.AgentState
markSessionViewLoading sessionId =
    over (sessionViewDataAt sessionId) (Just << ApiData.toLoading << Maybe.withDefault NotAsked)





closeAgentChat : Flow Model ()
closeAgentChat =
    Flow.over (route << Route.chat) (\_ -> Nothing)
        |> Flow.seq replaceCurrentUrl





pushCurrentUrl : Flow Model ()
pushCurrentUrl =
    Flow.forAll route
        (\currentRoute ->
            Flow.forAll key
                (\k -> Flow.async (Flow.lift (Nav.pushUrl k (Route.toString currentRoute))))
        )





replaceCurrentUrl : Flow Model ()
replaceCurrentUrl =
    Flow.forAll route
        (\currentRoute ->
            Flow.forAll key
                (\k -> Flow.async (Flow.lift (Nav.replaceUrl k (Route.toString currentRoute))))
        )


mergeSessionSummary : Model.AgentSessionSummary -> Model.AgentState -> Model.AgentState
mergeSessionSummary summary agentState =
    let
        sessionId =
            summary.session.sessionId

        current =
            all (sessions << orElseT success ApiData.reloading << each) agentState

        merged =
            if List.any (Model.isSession sessionId) current then
                List.map (upsertSession summary) current

            else
                summary :: current
    in
    over sessions
        (ApiData.update always (Success (List.map (applySessionRenameOverride agentState.sessionRenames) merged)))
        agentState


upsertSession : Model.AgentSessionSummary -> Model.AgentSessionSummary -> Model.AgentSessionSummary
upsertSession summary existing =
    if Model.isSession summary.session.sessionId existing then
        summary

    else
        existing


watchAgentTurn : Model.AgentSessionView -> Flow Model ()
watchAgentTurn view =
    case view.session.activeTurnId of
        Just turnId ->
            Flow.unlessHas (agent << liveTurnAt view.session.sessionId << just)
                (Flow.setAll (agent << liveTurnAt view.session.sessionId) (Just (Model.liveTurnFor turnId view))
                    |> Flow.seq (openAgentTurnStream view.session.sessionId turnId)
                )

        Nothing ->
            Flow.pure ()


watchSelectedAgentTurn : Flow Model ()
watchSelectedAgentTurn =
    Flow.get
        |> Flow.andThen
            (\model ->
                case Model.selectedSessionView (Model.getAgent model) of
                    Just view ->
                        watchAgentTurn view

                    Nothing ->
                        Flow.pure ()
            )


openAgentTurnStream : String -> String -> Flow Model ()
openAgentTurnStream sessionId turnId =
    Flow.async (Flow.lift (Ports.openAgentTurnStream { sessionId = sessionId, turnId = turnId }))


closeAgentTurnStream : String -> Flow Model ()
closeAgentTurnStream turnId =
    callJs "closeAgentTurnStream" Encode.string (Decode.succeed ()) turnId


stopWatchingSession : String -> Flow Model ()
stopWatchingSession sessionId =
    Flow.try (agent << liveTurnAt sessionId << just << turnId)
        (\mTurnId ->
            Flow.setAll (agent << liveTurnAt sessionId) Nothing
                |> Flow.seq (closeAgentTurnStream (Maybe.withDefault "" mTurnId))
        )


applyAgentSessionView : Model.AgentSessionView -> Model.AgentState -> Model.AgentState
applyAgentSessionView view =
    mergeSessionSummary (Model.summaryFromView view)
        >> setSessionView view.session.sessionId (Success view)
        >> dropStaleLiveTurn view.session.sessionId view.session.activeTurnId


dropStaleLiveTurn : String -> Maybe String -> Model.AgentState -> Model.AgentState
dropStaleLiveTurn sessionId activeTurnId agentState =
    if try (liveTurnAt sessionId << just) agentState |> Maybe.unwrap True (Model.liveTurnSurvives activeTurnId) then
        agentState

    else
        set (liveTurnAt sessionId) Nothing agentState


handleAgentSessionResult : String -> Result Http.Error Model.AgentSessionView -> Flow Model ()
handleAgentSessionResult sessionId result =
    case result of
        Ok view ->
            agentChatAtBottom
                |> Flow.andThen
                    (\atBottom ->
                        Flow.get
                            |> Flow.andThen
                                (\model ->
                                    let
                                        entriesBefore =
                                            selectedChatEntries model
                                    in
                                    Flow.over agent (applyAgentSessionView view)
                                        |> Flow.seq
                                            (Flow.get
                                                |> Flow.andThen
                                                    (\after ->
                                                        Flow.when (atBottom && entriesBefore /= selectedChatEntries after) scrollAgentChatToBottom
                                                    )
                                            )
                                        |> Flow.seq watchSelectedAgentTurn
                                )
                    )

        Err err ->
            Flow.over (agent << sessionViewDataAt sessionId) (sessionViewAfterError err)
                |> Flow.seq (addToast False (Http.errorMessage err))


sessionViewAfterError : Http.Error -> Maybe (ApiData Model.AgentSessionView) -> Maybe (ApiData Model.AgentSessionView)
sessionViewAfterError err =
    Just << failedSessionView err << Maybe.withDefault NotAsked


failedSessionView : Http.Error -> ApiData Model.AgentSessionView -> ApiData Model.AgentSessionView
failedSessionView err =
    Maybe.unwrap (Error err) Success << ApiData.toMaybe


selectedChatEntries : Model -> List Model.ChatEntry
selectedChatEntries model =
    let
        agentState =
            Model.getAgent model
    in
    Model.selectedSessionView agentState
        |> Maybe.map (\view -> sessionEntries view agentState)
        |> Maybe.withDefault []


fetchAgentSession : String -> Flow Model ()
fetchAgentSession sessionId =
    AgentApi.fetchSession sessionId
        |> Flow.andThen (handleAgentSessionResult sessionId)


refreshAgentSession : String -> Flow Model ()
refreshAgentSession sessionId =
    Flow.over agent (markSessionViewLoading sessionId)
        |> Flow.seq (fetchAgentSession sessionId)


ensureAgentSessionView : String -> Flow Model ()
ensureAgentSessionView sessionId =
    Flow.forAll agent
        (\agentState ->
            Flow.unless (sessionViewFetching sessionId agentState) (refreshAgentSession sessionId)
        )


sessionViewFetching : String -> Model.AgentState -> Bool
sessionViewFetching sessionId =
    has (sessionViewDataAt sessionId << just << ApiData.loadingState)


ensureSelectedAgentSessionView : Flow Model ()
ensureSelectedAgentSessionView =
    Flow.try (agent << selectedSessionId << just)
        (\mSessionId ->
            case mSessionId of
                Just sessionId ->
                    ensureAgentSessionView sessionId

                Nothing ->
                    Flow.pure ()
        )


loadAgentSessions : Flow Model ()
loadAgentSessions =
    setAgentSessionsLoading
        |> Flow.seq AgentApi.listSessions
        |> Flow.andThen
            (\result ->
                case result of
                    Ok summaries ->
                        setAgentSessions (Success summaries)
                            |> Flow.seq pruneBlankAgentChats
                            |> Flow.seq ensureSelectedAgentSessionView
                            |> Flow.seq watchSelectedAgentTurn

                    Err err ->
                        setAgentSessions (Error err)
                            |> Flow.seq (addToast False (Http.errorMessage err))
            )


selectAgentSession : String -> Flow Model ()
selectAgentSession sessionId =
    openAgentChat sessionId


openAgentChat : String -> Flow Model ()
openAgentChat sessionId =
    selectAgentSessionAt sessionId Nothing
        |> Flow.seq (Flow.over (route << Route.chat) (\_ -> Just { sessionId = sessionId, mTurnId = Nothing }))
        |> Flow.seq pushCurrentUrl


selectAgentSessionAt : String -> Maybe String -> Flow Model ()
selectAgentSessionAt sessionId mTurnId =
    Flow.over agent
        (\s ->
            { s
                | selectedSessionId = Just sessionId
                , isSessionListOpen = False
                , sessionNameEdit = Nothing
                , highlightTurnId = mTurnId
                , isRestoringChat = False
            }
        )
        |> Flow.seq scrollAgentChatToBottom
        |> Flow.seq (ensureAgentSessionView sessionId)
        |> Flow.seq watchSelectedAgentTurn
        |> Flow.seq (scrollToAgentTurn mTurnId)
        |> Flow.seq (Flow.over agent (\s -> { s | lastChat = Just sessionId }))
        |> Flow.seq (callJs "storeLastChat" Encode.string (Decode.succeed ()) sessionId)


anchorAgentChatUnlessHighlighting : Flow Model ()
anchorAgentChatUnlessHighlighting =
    Flow.forAll agent
        (\agentState -> Flow.when (agentState.highlightTurnId == Nothing) scrollAgentChatToBottom)


toggleAgentPanel : Flow Model ()
toggleAgentPanel =
    Flow.forAll agent
        (\agentState ->
            let
                nextOpen =
                    not agentState.isPanelOpen

                needsRestore =
                    nextOpen && agentState.selectedSessionId == Nothing
            in
            Flow.over agent
                (\s ->
                    { s
                        | isPanelOpen = nextOpen
                        , isSessionListOpen = False
                        , isFocusMode = False
                        , isRestoringChat = needsRestore
                    }
                )
                |> Flow.seq restoreLastChat
                |> Flow.seq (Flow.when nextOpen loadAgentSessions)
                |> Flow.seq restoreLastChat
                |> Flow.seq (Flow.when (not nextOpen) closeAgentChat)
        )


restoreLastChat : Flow Model ()
restoreLastChat =
    Flow.forAll agent
        (\agentState ->
            let
                restorable =
                    Maybe.filter (\sessionId -> agentSessionListed sessionId agentState) agentState.lastChat
            in
            Flow.when agentState.isRestoringChat
                (case restorable of
                    Just sessionId ->
                        Flow.get
                            |> Flow.andThen
                                (\model ->
                                    if Maybe.map .sessionId (get (route << Route.chat) model) == Just sessionId then
                                        selectAgentSessionAt sessionId Nothing

                                    else
                                        Flow.over (route << Route.chat) (\_ -> Just { sessionId = sessionId, mTurnId = Nothing })
                                            |> Flow.seq replaceCurrentUrl
                                )

                    Nothing ->
                        Flow.when (ApiData.settled agentState.sessions)
                            (Flow.over agent (\s -> { s | isRestoringChat = False }))
                )
        )


toggleAgentFocusMode : Flow Model ()
toggleAgentFocusMode =
    Flow.over agent (\s -> { s | isFocusMode = not s.isFocusMode })


exitAgentFocusMode : Flow Model ()
exitAgentFocusMode =
    Flow.over agent (\s -> { s | isFocusMode = False })


toggleAgentSessionList : Flow Model ()
toggleAgentSessionList =
    Flow.over agent (\s -> { s | isSessionListOpen = not s.isSessionListOpen })


startAgentSessionNameEdit : String -> String -> Flow Model ()
startAgentSessionNameEdit sessionId currentName =
    Flow.over agent
        (\s ->
            { s
                | sessionNameEdit =
                    Just
                        { sessionId = sessionId
                        , value = currentName
                        , saving = False
                        }
            }
        )
        |> Flow.seq (Flow.attemptTask (Dom.focus agentSessionNameInputId))


updateAgentSessionNameEdit : String -> Flow Model ()
updateAgentSessionNameEdit value =
    Flow.over agent
        (\s ->
            { s
                | sessionNameEdit =
                    Maybe.map (\edit -> { edit | value = value }) s.sessionNameEdit
            }
        )


cancelAgentSessionNameEdit : Flow Model ()
cancelAgentSessionNameEdit =
    Flow.over agent (\s -> { s | sessionNameEdit = Nothing })


saveAgentSessionName : Flow Model ()
saveAgentSessionName =
    Flow.forAll agent
        (\agentState ->
            case agentState.sessionNameEdit of
                Nothing ->
                    Flow.pure ()

                Just edit ->
                    let
                        name =
                            String.trim edit.value
                    in
                    if String.isEmpty name then
                        addToast False "Enter a chat name first."

                    else
                        Flow.over agent (setSessionNameEditSaving edit.sessionId True)
                            |> Flow.seq
                                (AgentApi.renameSession edit.sessionId name
                                    |> Flow.andThen
                                        (\result ->
                                            case result of
                                                Ok view ->
                                                    Flow.over agent (applyAgentSessionRename edit.sessionId view)
                                                        |> Flow.seq (Flow.over agent (clearSessionNameEdit edit.sessionId))

                                                Err err ->
                                                    Flow.over agent (setSessionNameEditSaving edit.sessionId False)
                                                        |> Flow.seq (addToast False (Http.errorMessage err))
                                        )
                                )
        )


setSessionNameEditSaving : String -> Bool -> Model.AgentState -> Model.AgentState
setSessionNameEditSaving sessionId saving agentState =
    case agentState.sessionNameEdit of
        Just edit ->
            if edit.sessionId == sessionId then
                { agentState | sessionNameEdit = Just { edit | saving = saving } }

            else
                agentState

        Nothing ->
            agentState


applyAgentSessionRename : String -> Model.AgentSessionView -> Model.AgentState -> Model.AgentState
applyAgentSessionRename sessionId renamedView =
    recordSessionRename sessionId renamedView.session
        >> mergeSessionSummary (Model.summaryFromView renamedView)


recordSessionRename : String -> Model.AgentSession -> Model.AgentState -> Model.AgentState
recordSessionRename sessionId session agentState =
    session.sessionName
        |> Maybe.map (\name -> set (sessionRenames << Dict.Accessors.at sessionId) (Just ( name, session.updatedAt )) agentState)
        |> Maybe.withDefault agentState


applySessionRenameOverride : Dict String ( String, Model.SessionTimestamp ) -> Model.AgentSessionSummary -> Model.AgentSessionSummary
applySessionRenameOverride renames summary =
    Dict.get summary.session.sessionId renames
        |> Maybe.filter (\( _, renamedAt ) -> Model.sessionTimestampAtLeast renamedAt summary.session.updatedAt)
        |> Maybe.map (\( name, _ ) -> summary |> over title (always name) |> over (session << sessionName) (always (Just name)))
        |> Maybe.withDefault summary


clearSessionNameEdit : String -> Model.AgentState -> Model.AgentState
clearSessionNameEdit sessionId agentState =
    case agentState.sessionNameEdit of
        Just edit ->
            if edit.sessionId == sessionId then
                { agentState | sessionNameEdit = Nothing }

            else
                agentState

        Nothing ->
            agentState


archiveAgentSession : String -> Flow Model ()
archiveAgentSession sessionId =
    withAgentSessionRequest sessionId
        (Model.ArchivingAgentSession sessionId)
        (AgentApi.archive sessionId
            |> Flow.andThen
                (\result ->
                    case result of
                        Ok view ->
                            Flow.over agent (applyAgentSessionView view)
                                |> Flow.seq (stopWatchingSession sessionId)
                                |> Flow.seq
                                    (Flow.forAll agent
                                        (\agentState ->
                                            Flow.when (agentState.selectedSessionId == Just sessionId) closeAgentChat
                                        )
                                    )

                        Err err ->
                            clearRequestIfMatches (Model.ArchivingAgentSession sessionId)
                                |> Flow.seq (addToast False (Http.errorMessage err))
                )
        )


deleteAgentSession : String -> Flow Model ()
deleteAgentSession sessionId =
    withAgentSessionRequest sessionId
        (Model.DeletingAgentSession sessionId)
        (AgentApi.delete_ sessionId
            |> Flow.andThen
                (\result ->
                    case result of
                        Ok () ->
                            Flow.over agent (dropAgentSession sessionId)
                                |> Flow.seq (stopWatchingSession sessionId)
                                |> Flow.seq
                                    (Flow.forAll agent
                                        (\agentState ->
                                            Flow.when (agentState.selectedSessionId == Just sessionId) closeAgentChat
                                        )
                                    )

                        Err err ->
                            clearRequestIfMatches (Model.DeletingAgentSession sessionId)
                                |> Flow.seq (addToast False (Http.errorMessage err))
                )
        )


confirmDeleteAgentSession : String -> Flow Model ()
confirmDeleteAgentSession sessionId =
    let
        cfg =
            { id = "modal-confirm"
            , title = "Delete chat"
            , subtitle = Just ("Chat #" ++ String.left 12 sessionId)
            , bodyLines =
                [ "This permanently removes the chat metadata, runner logs, worktree, and agent branch."
                , "This cannot be undone."
                ]
            , onConfirm = deleteAgentSession sessionId
            }
    in
    Flow.modify (\(Model.Model m) -> Model.Model { m | modalConfirm = cfg })
        |> Flow.seq (openDialog "modal-confirm")


toggleAgentArchived : Flow Model ()
toggleAgentArchived =
    Flow.over agent (\s -> { s | showArchived = not s.showArchived })


readAgentPrompt : Flow Model String
readAgentPrompt =
    callJs "agentPrompt" Encode.string Decode.string "read"


clearAgentPrompt : Flow Model ()
clearAgentPrompt =
    callJs "agentPrompt" Encode.string (Decode.succeed ()) "clear"


setChangesetOperation : String -> Model.ChangesetOperationKind -> Flow Model ()
setChangesetOperation sessionId kind =
    Flow.over agent (\agentState -> { agentState | changesetOperation = Just { sessionId = sessionId, kind = kind } })


clearChangesetOperation : String -> Flow Model ()
clearChangesetOperation sessionId =
    Flow.over agent
        (\agentState ->
            case agentState.changesetOperation of
                Just operation ->
                    if operation.sessionId == sessionId then
                        { agentState | changesetOperation = Nothing }

                    else
                        agentState

                Nothing ->
                    agentState
        )


setPendingSteer : String -> Maybe String -> Flow Model ()
setPendingSteer sessionId prompt =
    Flow.setAll (agent << liveTurnAt sessionId << just << pendingSteer) prompt


createAgentSession : Flow Model ()
createAgentSession =
    withAgentRequest Model.CreatingAgentSession
        (Flow.over agent
            (\agentState ->
                { agentState
                    | isSessionListOpen = False
                    , sessionNameEdit = Nothing
                }
            )
            |> Flow.seq
                (AgentApi.createSession
                    |> Flow.andThen
                        (\result ->
                            case result of
                                Ok view ->
                                    Flow.over agent (applyAgentSessionView view)
                                        |> Flow.seq (openAgentChat view.session.sessionId)

                                Err err ->
                                    addToast False (Http.errorMessage err)
                        )
                )
        )


archivedSummary : Model.AgentSessionSummary -> Bool
archivedSummary =
    .session >> .status >> Model.agentSessionArchived


refreshSelectedAgentSession : Flow Model ()
refreshSelectedAgentSession =
    Flow.forAll agent
        (\agentState ->
            agentState
                |> Model.selectedSessionSummary
                |> Maybe.filter (not << archivedSummary)
                |> Maybe.unwrap (Flow.pure ()) (.session >> .sessionId >> ensureAgentSessionView)
        )


refreshVisibleAgentSession : Flow Model ()
refreshVisibleAgentSession =
    Flow.forAll agent
        (\agentState ->
            Flow.when agentState.isPanelOpen
                (Flow.get
                    |> Flow.andThen
                        (\model ->
                            let
                                state =
                                    Model.getAgent model

                                selected =
                                    Model.selectedSessionSummary state
                                        |> Maybe.filter (not << archivedSummary)
                                        |> Maybe.map (.session >> .sessionId)
                                        |> Maybe.toList

                                watched =
                                    all (liveTurns << Dict.Accessors.eachIdx) state
                                        |> List.filterMap unfinishedSession
                            in
                            Flow.batchM (List.map ensureAgentSessionView (List.unique (selected ++ watched)))
                        )
                )
        )


unfinishedSession : ( String, Model.AgentLiveTurn ) -> Maybe String
unfinishedSession ( sessionId, live ) =
    if live.finished then
        Nothing

    else
        Just sessionId


withSelectedAgentSession : (Model.AgentSessionView -> Flow Model ()) -> Flow Model ()
withSelectedAgentSession fn =
    Flow.get
        |> Flow.andThen
            (\model ->
                case Model.selectedSessionView (Model.getAgent model) of
                    Just view ->
                        fn view

                    Nothing ->
                        addToast False "Select or create an agent session first."
            )


stopAgentTurn : Flow Model ()
stopAgentTurn =
    withSelectedAgentSession
        (\view ->
            let
                sessionId =
                    view.session.sessionId

                request =
                    Model.StoppingAgentTurn sessionId
            in
            Flow.over agent (\s -> { s | request = Just request })
                |> Flow.seq (AgentApi.stop sessionId)
                |> FlowError.foldResult
                    (\stoppedView ->
                        Flow.over agent (applyAgentSessionView stoppedView)
                    )
                    (\err -> addToast False (Http.errorMessage err))
                |> Flow.seq (clearRequestIfMatches request)
        )


submitAgentPrompt : Flow Model ()
submitAgentPrompt =
    submitAgentPromptFrom readAgentPrompt


submitAgentPromptFrom : Flow Model String -> Flow Model ()
submitAgentPromptFrom promptSource =
    withSelectedAgentSession
        (\view ->
            Flow.forAll agent
                (\agentState ->
                    if agentSessionRunning view.session.sessionId agentState then
                        steerAgentTurn view promptSource

                    else
                        sendAgentTurn view promptSource
                )
        )


withAgentPrompt : Flow Model String -> (String -> Flow Model ()) -> Flow Model ()
withAgentPrompt promptSource send =
    promptSource
        |> Flow.andThen
            (\rawPrompt ->
                case String.trim rawPrompt of
                    "" ->
                        addToast False "Enter an agent prompt first."

                    prompt ->
                        send prompt
            )


sendAgentTurn : Model.AgentSessionView -> Flow Model String -> Flow Model ()
sendAgentTurn view promptSource =
    let
        sessionId =
            view.session.sessionId
    in
    withAgentRequest (Model.SendingAgentPrompt sessionId)
        (withAgentPrompt promptSource
            (\prompt ->
                Flow.setAll (agent << liveTurnAt sessionId) (Just (Model.liveTurnFor "" view))
                    |> Flow.seq
                        (Flow.over (agent << liveTurnAt sessionId << just << entries)
                            (\entriesBefore ->
                                entriesBefore
                                    ++ [ Model.ChatTurnEntry { turnId = "", prompt = prompt, assistant = "", status = Model.ChatPending } ]
                            )
                        )
                    |> Flow.seq clearAgentPrompt
                    |> Flow.seq scrollAgentChatToBottom
                    |> Flow.seq (AgentApi.sendTurn sessionId prompt)
                    |> FlowError.foldResult
                        (\turn ->
                            Flow.setAll (agent << liveTurnAt sessionId << just << turnId) turn.turnId
                                |> Flow.seq (openAgentTurnStream sessionId turn.turnId)
                                |> Flow.seq (refreshAgentSession sessionId)
                        )
                        (\err ->
                            let
                                message =
                                    Http.errorMessage err
                            in
                            Flow.setAll (agent << liveTurnAt sessionId << just << finished) True
                                |> Flow.seq (Flow.over (agent << liveTurnAt sessionId << just << entries) (Model.failLatestPendingChatTurn message))
                                |> Flow.seq (addToast False message)
                        )
            )
        )


steerAgentTurn : Model.AgentSessionView -> Flow Model String -> Flow Model ()
steerAgentTurn view promptSource =
    let
        sessionId =
            view.session.sessionId
    in
    withAgentRequest (Model.SteeringAgentTurn sessionId)
        (withAgentPrompt promptSource
            (\prompt ->
                sendSteer sessionId { wire = prompt, shown = prompt }
                    |> Flow.seq clearAgentPrompt
            )
        )


sendSteer : String -> { wire : String, shown : String } -> Flow Model ()
sendSteer sessionId { wire, shown } =
    setPendingSteer sessionId (Just shown)
        |> Flow.seq scrollAgentChatToBottom
        |> Flow.seq (AgentApi.steer sessionId wire)
        |> FlowError.foldResult
            (\() -> Flow.pure ())
            (\err ->
                setPendingSteer sessionId Nothing
                    |> Flow.seq (clearRequestIfMatches (Model.SteeringAgentTurn sessionId))
                    |> Flow.seq (addToast False (Http.errorMessage err))
                    |> Flow.seq (refreshAgentSession sessionId)
            )


answerAgentQuestion : String -> String -> String -> Flow Model ()
answerAgentQuestion sessionId wire shown =
    Flow.when (not (String.isEmpty wire))
        (withAgentRequest (Model.SteeringAgentTurn sessionId)
            (sendSteer sessionId { wire = wire, shown = shown })
        )


pickAgentQuestionOption : String -> Model.PendingQuestion -> Int -> Flow Model ()
pickAgentQuestionOption sessionId question optionNumber =
    if question.multi then
        toggleAgentQuestionOption sessionId optionNumber

    else
        Flow.over (agent << liveTurnAt sessionId << just << pendingQuestion)
            (Maybe.map (\pending -> { pending | picked = Set.singleton optionNumber }))
            |> Flow.seq (answerAgentQuestion sessionId (String.fromInt optionNumber) (Model.questionAnswerLabel question [ optionNumber ]))


toggleAgentQuestionOption : String -> Int -> Flow Model ()
toggleAgentQuestionOption sessionId optionNumber =
    Flow.over (agent << liveTurnAt sessionId << just << pendingQuestion) (Maybe.map (togglePicked optionNumber))


submitAgentQuestion : String -> Model.PendingQuestion -> Flow Model ()
submitAgentQuestion sessionId question =
    answerAgentQuestion sessionId (pickedQuestionNumbers question) (Model.questionAnswerLabel question (Set.toList question.picked))


pickedQuestionNumbers : Model.PendingQuestion -> String
pickedQuestionNumbers =
    String.join "," << List.map String.fromInt << Set.toList << .picked


togglePicked : Int -> Model.PendingQuestion -> Model.PendingQuestion
togglePicked optionNumber question =
    { question
        | picked =
            if Set.member optionNumber question.picked then
                Set.remove optionNumber question.picked

            else
                Set.insert optionNumber question.picked
    }


investigateStepWithAgent : Int -> String -> Flow Model ()
investigateStepWithAgent stepId log =
    Flow.over agent (\s -> { s | isPanelOpen = True })
        |> Flow.seq loadAgentSessions
        |> Flow.seq
            (withAgentRequest Model.CreatingAgentSession
                (AgentApi.createSession
                    |> FlowError.foldResult
                        (\view ->
                            Flow.over agent (applyAgentSessionView view)
                                |> Flow.seq (openAgentChat view.session.sessionId)
                                |> Flow.seq (applyAgentChatFromUrl True)
                                |> Flow.seq (clearRequestIfMatches Model.CreatingAgentSession)
                                |> Flow.seq (submitAgentPromptFrom (Flow.pure (investigateStepPrompt stepId log)))
                        )
                        (\err -> addToast False (Http.errorMessage err))
                )
            )


investigateStepPrompt : Int -> String -> String
investigateStepPrompt stepId log =
    "Investigate why step " ++ String.fromInt stepId ++ " failed:\n\n" ++ log


applyAgentChanges : Flow Model ()
applyAgentChanges =
    withSelectedAgentSession
        (\view ->
            let
                sessionId =
                    view.session.sessionId
            in
            whenAgentSessionAllowed sessionId
                (setChangesetOperation sessionId Model.ApplyingChangeset
                    |> Flow.seq
                        (AgentApi.prepareApply sessionId
                            |> Flow.andThen
                                (\prepareResult ->
                                    case prepareResult of
                                        Ok preparedView ->
                                            if preparedView.session.status == "prepare_conflict" then
                                                Flow.over agent (applyAgentSessionView preparedView)
                                                    |> Flow.seq (clearChangesetOperation sessionId)

                                            else
                                                case preparedView.session.preparedApply of
                                                    Just candidate ->
                                                        AgentApi.confirmApply
                                                            sessionId
                                                            candidate.targetHead
                                                            candidate.candidateHead
                                                            |> Flow.andThen
                                                                (\confirmResult ->
                                                                    case confirmResult of
                                                                        Ok applyView ->
                                                                            Flow.over agent (applyAgentSessionView applyView.sessionView)
                                                                                |> Flow.seq (clearChangesetOperation sessionId)
                                                                                |> Flow.seq (markInvalidatedStatusesLoading applyView)
                                                                                |> Flow.seq reloadWorkspaceData
                                                                                |> Flow.seq loadAgentSessions

                                                                        Err err ->
                                                                            clearChangesetOperation sessionId
                                                                                |> Flow.seq (addToast False (Http.errorMessage err))
                                                                )

                                                    Nothing ->
                                                        Flow.over agent (applyAgentSessionView preparedView)
                                                            |> Flow.seq (clearChangesetOperation sessionId)

                                        Err err ->
                                            clearChangesetOperation sessionId
                                                |> Flow.seq (addToast False (Http.errorMessage err))
                                )
                        )
                )
        )


markInvalidatedStatusesLoading : Model.AgentApplyView -> Flow Model ()
markInvalidatedStatusesLoading applyView =
    let
        wipeProject projectId =
            set (projects << records << success << by .id (Just projectId) << projectStepRecords << runState) (ApiData.loading Nothing)

        wipeStep stepId =
            set (projects << records << success << each << tables << values << records << success << by .id (Just stepId) << runState) (ApiData.loading Nothing)
    in
    Flow.modify
        (\model ->
            List.foldl wipeStep (List.foldl wipeProject model applyView.invalidatedProjectIds) applyView.invalidatedStepIds
        )


discardAgentSession : Flow Model ()
discardAgentSession =
    withSelectedAgentSession
        (\view ->
            let
                sessionId =
                    view.session.sessionId
            in
            whenAgentSessionAllowed sessionId
                (setChangesetOperation sessionId Model.DiscardingChangeset
                    |> Flow.seq
                        (AgentApi.discardSession sessionId
                            |> Flow.andThen
                                (\result ->
                                    case result of
                                        Ok discardedView ->
                                            Flow.over agent (applyAgentSessionView discardedView)
                                                |> Flow.seq (clearChangesetOperation sessionId)
                                                |> Flow.seq loadAgentSessions

                                        Err err ->
                                            clearChangesetOperation sessionId
                                                |> Flow.seq (addToast False (Http.errorMessage err))
                                )
                        )
                )
        )


listenAndProcessAgentTurns : Flow Model Decode.Value
listenAndProcessAgentTurns =
    Flow.subscribe onAgentTurnIn Channels.agentTurns


onAgentTurnIn : Decode.Value -> Flow Model ()
onAgentTurnIn value =
    case Decode.decodeValue AgentApi.turnEvent value of
        Ok (Model.AgentTurnChunk { sessionId, chunk }) ->
            Flow.forAll agent
                (\agentState ->
                    Flow.if_ (agentState.selectedSessionId == Just sessionId)
                        agentChatAtBottom
                        (Flow.pure False)
                        |> Flow.andThen
                            (\atBottom ->
                                Flow.over (agent << liveTurnAt sessionId << just) (Model.ingestLiveChunk chunk)
                                    |> Flow.seq (Flow.when atBottom scrollAgentChatToBottom)
                            )
                )

        Ok (Model.AgentTurnDone sessionId) ->
            Flow.setAll (agent << liveTurnAt sessionId << just << finished) True
                |> Flow.seq (refreshAgentSession sessionId)

        Ok Model.AgentTurnHeartbeat ->
            Flow.pure ()

        Ok (Model.AgentTurnError { sessionId, message }) ->
            Flow.setAll (agent << liveTurnAt sessionId << just << streamError) (Just message)
                |> Flow.seq (addToast False message)

        Err err ->
            addToast False ("Agent stream decode error: " ++ Decode.errorToString err)


requestProjectStatus : Int -> Maybe String -> Flow Model ()
requestProjectStatus projectId commit =
    callApi void (Api.refreshProjectStatus projectId commit)
        |> ignoreResult


resyncWorkspace : String -> Flow Model ()
resyncWorkspace snapshotCommit =
    refetchCommitHash
        |> Flow.seq (Flow.get |> Flow.assertCondition (has (commitHash << success << where_ ((==) snapshotCommit))))
        |> Flow.seq loadProjects
        |> Flow.seq (Flow.forAll currentProjectId (\projectId -> requestProjectStatus projectId Nothing))


listenAndProcessStepStatus : Flow Model Decode.Value
listenAndProcessStepStatus =
    Flow.subscribe onStepStatusIn Channels.stepStatus


onStepStatusIn : Decode.Value -> Flow Model ()
onStepStatusIn value =
    case Decode.decodeValue ApiDecode.stepStatusEvent value of
        Ok (SSESnapshot { commit, steps }) ->
            Flow.get
                |> Flow.andThen
                    (\model ->
                        Flow.when (headMovedRemotely model commit) (Flow.async (resyncWorkspace commit))
                            |> Flow.seq (applyStepStatuses (Dict.fromList (List.map (\step -> ( step.stepId, ( commit, step.status ) )) steps)))
                    )

        Ok SSEHeartbeat ->
            Flow.pure ()

        Ok (SSEError err) ->
            addToast False ("SSE Error: " ++ err)

        Err err ->
            addToast False ("SSE Decode Error: " ++ Decode.errorToString err)


headMovedRemotely : Model -> String -> Bool
headMovedRemotely model snapshotCommit =
    has (commitHash << success << where_ ((/=) snapshotCommit)) model
        && not (has (route << Route.page << Route.project << mCommit << just) model)


applyStepStatuses : Dict Int ( String, Status ) -> Flow Model ()
applyStepStatuses statuses =
    Flow.get |> Flow.andThen (applyListedStatuses statuses)


applyListedStatuses : Dict Int ( String, Status ) -> Model -> Flow Model ()
applyListedStatuses statuses model =
    let
        listed =
            all (stepRecordsListed statuses) model

        listedIds =
            Set.fromList (List.filterMap (get recordId) listed)

        unlisted =
            Dict.filter (\stepId _ -> not (Set.member stepId listedIds))

        hooks =
            statuses
                |> Dict.filter (\_ -> succeeded)
                |> Dict.keys
                |> List.map runAndClearStepStatusHook

        settles =
            Dict.toList statuses
                |> List.map (\( stepId, ( commit, status_ ) ) -> settlePendingBuild commit stepId status_)

        reviewReloads =
            listed
                |> List.filter (renewsReview statuses)
                |> List.map (always (Flow.async loadProjectReviews))
    in
    Flow.over (remkT stepRecords) (applyStatusToStepRecord model statuses)
        |> Flow.seq (Flow.over stepStatusBuffer (\buffer -> Dict.union (unlisted buffer) (unlisted statuses)))
        |> Flow.seq (Flow.batchM (hooks ++ settles ++ reviewReloads))


renewsReview : Dict Int ( String, Status ) -> StepRecord -> Bool
renewsReview statuses record =
    has (review << just) record
        && has (runState << success << status << where_ ((==) (Success StatusRunning))) record
        && announcedSuccess statuses record


announcedStatus : Dict Int ( String, Status ) -> StepRecord -> Maybe ( String, Status )
announcedStatus statuses =
    get recordId >> Maybe.andThen (flip Dict.get statuses)


announcedSuccess : Dict Int ( String, Status ) -> StepRecord -> Bool
announcedSuccess statuses =
    announcedStatus statuses >> Maybe.unwrap False succeeded


succeeded : ( String, Status ) -> Bool
succeeded =
    Tuple.second >> (==) StatusSuccess


applyStatusToStepRecord : Model -> Dict Int ( String, Status ) -> StepRecord -> StepRecord
applyStatusToStepRecord model statuses record =
    case announcedStatus statuses record of
        Just ( commit, status_ ) ->
            if acceptsCommit model commit record then
                applySnapshotToRecord commit status_ record

            else
                record

        Nothing ->
            record


acceptsCommit : Model -> String -> StepRecord -> Bool
acceptsCommit model commit =
    Maybe.unwrap True ((==) commit) << Model.stepRevision model


applySnapshotToRecord : String -> Status -> StepRecord -> StepRecord
applySnapshotToRecord commit status_ record =
    let
        current =
            get runState record

        updated =
            applyStatusSnapshot commit status_ current
    in
    if updated == current then
        record

    else
        set runState updated record


settlePendingBuild : String -> Int -> Status -> Flow Model ()
settlePendingBuild snapshotCommit stepId status_ =
    let
        settled =
            case status_ of
                StatusSuccess ->
                    Just
                        (Flow.async loadProjectReviews
                            |> Flow.seq (addToast True "The viewed revision is built. Comparing with the reviewed output.")
                        )

                StatusFailure (Just error) ->
                    Just (addToast False ("Building this revision failed: " ++ error))

                StatusFailure Nothing ->
                    Just (addToast False "Building this revision failed.")

                _ ->
                    Nothing
    in
    Flow.get
        |> Flow.andThen
            (\model ->
                case ( settled, Dict.get stepId (Model.getPendingBuilds model) ) of
                    ( Just report, Just requestedRevision ) ->
                        Flow.over pendingBuilds (Dict.remove stepId)
                            |> Flow.seq (Flow.when (requestedRevision == snapshotCommit) report)

                    _ ->
                        Flow.pure ()
            )


startClusterStatusStream : Flow Model Decode.Value
startClusterStatusStream =
    Flow.subscribe onClusterStatusIn Channels.clusterStatus


onClusterStatusIn : Decode.Value -> Flow Model ()
onClusterStatusIn value =
    let
        decoder =
            Decode.map3
                (\statusStr detail ids ->
                    ( clusterStatusFromString statusStr, detail, ids )
                )
                (Decode.field "status" Decode.string)
                (Decode.maybe (Decode.field "detail" Decode.string))
                (Decode.field "runningStepIds" (Decode.list Decode.int))
    in
    case Decode.decodeValue decoder value of
        Ok ( status, detail, ids ) ->
            Flow.setAll clusterStatus (ApiData.Success status)
                |> Flow.seq (Flow.setAll clusterDetail detail)
                |> Flow.seq (Flow.setAll runningStepIds ids)

        Err _ ->
            Flow.pure ()


clusterStatusFromString : String -> Model.ClusterStatus
clusterStatusFromString statusStr =
    case statusStr of
        "available" ->
            Model.ClusterAvailable

        "degraded" ->
            Model.ClusterDegraded

        "unavailable" ->
            Model.ClusterUnavailable

        _ ->
            Model.ClusterUnknown


toggleStatusBar : Flow Model ()
toggleStatusBar =
    Flow.over statusBarOpen not


stepOutputRoute : Model -> Int -> Maybe Route
stepOutputRoute model stepId =
    let
        containing =
            projectsContainingEntity stepId

        openProjectId =
            try currentProjectId model
    in
    try (containing << where_ (.id >> (==) openProjectId) << recordId << just) model
        |> Maybe.orElse (try (containing << recordId << just) model)
        |> Maybe.map
            (\projectId ->
                Route.fromPage
                    (Route.Project
                        { projectId = projectId
                        , mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }
                        , mCommit = Nothing
                        , mCompare = Nothing
                        }
                    )
            )


knownProjectRoute : Model -> Int -> Maybe Route
knownProjectRoute model projectId =
    if has (projects << records << success << by .id (Just projectId)) model then
        Just
            (Route.fromPage
                (Route.Project
                    { projectId = projectId, mHighlight = Nothing, mCommit = Nothing, mCompare = Nothing }
                )
            )

    else
        Nothing


openRunningStep : Int -> Flow Model ()
openRunningStep stepId =
    Flow.get
        |> Flow.andThen
            (\model ->
                stepOutputRoute model stepId
                    |> Maybe.unwrap (Flow.pure ())
                        (\route ->
                            Flow.setAll statusBarOpen False
                                |> Flow.seq (goToRoute route)
                        )
            )
