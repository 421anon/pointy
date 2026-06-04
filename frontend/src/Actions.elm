module Actions exposing (..)

import Accessors exposing (An_Optic, all, each, get, has, just, keyI, over, set, try, values)
import Api.Api as Api
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Api.Decode as ApiDecode
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Channels
import Components.Select exposing (selected)
import Debounce
import Dict
import DnDList
import Extra.Accessors exposing (A_Traversal, by, orElseT, remkT, where_)
import Extra.FlowError as FlowError exposing (FlowError)
import Extra.Http as Http
import Extra.List as List
import File.Select as Select
import Flow exposing (Flow)
import Grid
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AddMode(..), BaseRecord, CompareActiveData, CompareFile, CompareMode(..), CompareSelection, CompareSource(..), CompareState(..), Model, ProjectRecord, Status(..), StepRecord, StepStatusEvent(..), Table, TableTag(..), TemplateSource(..), dndSystem)
import Model.Lenses exposing (..)
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
                stashed =
                    case ( t.inspected, t.edited ) of
                        ( False, Just r ) ->
                            set (draftAt r.id) (Just r) t

                        _ ->
                            t

                mRecordToEdit =
                    try (success << by .id mRecordId) t.records

                formIsOpen =
                    t.edited /= Nothing && not t.nameEditOnly

                notEditingExistingRecord =
                    (t.edited |> Maybe.andThen .id) == Nothing

                togglingCurrentRecord =
                    (t.edited |> Maybe.map .id) == Just mRecordId

                clickedNewRecord =
                    mRecordToEdit == Nothing

                switchingMode =
                    togglingCurrentRecord && inspected /= t.inspected

                newedited =
                    if formIsOpen && not switchingMode && (togglingCurrentRecord || (clickedNewRecord && notEditingExistingRecord)) then
                        Nothing

                    else if inspected then
                        Just (Maybe.withDefault (TableSpec.getDefaultRecord spec) mRecordToEdit)

                    else
                        get (draftAt mRecordId) stashed
                            |> Maybe.orElse mRecordToEdit
                            |> Maybe.withDefault (TableSpec.getDefaultRecord spec)
                            |> Just
            in
            { stashed | nameEditOnly = False, inspected = inspected, edited = newedited }

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
                    optimisticCreate
                        tableLens
                        record
                        (Api.createStep (Just projectId) stepType record)

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
    Flow.forAll (stepConfig << success)
        (\stepConfig_ ->
            Flow.forAll (presets << success)
                (\presets_ ->
                    Flow.get
                        |> Flow.andThen
                            (\model ->
                                let
                                    mCommit_ =
                                        try (route << Route.project << mCommit << just) model
                                in
                                callApiMerge Model.updateProjectRecordList (projects << records) (Api.fetchProjects mCommit_ presets_ stepConfig_ |> Flow.map (Result.map sortProjects))
                            )
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


loadPresets : Flow Model ()
loadPresets =
    Flow.try (route << Route.project << mCommit << just)
        (callApi presets << Api.fetchPresets)
        |> Flow.return ()


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
                                            createStep spec edited_ |> Flow.return ()

                                        ( Nothing, AddFromOtherProject ) ->
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

                                                        mergeFn r =
                                                            { edited_
                                                                | runState =
                                                                    if argsChanged then
                                                                        ApiData.loading Nothing

                                                                    else
                                                                        r.runState
                                                            }
                                                    in
                                                    saveExistingRecord lens edited_ mergeFn spec
                                                )
                                )
                        )
            )


endRecordEdit : A_Traversal s (Table (BaseRecord a)) -> Flow s ()
endRecordEdit lens =
    Flow.over (remkT lens)
        (\t ->
            let
                cleared =
                    case ( t.inspected, t.edited ) of
                        ( False, Just r ) ->
                            set (draftAt r.id) Nothing t

                        _ ->
                            t
            in
            { cleared | edited = Nothing, addMode = AddNew }
        )


saveExistingRecord : A_Traversal Model (Table (BaseRecord a)) -> BaseRecord a -> (BaseRecord a -> BaseRecord a) -> TableSpec (BaseRecord a) -> Flow Model ()
saveExistingRecord lens record mergeFn spec =
    let
        clearUpdating =
            Flow.forAll now
                (\posix ->
                    Flow.over (remkT lens << records << success << by .id record.id)
                        (\r -> { r | isUpdating = False, lastModifiedAt = Just posix })
                )
                |> Flow.seq refetchCommitHash
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
        |> Flow.seq
            (callApi void (Api.saveRecord spec record)
                |> FlowError.foldResult (always clearUpdating) (always clearUpdating)
            )


onUrlRequest : Browser.UrlRequest -> Flow Model ()
onUrlRequest urlRequest =
    case urlRequest of
        Browser.Internal url ->
            Flow.forAll key (\k -> Flow.lift (Nav.pushUrl k (Url.toString url)))

        Browser.External href ->
            Flow.lift (Nav.load href)


goToRoute : Route -> Flow Model ()
goToRoute route =
    Flow.forAll key (\k -> Flow.lift (Nav.pushUrl k (Route.toString route)))


replaceRoute : Route -> Flow Model ()
replaceRoute route =
    Flow.forAll key (\k -> Flow.lift (Nav.replaceUrl k (Route.toString route)))


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


loadStepLog : Int -> Flow Model ()
loadStepLog id =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.project << mCommit << just) model

                    key =
                        Model.stepLogKey id mCommit_
                in
                Flow.over stepLogs (Dict.insert key (ApiData.loading Nothing))
                    |> Flow.seq
                        (Api.fetchStepLog id mCommit_
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
                    |> Flow.seq (clearStepLog id mCommit_)
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


shareEntity : Int -> Int -> Route.HighlightTarget -> List String -> Maybe Route.LineRange -> Flow Model ()
shareEntity projectId entityId target pathSegments mRange =
    Flow.try (commitHash << success)
        (\mCommit_ ->
            Flow.forAll origin
                (\origin_ ->
                    let
                        route_ =
                            Route.Project
                                { projectId = projectId
                                , mHighlight = Just { id = entityId, target = target, path = pathSegments, range = mRange }
                                , mCommit = mCommit_
                                }
                    in
                    callJs "copyToClipboard" Encode.string (Decode.succeed ()) (origin_ ++ Route.toString route_)
                )
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


downloadFile : Int -> String -> List String -> Flow Model ()
downloadFile stepId commit filePath =
    Flow.lift (Nav.load (Api.stepFileDownloadUrl stepId (Just commit) filePath))


downloadSrcFile : Int -> List String -> Flow Model ()
downloadSrcFile id filePath =
    Flow.lift (Nav.load (Api.srcFileDownloadUrl id filePath))


startCompare : CompareSelection -> Flow Model ()
startCompare sel =
    Flow.setAll compareState (CompareSelecting sel)


cancelCompare : Flow Model ()
cancelCompare =
    Flow.setAll compareState CompareIdle


selectCompareFile : CompareSelection -> Flow Model ()
selectCompareFile right =
    Flow.forAll (compareState << compareSelecting) <|
        \left ->
            let
                isSession d =
                    d.left == left && d.right == right
            in
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
                |> Flow.seq (fetchCompareSide isSession compareLeftContent left)
                |> Flow.seq (fetchCompareSide isSession compareRightContent right)


fetchCompareSide : (CompareActiveData -> Bool) -> An_Optic pr ls CompareActiveData (ApiData CompareFile) -> CompareSelection -> Flow Model ()
fetchCompareSide isSession contentLens sel =
    case Model.compareSelectionMode sel of
        CompareText ->
            let
                sideLens =
                    compareState << compareActive << where_ isSession << remkT contentLens
            in
            Flow.over sideLens ApiData.toLoading
                |> Flow.seq
                    (fetchCompareContent sel
                        |> Flow.andThen
                            (\result ->
                                case result of
                                    Ok s ->
                                        Flow.over sideLens
                                            (always
                                                (Success
                                                    { text = s
                                                    , delimitedGrid = Model.delimitedGridFromFile sel.path sel.mimeType s Nothing
                                                    }
                                                )
                                            )

                                    Err e ->
                                        addToast False (Http.errorMessage e)
                                            |> Flow.seq (Flow.over sideLens (always (Error e)))
                            )
                    )

        _ ->
            Flow.pure ()


fetchCompareContent : CompareSelection -> FlowError Http.Error Model String
fetchCompareContent sel =
    case sel.source of
        FromOutput commit_ ->
            Api.fetchFileContents sel.recordId (Just commit_) sel.path

        FromSrc ->
            Api.fetchSrcFileContents sel.recordId sel.path


shouldSkipFileContents : { r | mimeType : Maybe String } -> Bool
shouldSkipFileContents file_ =
    has (mimeType << just << where_ (String.startsWith "image/")) file_
        || has (mimeType << just << where_ (String.startsWith "text/html")) file_


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

        stepCommit =
            allStepTables << recordById recordId << runState << success << commit

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

                                -- Ensure extras has fully resolved before deciding column
                                -- metadata. The folder action fires fetchExtras after the
                                -- directory listing, so a fast click on a file races against
                                -- that response; reading the lens at click-time would snapshot
                                -- Loading and bake `Nothing` metadata into the grid.
                                ensureExtras =
                                    Flow.forAll parentExtrasLens
                                        (\extras ->
                                            case extras of
                                                ApiData.Success _ ->
                                                    Flow.pure (Ok ())

                                                ApiData.Error _ ->
                                                    -- Respect a prior failure rather than retrying
                                                    -- on every file click; the grid degrades to
                                                    -- string-typed columns.
                                                    Flow.pure (Ok ())

                                                _ ->
                                                    callApi parentExtrasLens
                                                        (Api.fetchExtras recordId (Just commit_) parentPath)
                                                        |> Flow.map (Result.map (always ()))
                                        )

                                materializeGrid content =
                                    Flow.forAll parentExtrasLens
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
                                            Flow.setAll
                                                (allStepTables << fileDelimitedGridAt recordId path)
                                                mGrid
                                        )
                            in
                            Flow.when (not (shouldSkipFileContents file_))
                                (ensureExtras
                                    |> Flow.andThen
                                        (\_ ->
                                            -- Extras errors are non-blocking; the grid simply
                                            -- falls back to string-typed columns. Always proceed
                                            -- to fetch the file content.
                                            callApi (allStepTables << fileContentAt recordId path)
                                                (Api.fetchFileContents recordId (Just commit_) path)
                                        )
                                    |> FlowError.andThen materializeGrid
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
            Flow.forAll (allStepTables << srcFilesItemAtPath recordId path << folder)
                (\_ ->
                    callApi (allStepTables << srcFilesChildrenAt recordId path)
                        (Api.fetchSrcDirectoryContents ApiDecode.directoryItemGeneric recordId path)
                        |> Flow.return ()
                )

        fileAction =
            Flow.forAll (allStepTables << srcFilesItemAtPath recordId path << file)
                (\file_ ->
                    Flow.when (not (shouldSkipFileContents file_))
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
        fileAnchor =
            Route.highlightAnchor target stepId path

        scrollToRange =
            case mRange of
                Just range ->
                    Flow.attemptTask (Scroll.scrollElementY ("viewer-" ++ fileAnchor) ("line-" ++ fileAnchor ++ "-" ++ String.fromInt range.from) 0.05 0)

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
                    try (Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path)) route_
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
            (Route.project << mHighlight << just << where_ (Route.highlightMatches target recordId path))
            (\highlight -> { highlight | range = Nothing })
        )


clearHighlightedFileOnClose : Route.HighlightTarget -> Int -> List String -> Flow Model ()
clearHighlightedFileOnClose target recordId path =
    let
        shouldClear highlight =
            Route.highlightMatches target recordId path highlight && Maybe.isJust highlight.range
    in
    overRouteReplace
        (over (Route.project << mHighlight) (Maybe.filter (not << shouldClear)))


updateGutterRange : Route.HighlightTarget -> Int -> List String -> Route.LineRange -> Flow Model ()
updateGutterRange target recordId path range =
    overRouteReplace
        (set (Route.project << mHighlight)
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

        changedRecords =
            computeChangedSortRecords records_ allUpdatedRecords

        persistChangedRecords =
            Flow.batchM (List.map (persistRecordChange Nothing tableSpec) changedRecords)
                |> Flow.seq refetchCommitHash
                |> Flow.return (Ok ())
    in
    Flow.setAll (TableSpec.getLens tableSpec << records << success) allUpdatedRecords
        |> Flow.seq
            (case mProjectId of
                Just projectId ->
                    saveProject projectId

                Nothing ->
                    persistChangedRecords
            )
        |> Flow.return ()


onSelectSearch : Maybe Int -> Int -> Flow Model ()
onSelectSearch mProjectId stepId =
    Flow.get
        |> Flow.andThen
            (\model ->
                let
                    mCommit_ =
                        try (route << Route.project << mCommit << just) model

                    pickedProjectId =
                        mProjectId |> Maybe.orElse (try (projectsContainingEntity stepId << recordId << just) model)
                in
                pickedProjectId
                    |> Maybe.unwrap (Flow.pure ()) (\pId -> goToRoute (Project { projectId = pId, mHighlight = Just { id = stepId, target = Route.Output, path = [], range = Nothing }, mCommit = mCommit_ }))
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


closeDialog : String -> Flow Model ()
closeDialog id =
    callJs "closeDialog" Encode.string (Decode.succeed ()) id


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
        Ok (SSESnapshot { commit, steps }) ->
            Flow.batchM (List.map (\s -> updateStepStatus commit s.stepId s.status) steps)

        Ok SSEHeartbeat ->
            Flow.pure ()

        Ok (SSEError err) ->
            addToast False ("SSE Error: " ++ err)

        Err err ->
            addToast False ("SSE Decode Error: " ++ Decode.errorToString err)


updateStepStatus : String -> Int -> Status -> Flow Model ()
updateStepStatus snapshotCommit stepId newStatus =
    let
        stepRunState =
            projects << records << success << each << tables << values << records << success << by .id (Just stepId) << runState

        defaultRunState =
            { commit = snapshotCommit, status = NotAsked, directoryView = { children = NotAsked, expanded = False, extras = NotAsked } }
    in
    Flow.over stepRunState
        (\rs ->
            let
                current =
                    ApiData.withDefault defaultRunState rs
            in
            Success { current | commit = snapshotCommit, status = Success newStatus }
        )
        |> Flow.seq (Flow.when (newStatus == StatusSuccess) (runAndClearStepStatusHook stepId))
