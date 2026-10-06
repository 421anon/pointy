module Model.Lenses exposing (..)

import Accessors exposing (A_Prism, Lens, Prism, Traversal, all, each, get, just, lens, new, over, prism, set, traversal, try, values)
import Api.ApiData as ApiData exposing (ApiData(..), success)
import Browser.Navigation
import Components.Select exposing (SelectState)
import Debounce exposing (Debounce)
import Dict exposing (Dict)
import Dict.Accessors
import Extra.Accessors exposing (by, orElseT, where_)
import Flow exposing (Flow)
import Http
import Json.Decode exposing (Value)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (AgentLiveTurn, AgentSession, AgentSessionSummary, AgentSessionView, AgentState, ChatEntry, ChildRef, ClusterStatus, CompareActiveData, CompareFile, CompareSelection, CompareState(..), DelimitedGrid, DirectoryFile, DirectoryFolder, DirectoryItem(..), IngestJob, ListingPreferences, ListingSort, Model(..), OrganizeQueue, PendingQuestion, ProjectRecord, ReviewDraft, ScratchState, SessionTimestamp, StepRecord, Table, TemplateSource, UploadProgress, UserRepoInfo)
import Model.Shadow exposing (Presets, StepConfig)
import Route exposing (HighlightTarget(..), Page(..), ProjectParams, Route)
import Set exposing (Set)
import Time
import Toast exposing (Toast)


blackhole : Prism pr s Never x y
blackhole =
    prism "blackhole" never Err


whitehole : Lens ls Never a x y
whitehole =
    lens "whitehole" never always


void : Traversal s a x y
void =
    blackhole << whitehole


currentProject : Lens ls Model (ApiData ProjectRecord) x y
currentProject =
    let
        get_ m =
            case try currentProjectPath m of
                Nothing ->
                    NotAsked

                Just _ ->
                    get projects m
                        |> ApiData.andThenMaybe
                            (\projects_ -> tryProjectPath m |> Maybe.andThen (Model.projectAtPath projects_))
                            (Http.BadUrl "Not a project route")

        tryProjectPath =
            try (route << Route.page << projectRoute << projectPath)

        set (Model m) value =
            case ( m.projects, value ) of
                ( Success projects_, Success project ) ->
                    case project.id of
                        Just projectId ->
                            Model { m | projects = Success (Dict.insert projectId project projects_) }

                        Nothing ->
                            Model m

                _ ->
                    Model m
    in
    lens ".currentProject" get_ set


currentProjectId : Traversal Model Int x y
currentProjectId =
    currentProject << success << recordId << just


currentProjectPath : Traversal Model (List Int) x y
currentProjectPath =
    route << Route.page << projectRoute << projectPath


recordId : Lens ls { a | id : b } b x y
recordId =
    lens ".id" .id (\record id_ -> { record | id = id_ })


stepRevisionById : Int -> Model -> Maybe String
stepRevisionById stepId model =
    try (stepRecordById stepId) model |> Maybe.andThen (Model.stepRevision model)


children : Lens ls { a | children : b } b x y
children =
    lens ".children" .children (\record children_ -> { record | children = children_ })


commitHash : Lens ls Model (ApiData String) x y
commitHash =
    lens ".commitHash" Model.getCommitHash (\(Model m) commitHash_ -> Model { m | commitHash = commitHash_ })


stepConfig : Lens ls Model (ApiData StepConfig) x y
stepConfig =
    lens ".stepConfig" Model.getStepConfig (\(Model m) value -> Model { m | stepConfig = value })


presets : Lens ls Model (ApiData Presets) x y
presets =
    lens ".presets" Model.getPresets (\(Model m) value -> Model { m | presets = value })


commit : Lens ls { a | commit : b } b x y
commit =
    lens "commit" .commit (\t commit_ -> { t | commit = commit_ })


runState : Lens ls { a | runState : b } b x y
runState =
    lens "runState" .runState (\t rs -> { t | runState = rs })


review : Lens ls { a | review : b } b x y
review =
    lens "review" .review (\t r -> { t | review = r })


reviewRevision : Lens ls { a | revision : b } b x y
reviewRevision =
    lens "reviewRevision" .revision (\reviewed revision_ -> { reviewed | revision = revision_ })


isReadOnlyRoute : Model -> Bool
isReadOnlyRoute =
    get route >> .page >> Route.viewedCommit >> Maybe.isJust


route : Lens ls Model Route x y
route =
    lens ".route" Model.getRoute (\(Model m) route_ -> Model { m | route = route_ })


projectRoute : Prism pr Page ProjectParams x y
projectRoute =
    prism ">Project"
        Project
        (\page_ ->
            case page_ of
                Project params ->
                    Ok params

                _ ->
                    Err page_
        )


projectPath : Lens ls { a | projectPath : b } b x y
projectPath =
    lens ".projectPath" .projectPath (\p projectPath_ -> { p | projectPath = projectPath_ })


projects : Lens ls Model (ApiData (Dict Int ProjectRecord)) x y
projects =
    lens ".projects" Model.getProjects (\(Model m) projects_ -> Model { m | projects = projects_ })


store : Lens ls Model ( Dict Int ProjectRecord, Dict Int StepRecord ) x y
store =
    lens ".store"
        (\model -> ( projectsDict model, get steps model ))
        (\model ( projects_, steps_ ) -> set steps steps_ (set projects (ApiData.Success projects_) model))


projectsDict : Model -> Dict Int ProjectRecord
projectsDict =
    get projects >> ApiData.withDefault Dict.empty


projectSteps : Int -> Traversal Model StepRecord x y
projectSteps projectId =
    let
        childStepIds model =
            all (projectRecordById projectId << children << each << where_ (\link -> link.kind == Model.StepChild)) model
                |> List.map .id
    in
    traversal "projectSteps"
        (\model -> List.filterMap (\stepId -> try (stepRecordById stepId) model) (childStepIds model))
        (\fn model -> List.foldl (\stepId acc -> over (stepRecordById stepId) fn acc) model (childStepIds model))


projectRecordById : Int -> Traversal Model ProjectRecord x y
projectRecordById projectId =
    projects << success << Dict.Accessors.id projectId << just


steps : Lens ls Model (Dict Int StepRecord) x y
steps =
    lens ".steps" Model.getSteps (\(Model m) steps_ -> Model { m | steps = steps_ })


projectForms : Lens ls Model (Table ProjectRecord) x y
projectForms =
    lens ".projectForms" Model.getProjectForms (\(Model m) forms -> Model { m | projectForms = forms })


stepForms : Lens ls Model (Dict String (Table StepRecord)) x y
stepForms =
    lens ".stepForms" Model.getStepForms (\(Model m) forms -> Model { m | stepForms = forms })


stepFormsAt : String -> Lens ls Model (Table StepRecord) x y
stepFormsAt typeName =
    stepForms << lens ("[" ++ typeName ++ "]") (Dict.get typeName >> Maybe.withDefault Model.initialTable) (\forms table -> Dict.insert typeName table forms)


listingPreferences : Lens ls Model ListingPreferences x y
listingPreferences =
    lens ".listingPreferences" Model.getListingPreferences (\(Model m) prefs -> Model { m | listingPreferences = prefs })


listingSort : Lens ls ListingPreferences ListingSort x y
listingSort =
    lens ".sort" .sort (\prefs sort_ -> { prefs | sort = sort_ })


listingDescending : Lens ls ListingPreferences Bool x y
listingDescending =
    lens ".descending" .descending (\prefs value -> { prefs | descending = value })


listingFoldersFirst : Lens ls ListingPreferences Bool x y
listingFoldersFirst =
    lens ".foldersFirst" .foldersFirst (\prefs value -> { prefs | foldersFirst = value })


listingShowHidden : Lens ls ListingPreferences Bool x y
listingShowHidden =
    lens ".showHidden" .showHidden (\prefs value -> { prefs | showHidden = value })


listingGroupByType : Lens ls ListingPreferences Bool x y
listingGroupByType =
    lens ".groupByType" .groupByType (\prefs value -> { prefs | groupByType = value })


stepRecordById : Int -> Traversal Model StepRecord x y
stepRecordById stepId =
    steps << Dict.Accessors.id stepId << just


viewedRevision : Traversal Model String x y
viewedRevision =
    orElseT (route << Route.page << Route.viewedCommitT)
        (commitHash << orElseT success ApiData.reloading)


stepShownRevision : Int -> Traversal Model String x y
stepShownRevision stepId =
    orElseT (stepRecordById stepId << runState << success << commit)
        (orElseT (stepRecordById stepId << review << just << reviewRevision) viewedRevision)


selectExistingSteps : Lens ls { a | selectExistingSteps : b } b x y
selectExistingSteps =
    lens ".selectExistingSteps" .selectExistingSteps (\t selectExistingSteps_ -> { t | selectExistingSteps = selectExistingSteps_ })


argSelectStates : Lens ls { a | argSelectStates : b } b x y
argSelectStates =
    lens ".argSelectStates" .argSelectStates (\t argSelectStates_ -> { t | argSelectStates = argSelectStates_ })


isUpdating : Lens ls { a | isUpdating : b } b x y
isUpdating =
    lens ".isUpdating" .isUpdating (\t isUpdating_ -> { t | isUpdating = isUpdating_ })


nameEditOnly : Lens ls { a | nameEditOnly : Bool } Bool x y
nameEditOnly =
    lens ".nameEditOnly" .nameEditOnly (\t value -> { t | nameEditOnly = value })


addMode : Lens ls { a | addMode : b } b x y
addMode =
    lens ".addMode" .addMode (\t addMode_ -> { t | addMode = addMode_ })


mimeType : Lens ls { a | mimeType : b } b x y
mimeType =
    lens ".mimeType" .mimeType (\t mimeType_ -> { t | mimeType = mimeType_ })


stepRecords : Traversal Model StepRecord x y
stepRecords =
    steps << values


stepRecordsListed : Dict Int a -> Traversal Model StepRecord x y
stepRecordsListed statuses =
    stepRecords << where_ (\step -> Maybe.unwrap False (\id -> Dict.member id statuses) step.id)


args : Lens ls { a | args : b } b x y
args =
    lens ".args" .args (\t args_ -> { t | args = args_ })


note : Lens ls { a | note : String } String x y
note =
    lens "note" .note (\t note_ -> { t | note = note_ })


name : Lens ls { a | name : String } String x y
name =
    lens "name" .name (\t name_ -> { t | name = name_ })


status : Lens ls { a | status : b } b x y
status =
    lens "status" .status (\t status_ -> { t | status = status_ })


comparison : Lens ls { a | comparison : b } b x y
comparison =
    lens "comparison" .comparison (\t c -> { t | comparison = c })


sortKey : Lens ls { a | sortKey : b } b x y
sortKey =
    lens ".sortKey" .sortKey (\t sortKey_ -> { t | sortKey = sortKey_ })


isOpen : Lens ls { a | isOpen : b } b x y
isOpen =
    lens ".isOpen" .isOpen (\t isOpen_ -> { t | isOpen = isOpen_ })


edited : Lens ls { a | edited : Maybe b } (Maybe b) x y
edited =
    lens ".edited" .edited (\t record -> { t | edited = record })


drafts : Lens ls { a | drafts : b } b x y
drafts =
    lens ".drafts" .drafts (\t drafts_ -> { t | drafts = drafts_ })


newDraft : Lens ls { a | newDraft : Maybe b } (Maybe b) x y
newDraft =
    lens ".newDraft" .newDraft (\t record -> { t | newDraft = record })


draftAt : Maybe Int -> Lens ls (Table a) (Maybe a) x y
draftAt mId =
    case mId of
        Just rid ->
            drafts << Dict.Accessors.at_ String.fromInt rid

        Nothing ->
            newDraft


stepLogs : Lens ls Model (Dict String (ApiData String)) x y
stepLogs =
    lens ".stepLogs" Model.getStepLogs (\(Model m) stepLogs_ -> Model { m | stepLogs = stepLogs_ })


notices : Lens ls Model (Dict String (ApiData (List Model.Notice))) x y
notices =
    lens ".notices" Model.getNotices (\(Model m) notices_ -> Model { m | notices = notices_ })


userRepoInfo : Lens ls Model (ApiData UserRepoInfo) x y
userRepoInfo =
    lens ".userRepoInfo" Model.getUserRepoInfo (\(Model m) userRepoInfo_ -> Model { m | userRepoInfo = userRepoInfo_ })


autocomplete : Lens ls Model (Dict String Model.AutocompleteState) x y
autocomplete =
    lens ".autocomplete" Model.getAutocomplete (\(Model m) autocomplete_ -> Model { m | autocomplete = autocomplete_ })


suggestions : Lens ls { a | suggestions : b } b x y
suggestions =
    lens ".suggestions" .suggestions (\t suggestions_ -> { t | suggestions = suggestions_ })


autocompleteDebounce : Lens ls Model (Debounce Model.AutocompleteJob) x y
autocompleteDebounce =
    lens ".autocompleteDebounce" Model.getAutocompleteDebounce (\(Model m) value -> Model { m | autocompleteDebounce = value })


nextClientId : Lens ls Model Int x y
nextClientId =
    lens ".nextClientId" Model.getNextClientId (\(Model t) nextClientId_ -> Model { t | nextClientId = nextClientId_ })


folderExpanded : Lens ls { a | expanded : b } b x y
folderExpanded =
    lens ".expanded" .expanded (\folder_ expanded_ -> { folder_ | expanded = expanded_ })


folderExtras : Lens ls { a | extras : b } b x y
folderExtras =
    lens ".extras" .extras (\folder_ extras_ -> { folder_ | extras = extras_ })


fileContent : Lens ls { a | content : b } b x y
fileContent =
    lens ".content" .content (\file_ content_ -> { file_ | content = content_ })


fileEditedContent : Lens ls { a | editedContent : b } b x y
fileEditedContent =
    lens ".editedContent" .editedContent (\file_ editedContent_ -> { file_ | editedContent = editedContent_ })


fileIsViewing : Lens ls { a | view : { b | isViewing : c } } c x y
fileIsViewing =
    lens ".view" .view (\file_ view_ -> { file_ | view = view_ })
        << lens ".isViewing" .isViewing (\view isViewing_ -> { view | isViewing = isViewing_ })


fileZoom : Lens ls { a | view : { b | zoom : c } } c x y
fileZoom =
    lens ".view" .view (\file_ view_ -> { file_ | view = view_ })
        << lens ".zoom" .zoom (\view zoom_ -> { view | zoom = zoom_ })


filePlainScrollTop : Lens ls { a | view : { b | plainScrollTop : c } } c x y
filePlainScrollTop =
    lens ".view" .view (\file_ view_ -> { file_ | view = view_ })
        << lens ".plainScrollTop" .plainScrollTop (\view value -> { view | plainScrollTop = value })


filePlainLineCount : Lens ls { a | plainLineCount : b } b x y
filePlainLineCount =
    lens ".plainLineCount" .plainLineCount (\file_ plainLineCount_ -> { file_ | plainLineCount = plainLineCount_ })


fileSeekWindow : Lens ls { a | seekWindow : b } b x y
fileSeekWindow =
    lens ".seekWindow" .seekWindow (\file_ seekWindow_ -> { file_ | seekWindow = seekWindow_ })


fileDelimitedGrid : Lens ls { a | delimitedGrid : b } b x y
fileDelimitedGrid =
    lens ".delimitedGrid" .delimitedGrid (\file_ delimitedGrid_ -> { file_ | delimitedGrid = delimitedGrid_ })


gridState : Lens ls { a | grid : b } b x y
gridState =
    lens ".grid" .grid (\rec grid_ -> { rec | grid = grid_ })


directoryView : Lens ls { a | directoryView : b } b x y
directoryView =
    lens ".directoryView" .directoryView (\record directoryView_ -> { record | directoryView = directoryView_ })


srcFiles : Lens ls { a | srcFiles : b } b x y
srcFiles =
    lens ".srcFiles" .srcFiles (\record srcFiles_ -> { record | srcFiles = srcFiles_ })


srcFileDraft : Lens ls { a | srcFileDraft : b } b x y
srcFileDraft =
    lens ".srcFileDraft" .srcFileDraft (\record srcFileDraft_ -> { record | srcFileDraft = srcFileDraft_ })


srcFileWriting : Lens ls { a | srcFileWriting : b } b x y
srcFileWriting =
    lens ".srcFileWriting" .srcFileWriting (\record srcFileWriting_ -> { record | srcFileWriting = srcFileWriting_ })


file : Prism pr DirectoryItem DirectoryFile x y
file =
    let
        split item =
            case item of
                File file_ ->
                    Ok file_

                Folder _ ->
                    Err item
    in
    prism ">File" File split


folder : Prism pr DirectoryItem DirectoryFolder x y
folder =
    let
        split item =
            case item of
                Folder folder_ ->
                    Ok folder_

                File _ ->
                    Err item
    in
    prism ">Folder" Folder split


entryAt : String -> Traversal (ApiData (Dict String a)) a x y
entryAt key_ =
    success << Dict.Accessors.at key_ << just


reversePrism : A_Prism pr s a -> Traversal a s x y
reversePrism prism_ =
    traversal (">rev(" ++ Accessors.name prism_ ++ ")")
        (List.singleton << new prism_)
        (\fi o -> try prism_ (fi (new prism_ o)) |> Maybe.withDefault o)


entryAtPath : List String -> Traversal DirectoryFolder DirectoryItem x y
entryAtPath path =
    case path of
        [] ->
            reversePrism folder

        segment :: [] ->
            children << entryAt segment

        segment :: rest ->
            children << entryAt segment << folder << entryAtPath rest


directoryItemAtPath : Int -> List String -> Traversal Model DirectoryItem x y
directoryItemAtPath recordId_ path =
    stepRecordById recordId_ << runState << success << directoryView << entryAtPath path


srcFilesItemAtPath : Int -> List String -> Traversal Model DirectoryItem x y
srcFilesItemAtPath recordId_ path =
    stepRecordById recordId_ << srcFiles << entryAtPath path


directoryItemForTargetAt : HighlightTarget -> Int -> List String -> Traversal Model DirectoryItem x y
directoryItemForTargetAt target recordId_ path =
    case target of
        Output ->
            directoryItemAtPath recordId_ path

        Source ->
            srcFilesItemAtPath recordId_ path


childrenAt : Int -> List String -> Traversal Model (ApiData (Dict String DirectoryItem)) x y
childrenAt recordId_ path =
    directoryItemAtPath recordId_ path << folder << children


extrasAt : Int -> List String -> Traversal Model (ApiData (Dict String Value)) x y
extrasAt recordId_ path =
    directoryItemAtPath recordId_ path << folder << folderExtras


rootExtrasAt : Int -> Traversal Model (ApiData (Dict String Value)) x y
rootExtrasAt recordId_ =
    stepRecordById recordId_ << runState << success << directoryView << folderExtras


folderExpandedAt : Int -> List String -> Traversal Model Bool x y
folderExpandedAt recordId_ path =
    directoryItemAtPath recordId_ path << folder << folderExpanded


fileContentAt : Int -> List String -> Traversal Model (ApiData String) x y
fileContentAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileContent


fileIsViewingAt : Int -> List String -> Traversal Model Bool x y
fileIsViewingAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileIsViewing


fileZoomAt : Int -> List String -> Traversal Model Float x y
fileZoomAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileZoom


fileDelimitedGridAt : Int -> List String -> Traversal Model (Maybe DelimitedGrid) x y
fileDelimitedGridAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileDelimitedGrid


filePlainScrollTopAt : Int -> List String -> Traversal Model Float x y
filePlainScrollTopAt recordId_ path =
    directoryItemAtPath recordId_ path << file << filePlainScrollTop


filePlainLineCountAt : Int -> List String -> Traversal Model Int x y
filePlainLineCountAt recordId_ path =
    directoryItemAtPath recordId_ path << file << filePlainLineCount


fileSeekWindowAt : Int -> List String -> Traversal Model (ApiData Model.SeekWindow) x y
fileSeekWindowAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileSeekWindow


srcFilesChildrenAt : Int -> List String -> Traversal Model (ApiData (Dict String DirectoryItem)) x y
srcFilesChildrenAt recordId_ path =
    srcFilesItemAtPath recordId_ path << folder << children


srcFilesFolderExpandedAt : Int -> List String -> Traversal Model Bool x y
srcFilesFolderExpandedAt recordId_ path =
    srcFilesItemAtPath recordId_ path << folder << folderExpanded


srcFilesFileContentAt : Int -> List String -> Traversal Model (ApiData String) x y
srcFilesFileContentAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileContent


srcFilesFileEditedContentAt : Int -> List String -> Traversal Model (Maybe String) x y
srcFilesFileEditedContentAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileEditedContent


srcFilesFileIsViewingAt : Int -> List String -> Traversal Model Bool x y
srcFilesFileIsViewingAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileIsViewing


srcFilesFileSeekWindowAt : Int -> List String -> Traversal Model (ApiData Model.SeekWindow) x y
srcFilesFileSeekWindowAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileSeekWindow


srcFilesFilePlainScrollTopAt : Int -> List String -> Traversal Model Float x y
srcFilesFilePlainScrollTopAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << filePlainScrollTop


srcFilesFilePlainLineCountAt : Int -> List String -> Traversal Model Int x y
srcFilesFilePlainLineCountAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << filePlainLineCount


seekWindowAt : HighlightTarget -> Int -> List String -> Traversal Model (ApiData Model.SeekWindow) x y
seekWindowAt target recordId_ path =
    case target of
        Output ->
            fileSeekWindowAt recordId_ path

        Source ->
            srcFilesFileSeekWindowAt recordId_ path


plainLineCountAt : HighlightTarget -> Int -> List String -> Traversal Model Int x y
plainLineCountAt target recordId_ path =
    case target of
        Output ->
            filePlainLineCountAt recordId_ path

        Source ->
            srcFilesFilePlainLineCountAt recordId_ path


plainScrollTopAt : HighlightTarget -> Int -> List String -> Traversal Model Float x y
plainScrollTopAt target recordId_ path =
    case target of
        Output ->
            filePlainScrollTopAt recordId_ path

        Source ->
            srcFilesFilePlainScrollTopAt recordId_ path


searchBox : Lens ls Model (SelectState ChildRef) x y
searchBox =
    lens ".searchBox" Model.getSearchBox (\(Model m) searchBox_ -> Model { m | searchBox = searchBox_ })


organizeQueue : Lens ls Model OrganizeQueue x y
organizeQueue =
    lens ".organizeQueue" Model.getOrganizeQueue (\(Model m) queue -> Model { m | organizeQueue = queue })


listingSelection : Lens ls Model (Maybe Model.ListingSelection) x y
listingSelection =
    lens ".listingSelection" Model.getListingSelection (\(Model m) selection -> Model { m | listingSelection = selection })


organizeClipboard : Lens ls Model (Maybe Model.OrganizeClipboard) x y
organizeClipboard =
    lens ".organizeClipboard" Model.getOrganizeClipboard (\(Model m) clipboard -> Model { m | organizeClipboard = clipboard })


organizeDialog : Lens ls Model (Maybe Model.OrganizeDialog) x y
organizeDialog =
    lens ".organizeDialog" Model.getOrganizeDialog (\(Model m) dialog -> Model { m | organizeDialog = dialog })


organizeContextMenu : Lens ls Model (Maybe Model.OrganizeContextMenu) x y
organizeContextMenu =
    lens ".organizeContextMenu" Model.getOrganizeContextMenu (\(Model m) menu -> Model { m | organizeContextMenu = menu })


organizeDrag : Lens ls Model (Maybe Model.OrganizeDrag) x y
organizeDrag =
    lens ".organizeDrag" Model.getOrganizeDrag (\(Model m) drag -> Model { m | organizeDrag = drag })


mCommit : Lens ls { a | mCommit : b } b x y
mCommit =
    lens ".mCommit" .mCommit (\p t -> { p | mCommit = t })


mHighlight : Lens ls { a | mHighlight : b } b x y
mHighlight =
    lens ".mHighlight" .mHighlight (\p t -> { p | mHighlight = t })


uploadProgress : Lens ls Model (Dict Int UploadProgress) x y
uploadProgress =
    lens ".uploadProgress" Model.getUploadProgress (\(Model m) up -> Model { m | uploadProgress = up })


ingestJobs : Lens ls Model (Dict Int IngestJob) x y
ingestJobs =
    lens ".ingestJobs" Model.getIngestJobs (\(Model m) jobs -> Model { m | ingestJobs = jobs })


pendingIngestSteps : Lens ls Model (Set Int) x y
pendingIngestSteps =
    lens ".pendingIngestSteps" Model.getPendingIngestSteps (\(Model m) pending -> Model { m | pendingIngestSteps = pending })


scratch : Lens ls Model ScratchState x y
scratch =
    lens ".scratch" Model.getScratchState (\(Model m) state -> Model { m | scratch = state })


scratchRoot : Lens ls Model (ApiData (Maybe String)) x y
scratchRoot =
    scratch << lens ".root" .root (\state root -> { state | root = root })


scratchListing : Lens ls Model (ApiData Model.ScratchListing) x y
scratchListing =
    scratch << lens ".listing" .listing (\state listing -> { state | listing = listing })


scratchError : Lens ls Model (Maybe String) x y
scratchError =
    scratch << lens ".error" .error (\state error -> { state | error = error })


scratchPickerStepId : Lens ls Model (Maybe Int) x y
scratchPickerStepId =
    scratch << lens ".pickerStepId" .pickerStepId (\state stepId -> { state | pickerStepId = stepId })


stepStatusHooks : Lens ls Model (Dict Int (Flow Model ())) x y
stepStatusHooks =
    lens ".stepStatusHooks" Model.getStepStatusHooks (\(Model m) hooks -> Model { m | stepStatusHooks = hooks })


stepStatusBuffer : Lens ls Model (Dict Int ( String, Model.Status )) x y
stepStatusBuffer =
    lens ".stepStatusBuffer" Model.getStepStatusBuffer (\(Model m) buf -> Model { m | stepStatusBuffer = buf })


pendingBuilds : Lens ls Model (Dict Int String) x y
pendingBuilds =
    lens ".pendingBuilds" Model.getPendingBuilds (\(Model m) builds -> Model { m | pendingBuilds = builds })


pendingStops : Lens ls Model (Set Int) x y
pendingStops =
    lens ".pendingStops" Model.getPendingStops (\(Model m) stops -> Model { m | pendingStops = stops })


openDiff : Lens ls Model (Maybe ( Int, Float )) x y
openDiff =
    lens ".openDiff" Model.getOpenDiff (\(Model m) shown -> Model { m | openDiff = shown })


reviewDraft : Lens ls Model (Maybe ReviewDraft) x y
reviewDraft =
    lens ".reviewDraft" Model.getReviewDraft (\(Model m) draft -> Model { m | reviewDraft = draft })


gutterDrag : Lens ls Model (Maybe Model.GutterDrag) x y
gutterDrag =
    lens ".gutterDrag" Model.getGutterDrag (\(Model m) drag -> Model { m | gutterDrag = drag })


compareState : Lens ls Model CompareState x y
compareState =
    lens ".compareState" Model.getCompareState (\(Model m) s -> Model { m | compareState = s })


compareActive : Prism pr CompareState CompareActiveData x y
compareActive =
    prism ">CompareActive"
        CompareActive
        (\s ->
            case s of
                CompareActive d ->
                    Ok d

                other ->
                    Err other
        )


compareSelecting : Prism pr CompareState CompareSelection x y
compareSelecting =
    prism ">CompareSelecting"
        CompareSelecting
        (\s ->
            case s of
                CompareSelecting selection ->
                    Ok selection

                other ->
                    Err other
        )


compareLeftContent : Lens ls CompareActiveData (ApiData CompareFile) x y
compareLeftContent =
    lens ".leftContent" .leftContent (\d v -> { d | leftContent = v })


compareRightContent : Lens ls CompareActiveData (ApiData CompareFile) x y
compareRightContent =
    lens ".rightContent" .rightContent (\d v -> { d | rightContent = v })


compareLeftInspect : Lens ls CompareActiveData Bool x y
compareLeftInspect =
    lens ".leftInspect" .leftInspect (\d v -> { d | leftInspect = v })


compareRightInspect : Lens ls CompareActiveData Bool x y
compareRightInspect =
    lens ".rightInspect" .rightInspect (\d v -> { d | rightInspect = v })


key : Lens ls Model Browser.Navigation.Key x y
key =
    lens ".key" Model.getKey (\(Model m) key_ -> Model { m | key = key_ })


origin : Lens ls Model String x y
origin =
    lens ".origin" Model.getOrigin (\(Model m) origin_ -> Model { m | origin = origin_ })


agent : Lens ls Model AgentState x y
agent =
    lens ".agent" Model.getAgent (\(Model m) agent_ -> Model { m | agent = agent_ })


sessions : Lens ls AgentState (ApiData (List AgentSessionSummary)) x y
sessions =
    lens ".sessions" .sessions (\s sessions_ -> { s | sessions = sessions_ })


sessionViews : Lens ls AgentState (Dict String (ApiData AgentSessionView)) x y
sessionViews =
    lens ".sessionViews" .sessionViews (\s views -> { s | sessionViews = views })


sessionViewDataAt : String -> Lens ls AgentState (Maybe (ApiData AgentSessionView)) x y
sessionViewDataAt sessionId =
    sessionViews << Dict.Accessors.at sessionId


sessionRenames : Lens ls AgentState (Dict String ( String, SessionTimestamp )) x y
sessionRenames =
    lens ".sessionRenames" .sessionRenames (\s renames -> { s | sessionRenames = renames })


selectedSessionId : Lens ls AgentState (Maybe String) x y
selectedSessionId =
    lens ".selectedSessionId" .selectedSessionId (\s sessionId -> { s | selectedSessionId = sessionId })


session : Lens ls AgentSessionSummary AgentSession x y
session =
    lens ".session" .session (\summary session_ -> { summary | session = session_ })


title : Lens ls AgentSessionSummary String x y
title =
    lens ".title" .title (\summary title_ -> { summary | title = title_ })


sessionName : Lens ls AgentSession (Maybe String) x y
sessionName =
    lens ".sessionName" .sessionName (\session_ name_ -> { session_ | sessionName = name_ })


liveTurns : Lens ls AgentState (Dict String AgentLiveTurn) x y
liveTurns =
    lens ".liveTurns" .liveTurns (\s liveTurns_ -> { s | liveTurns = liveTurns_ })


liveTurnAt : String -> Lens ls AgentState (Maybe AgentLiveTurn) x y
liveTurnAt sessionId =
    liveTurns << Dict.Accessors.at sessionId


turnId : Lens ls { a | turnId : b } b x y
turnId =
    lens ".turnId" .turnId (\t turnId_ -> { t | turnId = turnId_ })


finished : Lens ls { a | finished : b } b x y
finished =
    lens ".finished" .finished (\t finished_ -> { t | finished = finished_ })


entries : Lens ls { a | entries : b } b x y
entries =
    lens ".entries" .entries (\t entries_ -> { t | entries = entries_ })


pendingQuestion : Lens ls { a | pendingQuestion : b } b x y
pendingQuestion =
    lens ".pendingQuestion" .pendingQuestion (\t question -> { t | pendingQuestion = question })


streamError : Lens ls { a | streamError : b } b x y
streamError =
    lens ".streamError" .streamError (\t error -> { t | streamError = error })


pendingSteer : Lens ls { a | pendingSteer : b } b x y
pendingSteer =
    lens ".pendingSteer" .pendingSteer (\t steer -> { t | pendingSteer = steer })


applying : Lens ls { a | applying : b } b x y
applying =
    lens ".applying" .applying (\t applying_ -> { t | applying = applying_ })


autoApply : Lens ls { a | autoApply : b } b x y
autoApply =
    lens ".autoApply" .autoApply (\t autoApply_ -> { t | autoApply = autoApply_ })


sessionAt : String -> Traversal AgentState AgentSessionSummary x y
sessionAt sessionId =
    sessions << orElseT success ApiData.reloading << by (.session >> .sessionId) sessionId


agentSessionRunning : String -> AgentState -> Bool
agentSessionRunning sessionId agentState =
    (try (liveTurnAt sessionId << just << finished) agentState |> Maybe.unwrap False not)
        || (try (sessionAt sessionId) agentState |> Maybe.unwrap False sessionHasRunner)


sessionHasRunner : AgentSessionSummary -> Bool
sessionHasRunner summary =
    (summary.session.activeTurnId /= Nothing) || (summary.session.status == "running")


agentSessionBlocked : String -> AgentState -> Bool
agentSessionBlocked sessionId agentState =
    Model.agentMutationPending agentState || agentSessionRunning sessionId agentState


liveEntries : String -> AgentState -> List ChatEntry
liveEntries sessionId =
    try (liveTurnAt sessionId << just << entries) >> Maybe.withDefault []


sessionEntries : AgentSessionView -> AgentState -> List ChatEntry
sessionEntries view agentState =
    try (liveTurnAt view.session.sessionId << just << entries) agentState
        |> Maybe.withDefault (Model.persistedTranscript view)


agentSessionBlank : AgentSessionSummary -> AgentState -> Bool
agentSessionBlank summary agentState =
    (summary.turnCount == 0)
        && List.isEmpty (liveEntries summary.session.sessionId agentState)
        && (summary.session.sessionName == Nothing)
        && (summary.session.activeTurnId == Nothing)
        && not (Model.agentSessionArchived summary.session.status)


sessionPendingQuestion : AgentSessionView -> AgentState -> Maybe PendingQuestion
sessionPendingQuestion view agentState =
    try (liveTurnAt view.session.sessionId << just << pendingQuestion) agentState
        |> Maybe.withDefault (Model.persistedQuestion view)


sessionPendingSteer : AgentSessionView -> AgentState -> Maybe String
sessionPendingSteer view agentState =
    try (liveTurnAt view.session.sessionId << just << pendingSteer) agentState
        |> Maybe.withDefault Nothing


now : Lens ls Model Time.Posix x y
now =
    lens ".now" Model.getNow (\(Model m) t -> Model { m | now = t })


templateSource : Lens ls { a | templateSource : TemplateSource } TemplateSource x y
templateSource =
    lens ".templateSource" .templateSource (\p t -> { p | templateSource = t })


presetSelect : Lens ls { a | presetSelect : SelectState ChildRef } (SelectState ChildRef) x y
presetSelect =
    lens ".presetSelect" .presetSelect (\p s -> { p | presetSelect = s })


templatesSelect : Lens ls { a | templatesSelect : SelectState ChildRef } (SelectState ChildRef) x y
templatesSelect =
    lens ".templatesSelect" .templatesSelect (\p s -> { p | templatesSelect = s })


clusterStatus : Lens ls Model (ApiData ClusterStatus) x y
clusterStatus =
    lens ".clusterStatus" Model.getClusterStatus (\(Model m) s -> Model { m | clusterStatus = s })


clusterDetail : Lens ls Model (Maybe String) x y
clusterDetail =
    lens ".clusterDetail" Model.getClusterDetail (\(Model m) d -> Model { m | clusterDetail = d })


runningStepIds : Lens ls Model (List Int) x y
runningStepIds =
    lens ".runningStepIds" Model.getRunningStepIds (\(Model m) ids -> Model { m | runningStepIds = ids })


statusBarOpen : Lens ls Model Bool x y
statusBarOpen =
    lens ".statusBarOpen" Model.getStatusBarOpen (\(Model m) open -> Model { m | statusBarOpen = open })


sidebarOpen : Lens ls Model Bool x y
sidebarOpen =
    lens ".sidebarOpen" Model.getSidebarOpen (\(Model m) open -> Model { m | sidebarOpen = open })


sidebarScroll : Lens ls Model Model.SidebarScroll x y
sidebarScroll =
    lens ".sidebarScroll" Model.getSidebarScroll (\(Model m) scroll -> Model { m | sidebarScroll = scroll })


sidebarExpanded : Lens ls Model (Set Int) x y
sidebarExpanded =
    lens ".sidebarExpanded" Model.getSidebarExpanded (\(Model m) expanded -> Model { m | sidebarExpanded = expanded })


projectRollups : Lens ls Model (Dict Int (ApiData Model.ProjectRollup)) x y
projectRollups =
    lens ".projectRollups" Model.getProjectRollups (\(Model m) rollups -> Model { m | projectRollups = rollups })


projectsRequest : Lens ls Model Int x y
projectsRequest =
    lens ".projectsRequest" Model.getProjectsRequest (\(Model m) request -> Model { m | projectsRequest = request })


rollupRequests : Lens ls Model (Dict Int Int) x y
rollupRequests =
    lens ".rollupRequests" Model.getRollupRequests (\(Model m) requests -> Model { m | rollupRequests = requests })


toasts : Lens ls Model (List (Toast (Flow Model ()))) x y
toasts =
    lens ".toasts" Model.getToasts (\(Model t) toasts_ -> Model { t | toasts = toasts_ })


nextToastId : Lens ls Model Int x y
nextToastId =
    lens ".nextToastId" Model.getNextToastId (\(Model t) nextToastId_ -> Model { t | nextToastId = nextToastId_ })


needsIntro : Lens ls { a | needsIntro : b } b x y
needsIntro =
    lens ".needsIntro" .needsIntro (\t needsIntro_ -> { t | needsIntro = needsIntro_ })
