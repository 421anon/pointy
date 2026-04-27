module Model.Lenses exposing (..)

import Accessors exposing (A_Prism, An_Optic, Lens, Prism, Traversal, each, get, has, just, lens, new, prism, traversal, try, values)
import Api.ApiData as ApiData exposing (ApiData, success)
import Components.Select exposing (SelectState)
import Dict exposing (Dict)
import Dict.Accessors
import Extra.Accessors exposing (by, remkT, where_)
import Flow exposing (Flow)
import Http
import List.Extra as List
import Model.Core as Model exposing (DirectoryFile, DirectoryFolder, DirectoryItem(..), Model(..), ProjectRecord, Status, StepRecord, Table, UploadProgress, UserRepoInfo)
import Model.Shadow exposing (StepConfig)
import Route exposing (ProjectParams, Route(..))
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
        tryProjectId =
            try (route << projectRoute << projectId)

        get_ m =
            get (projects << records) m
                |> ApiData.andThenMaybe (List.find (.id >> (==) (tryProjectId m))) (Http.BadUrl "Not a project route")

        set m project =
            if project.id == tryProjectId m then
                Accessors.set (projects << records << success << by .id project.id) project m

            else
                m
    in
    lens ".currentProject" get_ (\m -> ApiData.unwrap m (set m))


currentProjectId : Traversal Model Int x y
currentProjectId =
    currentProject << success << recordId << just


projectStep : Maybe Int -> Maybe Int -> Traversal Model StepRecord x y
projectStep mPid mRid =
    projects << records << success << by .id mPid << projectStepRecords << where_ (.id >> (==) mRid)


route : Lens ls Model Route x y
route =
    lens ".route" Model.getRoute (\(Model m) route_ -> Model { m | route = route_ })


projectRoute : Prism pr Route ProjectParams x y
projectRoute =
    prism ">Project"
        Project
        (\route_ ->
            case route_ of
                Project params ->
                    Ok params

                _ ->
                    Err route_
        )


projectId : Lens ls { a | projectId : Int } Int x y
projectId =
    lens ".projectId" .projectId (\p projectId_ -> { p | projectId = projectId_ })


projects : Lens ls Model (Table ProjectRecord) x y
projects =
    lens ".projects" Model.getProjects (\(Model m) table -> Model { m | projects = table })


currentTableOf : String -> Traversal Model (Table StepRecord) x y
currentTableOf key =
    currentProject << success << tableInProject key


tableInProject : String -> Traversal ProjectRecord (Table StepRecord) x y
tableInProject key =
    tables << Dict.Accessors.at key << just


args : Lens ls { a | args : b } b x y
args =
    lens ".args" .args (\t args_ -> { t | args = args_ })


records : Lens ls { a | records : b } b x y
records =
    lens ".records" .records (\t records_ -> { t | records = records_ })


edited : Lens ls { a | edited : Maybe b } (Maybe b) x y
edited =
    lens ".edited" .edited (\t record -> { t | edited = record })


stepConfig : Lens ls Model (ApiData StepConfig) x y
stepConfig =
    lens ".stepConfig" Model.getStepConfig (\(Model m) stepConfig_ -> Model { m | stepConfig = stepConfig_ })


commitHash : Lens ls Model (ApiData String) x y
commitHash =
    lens ".commitHash" Model.getCommitHash (\(Model m) commitHash_ -> Model { m | commitHash = commitHash_ })


userRepoInfo : Lens ls Model (ApiData UserRepoInfo) x y
userRepoInfo =
    lens ".userRepoInfo" Model.getUserRepoInfo (\(Model m) userRepoInfo_ -> Model { m | userRepoInfo = userRepoInfo_ })


isOpen : Lens ls { a | isOpen : b } b x y
isOpen =
    lens ".isOpen" .isOpen (\t isOpen_ -> { t | isOpen = isOpen_ })


showHiddenRecords : Lens ls { a | showHiddenRecords : b } b x y
showHiddenRecords =
    lens ".showHiddenRecords" .showHiddenRecords (\t showHiddenRecords_ -> { t | showHiddenRecords = showHiddenRecords_ })


folderExpanded : Lens ls { a | expanded : b } b x y
folderExpanded =
    lens ".expanded" .expanded (\folder_ expanded_ -> { folder_ | expanded = expanded_ })


fileContent : Lens ls { a | content : b } b x y
fileContent =
    lens ".content" .content (\file_ content_ -> { file_ | content = content_ })


fileIsViewing : Lens ls { a | view : { b | isViewing : c } } c x y
fileIsViewing =
    lens ".view" .view (\file_ view_ -> { file_ | view = view_ })
        << lens ".isViewing" .isViewing (\view isViewing_ -> { view | isViewing = isViewing_ })


fileZoom : Lens ls { a | view : { b | zoom : c } } c x y
fileZoom =
    lens ".view" .view (\file_ view_ -> { file_ | view = view_ })
        << lens ".zoom" .zoom (\view zoom_ -> { view | zoom = zoom_ })


children : Lens ls { a | children : b } b x y
children =
    lens ".children" .children (\folder_ children_ -> { folder_ | children = children_ })


recordId : Lens ls { a | id : b } b x y
recordId =
    lens ".id" .id (\record id_ -> { record | id = id_ })


recordById : Int -> Traversal (Table { a | id : Maybe Int }) { a | id : Maybe Int } x y
recordById id_ =
    records << success << by .id (Just id_)


directoryView : Lens ls { a | directoryView : b } b x y
directoryView =
    lens "directoryView" .directoryView (\record directoryView_ -> { record | directoryView = directoryView_ })


srcFiles : Lens ls { a | srcFiles : b } b x y
srcFiles =
    lens "srcFiles" .srcFiles (\record srcFiles_ -> { record | srcFiles = srcFiles_ })


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


entryAt : String -> Traversal (ApiData (Dict String a)) a x y
entryAt key =
    success << Dict.Accessors.at key << just


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
            -- Final segment, just return the item (could be file or folder)
            children << entryAt segment

        segment :: rest ->
            -- More segments, must be a folder to continue
            children << entryAt segment << folder << entryAtPath rest


projectsContainingEntity : Int -> Traversal Model ProjectRecord x y
projectsContainingEntity entityId_ =
    projects << records << success << each << where_ (has (projectStepRecords << where_ (.id >> (==) (Just entityId_))))


recordDirectoryView : Int -> Traversal (Table StepRecord) DirectoryFolder x y
recordDirectoryView recordId_ =
    recordById recordId_ << runState << success << directoryView


directoryItemAtPath : Int -> List String -> Traversal (Table StepRecord) DirectoryItem x y
directoryItemAtPath recordId_ path =
    recordDirectoryView recordId_ << entryAtPath path


fileContentAt : Int -> List String -> Traversal (Table StepRecord) (ApiData String) x y
fileContentAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileContent


fileIsViewingAt : Int -> List String -> Traversal (Table StepRecord) Bool x y
fileIsViewingAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileIsViewing


fileZoomAt : Int -> List String -> Traversal (Table StepRecord) Float x y
fileZoomAt recordId_ path =
    directoryItemAtPath recordId_ path << file << fileZoom


folderExpandedAt : Int -> List String -> Traversal (Table StepRecord) Bool x y
folderExpandedAt recordId_ path =
    directoryItemAtPath recordId_ path << folder << folderExpanded


childrenAt : Int -> List String -> Traversal (Table StepRecord) (ApiData (Dict String DirectoryItem)) x y
childrenAt recordId_ path =
    directoryItemAtPath recordId_ path << folder << children


recordSrcFiles : Int -> Traversal (Table StepRecord) DirectoryFolder x y
recordSrcFiles recordId_ =
    recordById recordId_ << srcFiles


srcFilesItemAtPath : Int -> List String -> Traversal (Table StepRecord) DirectoryItem x y
srcFilesItemAtPath recordId_ path =
    recordSrcFiles recordId_ << entryAtPath path


srcFilesFileContentAt : Int -> List String -> Traversal (Table StepRecord) (ApiData String) x y
srcFilesFileContentAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileContent


srcFilesFileIsViewingAt : Int -> List String -> Traversal (Table StepRecord) Bool x y
srcFilesFileIsViewingAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileIsViewing


srcFilesFileZoomAt : Int -> List String -> Traversal (Table StepRecord) Float x y
srcFilesFileZoomAt recordId_ path =
    srcFilesItemAtPath recordId_ path << file << fileZoom


srcFilesFolderExpandedAt : Int -> List String -> Traversal (Table StepRecord) Bool x y
srcFilesFolderExpandedAt recordId_ path =
    srcFilesItemAtPath recordId_ path << folder << folderExpanded


srcFilesChildrenAt : Int -> List String -> Traversal (Table StepRecord) (ApiData (Dict String DirectoryItem)) x y
srcFilesChildrenAt recordId_ path =
    srcFilesItemAtPath recordId_ path << folder << children


toasts : Lens ls Model (List Toast) x y
toasts =
    lens ".toasts" Model.getToasts (\(Model t) toasts_ -> Model { t | toasts = toasts_ })


nextToastId : Lens ls Model Int x y
nextToastId =
    lens ".nextToastId" Model.getNextToastId (\(Model t) nextToastId_ -> Model { t | nextToastId = nextToastId_ })


nextClientId : Lens ls Model Int x y
nextClientId =
    lens ".nextClientId" Model.getNextClientId (\(Model t) nextClientId_ -> Model { t | nextClientId = nextClientId_ })


name : Lens ls { a | name : String } String x y
name =
    lens "name" .name (\t name_ -> { t | name = name_ })


note : Lens ls { a | note : String } String x y
note =
    lens "note" .note (\t note_ -> { t | note = note_ })


outPath : Lens ls { a | outPath : b } b x y
outPath =
    lens "outPath" .outPath (\t outPath_ -> { t | outPath = outPath_ })


status : Lens ls { a | status : b } b x y
status =
    lens "status" .status (\t status_ -> { t | status = status_ })


runState : Lens ls { a | runState : b } b x y
runState =
    lens "runState" .runState (\t rs -> { t | runState = rs })


statusAt : An_Optic pr ls Model (Table StepRecord) -> Int -> Traversal Model (ApiData Status) x y
statusAt lens recordId_ =
    remkT lens << recordById recordId_ << runState << success << status


projectStepRecords : Traversal ProjectRecord StepRecord x y
projectStepRecords =
    tables << values << records << success << each


sortKey : Lens ls { a | sortKey : b } b x y
sortKey =
    lens ".sortKey" .sortKey (\t sortKey_ -> { t | sortKey = sortKey_ })


dnd : Lens ls { a | dnd : b } b x y
dnd =
    lens ".dnd" .dnd (\t dnd_ -> { t | dnd = dnd_ })


dndAffected : Lens ls { a | dndAffected : b } b x y
dndAffected =
    lens ".dndAffected" .dndAffected (\t dndAffected_ -> { t | dndAffected = dndAffected_ })


allEntities : An_Optic pr ls ProjectRecord (Table a) -> Traversal Model a x y
allEntities entities =
    projects << records << success << each << remkT entities << records << success << each


selectExistingSteps : Lens ls { a | selectExistingSteps : b } b x y
selectExistingSteps =
    lens ".selectExistingSteps" .selectExistingSteps (\t selectExistingSteps_ -> { t | selectExistingSteps = selectExistingSteps_ })


argSelectStates : Lens ls { a | argSelectStates : b } b x y
argSelectStates =
    lens ".argSelectStates" .argSelectStates (\t argSelectStates_ -> { t | argSelectStates = argSelectStates_ })


isUpdating : Lens ls { a | isUpdating : b } b x y
isUpdating =
    lens ".isUpdating" .isUpdating (\t isUpdating_ -> { t | isUpdating = isUpdating_ })


clientId : Lens ls { a | clientId : b } b x y
clientId =
    lens ".clientId" .clientId (\t clientId_ -> { t | clientId = clientId_ })


addMode : Lens ls { a | addMode : b } b x y
addMode =
    lens ".addMode" .addMode (\t addMode_ -> { t | addMode = addMode_ })


mimeType : Lens ls { a | mimeType : b } b x y
mimeType =
    lens ".mimeType" .mimeType (\t mimeType_ -> { t | mimeType = mimeType_ })


searchBox : Lens ls Model SelectState x y
searchBox =
    lens ".searchBox" Model.getSearchBox (\(Model m) searchBox_ -> Model { m | searchBox = searchBox_ })


tables : Lens ls { a | tables : b } b x y
tables =
    lens ".tables" .tables (\p t -> { p | tables = t })


mCommit : Lens ls { a | mCommit : b } b x y
mCommit =
    lens ".mCommit" .mCommit (\p t -> { p | mCommit = t })


mHighlight : Lens ls { a | mHighlight : b } b x y
mHighlight =
    lens ".mHighlight" .mHighlight (\p t -> { p | mHighlight = t })


uploadProgress : Lens ls Model (Dict Int UploadProgress) x y
uploadProgress =
    lens ".uploadProgress" Model.getUploadProgress (\(Model m) up -> Model { m | uploadProgress = up })


stepStatusHooks : Lens ls Model (Dict Int (Flow Model ())) x y
stepStatusHooks =
    lens ".stepStatusHooks" Model.getStepStatusHooks (\(Model m) hooks -> Model { m | stepStatusHooks = hooks })


lastSuccesses : Lens ls Model (Dict Int (ApiData (List Model.LastSuccess))) x y
lastSuccesses =
    lens ".lastSuccesses" Model.getLastSuccesses (\(Model m) ls -> Model { m | lastSuccesses = ls })
