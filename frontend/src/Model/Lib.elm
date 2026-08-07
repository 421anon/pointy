module Model.Lib exposing (..)

import Accessors exposing (all, each, get, has, over, values)
import Api.ApiData as ApiData exposing (success)
import Components.Select exposing (Item)
import Dict exposing (Dict)
import Dict.Extra
import Model.Core exposing (FileIndexEntry, Model, ProjectRecord, StepRecord, getSortKey)
import Model.Lenses exposing (commitHash, fileIndex, presets, projectStepRecords, projects, records, searchBox, stepConfig, tables)
import Route exposing (HighlightTarget(..))


sortProjects : Dict String ProjectRecord -> List ProjectRecord
sortProjects =
    let
        sort accessor =
            over (accessor << records << success) (List.sortBy getSortKey)
    in
    Dict.values
        >> List.sortBy getSortKey
        >> List.map
            (sort (tables << values))


getSearchItems : Model -> List Item
getSearchItems model =
    let
        search =
            get searchBox model

        filesByStep =
            if not search.active || String.isEmpty (String.trim search.input) then
                Dict.empty

            else
                all (fileIndex << success << each) model
                    |> Dict.Extra.groupBy (\entry -> ( entry.projectId, entry.stepId ))

        stepFiles project step =
            Maybe.withDefault [] (Dict.get ( Maybe.withDefault 0 project.id, Maybe.withDefault 0 step.id ) filesByStep)
    in
    all (projects << records << success << each) model
        |> List.concatMap
            (\project ->
                all projectStepRecords project
                    |> List.concatMap
                        (\step ->
                            { id = Just (step.id |> Maybe.withDefault 0)
                            , name = "(" ++ step.type_ ++ ") " ++ step.name ++ " — " ++ project.name
                            , mProjectId = project.id
                            , mHighlight = Just { id = step.id |> Maybe.withDefault 0, target = Output, path = [], range = Nothing }
                            }
                                :: List.map (fileSearchItem project step) (stepFiles project step)
                        )
            )


fileSearchItem : ProjectRecord -> StepRecord -> FileIndexEntry -> Item
fileSearchItem project step entry =
    { id = Just entry.stepId
    , name = String.join "/" entry.path ++ " — " ++ step.name ++ " — " ++ project.name
    , mProjectId = Just entry.projectId
    , mHighlight = Just { id = entry.stepId, target = entry.target, path = entry.path, range = Nothing }
    }


isWorkspaceReloading : Model -> Bool
isWorkspaceReloading model =
    has (projects << records << ApiData.reloading) model
        || has (commitHash << ApiData.reloading) model
        || has (stepConfig << ApiData.reloading) model
        || has (presets << ApiData.reloading) model
