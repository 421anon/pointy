module Model.Lib exposing (..)

import Accessors exposing (all, each, has, over, values)
import Api.ApiData as ApiData exposing (success)
import Components.Select exposing (Item)
import Dict exposing (Dict)
import Model.Core exposing (Model, ProjectRecord, getSortKey)
import Model.Lenses exposing (commitHash, presets, projectStepRecords, projects, records, stepConfig, tables)


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
    all (projects << records << success << each) model
        |> List.concatMap
            (\project ->
                all projectStepRecords project
                    |> List.map
                        (\step ->
                            { id = Just (step.id |> Maybe.withDefault 0)
                            , name = "(" ++ step.type_ ++ ") " ++ step.name ++ " — " ++ project.name
                            , mProjectId = project.id
                            }
                        )
            )


isWorkspaceReloading : Model -> Bool
isWorkspaceReloading model =
    has (projects << records << ApiData.reloading) model
        || has (commitHash << ApiData.reloading) model
        || has (stepConfig << ApiData.reloading) model
        || has (presets << ApiData.reloading) model
