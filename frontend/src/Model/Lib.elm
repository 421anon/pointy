module Model.Lib exposing (..)

import Accessors exposing (all, each, get, has, over, try, values)
import Api.ApiData as ApiData exposing (success)
import Components.Select exposing (Item)
import Dict exposing (Dict)
import Model.Core as Model exposing (Model, ProjectRecord, getSortKey)
import Model.Lenses exposing (commitHash, currentProjectPath, presets, projectStepRecords, projects, records, stepConfig, subProjects, tables)


linkProjects : Dict String ProjectRecord -> List ProjectRecord
linkProjects projectsByKey =
    let
        decoded =
            Dict.values projectsByKey

        projectsById =
            decoded
                |> List.filterMap (\project -> Maybe.map (\id -> ( id, project )) project.id)
                |> Dict.fromList

        linkEntry entry =
            entry.id
                |> Maybe.andThen (\id -> Dict.get id projectsById)
                |> Maybe.map (\child -> { child | hidden = entry.hidden, sortKey = entry.sortKey })

        sort accessor =
            over (accessor << records << success) (List.sortBy getSortKey)
    in
    decoded
        |> List.sortBy getSortKey
        |> List.map
            (over (subProjects << records << success) (List.filterMap linkEntry)
                >> sort (tables << values)
                >> sort subProjects
            )


canonicalPathTo : Model -> Int -> List Int
canonicalPathTo model =
    Model.canonicalProjectPath
        (try currentProjectPath model |> Maybe.withDefault [])
        (get (projects << records) model |> ApiData.toMaybe |> Maybe.withDefault [])


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


lastKnownWorkspace : Model -> Model
lastKnownWorkspace model =
    over stepConfig ApiData.stopLoading model
        |> over presets ApiData.stopLoading
        |> over (projects << records) ApiData.stopLoading
