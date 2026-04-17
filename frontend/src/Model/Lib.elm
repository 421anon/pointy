module Model.Lib exposing (..)

import Accessors exposing (all, each, over, values)
import Api.ApiData exposing (success)
import Components.Select exposing (Item)
import Dict exposing (Dict)
import Model.Core exposing (Model, ProjectRecord, getSortKey)
import Model.Lenses exposing (projectStepRecords, projects, records, tables)


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
                            }
                        )
            )
