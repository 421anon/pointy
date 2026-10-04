module Model.Lib exposing (..)

import Accessors exposing (get, has, over, try)
import Api.ApiData as ApiData
import Components.Select exposing (Item)
import Dict exposing (Dict)
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildKind(..), ChildLink, ChildRef, Model, ProjectRecord)
import Model.Lenses exposing (commitHash, currentProjectPath, presets, projects, projectsDict, stepConfig, steps)
import Route


canonicalPathTo : Model -> Int -> List Int
canonicalPathTo model targetId =
    Model.canonicalProjectPath
        (try currentProjectPath model |> Maybe.withDefault [])
        (projectsDict model)
        targetId


canonicalNamePath : Model -> Int -> String
canonicalNamePath model projectId =
    let
        projects_ =
            projectsDict model

        names =
            Model.canonicalProjectPath (try currentProjectPath model |> Maybe.withDefault []) projects_ projectId
                |> List.filterMap (\id -> Dict.get id projects_ |> Maybe.map .name)

        rootName =
            Dict.get Route.rootProjectId projects_
                |> Maybe.map .name
                |> Maybe.filter (not << String.isEmpty)
                |> Maybe.withDefault "Home"
    in
    "/" ++ String.join "/" (rootName :: names)


entityParents : Model -> ChildKind -> Int -> List ( Int, ChildLink )
entityParents model kind id =
    Model.childLinksTo kind id (projectsDict model)


entityOtherParents : Model -> ChildKind -> Int -> Maybe Int -> List Int
entityOtherParents model kind id mCurrentParentId =
    entityParents model kind id
        |> List.map Tuple.first
        |> List.filter (\parentId -> Just parentId /= mCurrentParentId)


entityPathLabel : Model -> ChildKind -> Int -> String
entityPathLabel model kind id =
    let
        projects_ =
            projectsDict model

        stepName =
            Dict.get id (get steps model)

        unnamed =
            "#" ++ String.fromInt id

        ( name, typeLabel, path ) =
            case kind of
                ProjectChild ->
                    ( Dict.get id projects_ |> Maybe.map .name |> Maybe.withDefault unnamed
                    , "(folder) "
                    , canonicalNamePath model id
                    )

                StepChild ->
                    ( stepName |> Maybe.map .name |> Maybe.withDefault unnamed
                    , stepName |> Maybe.map (\step -> "(" ++ step.type_ ++ ") ") |> Maybe.withDefault "(step) "
                    , case Model.childLinksTo StepChild id projects_ of
                        [] ->
                            "/unfiled"

                        ( parentId, _ ) :: _ ->
                            canonicalNamePath model parentId
                    )
    in
    typeLabel ++ name ++ " — " ++ path


linkCreatesCycle : Dict Int ProjectRecord -> Int -> ChildRef -> Bool
linkCreatesCycle projects_ targetId ref =
    ref.kind
        == ProjectChild
        && (ref.id == targetId || Model.isAncestorProject projects_ ref.id targetId)


linkValid : Model -> Int -> ChildRef -> Bool
linkValid model targetId ref =
    let
        projects_ =
            projectsDict model
    in
    not (linkCreatesCycle projects_ targetId ref)
        && not (List.any (Model.sameEntity ref) (Dict.get targetId projects_ |> Maybe.unwrap [] .children))


linkCandidates : Model -> Int -> List ( ChildRef, String )
linkCandidates model parentId =
    let
        projects_ =
            projectsDict model

        candidate ref =
            if linkValid model parentId ref then
                Just ( ref, entityPathLabel model ref.kind ref.id )

            else
                Nothing

        stepRefs =
            Dict.keys (get steps model) |> List.map (\id -> { kind = StepChild, id = id })

        projectRefs =
            Dict.keys projects_
                |> List.filter ((/=) Route.rootProjectId)
                |> List.map (\id -> { kind = ProjectChild, id = id })
    in
    List.filterMap candidate (stepRefs ++ projectRefs)


getSearchItems : Model -> List (Item ChildRef)
getSearchItems model =
    let
        projects_ =
            projectsDict model

        projectPathNames =
            Dict.keys projects_
                |> List.map (\projectId -> ( projectId, canonicalNamePath model projectId ))
                |> Dict.fromList

        stepParent =
            Dict.foldl
                (\parentId project acc ->
                    List.foldl
                        (\link acc_ ->
                            if link.kind == StepChild then
                                Dict.insert link.id parentId acc_

                            else
                                acc_
                        )
                        acc
                        project.children
                )
                Dict.empty
                projects_

        pathLabel stepId =
            Dict.get stepId stepParent
                |> Maybe.andThen (\parentId -> Dict.get parentId projectPathNames)
                |> Maybe.withDefault "/unfiled"
    in
    Dict.toList (get steps model)
        |> List.map
            (\( id, step ) ->
                { id = Just id
                , name = "(" ++ step.type_ ++ ") " ++ step.name ++ " — " ++ pathLabel id
                , mProjectId = Nothing
                , ref = Just { kind = StepChild, id = id }
                }
            )


isWorkspaceReloading : Model -> Bool
isWorkspaceReloading model =
    has (projects << ApiData.reloading) model
        || has (commitHash << ApiData.reloading) model
        || has (stepConfig << ApiData.reloading) model
        || has (presets << ApiData.reloading) model


lastKnownWorkspace : Model -> Model
lastKnownWorkspace model =
    over stepConfig ApiData.stopLoading model
        |> over presets ApiData.stopLoading
        |> over projects ApiData.stopLoading
