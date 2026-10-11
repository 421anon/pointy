module Model.Lib exposing (..)

import Accessors exposing (get, has, over, try)
import Api.ApiData as ApiData
import Components.Select as Select exposing (Item)
import Dict exposing (Dict)
import Fuzzy
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildKind(..), ChildLink, ChildRef, Model, ProjectRecord)
import Model.Lenses exposing (commitHash, currentProject, currentProjectPath, presets, projects, projectsDict, relatedSearch, stepConfig, steps)
import Model.Shadow exposing (StepArgValue(..))
import Route
import Set exposing (Set)
import Time


canonicalPathTo : Model -> Int -> List Int
canonicalPathTo model targetId =
    Model.canonicalProjectPath
        (try currentProjectPath model |> Maybe.withDefault [])
        (projectsDict model)
        targetId


canonicalPathNames : Model -> Int -> List String
canonicalPathNames model projectId =
    let
        projects_ =
            projectsDict model
    in
    Model.canonicalProjectPath (try currentProjectPath model |> Maybe.withDefault []) projects_ projectId
        |> List.filterMap (\id -> Dict.get id projects_ |> Maybe.map .name)


rootProjectName : Model -> String
rootProjectName model =
    Dict.get Route.rootProjectId (projectsDict model)
        |> Maybe.map .name
        |> Maybe.filter (not << String.isEmpty)
        |> Maybe.withDefault "Home"


canonicalNamePath : Model -> Int -> String
canonicalNamePath model projectId =
    "/" ++ String.join "/" (rootProjectName model :: canonicalPathNames model projectId)


entityParents : Model -> ChildKind -> Int -> List ( Int, ChildLink )
entityParents model kind id =
    Model.childLinksTo kind id (projectsDict model)


entityOtherParents : Model -> ChildKind -> Int -> Maybe Int -> List Int
entityOtherParents model kind id mCurrentParentId =
    entityParents model kind id
        |> List.map Tuple.first
        |> List.filter (\parentId -> Just parentId /= mCurrentParentId)


currentProjectStepIds : Model -> Set Int
currentProjectStepIds model =
    get currentProject model
        |> ApiData.toMaybe
        |> Maybe.unwrap Set.empty (Model.childIdsOf StepChild >> Set.fromList)


entityPathLabel : Model -> ChildKind -> Int -> String
entityPathLabel model kind id =
    let
        projects_ =
            projectsDict model

        stepName =
            Dict.get id (get steps model)

        unnamed =
            "#" ++ String.fromInt id

        ( name, typeLabel, mPath ) =
            case kind of
                ProjectChild ->
                    ( Dict.get id projects_ |> Maybe.map .name |> Maybe.withDefault unnamed
                    , "(folder) "
                    , Just (canonicalNamePath model id)
                    )

                StepChild ->
                    ( stepName |> Maybe.map .name |> Maybe.withDefault unnamed
                    , stepName |> Maybe.map (\step -> "(" ++ step.type_ ++ ") ") |> Maybe.withDefault "(step) "
                    , Model.childLinksTo StepChild id projects_
                        |> List.head
                        |> Maybe.map (\( parentId, _ ) -> canonicalNamePath model parentId)
                    )
    in
    typeLabel ++ name ++ Maybe.unwrap "" (\path -> " — " ++ path) mPath


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


type SearchField
    = SearchName
    | SearchId
    | SearchType
    | SearchFolder Int
    | SearchNote
    | SearchArg String


stepTypeName : Model -> String -> String
stepTypeName model type_ =
    get stepConfig model
        |> ApiData.toMaybe
        |> Maybe.andThen (Dict.get type_)
        |> Maybe.andThen .displayName
        |> Maybe.withDefault type_


searchFolder : Fuzzy.Match SearchField -> Maybe Int
searchFolder match =
    List.findMap
        (\field ->
            case field.tag of
                SearchFolder projectId ->
                    Just projectId

                _ ->
                    Nothing
        )
        match.fields


stepSearchItems : Model -> List ( Item ChildRef, Fuzzy.Document SearchField )
stepSearchItems model =
    let
        projects_ =
            projectsDict model

        steps_ =
            get steps model

        pathNamesByProject =
            Dict.keys projects_
                |> List.map (\projectId -> ( projectId, canonicalPathNames model projectId ))
                |> Dict.fromList

        stepParents =
            stepParentLinks model

        folderName projectId =
            Dict.get projectId projects_ |> Maybe.unwrap "" .name

        ancestorNames projectId =
            Dict.get projectId pathNamesByProject
                |> Maybe.andThen List.init
                |> Maybe.withDefault []

        folderFields parents =
            List.concatMap
                (\( parentId, _ ) ->
                    if parentId == Route.rootProjectId then
                        []

                    else
                        let
                            ancestors =
                                ancestorNames parentId
                        in
                        [ { tag = SearchFolder parentId, text = folderName parentId, weight = 50, precision = Fuzzy.Loose } ]
                            ++ (if List.isEmpty ancestors then
                                    []

                                else
                                    [ { tag = SearchFolder parentId, text = String.join " " ancestors, weight = 40, precision = Fuzzy.Words } ]
                               )
                )
                parents

        argFields args =
            Dict.toList args
                |> List.concatMap
                    (\( key, value ) ->
                        List.map
                            (\text -> { tag = SearchArg key, text = text, weight = 25, precision = Fuzzy.Words })
                            (flattenArgValue steps_ value)
                    )

        documentFor parents id step =
            let
                type_ =
                    step.type_

                typeName =
                    stepTypeName model type_

                typeText =
                    if typeName == type_ then
                        type_

                    else
                        typeName ++ " " ++ type_

                noteText =
                    String.join " " (String.words step.note)

                noteFields =
                    if String.isEmpty noteText then
                        []

                    else
                        [ { tag = SearchNote, text = noteText, weight = 35, precision = Fuzzy.Words } ]
            in
            { fields =
                [ { tag = SearchName, text = step.name, weight = 100, precision = Fuzzy.Loose }
                , { tag = SearchId, text = String.fromInt id, weight = 120, precision = Fuzzy.Whole }
                , { tag = SearchType, text = typeText, weight = 70, precision = Fuzzy.Loose }
                ]
                    ++ folderFields parents
                    ++ noteFields
                    ++ argFields step.args
            , demoted = List.all (Tuple.second >> .hidden) parents
            }

        itemFor parents id step =
            { id = Just id
            , name = step.name
            , mProjectId =
                List.find (not << .hidden << Tuple.second) parents
                    |> Maybe.orElse (List.head parents)
                    |> Maybe.map Tuple.first
            , ref = Just { kind = StepChild, id = id }
            }
    in
    Dict.toList steps_
        |> List.sortBy (\( id, step ) -> ( Maybe.unwrap 0 (Time.posixToMillis >> negate) step.lastModifiedAt, String.length step.name, id ))
        |> List.filterMap
            (\( id, step ) ->
                Dict.get id stepParents
                    |> Maybe.map (\parents -> ( itemFor parents id step, documentFor parents id step ))
            )


stepParentLinks : Model -> Dict Int (List ( Int, ChildLink ))
stepParentLinks model =
    Dict.foldl
        (\parentId project acc ->
            List.foldl
                (\link acc_ ->
                    if link.kind == StepChild then
                        Dict.update link.id
                            (\existing -> Just (( parentId, link ) :: Maybe.withDefault [] existing))
                            acc_

                    else
                        acc_
                )
                acc
                project.children
        )
        Dict.empty
        (projectsDict model)
        |> Dict.map (\_ -> List.reverse)


stepParentProjectId : Model -> Int -> Maybe Int
stepParentProjectId model stepId =
    Dict.get stepId (stepParentLinks model)
        |> Maybe.andThen
            (\links ->
                (List.find (not << .hidden << Tuple.second) links |> Maybe.orElse (List.head links))
                    |> Maybe.map Tuple.first
            )


relatedSearchItems : Model -> List (Select.RelatedItem ChildRef)
relatedSearchItems model =
    get relatedSearch model
        |> .results
        |> ApiData.toMaybe
        |> Maybe.withDefault []
        |> List.filterMap
            (\related ->
                Maybe.map2
                    (\step projectId ->
                        { item =
                            { id = Just related.stepId
                            , name = step.name
                            , mProjectId = Just projectId
                            , ref = Just { kind = StepChild, id = related.stepId }
                            }
                        , detail = related.detail
                        }
                    )
                    (Dict.get related.stepId (get steps model))
                    (stepParentProjectId model related.stepId)
            )


flattenArgValue : Dict Int Model.StepRecord -> StepArgValue -> List String
flattenArgValue steps_ value =
    case value of
        TStringValue text ->
            [ text ]

        TEnumValue text ->
            [ text ]

        TStepValue stepId ->
            Dict.get stepId steps_ |> Maybe.map .name |> Maybe.toList

        TListValue items ->
            List.concatMap (flattenArgValue steps_) items

        TRecordValue record ->
            List.concatMap (flattenArgValue steps_) (Dict.values record)

        TIntValue _ ->
            []

        TBoolValue _ ->
            []

        TUploadHashValue _ ->
            []


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
