module Model.Lib exposing (..)

import Accessors exposing (get, has, over, set, try)
import Api.ApiData as ApiData
import Components.Select as Select exposing (Item)
import Dict exposing (Dict)
import Fuzzy
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (ChildKind(..), ChildLink, ChildRef, Model, ProjectRecord, SearchField(..))
import Model.Lenses exposing (commitHash, currentProject, currentProjectPath, loadedProjects, presets, projects, relatedSearch, searchBox, stepConfig, stepSearch, steps)
import Model.Shadow exposing (StepArgValue(..))
import Route
import Set exposing (Set)
import Time


canonicalPathTo : Model -> Dict Int ProjectRecord -> Int -> List Int
canonicalPathTo model projects_ targetId =
    Model.canonicalProjectPath
        (try currentProjectPath model |> Maybe.withDefault [])
        projects_
        targetId


canonicalPathNames : Model -> Dict Int ProjectRecord -> Int -> List String
canonicalPathNames model projects_ projectId =
    canonicalPathTo model projects_ projectId
        |> List.filterMap (\id -> Dict.get id projects_ |> Maybe.map .name)


rootProjectName : Dict Int ProjectRecord -> String
rootProjectName projects_ =
    Dict.get Route.rootProjectId projects_
        |> Maybe.map .name
        |> Maybe.filter (not << String.isEmpty)
        |> Maybe.withDefault "Home"


canonicalNamePath : Model -> Dict Int ProjectRecord -> Int -> String
canonicalNamePath model projects_ projectId =
    "/" ++ String.join "/" (rootProjectName projects_ :: canonicalPathNames model projects_ projectId)


entityOtherParents : Dict Int ProjectRecord -> ChildKind -> Int -> Maybe Int -> List Int
entityOtherParents projects_ kind id mCurrentParentId =
    Model.childLinksTo kind id projects_
        |> List.map Tuple.first
        |> List.filter (\parentId -> Just parentId /= mCurrentParentId)


currentProjectStepIds : Model -> Set Int
currentProjectStepIds model =
    get currentProject model
        |> ApiData.toMaybe
        |> Maybe.unwrap Set.empty (Model.childIdsOf StepChild >> Set.fromList)


entityPathLabel : Model -> Dict Int ProjectRecord -> ChildKind -> Int -> String
entityPathLabel model projects_ kind id =
    let
        stepName =
            Dict.get id (get steps model)

        unnamed =
            "#" ++ String.fromInt id

        ( name, typeLabel, mPath ) =
            case kind of
                ProjectChild ->
                    ( Dict.get id projects_ |> Maybe.map .name |> Maybe.withDefault unnamed
                    , "(folder) "
                    , Just (canonicalNamePath model projects_ id)
                    )

                StepChild ->
                    ( stepName |> Maybe.map .name |> Maybe.withDefault unnamed
                    , stepName |> Maybe.map (\step -> "(" ++ step.type_ ++ ") ") |> Maybe.withDefault "(step) "
                    , Model.childLinksTo StepChild id projects_
                        |> List.head
                        |> Maybe.map (\( parentId, _ ) -> canonicalNamePath model projects_ parentId)
                    )
    in
    typeLabel ++ name ++ Maybe.unwrap "" (\path -> " — " ++ path) mPath


linkCreatesCycle : Dict Int ProjectRecord -> Int -> ChildRef -> Bool
linkCreatesCycle projects_ targetId ref =
    ref.kind
        == ProjectChild
        && (ref.id == targetId || Model.isAncestorProject projects_ ref.id targetId)


linkValid : Dict Int ProjectRecord -> Int -> ChildRef -> Bool
linkValid projects_ targetId ref =
    not (linkCreatesCycle projects_ targetId ref)
        && not (List.any (Model.sameEntity ref) (Dict.get targetId projects_ |> Maybe.unwrap [] .children))


linkCandidates : Model -> Dict Int ProjectRecord -> Int -> List ( ChildRef, String )
linkCandidates model projects_ parentId =
    let
        candidate ref =
            if linkValid projects_ parentId ref then
                Just ( ref, entityPathLabel model projects_ ref.kind ref.id )

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


stepSearchItems : Model -> Dict Int ProjectRecord -> List ( Item ChildRef, Fuzzy.Document SearchField )
stepSearchItems model projects_ =
    let
        steps_ =
            get steps model

        pathNamesByProject =
            Dict.keys projects_
                |> List.map (\projectId -> ( projectId, canonicalPathNames model projects_ projectId ))
                |> Dict.fromList

        stepParents =
            stepParentLinks projects_

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


refreshStepSearch : Model -> Model
refreshStepSearch model =
    let
        entries =
            case try loadedProjects model of
                Just projects_ ->
                    let
                        cached =
                            (get stepSearch model).entries
                                |> List.filterMap (\( item, indexed ) -> item.id |> Maybe.map (\id -> ( id, indexed )))
                                |> Dict.fromList
                    in
                    stepSearchItems model projects_
                        |> List.map
                            (\( item, document ) ->
                                ( item
                                , Fuzzy.reindex (item.id |> Maybe.andThen (\id -> Dict.get id cached)) document
                                )
                            )

                Nothing ->
                    []
    in
    rankStepSearch (over stepSearch (\state -> { state | entries = entries }) model)


rankStepSearch : Model -> Model
rankStepSearch model =
    let
        input =
            (get searchBox model).input

        query =
            Fuzzy.parse input

        state =
            get stepSearch model

        matches =
            case ( Fuzzy.isEmpty query, try loadedProjects model ) of
                ( False, Just projects_ ) ->
                    Select.rankedItems query Tuple.second state.entries
                        |> List.map
                            (\( ( item, _ ), match ) ->
                                { item = item
                                , match = match
                                , detail = Just (stepSearchDetail model projects_ query match item)
                                }
                            )

                _ ->
                    []
    in
    set stepSearch { state | input = input, matches = matches } model


searchSteps : Model -> Model
searchSteps model =
    if Fuzzy.isEmpty (Fuzzy.parse (get stepSearch model).input) then
        refreshStepSearch model

    else
        rankStepSearch model


warmStepSearch : Model -> Model
warmStepSearch model =
    let
        state =
            get stepSearch model
    in
    if List.isEmpty state.entries || not (Fuzzy.isEmpty (Fuzzy.parse state.input)) then
        refreshStepSearch model

    else
        model


stepSearchFolder : Fuzzy.Match SearchField -> Item ChildRef -> Maybe Int
stepSearchFolder match item =
    searchFolder match |> Maybe.orElse item.mProjectId


stepSearchDetail : Model -> Dict Int ProjectRecord -> Fuzzy.Query -> Fuzzy.Match SearchField -> Item ChildRef -> String
stepSearchDetail model projects_ query match item =
    let
        fieldDetail field =
            case field.tag of
                SearchNote ->
                    Just ("Note: " ++ Fuzzy.excerpt query field.text)

                SearchArg key ->
                    Just (key ++ ": " ++ Fuzzy.excerpt query field.text)

                _ ->
                    Nothing

        fallback () =
            let
                path =
                    stepSearchFolder match item
                        |> Maybe.unwrap "" (\projectId -> " · " ++ canonicalNamePath model projects_ projectId)

                typeName =
                    item.id
                        |> Maybe.andThen (\id -> Dict.get id (get steps model))
                        |> Maybe.unwrap "" (\step -> stepTypeName model step.type_)
            in
            typeName ++ path
    in
    List.findMap fieldDetail match.fields
        |> Maybe.withDefaultLazy fallback


stepParentLinks : Dict Int ProjectRecord -> Dict Int (List ( Int, ChildLink ))
stepParentLinks projects_ =
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
        projects_
        |> Dict.map (\_ -> List.reverse)


stepParentProjectId : Dict Int ProjectRecord -> Int -> Maybe Int
stepParentProjectId projects_ stepId =
    Dict.get stepId (stepParentLinks projects_)
        |> Maybe.andThen
            (\links ->
                (List.find (not << .hidden << Tuple.second) links |> Maybe.orElse (List.head links))
                    |> Maybe.map Tuple.first
            )


relatedSearchItems : Model -> Dict Int ProjectRecord -> List (Select.RelatedItem ChildRef)
relatedSearchItems model projects_ =
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
                    (stepParentProjectId projects_ related.stepId)
            )


flattenArgValue : Dict Int Model.StepRecord -> StepArgValue -> List String
flattenArgValue steps_ value =
    case value of
        TStringValue text ->
            [ text ]

        TEnumValue text ->
            [ text ]

        TStepValue ref ->
            Dict.get ref.step steps_ |> Maybe.map .name |> Maybe.toList

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
