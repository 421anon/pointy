module View.Lib exposing (..)

import Accessors exposing (get)
import Actions
import Api.ApiData as ApiData
import Components.Select as Select
import Dict
import Flow exposing (Flow)
import Fuzzy
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model)
import Model.Lenses as Lenses exposing (searchBox)
import Model.Lib as Lib
import Organize
import Route
import View.Icons exposing (iconCustom)
import View.Organize


viewLoading : Html a -> Html a
viewLoading prevState =
    Html.div [ class "loading-wrapper" ]
        [ prevState
        , Html.div [ class "loading-overlay" ] [ iconCustom True "progress_activity" [ class "loading-icon" ] ]
        ]


viewPage : { header : List (Html a), content : Html a } -> Html a
viewPage { header, content } =
    Html.div [ class "page" ]
        [ Html.div [ class "page-header" ] header
        , Html.div [] [ content ]
        ]


viewSearchBox : Model -> Html (Flow Model ())
viewSearchBox model =
    let
        state =
            get searchBox model

        searchItems =
            if state.active || not (String.isEmpty state.input) then
                Lib.stepSearchItems model

            else
                []

        availableItems =
            List.map Tuple.first searchItems

        documents =
            searchItems
                |> List.filterMap
                    (\( item, document ) ->
                        item.id |> Maybe.map (\id -> ( id, document ))
                    )
                |> Dict.fromList

        toSearchDocument item =
            item.id
                |> Maybe.andThen (\id -> Dict.get id documents)
                |> Maybe.withDefault { fields = [], demoted = False }

        toMenuItemDetail query match item =
            let
                fieldDetail field =
                    case field.tag of
                        Lib.SearchNote ->
                            Just ("Note: " ++ Fuzzy.excerpt query field.text)

                        Lib.SearchArg key ->
                            Just (key ++ ": " ++ Fuzzy.excerpt query field.text)

                        _ ->
                            Nothing

                fallback =
                    let
                        path =
                            folderFor match item
                                |> Maybe.map (Lib.canonicalNamePath model)
                                |> Maybe.withDefault "/unfiled"

                        typeName =
                            item.id
                                |> Maybe.andThen (\id -> Dict.get id (get Lenses.steps model))
                                |> Maybe.map (\step -> Lib.stepTypeName model step.type_)
                                |> Maybe.withDefault ""
                    in
                    typeName ++ " · " ++ path
            in
            List.findMap fieldDetail match.fields
                |> Maybe.withDefault fallback
                |> Just

        folderFor match item =
            Lib.searchFolder match |> Maybe.orElse item.mProjectId

        onSelect match item =
            case item.id of
                Just stepId ->
                    folderFor match item
                        |> Maybe.map (\projectId -> Actions.onSelectSearch projectId stepId)
                        |> Maybe.withDefault (Organize.openLinkExisting stepId)

                Nothing ->
                    Flow.pure ()

        relatedItems =
            Lib.relatedSearchItems model
    in
    Select.view
        { optic = searchBox
        , selectState = state
        , selected_ = []
        , availableItems = availableItems
        , readOnly = False
        , hasChanged = False
        , label = ""
        , mHint = Nothing
        , placeholder = "Search for steps"
        , inputIcon = Just "search"
        , toInputItemName = .name
        , toInputItemTooltip = \_ -> []
        , onInputItemClick = \_ -> Nothing
        , toMenuItemName = .name
        , toMenuItemTooltip = \item -> item.id |> Maybe.map (\id -> [ "id: " ++ String.fromInt id ]) |> Maybe.withDefault []
        , toSearchDocument = toSearchDocument
        , related = relatedItems
        , toMenuItemDetail = toMenuItemDetail
        , onChange = Flow.pure ()
        , onInputChange = Actions.onSearchInputChange
        , onRemove = \_ -> Flow.pure ()
        , activeAfterSelect = False
        , clearInputAfterSelect = False
        , onSelect = onSelect
        , alignRight = True
        , inputItemStyle = \_ -> []
        }


boolText : Bool -> String
boolText value =
    if value then
        "true"

    else
        "false"


statusPresentation : Model.Status -> { className : String, label : String }
statusPresentation status =
    case status of
        Model.StatusNotStarted ->
            { className = "status-not-started", label = "Not Started" }

        Model.StatusRunning ->
            { className = "status-running", label = "Running" }

        Model.StatusSuccess ->
            { className = "status-success", label = "Success" }

        Model.StatusFailure _ ->
            { className = "status-failure", label = "Failure" }

        Model.StatusBuiltNotCertified ->
            { className = "status-built-not-certified", label = "Not Certified" }

        Model.StatusCertificationFailed _ ->
            { className = "status-certification-failed", label = "Certification Failed" }


rollupChildFor : Model -> Maybe Int -> Int -> Maybe Model.RollupChild
rollupChildFor model mParentProjectId folderProjectId =
    mParentProjectId
        |> Maybe.andThen (\parentId -> Dict.get parentId (Model.getProjectRollups model))
        |> Maybe.andThen ApiData.toMaybe
        |> Maybe.andThen (\rollup -> Model.rollupChildById rollup folderProjectId)


viewRollupSummary : Model -> Maybe Int -> Int -> Html msg
viewRollupSummary model mParentProjectId folderProjectId =
    let
        chip status count =
            let
                presentation =
                    statusPresentation status
            in
            Html.span
                [ class "rollup-chip"
                , Html.Attributes.title (presentation.label ++ ": " ++ String.fromInt count)
                ]
                [ Html.span [ class ("status-indicator " ++ presentation.className) ] []
                , Html.text (String.fromInt count)
                ]
    in
    Html.viewMaybe
        (\child ->
            Html.span [ class "rollup-summary" ]
                (List.map (\( status, count ) -> chip status count) (List.filter (\( _, count ) -> count > 0) (Model.rollupStatusEntries child.statuses))
                    ++ [ Html.span [ class "rollup-chip rollup-total", Html.Attributes.title "Total steps" ]
                            [ Html.text (String.fromInt child.steps) ]
                       ]
                )
        )
        (rollupChildFor model mParentProjectId folderProjectId)


viewAlsoInLinks : Model -> Maybe Int -> Model.ChildRef -> List (Html (Flow Model ()))
viewAlsoInLinks model mCurrentParentId ref =
    let
        otherParents =
            Lib.entityOtherParents model ref.kind ref.id mCurrentParentId

        mCommit_ =
            Route.viewedCommit (Model.getRoute model).page

        parentEntry parentId =
            Html.a
                [ Route.href
                    (Route.fromPage
                        (Route.projectPage (Lib.canonicalPathTo model parentId) mCommit_)
                    )
                , class "listing-menu-item"
                , Html.Attributes.title (Lib.canonicalNamePath model parentId)
                ]
                [ iconCustom False "folder" []
                , Html.text (Dict.get parentId (Lenses.projectsDict model) |> Maybe.unwrap ("#" ++ String.fromInt parentId) .name)
                ]
    in
    List.map parentEntry otherParents


viewAlsoInButton : String -> Model -> Maybe Int -> Model.ChildRef -> Html (Flow Model ())
viewAlsoInButton popoverId model mCurrentParentId ref =
    let
        links =
            viewAlsoInLinks model mCurrentParentId ref
    in
    Html.viewIf (not (List.isEmpty links)) <|
        View.Organize.viewMenuPopover
            { popoverId = popoverId
            , wrapperClass = "listing-menu-details"
            , triggerAttrs =
                [ class "icon-btn"
                , Html.Attributes.title "Also in"
                ]
            , triggerContent = [ iconCustom True "account_tree" [] ]
            , content = links
            }
