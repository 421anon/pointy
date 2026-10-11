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

        searching =
            state.active && not (Fuzzy.isEmpty (Fuzzy.parse state.input))

        searchEntries =
            if searching then
                Lib.stepSearchEntries model

            else
                []

        availableItems =
            List.map Tuple.first searchEntries

        indexes =
            searchEntries
                |> List.filterMap
                    (\( item, indexed ) ->
                        item.id |> Maybe.map (\id -> ( id, indexed ))
                    )
                |> Dict.fromList

        unindexed =
            Fuzzy.index { fields = [], demoted = False }

        toSearchDocument item =
            item.id
                |> Maybe.andThen (\id -> Dict.get id indexes)
                |> Maybe.withDefault unindexed

        toMenuItemDetail query match item =
            let
                fieldDetail field =
                    case field.tag of
                        Model.SearchNote ->
                            Just ("Note: " ++ Fuzzy.excerpt query field.text)

                        Model.SearchArg key ->
                            Just (key ++ ": " ++ Fuzzy.excerpt query field.text)

                        _ ->
                            Nothing

                fallback =
                    let
                        path =
                            folderFor match item
                                |> Maybe.unwrap "" (\projectId -> " · " ++ Lib.canonicalNamePath model projectId)

                        typeName =
                            item.id
                                |> Maybe.andThen (\id -> Dict.get id (get Lenses.steps model))
                                |> Maybe.map (\step -> Lib.stepTypeName model step.type_)
                                |> Maybe.withDefault ""
                    in
                    typeName ++ path
            in
            List.findMap fieldDetail match.fields
                |> Maybe.withDefault fallback
                |> Just

        folderFor match item =
            Lib.searchFolder match |> Maybe.orElse item.mProjectId

        onSelect match item =
            Maybe.map2 Actions.onSelectSearch (folderFor match item) item.id
                |> Maybe.withDefault (Flow.pure ())

        relatedItems =
            if searching then
                Lib.relatedSearchItems model

            else
                []

        statusNotice =
            if searching then
                case (get Lenses.relatedSearch model).results of
                    ApiData.Loading _ ->
                        Just "Looking for related steps…"

                    ApiData.Error _ ->
                        Just "Related steps are unavailable right now"

                    _ ->
                        Nothing

            else
                Nothing
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
        , statusNotice = statusNotice
        , emptyNotice =
            if searching then
                Just "No matching steps"

            else
                Nothing
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
