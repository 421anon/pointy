module View.Lib exposing (..)

import Accessors exposing (get)
import Actions
import Api.ApiData as ApiData
import Components.Select as Select
import Dict
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
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

        availableItems =
            if state.active || not (String.isEmpty state.input) then
                Lib.getSearchItems model

            else
                []
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
        , toMenuItemName = \item -> item.id |> Maybe.map (\id -> "[" ++ String.fromInt id ++ "] " ++ item.name) |> Maybe.withDefault item.name
        , toMenuItemTooltip = \_ -> []
        , onChange = Flow.pure ()
        , onRemove = \_ -> Flow.pure ()
        , activeAfterSelect = False
        , clearInputAfterSelect = False
        , onSelect =
            \item ->
                Maybe.unwrap (Flow.pure ())
                    (Actions.onSelectSearch item.mProjectId)
                    item.id
        , alignRight = True
        , inputItemStyle = \_ -> []
        }


boolText : Bool -> String
boolText value =
    if value then
        "true"

    else
        "false"


statusIndicatorClass : Model.Status -> String
statusIndicatorClass status =
    case status of
        Model.StatusNotStarted ->
            "status-not-started"

        Model.StatusRunning ->
            "status-running"

        Model.StatusSuccess ->
            "status-success"

        Model.StatusFailure _ ->
            "status-failure"

        Model.StatusBuiltNotCertified ->
            "status-built-not-certified"

        Model.StatusCertificationFailed _ ->
            "status-certification-failed"


statusLabel : Model.Status -> String
statusLabel status =
    case status of
        Model.StatusNotStarted ->
            "Not Started"

        Model.StatusRunning ->
            "Running"

        Model.StatusSuccess ->
            "Success"

        Model.StatusFailure _ ->
            "Failure"

        Model.StatusBuiltNotCertified ->
            "Not Certified"

        Model.StatusCertificationFailed _ ->
            "Certification Failed"


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
            Html.span
                [ class "rollup-chip"
                , Html.Attributes.title (statusLabel status ++ ": " ++ String.fromInt count)
                ]
                [ Html.span [ class ("status-indicator " ++ statusIndicatorClass status) ] []
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
