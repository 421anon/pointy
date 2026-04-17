module View.Lib exposing (..)

import Accessors exposing (get)
import Actions
import Components.Select as Select
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Maybe.Extra as Maybe
import Model.Core exposing (Model)
import Model.Lenses exposing (searchBox)
import Model.Lib as Lib
import View.Icons exposing (iconCustom)


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
        , Html.div [ class "page-content" ] [ content ]
        ]


viewSearchBox : Model -> Html (Flow Model ())
viewSearchBox model =
    Select.view
        { optic = searchBox
        , selectState = get searchBox model
        , selected_ = []
        , availableItems = Lib.getSearchItems model
        , readOnly = False
        , hasChanged = False
        , label = ""
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
        , onSelect = .id >> Maybe.unwrap (Flow.pure ()) Actions.onSelectSearch
        , alignRight = True
        , inputItemStyle = \_ -> []
        }
