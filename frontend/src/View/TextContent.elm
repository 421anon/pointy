module View.TextContent exposing (view)

import Accessors exposing (just, over)
import Api.ApiData exposing (ApiData(..))
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Html.Lazy
import Maybe.Extra as Maybe
import Model.Core exposing (TextContent)
import Model.Lenses exposing (fileDelimitedGrid, gridState)
import View.PlainText as PlainText


view : String -> ((TextContent -> TextContent) -> msg) -> List (Html msg) -> ApiData TextContent -> Html msg
view viewerId update actions content =
    case content of
        Success file ->
            let
                plain () =
                    PlainText.view
                        { id = viewerId
                        , layout = PlainText.FillParent file.plainViewportHeight
                        , scrollTop = file.plainScrollTop
                        , onScroll =
                            Just
                                (\metrics ->
                                    update (\current -> { current | plainScrollTop = metrics.scrollTop, plainViewportHeight = metrics.clientHeight })
                                )
                        , lineSelection = Nothing
                        , loading = Nothing
                        , startLine = 1
                        , lineCount = file.lineCount
                        }
                        (Html.Lazy.lazy PlainText.viewText file.text)

                updateGrid changeGrid =
                    update (over (fileDelimitedGrid << just << gridState) changeGrid >> (\current -> { current | plainScrollTop = 0 }))

                mToolbarActions =
                    if List.isEmpty actions then
                        Nothing

                    else
                        Just (Html.div [ class "delimited-grid-toolbar-actions" ] actions)
            in
            case file.delimitedGrid of
                Just grid ->
                    Grid.view updateGrid plain (Html.viewMaybe identity mToolbarActions) grid.grid

                Nothing ->
                    Maybe.unpack plain
                        (\toolbarActions ->
                            Html.div [ class "delimited-grid-shell" ]
                                [ Html.div [ class "delimited-grid-toolbar" ] [ toolbarActions ]
                                , plain ()
                                ]
                        )
                        mToolbarActions

        Error _ ->
            Html.div [ class "text-content-error" ] [ Html.text "Failed to load file." ]

        _ ->
            Html.div [ class "text-content-loading" ]
                [ Html.span [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading file…" ] ]
