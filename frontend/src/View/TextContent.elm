module View.TextContent exposing (view)

import Api.ApiData exposing (ApiData(..))
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Lazy
import Model.Core exposing (TextContent)
import View.PlainText as PlainText


view : String -> ((TextContent -> TextContent) -> msg) -> ApiData TextContent -> Html msg
view viewerId update content =
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
                    update
                        (\current ->
                            { current
                                | delimitedGrid = Maybe.map (\delimited -> { delimited | grid = changeGrid delimited.grid }) current.delimitedGrid
                                , plainScrollTop = 0
                            }
                        )
            in
            case file.delimitedGrid of
                Just grid ->
                    Grid.view updateGrid plain grid.grid

                Nothing ->
                    plain ()

        Error _ ->
            Html.div [ class "text-content-error" ] [ Html.text "Failed to load file." ]

        _ ->
            Html.div [ class "text-content-loading" ]
                [ Html.span [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading file…" ] ]
