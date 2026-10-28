module View.TextContent exposing (view)

import Api.ApiData exposing (ApiData(..))
import Grid
import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Extra as Html
import Html.Lazy
import Model.Core exposing (TextContent)
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
                    update
                        (\current ->
                            { current
                                | delimitedGrid = Maybe.map (\delimited -> { delimited | grid = changeGrid delimited.grid }) current.delimitedGrid
                                , plainScrollTop = 0
                            }
                        )

                toolbarActions =
                    Html.viewIf (not (List.isEmpty actions))
                        (Html.div [ class "delimited-grid-toolbar-actions" ] actions)
            in
            case file.delimitedGrid of
                Just grid ->
                    Grid.view updateGrid plain toolbarActions grid.grid

                Nothing ->
                    if List.isEmpty actions then
                        plain ()

                    else
                        Html.div [ class "delimited-grid-shell" ]
                            [ Html.div [ class "delimited-grid-toolbar" ] [ toolbarActions ]
                            , plain ()
                            ]

        Error _ ->
            Html.div [ class "text-content-error" ] [ Html.text "Failed to load file." ]

        _ ->
            Html.div [ class "text-content-loading" ]
                [ Html.span [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading file…" ] ]
