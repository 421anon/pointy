module View.Dialog exposing (viewConfirm)

import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes
import Html.Events as Ev
import Html.Extra as Html
import Maybe.Extra as Maybe
import Model.Core exposing (Model)


viewConfirm :
    { id : String
    , title : String
    , subtitle : Maybe String
    , bodyLines : List String
    , onConfirm : Flow Model a
    }
    -> Html (Flow Model ())
viewConfirm { id, title, subtitle, bodyLines, onConfirm } =
    Html.node "dialog"
        [ Html.Attributes.id id
        , Html.Attributes.class "dialog"
        ]
        [ Html.div [ Html.Attributes.class "dialog-content" ]
            [ Html.span [ Html.Attributes.class "dialog-title" ] [ Html.text title ]
            , Maybe.unwrap Html.nothing (\st -> Html.span [ Html.Attributes.class "dialog-subtitle" ] [ Html.text st ]) subtitle
            , Html.div [ Html.Attributes.class "dialog-body" ]
                (List.map (\line -> Html.span [] [ Html.text line ]) bodyLines)
            , Html.div [ Html.Attributes.class "dialog-actions" ]
                [ Html.form [ Html.Attributes.attribute "method" "dialog" ]
                    [ Html.button
                        [ Html.Attributes.attribute "value" "confirm"
                        , Html.Attributes.class "btn btn-danger"
                        , Html.Attributes.autofocus True
                        , Ev.onClick onConfirm |> Html.Attributes.map (Flow.map (always ()))
                        ]
                        [ Html.text "Confirm" ]
                    , Html.button
                        [ Html.Attributes.attribute "value" "cancel"
                        , Html.Attributes.class "btn"
                        ]
                        [ Html.text "Cancel" ]
                    ]
                ]
            ]
        ]
