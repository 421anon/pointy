module View.Icons exposing (icon, iconCustom)

import Html exposing (Html, span)
import Html.Attributes exposing (class)


icon : Bool -> String -> Html msg
icon filled name =
    let
        classes =
            "material-symbols-outlined"
                ++ (if filled then
                        " material-symbols-outlined--filled"

                    else
                        ""
                   )
    in
    span [ class classes ] [ Html.text name ]


iconCustom : Bool -> String -> List (Html.Attribute msg) -> Html msg
iconCustom filled name attrs =
    let
        classes =
            "material-symbols-outlined"
                ++ (if filled then
                        " material-symbols-outlined--filled"

                    else
                        ""
                   )
    in
    span (class classes :: attrs) [ Html.text name ]
