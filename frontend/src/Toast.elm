module Toast exposing
    ( Past
    , Toast
    , ToastAction
    , encodePast
    , pastDecoder
    , view
    , viewPast
    )

import Flow exposing (Flow)
import Html exposing (Html, button, details, div, span, summary, text)
import Html.Attributes exposing (attribute, class, classList, title, type_)
import Html.Events as Events
import Html.Extra as Html
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Json.Encode as Encode
import Time
import View.Icons exposing (iconCustom)


type alias Toast action =
    { message : String
    , id : Int
    , isSuccess : Bool
    , needsIntro : Bool
    , mAction : Maybe (ToastAction action)
    }


type alias ToastAction action =
    { label : String
    , run : action
    }


type alias Past =
    { message : String
    , id : Int
    , isSuccess : Bool
    , addedAt : Time.Posix
    }


pastDecoder : Decode.Decoder Past
pastDecoder =
    Decode.map4 Past
        (Decode.field "message" Decode.string)
        (Decode.field "id" Decode.int)
        (Decode.field "isSuccess" Decode.bool)
        (Decode.field "addedAt" (Decode.map Time.millisToPosix Decode.int))


encodePast : Past -> Encode.Value
encodePast past =
    Encode.object
        [ ( "message", Encode.string past.message )
        , ( "id", Encode.int past.id )
        , ( "isSuccess", Encode.bool past.isSuccess )
        , ( "addedAt", Encode.int (Time.posixToMillis past.addedAt) )
        ]


view : Flow s () -> Maybe (Flow s ()) -> Toast (Flow s ()) -> Html (Flow s ())
view dismiss mDismissAll toast =
    let
        footerActions =
            List.filterMap identity
                [ Maybe.map (\action -> button [ class "toast-action", type_ "button", Events.onClick (Flow.seq action.run dismiss) ] [ text action.label ]) toast.mAction
                , Maybe.map (\dismissAll -> button [ class "toast-action", type_ "button", Events.onClick dismissAll ] [ text "Dismiss all" ]) mDismissAll
                ]
    in
    div
        [ class "toast-wrapper"
        ]
        [ div
            [ class "toast"
            , classList
                [ ( "toast-success", toast.isSuccess )
                , ( "needs-intro", toast.needsIntro )
                ]
            , Events.on "animationend" (Decode.when (Decode.field "animationName" Decode.string) ((==) "toast-out") (Decode.succeed dismiss))
            ]
            [ div [ class "toast-message" ]
                [ span [] [ text toast.message ]
                , button
                    [ class "toast-dismiss"
                    , type_ "button"
                    , title "Dismiss"
                    , attribute "aria-label" "Dismiss"
                    , Events.onClick dismiss
                    ]
                    [ iconCustom False "close" [ attribute "aria-hidden" "true" ] ]
                ]
            , Html.viewIf (not (List.isEmpty footerActions)) (div [ class "toast-footer" ] footerActions)
            ]
        ]


viewPast : Time.Posix -> Past -> ( String, Html msg )
viewPast now past =
    ( String.fromInt past.id
    , details [ class "toast-history__entry", classList [ ( "toast-history__entry--success", past.isSuccess ) ] ]
        [ summary []
            [ span [ class "toast-history__preview" ] [ text (Maybe.withDefault "" (List.head (String.lines past.message))) ]
            , span [ class "toast-history__age" ] [ text (ageText (Time.posixToMillis now - Time.posixToMillis past.addedAt)) ]
            ]
        , div [ class "toast-history__message" ] [ text past.message ]
        ]
    )


ageText : Int -> String
ageText millis =
    let
        minutes =
            millis // 60000
    in
    if minutes < 1 then
        "just now"

    else if minutes < 60 then
        String.fromInt minutes ++ "m"

    else if minutes < 1440 then
        String.fromInt (minutes // 60) ++ "h"

    else
        String.fromInt (minutes // 1440) ++ "d"
