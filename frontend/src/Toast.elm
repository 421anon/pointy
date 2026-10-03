module Toast exposing
    ( Toast
    , ToastAction
    , view
    )

import Flow exposing (Flow)
import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Html.Extra as Html
import Json.Decode as Decode
import Json.Decode.Extra as Decode


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


view : Flow s () -> Toast (Flow s ()) -> Html (Flow s ())
view dismiss toast =
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
            [ span [ class "toast-message" ] [ text toast.message ]
            , Html.viewMaybe
                (\action ->
                    button
                        [ class "toast-action"
                        , Events.onClick (Flow.seq action.run dismiss)
                        ]
                        [ text action.label ]
                )
                toast.mAction
            ]
        ]
