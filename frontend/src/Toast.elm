module Toast exposing
    ( Toast
    , view
    )

import Flow exposing (Flow)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Extra as Decode


type alias Toast =
    { message : String
    , id : Int
    , isSuccess : Bool
    , needsIntro : Bool
    }

view : Flow s () -> Toast -> Html (Flow s ())
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
            [ span [ class "toast-message" ] [ text toast.message ] ]
        ]
