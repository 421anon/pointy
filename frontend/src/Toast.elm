module Toast exposing
    ( Toast
    , view
    )

import Flow exposing (Flow)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events as Events
import Json.Decode as Decode


type alias Toast =
    { message : String
    , id : Int
    , isSuccess : Bool
    }


view : Flow s () -> Toast -> Html (Flow s ())
view dismiss toast =
    div [ class "toast", classList [ ( "toast-success", toast.isSuccess ) ], Events.on "animationend" (Decode.succeed dismiss) ]
        [ span [ class "toast-message" ] [ text toast.message ] ]
