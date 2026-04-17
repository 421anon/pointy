module Toast exposing
    ( Toast
    , view
    )

import Flow exposing (Flow)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)


type alias Toast =
    { message : String
    , id : Int
    , isSuccess : Bool
    }


view : Toast -> Html (Flow s ())
view toast =
    div [ class "toast", classList [ ( "toast-success", toast.isSuccess ) ] ]
        [ span [ class "toast-message" ] [ text toast.message ] ]
