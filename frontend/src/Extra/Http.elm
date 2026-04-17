module Extra.Http exposing (errorMessage)

import Http


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl _ ->
            "Invalid request URL."

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Cannot connect to the server."

        Http.BadStatus 400 ->
            "Invalid request."

        Http.BadStatus 500 ->
            "Server error. Try again later."

        Http.BadStatus code ->
            "Error " ++ String.fromInt code ++ " from server."

        Http.BadBody message ->
            message
