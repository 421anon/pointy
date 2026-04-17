port module Ports exposing (ffiIn, ffiOut, openStepStatusStream, stepStatusIn)

import Json.Decode
import Json.Encode


port ffiOut : { key : String, fn : String, value : Json.Encode.Value } -> Cmd msg


port ffiIn : ({ key : String, value : Json.Decode.Value } -> msg) -> Sub msg


port openStepStatusStream : { projectId : Int, commit : Maybe String } -> Cmd msg


port stepStatusIn : (Json.Decode.Value -> msg) -> Sub msg
