port module Ports exposing (agentAutoApplyIn, agentNotificationClickIn, agentTurnIn, backendConnectionIn, backendEventIn, ffiIn, ffiOut, gutterDragEnd, openAgentTurnStream, openBackendEvents, organizeDragIn)

import Json.Decode
import Json.Encode


port ffiOut : { key : String, fn : String, value : Json.Encode.Value } -> Cmd msg


port ffiIn : ({ key : String, value : Json.Decode.Value } -> msg) -> Sub msg


port openBackendEvents : {} -> Cmd msg


port backendEventIn : ({ event : String, data : String } -> msg) -> Sub msg


port backendConnectionIn : (Bool -> msg) -> Sub msg


port openAgentTurnStream : { sessionId : String, turnId : String } -> Cmd msg


port agentTurnIn : (Json.Decode.Value -> msg) -> Sub msg


port agentAutoApplyIn : (Bool -> msg) -> Sub msg


port agentNotificationClickIn : (String -> msg) -> Sub msg


port gutterDragEnd : (Json.Decode.Value -> msg) -> Sub msg


port organizeDragIn : (Json.Decode.Value -> msg) -> Sub msg
