module Extra.Events exposing
    ( CustomEvent
    , map
    , withDefaults
    , withPreventDefault
    , withStopPrevent
    , withStopPropagation
    )


type alias CustomEvent msg =
    { message : msg
    , stopPropagation : Bool
    , preventDefault : Bool
    }


withDefaults : msg -> CustomEvent msg
withDefaults msg =
    { message = msg, stopPropagation = False, preventDefault = False }


{-|


# Deprecated.

Stopping propagation removes the ability to detect events in parent elements.

-}
withStopPropagation : msg -> CustomEvent msg
withStopPropagation msg =
    { message = msg, stopPropagation = True, preventDefault = False }


{-|


# Deprecated.

Stopping propagation removes the ability to detect events in parent elements.

-}
withStopPrevent : msg -> CustomEvent msg
withStopPrevent msg =
    { message = msg, stopPropagation = True, preventDefault = True }


withPreventDefault : msg -> CustomEvent msg
withPreventDefault msg =
    { message = msg, stopPropagation = False, preventDefault = True }


map : (a -> b) -> CustomEvent a -> CustomEvent b
map f event =
    { message = f event.message
    , stopPropagation = event.stopPropagation
    , preventDefault = event.preventDefault
    }
