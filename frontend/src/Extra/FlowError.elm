module Extra.FlowError exposing (..)

import Flow exposing (Flow)


type alias FlowError e s a =
    Flow s (Result e a)


throwError : e -> FlowError e s a
throwError e =
    Flow.pure (Err e)


catchError : (e -> FlowError e s a) -> FlowError e s a -> FlowError e s a
catchError handler =
    Flow.andThen
        (\res ->
            case res of
                Ok _ ->
                    Flow.pure res

                Err e ->
                    handler e
        )


foldResult : (a -> Flow s r) -> (e -> Flow s r) -> FlowError e s a -> Flow s r
foldResult onOk onErr =
    Flow.andThen
        (\res ->
            case res of
                Ok a ->
                    onOk a

                Err e ->
                    onErr e
        )


andThen : (a -> Flow s b) -> FlowError e s a -> FlowError e s b
andThen f =
    Flow.andThen
        (\res ->
            case res of
                Ok a ->
                    f a |> Flow.map Ok

                Err e ->
                    Flow.pure (Err e)
        )
