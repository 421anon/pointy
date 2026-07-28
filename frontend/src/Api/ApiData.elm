module Api.ApiData exposing (..)

import Accessors exposing (Prism, prism)
import Http


type ApiData a
    = NotAsked
    | Loading (Maybe a)
    | Success a
    | Error Http.Error


loading : Maybe a -> ApiData a
loading =
    Loading


map : (a -> b) -> ApiData a -> ApiData b
map f apiData =
    case apiData of
        NotAsked ->
            NotAsked

        Loading mv ->
            Loading (Maybe.map f mv)

        Success value ->
            Success (f value)

        Error error ->
            Error error


{-| Merges 2 ApiDatas. The loading state of the new one takes precedence.
-}
update : (a -> b -> a) -> ApiData a -> ApiData b -> ApiData a
update f new old =
    case ( new, old ) of
        ( Success a, Success b ) ->
            Success (f a b)

        ( Success a, NotAsked ) ->
            Success a

        ( Success a, Error _ ) ->
            Success a

        ( Success a, Loading mb ) ->
            case mb of
                Just b ->
                    Success (f a b)

                Nothing ->
                    Success a

        ( Loading ma, Loading mb ) ->
            case ( ma, mb ) of
                ( Just a, Just b ) ->
                    Loading (Just (f a b))

                _ ->
                    Loading Nothing

        ( Loading ma, Success b ) ->
            case ma of
                Just a ->
                    Loading (Just (f a b))

                Nothing ->
                    Loading Nothing

        ( Error err, _ ) ->
            Error err

        ( _, Error err ) ->
            Error err

        ( NotAsked, _ ) ->
            NotAsked

        ( _, NotAsked ) ->
            NotAsked


andThenMaybe : (a -> Maybe b) -> Http.Error -> ApiData a -> ApiData b
andThenMaybe f error apiData =
    case apiData of
        NotAsked ->
            NotAsked

        Loading mv ->
            Loading (Maybe.andThen f mv)

        Success value ->
            fromMaybe error (f value)

        Error err ->
            Error err


fromMaybe : Http.Error -> Maybe a -> ApiData a
fromMaybe error maybeValue =
    case maybeValue of
        Just value ->
            Success value

        Nothing ->
            Error error


toMaybe : ApiData a -> Maybe a
toMaybe apiData =
    case apiData of
        NotAsked ->
            Nothing

        Loading mv ->
            mv

        Success value ->
            Just value

        Error _ ->
            Nothing


unwrap : b -> (a -> b) -> ApiData a -> b
unwrap default f apiData =
    case apiData of
        NotAsked ->
            default

        Loading _ ->
            default

        Success value ->
            f value

        Error _ ->
            default


withDefault : a -> ApiData a -> a
withDefault default apiData =
    case apiData of
        NotAsked ->
            default

        Loading _ ->
            default

        Success value ->
            value

        Error _ ->
            default


fromResult : Result Http.Error a -> ApiData a
fromResult result =
    case result of
        Ok value ->
            Success value

        Err error ->
            Error error


toResult : ApiData a -> Result (ApiData a) a
toResult apiData =
    case apiData of
        NotAsked ->
            Err apiData

        Loading _ ->
            Err apiData

        Success value ->
            Ok value

        Error _ ->
            Err apiData


toLoading : ApiData a -> ApiData a
toLoading apiData =
    case apiData of
        Success value ->
            Loading (Just value)

        Loading mv ->
            Loading mv

        _ ->
            Loading Nothing


foldVisible : b -> (Maybe a -> b) -> (a -> b) -> (Http.Error -> b) -> ApiData a -> b
foldVisible onNotAsked onLoading onSuccess onError apiData =
    case apiData of
        NotAsked ->
            onNotAsked

        Loading mv ->
            onLoading mv

        Success value ->
            onSuccess value

        Error error ->
            onError error


success : Prism pr (ApiData a) a x y
success =
    prism ">Success" Success toResult
