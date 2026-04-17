module Api.ApiData exposing (..)

import Accessors exposing (Prism, prism)
import Http


type alias LoadingState a =
    { lastKnown : Maybe a
    , predicted : Maybe a
    }


type ApiData a
    = NotAsked
    | Loading (LoadingState a)
    | Success a
    | Error Http.Error


loading : Maybe a -> ApiData a
loading maybeValue =
    Loading
        { lastKnown = maybeValue
        , predicted = Nothing
        }


visibleLoadingValue : LoadingState a -> Maybe a
visibleLoadingValue loadingState =
    case loadingState.predicted of
        Just predicted ->
            Just predicted

        Nothing ->
            loadingState.lastKnown


map : (a -> b) -> ApiData a -> ApiData b
map f apiData =
    case apiData of
        NotAsked ->
            NotAsked

        Loading loadingState_ ->
            Loading
                { lastKnown = Maybe.map f loadingState_.lastKnown
                , predicted = Maybe.map f loadingState_.predicted
                }

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

        ( Success a, Loading loadingB ) ->
            case visibleLoadingValue loadingB of
                Just b ->
                    Success (f a b)

                Nothing ->
                    Success a

        ( Loading loadingA, Loading loadingB ) ->
            case ( visibleLoadingValue loadingA, visibleLoadingValue loadingB ) of
                ( Just a, Just b ) ->
                    loading (Just (f a b))

                _ ->
                    loading Nothing

        ( Loading loadingA, Success b ) ->
            case visibleLoadingValue loadingA of
                Just a ->
                    loading (Just (f a b))

                Nothing ->
                    loading Nothing

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

        Loading loadingState_ ->
            Loading
                { lastKnown = Maybe.andThen f loadingState_.lastKnown
                , predicted = Maybe.andThen f loadingState_.predicted
                }

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

        Loading loadingState_ ->
            visibleLoadingValue loadingState_

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
            loading (Just value)

        Loading loadingState ->
            Loading loadingState

        _ ->
            loading Nothing


foldVisible : b -> (Maybe a -> b) -> (a -> b) -> (Http.Error -> b) -> ApiData a -> b
foldVisible onNotAsked onLoading onSuccess onError apiData =
    case apiData of
        NotAsked ->
            onNotAsked

        Loading loadingState_ ->
            onLoading (visibleLoadingValue loadingState_)

        Success value ->
            onSuccess value

        Error error ->
            onError error


success : Prism pr (ApiData a) a x y
success =
    prism ">Success" Success toResult
