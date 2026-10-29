module Extra.Decode exposing (..)

import Basics.Extra exposing (flip)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


whenNotInside : Decoder Bool -> msg -> Decoder msg
whenNotInside matches msg =
    when matches not <| map (Maybe.withDefault msg) <| optionalField "parentNode" <| lazy (\() -> whenNotInside matches msg)


hasClass : String -> Decoder Bool
hasClass cls =
    map (List.member cls) decodeClassList


decodeClassList : Decoder (List String)
decodeClassList =
    withDefault [] <| field "classList" <| collection string


firstMatching : List (Decoder a) -> Decoder a
firstMatching decoders =
    decoders
        |> List.map maybe
        |> List.foldl
            (\de da ->
                da
                    |> andThen
                        (\a ->
                            case a of
                                Just _ ->
                                    da

                                Nothing ->
                                    de
                        )
            )
            (succeed Nothing)
        |> andThen
            (\a ->
                case a of
                    Just aa ->
                        succeed aa

                    Nothing ->
                        fail "No decoder succeeded"
            )


ifM : Decoder Bool -> Decoder a -> Decoder a
ifM =
    flip when identity
