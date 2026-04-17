module Extra.Decode exposing (..)

import Basics.Extra exposing (flip)
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)


whenNotInside : String -> msg -> Decoder msg
whenNotInside cls msg =
    when decodeClassList (not << List.member cls) <| map (Maybe.withDefault msg) <| optionalField "parentNode" <| lazy (\() -> whenNotInside cls msg)


{-| Decodes [`Element.classList`](https://developer.mozilla.org/en-US/docs/Web/API/Element/classList).

This property is not a regular array but a `DOMTokenList`, therefore `Json.Decode.Extra.collection`
is needed instead of `Json.Decode.list`.

-}
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
