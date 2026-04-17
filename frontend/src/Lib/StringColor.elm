module Lib.StringColor exposing (..)

import Bitwise


stringToColor : String -> String
stringToColor s =
    let
        h32 =
            hash32 s

        hue =
            360 * toFloat (Bitwise.and 0xFFFF h32) / 65536

        sat =
            0.5
                + 0.4
                * toFloat (Bitwise.and 0xFF (Bitwise.shiftRightZfBy 16 h32))
                / 255

        light =
            0.1
                + 0.5
                * toFloat (Bitwise.and 0xFF (Bitwise.shiftRightZfBy 24 h32))
                / 255

        ( r, g, b ) =
            hslToRgb hue sat light
    in
    "#" ++ hex2 r ++ hex2 g ++ hex2 b


hash32 : String -> Int
hash32 s =
    let
        fnv =
            String.foldl
                (\ch acc ->
                    Bitwise.xor acc (Char.toCode ch) * 16777619
                )
                2166136261
                s

        mix1 =
            Bitwise.xor fnv (Bitwise.shiftRightZfBy 16 fnv)

        mix2 =
            mix1 * 2246822507

        mix3 =
            Bitwise.xor mix2 (Bitwise.shiftRightZfBy 13 mix2)

        mix4 =
            mix3 * 3266489909
    in
    Bitwise.xor mix4 (Bitwise.shiftRightZfBy 16 mix4)


hslToRgb : Float -> Float -> Float -> ( Int, Int, Int )
hslToRgb h s l =
    let
        hh =
            h / 360

        q =
            if l < 0.5 then
                l * (1 + s)

            else
                l + s - l * s

        p =
            2 * l - q

        hueToRgb t =
            let
                tt =
                    if t < 0 then
                        t + 1

                    else if t > 1 then
                        t - 1

                    else
                        t
            in
            if tt < 1 / 6 then
                p + (q - p) * 6 * tt

            else if tt < 1 / 2 then
                q

            else if tt < 2 / 3 then
                p + (q - p) * (2 / 3 - tt) * 6

            else
                p

        to255 x =
            clampInt 0 255 (round (255 * x))
    in
    ( to255 (hueToRgb (hh + 1 / 3))
    , to255 (hueToRgb hh)
    , to255 (hueToRgb (hh - 1 / 3))
    )


hex2 : Int -> String
hex2 n =
    let
        d =
            "0123456789abcdef"
    in
    String.slice (n // 16) (n // 16 + 1) d
        ++ String.slice (modBy 16 n) (modBy 16 n + 1) d


clampInt : Int -> Int -> Int -> Int
clampInt lo hi x =
    if x < lo then
        lo

    else if x > hi then
        hi

    else
        x
