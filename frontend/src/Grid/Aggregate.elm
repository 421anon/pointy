module Grid.Aggregate exposing (Reducer, appliesTo, catalog, compute, label)

import Set


type Reducer
    = Mean
    | Median
    | Min
    | Max
    | Sum
    | Count
    | Distinct


catalog : List Reducer
catalog =
    [ Mean, Median, Min, Max, Sum, Count, Distinct ]


label : Reducer -> String
label reducer =
    case reducer of
        Mean ->
            "Mean"

        Median ->
            "Median"

        Min ->
            "Min"

        Max ->
            "Max"

        Sum ->
            "Sum"

        Count ->
            "Count"

        Distinct ->
            "Distinct"


appliesTo : Bool -> Reducer -> Bool
appliesTo numeric reducer =
    numeric || List.member reducer [ Count, Distinct ]


compute : Reducer -> List String -> String
compute reducer cells =
    case reducer of
        Mean ->
            summarize mean cells

        Median ->
            summarize median cells

        Min ->
            summarize minimum cells

        Max ->
            summarize maximum cells

        Sum ->
            summarize List.sum cells

        Count ->
            String.fromInt (countNonBlank cells)

        Distinct ->
            String.fromInt (distinctCount cells)


summarize : (List Float -> Float) -> List String -> String
summarize summarizeNumbers cells =
    case parseNumbers cells of
        [] ->
            ""

        numbers ->
            formatNumber (summarizeNumbers numbers)


mean : List Float -> Float
mean numbers =
    List.sum numbers / toFloat (List.length numbers)


median : List Float -> Float
median numbers =
    medianOfSorted (List.sort numbers)


minimum : List Float -> Float
minimum numbers =
    Maybe.withDefault 0 (List.minimum numbers)


maximum : List Float -> Float
maximum numbers =
    Maybe.withDefault 0 (List.maximum numbers)


countNonBlank : List String -> Int
countNonBlank cells =
    List.length (nonBlankCells cells)


distinctCount : List String -> Int
distinctCount cells =
    Set.size (Set.fromList (nonBlankCells cells))


nonBlankCells : List String -> List String
nonBlankCells cells =
    cells
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


parseNumbers : List String -> List Float
parseNumbers cells =
    cells
        |> List.map String.trim
        |> List.filterMap String.toFloat


medianOfSorted : List Float -> Float
medianOfSorted sorted =
    let
        middle =
            List.length sorted // 2
    in
    if modBy 2 (List.length sorted) == 0 then
        (elementAt (middle - 1) sorted + elementAt middle sorted) / 2

    else
        elementAt middle sorted


elementAt : Int -> List Float -> Float
elementAt index numbers =
    numbers
        |> List.drop index
        |> List.head
        |> Maybe.withDefault 0


formatNumber : Float -> String
formatNumber value =
    let
        scaled =
            round (value * 10000)
    in
    case scaled of
        0 ->
            if value == 0 then
                "0"

            else
                scientificNotation value

        _ ->
            fixedFromScaled 4 scaled


scientificNotation : Float -> String
scientificNotation value =
    let
        exponent =
            decimalExponent value

        mantissa =
            round (value * toFloat (10 ^ abs exponent) * 1000)
    in
    fixedFromScaled 3 mantissa ++ "e" ++ String.fromInt exponent


decimalExponent : Float -> Int
decimalExponent value =
    if abs value >= 10 then
        decimalExponent (value / 10) + 1

    else if abs value < 1 then
        decimalExponent (value * 10) - 1

    else
        0


fixedFromScaled : Int -> Int -> String
fixedFromScaled decimals scaled =
    let
        divisor =
            10 ^ decimals

        magnitude =
            abs scaled

        whole =
            String.fromInt (magnitude // divisor)

        fraction =
            magnitude
                |> modBy divisor
                |> String.fromInt
                |> String.padLeft decimals '0'
                |> stripTrailingZeros
    in
    signOf scaled ++ whole ++ decimalSuffix fraction


signOf : Int -> String
signOf scaled =
    if scaled < 0 then
        "-"

    else
        ""


decimalSuffix : String -> String
decimalSuffix fraction =
    if String.isEmpty fraction then
        ""

    else
        "." ++ fraction


stripTrailingZeros : String -> String
stripTrailingZeros text =
    if String.endsWith "0" text then
        stripTrailingZeros (String.dropRight 1 text)

    else
        text
