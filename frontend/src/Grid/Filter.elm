module Grid.Filter exposing (Filter, matches, parse)

import String


type Filter
    = Numeric NumericFilter
    | Text TextFilter


type NumericFilter
    = NCompare CompareOp Float
    | NRange Float Float


type TextFilter
    = TCompare CompareOp String
    | TContains String


type CompareOp
    = Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge


parse : Bool -> String -> Maybe Filter
parse isNumeric raw =
    case splitOperator (String.trim raw) of
        ( Just op, operand ) ->
            if isNumeric then
                numericComparison op operand

            else if operand == "" then
                Nothing

            else
                Just (Text (TCompare op operand))

        ( Nothing, text ) ->
            if isNumeric then
                parseNumeric text

            else
                Just (Text (TContains text))


matches : Filter -> String -> Bool
matches filter raw =
    case filter of
        Numeric numeric ->
            case toNumber raw of
                Just value ->
                    matchesNumeric numeric value

                Nothing ->
                    False

        Text text ->
            matchesText text (String.trim raw)


splitOperator : String -> ( Maybe CompareOp, String )
splitOperator text =
    if String.startsWith ">=" text then
        prefixed Ge 2 text

    else if String.startsWith "<=" text then
        prefixed Le 2 text

    else if String.startsWith "!=" text then
        prefixed Neq 2 text

    else if String.startsWith "==" text then
        prefixed Eq 2 text

    else if String.startsWith "=" text then
        prefixed Eq 1 text

    else if String.startsWith ">" text then
        prefixed Gt 1 text

    else if String.startsWith "<" text then
        prefixed Lt 1 text

    else
        ( Nothing, text )


prefixed : CompareOp -> Int -> String -> ( Maybe CompareOp, String )
prefixed op prefixLength text =
    ( Just op, String.dropLeft prefixLength text |> String.trim )


numericComparison : CompareOp -> String -> Maybe Filter
numericComparison op operand =
    case toNumber operand of
        Just value ->
            Just (Numeric (NCompare op value))

        Nothing ->
            Nothing


parseNumeric : String -> Maybe Filter
parseNumeric text =
    case toNumber text of
        Just value ->
            Just (Numeric (NCompare Eq value))

        Nothing ->
            parseRange text


parseRange : String -> Maybe Filter
parseRange text =
    List.filterMap (splitAtDash text) (dashIndices text) |> List.head


dashIndices : String -> List Int
dashIndices text =
    String.indexes "-" text |> List.filter (\index -> index > 0)


splitAtDash : String -> Int -> Maybe Filter
splitAtDash text index =
    rangeBetween (String.left index text) (String.dropLeft (index + 1) text)


rangeBetween : String -> String -> Maybe Filter
rangeBetween left right =
    case ( toNumber left, toNumber right ) of
        ( Just lo, Just hi ) ->
            Just (Numeric (NRange (min lo hi) (max lo hi)))

        _ ->
            Nothing


toNumber : String -> Maybe Float
toNumber text =
    String.toFloat (String.trim text)


matchesNumeric : NumericFilter -> Float -> Bool
matchesNumeric numeric value =
    case numeric of
        NCompare op operand ->
            compareNumbers op value operand

        NRange lo hi ->
            value >= lo && value <= hi


matchesText : TextFilter -> String -> Bool
matchesText text cell =
    case text of
        TCompare op operand ->
            compareText op operand cell

        TContains needle ->
            String.contains (String.toLower needle) (String.toLower cell)


compareNumbers : CompareOp -> Float -> Float -> Bool
compareNumbers op left right =
    case op of
        Eq ->
            left == right

        Neq ->
            left /= right

        Lt ->
            left < right

        Le ->
            left <= right

        Gt ->
            left > right

        Ge ->
            left >= right


compareText : CompareOp -> String -> String -> Bool
compareText op operand cell =
    case op of
        Eq ->
            String.toLower cell == String.toLower operand

        Neq ->
            String.toLower cell /= String.toLower operand

        Lt ->
            compareTextValues (<) (<) cell operand

        Le ->
            compareTextValues (<=) (<=) cell operand

        Gt ->
            compareTextValues (>) (>) cell operand

        Ge ->
            compareTextValues (>=) (>=) cell operand


compareTextValues : (Float -> Float -> Bool) -> (String -> String -> Bool) -> String -> String -> Bool
compareTextValues floatOp stringOp cell operand =
    if cell == "" then
        False

    else
        case ( toNumber cell, toNumber operand ) of
            ( Just left, Just right ) ->
                floatOp left right

            _ ->
                stringOp (String.toLower cell) (String.toLower operand)
