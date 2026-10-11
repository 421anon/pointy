module Fuzzy exposing (Document, Field, Match, Precision(..), Query, Segment(..), excerpt, highlight, isEmpty, labelDocument, parse, rank)

import Array exposing (Array)
import List.Extra as List
import Maybe.Extra as Maybe


type Precision
    = Whole
    | Words
    | Loose


type alias Field tag =
    { tag : tag
    , text : String
    , weight : Int
    , precision : Precision
    }


type alias Document tag =
    { fields : List (Field tag)
    , demoted : Bool
    }


type alias Match tag =
    { score : Int
    , fields : List (Field tag)
    }


type Query
    = Query (List String)


type Segment
    = Plain String
    | Marked String


type alias Prepared =
    { joined : String
    , length : Int
    , words : List ( Int, Char )
    , units : List ( Int, Char )
    , origin : Array ( Int, Int )
    }


type alias Hit =
    { score : Int
    , start : Int
    , end : Int
    , positions : List Int
    }


type CharKind
    = Digit
    | Upper
    | Lower


type alias Fold =
    { joined : List Char
    , joinedLength : Int
    , words : List ( Int, Char )
    , units : List ( Int, Char )
    , origin : List ( Int, Int )
    , previous : Maybe CharKind
    , position : Int
    }


separators : String
separators =
    "_-./\\()[]{}<>:;,'\"`|+=*#@!?~&^%$"


isSeparator : Char -> Bool
isSeparator c =
    isSpace c || String.contains (String.fromChar c) separators


isSpace : Char -> Bool
isSpace c =
    List.member c [ ' ', '\t', '\n', '\u{000D}', '\u{000B}', '\u{000C}', '\u{00A0}' ]


charKind : Char -> CharKind
charKind c =
    if Char.isDigit c then
        Digit

    else if Char.toLower c /= c then
        Upper

    else
        Lower


startsWord : Maybe CharKind -> CharKind -> Bool
startsWord previous kind =
    case previous of
        Nothing ->
            True

        Just Lower ->
            kind == Upper || kind == Digit

        Just Upper ->
            kind == Digit

        Just Digit ->
            kind /= Digit


charLength : Char -> Int
charLength c =
    String.length (String.fromChar c)


prepare : String -> Prepared
prepare text =
    let
        state =
            String.foldl foldStep initialFold text
    in
    { joined = String.fromList (List.reverse state.joined)
    , length = state.joinedLength
    , words = List.reverse state.words
    , units = List.reverse state.units
    , origin = Array.fromList (List.reverse state.origin)
    }


initialFold : Fold
initialFold =
    { joined = []
    , joinedLength = 0
    , words = []
    , units = []
    , origin = []
    , previous = Nothing
    , position = 0
    }


foldStep : Char -> Fold -> Fold
foldStep c state =
    let
        nextPosition =
            state.position + charLength c
    in
    if isSeparator c then
        { state | position = nextPosition, previous = Nothing }

    else
        let
            kind =
                charKind c

            lowered =
                Char.toLower c

            loweredLength =
                charLength lowered

            offset =
                state.joinedLength
        in
        { position = nextPosition
        , previous = Just kind
        , joinedLength = offset + loweredLength
        , joined = lowered :: state.joined
        , words =
            if startsWord state.previous kind then
                ( offset, lowered ) :: state.words

            else
                state.words
        , units = ( offset, lowered ) :: state.units
        , origin = List.repeat loweredLength ( state.position, nextPosition ) ++ state.origin
        }


parse : String -> Query
parse input =
    Query (List.filterMap joinedToken (String.words input))


joinedToken : String -> Maybe String
joinedToken word =
    let
        joined =
            (prepare word).joined
    in
    if String.isEmpty joined then
        Nothing

    else
        Just joined


isEmpty : Query -> Bool
isEmpty (Query tokens) =
    List.isEmpty tokens


labelDocument : String -> Document ()
labelDocument text =
    { fields = [ { tag = (), text = text, weight = 1, precision = Loose } ]
    , demoted = False
    }


tokenHit : Precision -> Prepared -> String -> Maybe Hit
tokenHit precision prepared token =
    case precision of
        Whole ->
            if token == prepared.joined then
                Just
                    { score = 100
                    , start = 0
                    , end = prepared.length
                    , positions = List.range 0 (prepared.length - 1)
                    }

            else
                Nothing

        Words ->
            prefixHit prepared token
                |> Maybe.orElseLazy (\_ -> acronymHit prepared token)
                |> Maybe.orElseLazy (\_ -> substringHit prepared token)
                |> Maybe.orElseLazy (\_ -> typoHit prepared token)

        Loose ->
            prefixHit prepared token
                |> Maybe.orElseLazy (\_ -> acronymHit prepared token)
                |> Maybe.orElseLazy (\_ -> substringHit prepared token)
                |> Maybe.orElseLazy (\_ -> List.foldl keepBetter Nothing (List.filterMap identity [ subsequenceHit prepared token, typoHit prepared token ]))


prefixHit : Prepared -> String -> Maybe Hit
prefixHit prepared token =
    let
        tokenLength =
            String.length token

        boundaries =
            List.map Tuple.first prepared.words

        check ( offset, _ ) best =
            if String.startsWith token (String.dropLeft offset prepared.joined) then
                let
                    end =
                        offset + tokenLength

                    score =
                        if end == prepared.length || List.member end boundaries then
                            100

                        else if offset == 0 then
                            85

                        else
                            80
                in
                case best of
                    Just ( bestScore, _, _ ) ->
                        if score > bestScore then
                            Just ( score, offset, end )

                        else
                            best

                    Nothing ->
                        Just ( score, offset, end )

            else
                best
    in
    List.foldl check Nothing prepared.words
        |> Maybe.map
            (\( score, start, end ) ->
                { score = score
                , start = start
                , end = end
                , positions = List.range start (end - 1)
                }
            )


acronymHit : Prepared -> String -> Maybe Hit
acronymHit prepared token =
    let
        tokenChars =
            String.toList token

        wordChars =
            List.map Tuple.second prepared.words

        wordOffsets =
            List.map Tuple.first prepared.words

        tokenLength =
            List.length tokenChars

        matchAt index =
            List.take tokenLength (List.drop index wordChars) == tokenChars
    in
    if tokenLength < 2 then
        Nothing

    else
        List.filter matchAt (List.range 0 (List.length wordChars - 1))
            |> List.head
            |> Maybe.map
                (\index ->
                    let
                        starts =
                            List.take tokenLength (List.drop index wordOffsets)

                        initials =
                            List.take tokenLength (List.drop index wordChars)

                        lastOffset =
                            List.reverse starts |> List.head |> Maybe.withDefault 0

                        lastLength =
                            List.reverse initials
                                |> List.head
                                |> Maybe.map charLength
                                |> Maybe.withDefault 1
                    in
                    { score = 70
                    , start = List.head starts |> Maybe.withDefault 0
                    , end = lastOffset + lastLength
                    , positions = starts
                    }
                )


substringHit : Prepared -> String -> Maybe Hit
substringHit prepared token =
    case String.indexes token prepared.joined of
        index :: _ ->
            let
                tokenLength =
                    String.length token
            in
            Just
                { score = 50
                , start = index
                , end = index + tokenLength
                , positions = List.range index (index + tokenLength - 1)
                }

        [] ->
            Nothing


subsequenceHit : Prepared -> String -> Maybe Hit
subsequenceHit prepared token =
    let
        tokenChars =
            String.toList token

        tokenLength =
            List.length tokenChars
    in
    if tokenLength < 3 then
        Nothing

    else
        case tokenChars of
            [] ->
                Nothing

            first :: rest ->
                let
                    bound =
                        3 * tokenLength

                    wordStarts =
                        List.map Tuple.first prepared.words

                    candidate ( start, _ ) =
                        matchSubsequence rest start bound (List.dropWhile (\( offset, _ ) -> offset <= start) prepared.units)
                            |> Maybe.map
                                (\offsets ->
                                    let
                                        positions =
                                            start :: offsets

                                        last =
                                            List.reverse positions |> List.head |> Maybe.withDefault start

                                        span =
                                            last - start + 1

                                        bonus =
                                            if List.member start wordStarts then
                                                5

                                            else
                                                0
                                    in
                                    { score = max 10 (30 - 2 * (span - tokenLength)) + bonus
                                    , start = start
                                    , end = last + 1
                                    , positions = positions
                                    }
                                )
                in
                prepared.units
                    |> List.filter (\( _, unitChar ) -> unitChar == first)
                    |> List.filterMap candidate
                    |> List.foldl keepBetter Nothing


matchSubsequence : List Char -> Int -> Int -> List ( Int, Char ) -> Maybe (List Int)
matchSubsequence chars start bound units =
    case chars of
        [] ->
            Just []

        c :: rest ->
            case findSubsequenceChar c start bound units of
                Just ( offset, remaining ) ->
                    Maybe.map ((::) offset) (matchSubsequence rest start bound remaining)

                Nothing ->
                    Nothing


findSubsequenceChar : Char -> Int -> Int -> List ( Int, Char ) -> Maybe ( Int, List ( Int, Char ) )
findSubsequenceChar c start bound units =
    case units of
        [] ->
            Nothing

        ( offset, unitChar ) :: rest ->
            if offset - start >= bound then
                Nothing

            else if unitChar == c then
                Just ( offset, rest )

            else
                findSubsequenceChar c start bound rest


typoHit : Prepared -> String -> Maybe Hit
typoHit prepared token =
    let
        tokenChars =
            String.toList token
    in
    if List.length tokenChars < 5 then
        Nothing

    else
        wordSpans prepared
            |> List.filter (\( _, _, word ) -> String.length word >= 4)
            |> List.filter (\( _, _, word ) -> withinOneEdit tokenChars (String.toList word))
            |> List.head
            |> Maybe.map
                (\( start, end, _ ) ->
                    { score = 20
                    , start = start
                    , end = end
                    , positions = List.range start (end - 1)
                    }
                )


wordSpans : Prepared -> List ( Int, Int, String )
wordSpans prepared =
    let
        offsets =
            List.map Tuple.first prepared.words

        ends =
            List.drop 1 offsets ++ [ prepared.length ]
    in
    List.map2 (\start end -> ( start, end, String.slice start end prepared.joined )) offsets ends


withinOneEdit : List Char -> List Char -> Bool
withinOneEdit a b =
    if a == b then
        True

    else
        case ( a, b ) of
            ( [], [ _ ] ) ->
                True

            ( [ _ ], [] ) ->
                True

            ( x :: xs, y :: ys ) ->
                if x == y then
                    withinOneEdit xs ys

                else
                    xs == ys
                        || xs == b
                        || a == ys
                        || transposed x xs y ys

            _ ->
                False


transposed : Char -> List Char -> Char -> List Char -> Bool
transposed x xs y ys =
    case xs of
        x2 :: xs_ ->
            y == x2 && ys == x :: xs_

        [] ->
            False


keepBetter : Hit -> Maybe Hit -> Maybe Hit
keepBetter hit best =
    case best of
        Just current ->
            if hit.score > current.score then
                Just hit

            else
                best

        Nothing ->
            Just hit


rank : Query -> (a -> Document tag) -> List a -> List ( a, Match tag )
rank (Query tokens) toDocument items =
    if List.isEmpty tokens then
        List.map (\item -> ( item, { score = 0, fields = [] } )) items

    else
        List.filterMap (rankItem tokens toDocument) items
            |> List.sortBy (Tuple.second >> .score >> negate)


rankItem : List String -> (a -> Document tag) -> a -> Maybe ( a, Match tag )
rankItem tokens toDocument item =
    let
        document =
            toDocument item
    in
    if List.isEmpty document.fields then
        Nothing

    else
        let
            fieldHits =
                List.map
                    (\field ->
                        let
                            prepared =
                                prepare field.text
                        in
                        ( field, prepared, List.map (tokenHit field.precision prepared) tokens )
                    )
                    document.fields

            bests =
                List.indexedMap (\index _ -> List.foldl (bestField index) Nothing fieldHits) tokens
        in
        if List.any Maybe.isNothing bests then
            Nothing

        else
            let
                tokenBests =
                    List.filterMap identity bests

                base =
                    List.sum (List.map (\( score, _, _ ) -> score) tokenBests)

                matchFields =
                    List.unique (List.map (\( _, field, _ ) -> field) tokenBests)

                bonus =
                    List.map (fieldBonus tokens (String.concat tokens)) fieldHits
                        |> List.maximum
                        |> Maybe.withDefault 0

                total =
                    base + bonus
            in
            Just
                ( item
                , { score =
                        if document.demoted then
                            total // 2

                        else
                            total
                  , fields = matchFields
                  }
                )


bestField : Int -> ( Field tag, Prepared, List (Maybe Hit) ) -> Maybe ( Int, Field tag, Hit ) -> Maybe ( Int, Field tag, Hit )
bestField index ( field, _, hits ) best =
    case List.head (List.drop index hits) of
        Just (Just hit) ->
            let
                weighted =
                    hit.score * field.weight
            in
            case best of
                Just ( bestScore, _, _ ) ->
                    if weighted > bestScore then
                        Just ( weighted, field, hit )

                    else
                        best

                Nothing ->
                    Just ( weighted, field, hit )

        _ ->
            best


fieldBonus : List String -> String -> ( Field tag, Prepared, List (Maybe Hit) ) -> Int
fieldBonus tokens concatTokens ( field, prepared, hits ) =
    let
        whole =
            if prepared.joined == concatTokens then
                100 * field.weight

            else
                0

        together =
            if List.length tokens >= 2 && List.all Maybe.isJust hits then
                20 * field.weight * (1 + countAdjacent (List.filterMap identity hits))

            else
                0
    in
    whole + together


countAdjacent : List Hit -> Int
countAdjacent hits =
    case hits of
        first :: second :: rest ->
            (if first.end == second.start then
                1

             else
                0
            )
                + countAdjacent (second :: rest)

        _ ->
            0


highlight : Precision -> Query -> String -> List Segment
highlight precision (Query tokens) text =
    if List.isEmpty tokens then
        [ Plain text ]

    else
        let
            prepared =
                prepare text

            ranges =
                tokens
                    |> List.filterMap (tokenHit precision prepared)
                    |> List.concatMap .positions
                    |> List.filterMap (\position -> Array.get position prepared.origin)
                    |> List.unique
                    |> List.sortBy Tuple.first
                    |> mergeRanges
        in
        segments text ranges


segments : String -> List ( Int, Int ) -> List Segment
segments text ranges =
    let
        build cursor remaining =
            case remaining of
                ( start, end ) :: rest ->
                    let
                        plain =
                            String.slice cursor start text
                    in
                    (if String.isEmpty plain then
                        []

                     else
                        [ Plain plain ]
                    )
                        ++ [ Marked (String.slice start end text) ]
                        ++ build end rest

                [] ->
                    let
                        tail =
                            String.slice cursor (String.length text) text
                    in
                    if String.isEmpty tail then
                        []

                    else
                        [ Plain tail ]
    in
    build 0 ranges


mergeRanges : List ( Int, Int ) -> List ( Int, Int )
mergeRanges ranges =
    case ranges of
        first :: rest ->
            mergeInto rest first []

        [] ->
            []


mergeInto : List ( Int, Int ) -> ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
mergeInto ranges current acc =
    case ranges of
        ( start, end ) :: rest ->
            if start <= Tuple.second current then
                mergeInto rest ( Tuple.first current, max (Tuple.second current) end ) acc

            else
                mergeInto rest ( start, end ) (current :: acc)

        [] ->
            List.reverse (current :: acc)


excerpt : Query -> String -> String
excerpt (Query tokens) text =
    let
        prepared =
            prepare text

        start =
            tokens
                |> List.filterMap (tokenHit Words prepared)
                |> List.map .start
                |> List.minimum
                |> Maybe.andThen (\offset -> Array.get offset prepared.origin)
                |> Maybe.unwrap 0 Tuple.first
    in
    window start text


window : Int -> String -> String
window start text =
    let
        from =
            max 0 (start - 24)

        to =
            from + 72

        prefix =
            if from > 0 then
                "…"

            else
                ""

        suffix =
            if to < String.length text then
                "…"

            else
                ""
    in
    prefix ++ String.slice from to text ++ suffix
