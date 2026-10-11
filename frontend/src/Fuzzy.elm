module Fuzzy exposing (Document, Field, Indexed, Match, Precision(..), Query, Segment(..), excerpt, highlight, index, isEmpty, labelDocument, parse, rank, reindex)

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


type Indexed tag
    = Indexed (Document tag) (List ( Field tag, Prepared ))


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
    , starts : List Int
    , initials : String
    , units : List ( Int, Char )
    }


type alias Hit =
    { score : Int
    , start : Int
    , end : Int
    , positions : List Int
    }


type CharKind
    = Boundary
    | Digit
    | Upper
    | Lower


type alias Fold =
    { joined : List Char
    , length : Int
    , starts : List Int
    , initials : List Char
    , previous : CharKind
    , position : Int
    , origin : List ( Int, Int )
    }


isSeparator : Char -> Bool
isSeparator c =
    case c of
        ' ' ->
            True

        '\t' ->
            True

        '\n' ->
            True

        '\u{000D}' ->
            True

        '\u{000B}' ->
            True

        '\u{000C}' ->
            True

        '\u{00A0}' ->
            True

        '_' ->
            True

        '-' ->
            True

        '.' ->
            True

        '/' ->
            True

        '\\' ->
            True

        '(' ->
            True

        ')' ->
            True

        '[' ->
            True

        ']' ->
            True

        '{' ->
            True

        '}' ->
            True

        '<' ->
            True

        '>' ->
            True

        ':' ->
            True

        ';' ->
            True

        ',' ->
            True

        '\'' ->
            True

        '"' ->
            True

        '`' ->
            True

        '|' ->
            True

        '+' ->
            True

        '=' ->
            True

        '*' ->
            True

        '#' ->
            True

        '@' ->
            True

        '!' ->
            True

        '?' ->
            True

        '~' ->
            True

        '&' ->
            True

        '^' ->
            True

        '%' ->
            True

        '$' ->
            True

        _ ->
            False


charKind : Char -> Char -> CharKind
charKind c lowered =
    if Char.isDigit c then
        Digit

    else if lowered /= c then
        Upper

    else
        Lower


startsWord : CharKind -> CharKind -> Bool
startsWord previous kind =
    case previous of
        Boundary ->
            True

        Lower ->
            kind == Upper || kind == Digit

        Upper ->
            kind == Digit

        Digit ->
            kind /= Digit


charLength : Char -> Int
charLength c =
    if Char.toCode c > 0xFFFF then
        2

    else
        1


foldText : Bool -> String -> Fold
foldText withOrigin text =
    String.foldl (foldStep withOrigin)
        { joined = []
        , length = 0
        , starts = []
        , initials = []
        , previous = Boundary
        , position = 0
        , origin = []
        }
        text


foldStep : Bool -> Char -> Fold -> Fold
foldStep withOrigin c state =
    let
        nextPosition =
            state.position + charLength c
    in
    if isSeparator c then
        { state | position = nextPosition, previous = Boundary }

    else
        let
            lowered =
                Char.toLower c

            kind =
                charKind c lowered

            loweredLength =
                charLength lowered

            offset =
                state.length

            startsHere =
                startsWord state.previous kind
        in
        { joined = lowered :: state.joined
        , length = offset + loweredLength
        , starts =
            if startsHere then
                offset :: state.starts

            else
                state.starts
        , initials =
            if startsHere then
                lowered :: state.initials

            else
                state.initials
        , previous = kind
        , position = nextPosition
        , origin =
            if withOrigin then
                List.repeat loweredLength ( state.position, nextPosition ) ++ state.origin

            else
                state.origin
        }


prepared : Precision -> Fold -> Prepared
prepared precision state =
    let
        joined =
            String.fromList (List.reverse state.joined)
    in
    { joined = joined
    , length = state.length
    , starts = List.reverse state.starts
    , initials = String.fromList (List.reverse state.initials)
    , units =
        if precision == Loose then
            unitsOf joined

        else
            []
    }


prepare : Precision -> String -> Prepared
prepare precision text =
    prepared precision (foldText False text)


prepareWithOrigin : Precision -> String -> ( Prepared, Array ( Int, Int ) )
prepareWithOrigin precision text =
    let
        state =
            foldText True text
    in
    ( prepared precision state, Array.fromList (List.reverse state.origin) )


unitsOf : String -> List ( Int, Char )
unitsOf joined =
    String.foldl (\c ( offset, units ) -> ( offset + charLength c, ( offset, c ) :: units )) ( 0, [] ) joined
        |> Tuple.second
        |> List.reverse


index : Document tag -> Indexed tag
index document =
    Indexed document (List.map (\field -> ( field, prepare field.precision field.text )) document.fields)


reindex : Maybe (Indexed tag) -> Document tag -> Indexed tag
reindex cached document =
    case cached of
        Just ((Indexed previous _) as indexed) ->
            if previous == document then
                indexed

            else
                index document

        Nothing ->
            index document


parse : String -> Query
parse input =
    Query (List.filterMap joinedToken (String.words input))


joinedToken : String -> Maybe String
joinedToken word =
    let
        joined =
            (prepare Words word).joined
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
tokenHit precision prepared_ token =
    case precision of
        Whole ->
            if token == prepared_.joined then
                Just
                    { score = 100
                    , start = 0
                    , end = prepared_.length
                    , positions = List.range 0 (prepared_.length - 1)
                    }

            else
                Nothing

        Words ->
            prefixHit prepared_ token
                |> Maybe.orElseLazy (\_ -> acronymHit prepared_ token)
                |> Maybe.orElseLazy (\_ -> substringHit prepared_ token)
                |> Maybe.orElseLazy (\_ -> typoHit prepared_ token)

        Loose ->
            prefixHit prepared_ token
                |> Maybe.orElseLazy (\_ -> acronymHit prepared_ token)
                |> Maybe.orElseLazy (\_ -> substringHit prepared_ token)
                |> Maybe.orElseLazy (\_ -> List.foldl keepBetter Nothing (List.filterMap identity [ subsequenceHit prepared_ token, typoHit prepared_ token ]))


dropBelow : Int -> List Int -> List Int
dropBelow value offsets =
    case offsets of
        offset :: rest ->
            if offset < value then
                dropBelow value rest

            else
                offsets

        [] ->
            []


startsAt : Int -> List Int -> Bool
startsAt value offsets =
    case offsets of
        offset :: _ ->
            offset == value

        [] ->
            False


prefixHit : Prepared -> String -> Maybe Hit
prefixHit prepared_ token =
    let
        tokenLength =
            String.length token

        toHit ( score, start, end ) =
            { score = score
            , start = start
            , end = end
            , positions = List.range start (end - 1)
            }

        go starts ends best =
            case starts of
                [] ->
                    best

                offset :: rest ->
                    if String.slice offset (offset + tokenLength) prepared_.joined == token then
                        let
                            end =
                                offset + tokenLength

                            remainingEnds =
                                dropBelow end ends

                            score =
                                if end == prepared_.length || startsAt end remainingEnds then
                                    100

                                else if offset == 0 then
                                    85

                                else
                                    80

                            candidate =
                                ( score, offset, end )
                        in
                        if score == 100 then
                            Just candidate

                        else
                            case best of
                                Just ( bestScore, _, _ ) ->
                                    if score > bestScore then
                                        go rest remainingEnds (Just candidate)

                                    else
                                        go rest remainingEnds best

                                Nothing ->
                                    go rest remainingEnds (Just candidate)

                    else
                        go rest ends best
    in
    go prepared_.starts prepared_.starts Nothing
        |> Maybe.map toHit


acronymHit : Prepared -> String -> Maybe Hit
acronymHit prepared_ token =
    let
        tokenChars =
            String.toList token

        tokenLength =
            List.length tokenChars
    in
    if tokenLength < 2 then
        Nothing

    else
        String.indexes token prepared_.initials
            |> List.head
            |> Maybe.map
                (\initialsOffset ->
                    let
                        wordIndex =
                            List.length (String.toList (String.left initialsOffset prepared_.initials))

                        starts =
                            List.take tokenLength (List.drop wordIndex prepared_.starts)

                        lastOffset =
                            List.last starts |> Maybe.withDefault 0

                        lastLength =
                            List.last tokenChars |> Maybe.unwrap 1 charLength
                    in
                    { score = 70
                    , start = List.head starts |> Maybe.withDefault 0
                    , end = lastOffset + lastLength
                    , positions = starts
                    }
                )


substringHit : Prepared -> String -> Maybe Hit
substringHit prepared_ token =
    case String.indexes token prepared_.joined of
        index_ :: _ ->
            let
                tokenLength =
                    String.length token
            in
            Just
                { score = 50
                , start = index_
                , end = index_ + tokenLength
                , positions = List.range index_ (index_ + tokenLength - 1)
                }

        [] ->
            Nothing


subsequenceHit : Prepared -> String -> Maybe Hit
subsequenceHit prepared_ token =
    case String.toList token of
        first :: rest ->
            let
                tokenLength =
                    1 + List.length rest

                bound =
                    3 * tokenLength

                candidate start remaining =
                    matchSubsequence rest start bound remaining
                        |> Maybe.map
                            (\offsets ->
                                let
                                    positions =
                                        start :: offsets

                                    last =
                                        List.last positions |> Maybe.withDefault start

                                    span =
                                        last - start + 1

                                    bonus =
                                        if List.member start prepared_.starts then
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

                go units best =
                    case units of
                        ( start, unitChar ) :: remaining ->
                            if unitChar == first then
                                go remaining (Maybe.unwrap best (\hit -> keepBetter hit best) (candidate start remaining))

                            else
                                go remaining best

                        [] ->
                            best
            in
            if tokenLength < 3 then
                Nothing

            else
                go prepared_.units Nothing

        [] ->
            Nothing


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
typoHit prepared_ token =
    let
        tokenChars =
            String.toList token

        tokenLength =
            String.length token

        go starts =
            case starts of
                start :: rest ->
                    let
                        end =
                            List.head rest |> Maybe.withDefault prepared_.length

                        width =
                            end - start
                    in
                    if width >= 4 && abs (width - tokenLength) <= 2 && withinOneEdit tokenChars (String.toList (String.slice start end prepared_.joined)) then
                        Just
                            { score = 20
                            , start = start
                            , end = end
                            , positions = List.range start (end - 1)
                            }

                    else
                        go rest

                [] ->
                    Nothing
    in
    if List.length tokenChars < 5 then
        Nothing

    else
        go prepared_.starts


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


rank : Query -> (a -> Indexed tag) -> List a -> List ( a, Match tag )
rank (Query tokens) toIndexed items =
    if List.isEmpty tokens then
        List.map (\item -> ( item, { score = 0, fields = [] } )) items

    else
        List.filterMap (rankItem tokens toIndexed) items
            |> List.sortBy (Tuple.second >> .score >> negate)


rankItem : List String -> (a -> Indexed tag) -> a -> Maybe ( a, Match tag )
rankItem tokens toIndexed item =
    let
        (Indexed document fields) =
            toIndexed item
    in
    if List.isEmpty fields then
        Nothing

    else
        let
            fieldHits =
                List.map
                    (\( field, prepared_ ) ->
                        ( field, prepared_, List.map (tokenHit field.precision prepared_) tokens )
                    )
                    fields

            bests =
                List.indexedMap (\tokenIndex _ -> List.foldl (bestField tokenIndex) Nothing fieldHits) tokens
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
bestField tokenIndex ( field, _, hits ) best =
    case List.head (List.drop tokenIndex hits) of
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
fieldBonus tokens concatTokens ( field, prepared_, hits ) =
    let
        whole =
            if prepared_.joined == concatTokens then
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
            ( prepared_, origin ) =
                prepareWithOrigin precision text

            ranges =
                tokens
                    |> List.filterMap (tokenHit precision prepared_)
                    |> List.concatMap .positions
                    |> List.filterMap (\position -> Array.get position origin)
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
        ( prepared_, origin ) =
            prepareWithOrigin Words text

        start =
            tokens
                |> List.filterMap (tokenHit Words prepared_)
                |> List.map .start
                |> List.minimum
                |> Maybe.andThen (\offset -> Array.get offset origin)
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
