module Components.Markdown.Math exposing (Span, protect, restore, viewText)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Regex exposing (Regex)


type alias Span =
    { tex : String
    , display : Bool
    , source : String
    }


type alias Protected =
    { markdown : String
    , spans : Array Span
    }


type alias Scan =
    { outputReversed : List Char
    , spansReversed : List Span
    , spanCount : Int
    }


protect : String -> Protected
protect raw =
    let
        ( scan, rest ) =
            skipFencedCodeBlock { outputReversed = [], spansReversed = [], spanCount = 0 } (String.toList raw)
    in
    scanInline scan rest


restore : Array Span -> String -> String
restore spans text =
    if Array.isEmpty spans then
        text

    else
        Regex.replace sentinelRegex
            (\match ->
                spanAt spans match
                    |> Maybe.map .source
                    |> Maybe.withDefault match.match
            )
            text


viewText : Array Span -> (String -> Html msg) -> String -> Html msg
viewText spans toText text =
    case
        if Array.isEmpty spans then
            []

        else
            Regex.find sentinelRegex text
    of
        [] ->
            toText text

        matches ->
            let
                appendPlain chunk views =
                    if String.isEmpty chunk then
                        views

                    else
                        toText chunk :: views

                step match ( cursor, views ) =
                    ( match.index + String.length match.match
                    , viewSentinel spans match :: appendPlain (String.slice cursor match.index text) views
                    )

                ( end, viewsReversed ) =
                    List.foldl step ( 0, [] ) matches
            in
            Html.span [] (List.reverse (appendPlain (String.dropLeft end text) viewsReversed))


viewSentinel : Array Span -> Regex.Match -> Html msg
viewSentinel spans match =
    case spanAt spans match of
        Just span ->
            Html.node "tex-math"
                [ attribute "tex" span.tex
                , attribute "display"
                    (if span.display then
                        "block"

                     else
                        "inline"
                    )
                ]
                []

        Nothing ->
            Html.text match.match


spanAt : Array Span -> Regex.Match -> Maybe Span
spanAt spans match =
    case match.submatches of
        (Just digits) :: _ ->
            String.toInt digits |> Maybe.andThen (\index -> Array.get index spans)

        _ ->
            Nothing


sentinelOpen : Char
sentinelOpen =
    '\u{E000}'


sentinelClose : Char
sentinelClose =
    '\u{E001}'


sentinelRegex : Regex
sentinelRegex =
    Regex.fromString (String.fromChar sentinelOpen ++ "([0-9]+)" ++ String.fromChar sentinelClose)
        |> Maybe.withDefault Regex.never


finish : Scan -> Protected
finish scan =
    { markdown = String.fromList (List.reverse scan.outputReversed)
    , spans = Array.fromList (List.reverse scan.spansReversed)
    }


emit : List Char -> Scan -> Scan
emit chars scan =
    { scan | outputReversed = List.foldl (::) scan.outputReversed chars }


emitSpan : Span -> Scan -> Scan
emitSpan span scan =
    emit (sentinelOpen :: String.toList (String.fromInt scan.spanCount) ++ [ sentinelClose ])
        { scan | spansReversed = span :: scan.spansReversed, spanCount = scan.spanCount + 1 }


skipFencedCodeBlock : Scan -> List Char -> ( Scan, List Char )
skipFencedCodeBlock scan chars =
    case fencedCodeBlock chars of
        Just ( block, rest ) ->
            ( emit block scan, rest )

        Nothing ->
            ( scan, chars )


scanInline : Scan -> List Char -> Protected
scanInline scan chars =
    case chars of
        [] ->
            finish scan

        '\n' :: rest ->
            let
                ( afterFence, restAfterFence ) =
                    skipFencedCodeBlock (emit [ '\n' ] scan) rest
            in
            scanInline afterFence restAfterFence

        '\\' :: '[' :: rest ->
            let
                ( next, afterMath ) =
                    scanMath scan bracketDisplay rest
            in
            scanInline next afterMath

        '\\' :: '(' :: rest ->
            let
                ( next, afterMath ) =
                    scanMath scan parenInline rest
            in
            scanInline next afterMath

        '\\' :: escaped :: rest ->
            if isEscapable escaped then
                scanInline (emit [ '\\', escaped ] scan) rest

            else
                scanInline (emit [ '\\' ] scan) (escaped :: rest)

        '$' :: '$' :: rest ->
            let
                ( next, afterMath ) =
                    scanMath scan dollarDisplay rest
            in
            scanInline next afterMath

        '$' :: afterDollar :: rest ->
            if isWhitespace afterDollar then
                scanInline (emit [ '$' ] scan) (afterDollar :: rest)

            else
                let
                    ( next, afterMath ) =
                        scanMath scan dollarInline (afterDollar :: rest)
                in
                scanInline next afterMath

        '`' :: _ ->
            let
                ( openingRun, afterOpening ) =
                    spanWhile ((==) '`') chars
            in
            case codeSpanBody (List.length openingRun) [] afterOpening of
                Just ( body, rest ) ->
                    scanInline (emit (openingRun ++ body) scan) rest

                Nothing ->
                    scanInline (emit openingRun scan) afterOpening

        char :: rest ->
            scanInline (emit [ char ] scan) rest


type alias Delimiter =
    { opener : List Char
    , closer : List Char
    , display : Bool
    , acceptsCloser : Maybe Char -> List Char -> Bool
    }


bracketDisplay : Delimiter
bracketDisplay =
    { opener = [ '\\', '[' ], closer = [ '\\', ']' ], display = True, acceptsCloser = \_ _ -> True }


parenInline : Delimiter
parenInline =
    { opener = [ '\\', '(' ], closer = [ '\\', ')' ], display = False, acceptsCloser = \_ _ -> True }


dollarDisplay : Delimiter
dollarDisplay =
    { opener = [ '$', '$' ], closer = [ '$', '$' ], display = True, acceptsCloser = \_ _ -> True }


dollarInline : Delimiter
dollarInline =
    { opener = [ '$' ]
    , closer = [ '$' ]
    , display = False
    , acceptsCloser =
        \previous afterCloser ->
            case previous of
                Just before ->
                    not (isWhitespace before || startsWithDigit afterCloser)

                Nothing ->
                    False
    }


scanMath : Scan -> Delimiter -> List Char -> ( Scan, List Char )
scanMath scan delimiter afterOpener =
    case findCloser delimiter [] afterOpener of
        Just ( tex, rest ) ->
            let
                texString =
                    String.fromList tex
            in
            if String.isEmpty (String.trim texString) then
                ( emit delimiter.opener scan, afterOpener )

            else
                ( emitSpan
                    { tex = texString
                    , display = delimiter.display
                    , source = String.fromList (delimiter.opener ++ tex ++ delimiter.closer)
                    }
                    scan
                , rest
                )

        Nothing ->
            ( emit delimiter.opener scan, afterOpener )


findCloser : Delimiter -> List Char -> List Char -> Maybe ( List Char, List Char )
findCloser delimiter contentReversed chars =
    case ( acceptedCloser delimiter contentReversed chars, chars ) of
        ( Just afterCloser, _ ) ->
            Just ( List.reverse contentReversed, afterCloser )

        ( Nothing, [] ) ->
            Nothing

        ( Nothing, '\n' :: rest ) ->
            if startsBlankLine rest then
                Nothing

            else
                findCloser delimiter ('\n' :: contentReversed) rest

        ( Nothing, '\\' :: escaped :: rest ) ->
            findCloser delimiter (escaped :: '\\' :: contentReversed) rest

        ( Nothing, char :: rest ) ->
            findCloser delimiter (char :: contentReversed) rest


acceptedCloser : Delimiter -> List Char -> List Char -> Maybe (List Char)
acceptedCloser delimiter contentReversed chars =
    stripPrefix delimiter.closer chars
        |> Maybe.andThen
            (\afterCloser ->
                if delimiter.acceptsCloser (List.head contentReversed) afterCloser then
                    Just afterCloser

                else
                    Nothing
            )


stripPrefix : List Char -> List Char -> Maybe (List Char)
stripPrefix prefix chars =
    case ( prefix, chars ) of
        ( [], rest ) ->
            Just rest

        ( expected :: prefixRest, actual :: rest ) ->
            if expected == actual then
                stripPrefix prefixRest rest

            else
                Nothing

        ( _ :: _, [] ) ->
            Nothing


codeSpanBody : Int -> List Char -> List Char -> Maybe ( List Char, List Char )
codeSpanBody openingLength bodyReversed chars =
    case chars of
        [] ->
            Nothing

        '`' :: _ ->
            let
                ( run, rest ) =
                    spanWhile ((==) '`') chars

                withRun =
                    List.foldl (::) bodyReversed run
            in
            if List.length run == openingLength then
                Just ( List.reverse withRun, rest )

            else
                codeSpanBody openingLength withRun rest

        '\n' :: rest ->
            if startsBlankLine rest then
                Nothing

            else
                codeSpanBody openingLength ('\n' :: bodyReversed) rest

        char :: rest ->
            codeSpanBody openingLength (char :: bodyReversed) rest


fencedCodeBlock : List Char -> Maybe ( List Char, List Char )
fencedCodeBlock chars =
    let
        ( indent, afterIndent ) =
            spanWhile ((==) ' ') chars
    in
    case afterIndent of
        marker :: _ ->
            let
                ( fence, afterFence ) =
                    spanWhile ((==) marker) afterIndent

                ( infoString, _ ) =
                    spanWhile ((/=) '\n') afterFence
            in
            if
                List.length indent
                    <= 3
                    && (marker == '`' || marker == '~')
                    && List.length fence
                    >= 3
                    && not (marker == '`' && List.member '`' infoString)
            then
                let
                    ( body, rest ) =
                        fencedCodeBody marker (List.length fence) [] afterFence
                in
                Just ( indent ++ fence ++ body, rest )

            else
                Nothing

        [] ->
            Nothing


fencedCodeBody : Char -> Int -> List Char -> List Char -> ( List Char, List Char )
fencedCodeBody marker fenceLength bodyReversed chars =
    let
        ( line, afterLine ) =
            spanWhile ((/=) '\n') chars

        withLine =
            List.foldl (::) bodyReversed line
    in
    case afterLine of
        '\n' :: nextLines ->
            let
                ( nextLine, afterNextLine ) =
                    spanWhile ((/=) '\n') nextLines
            in
            if isClosingFence marker fenceLength nextLine then
                ( List.reverse (List.foldl (::) ('\n' :: withLine) nextLine), afterNextLine )

            else
                fencedCodeBody marker fenceLength ('\n' :: withLine) nextLines

        _ ->
            ( List.reverse withLine, afterLine )


isClosingFence : Char -> Int -> List Char -> Bool
isClosingFence marker fenceLength line =
    let
        ( indent, afterIndent ) =
            spanWhile ((==) ' ') line

        ( fence, afterFence ) =
            spanWhile ((==) marker) afterIndent
    in
    List.length indent <= 3 && List.length fence >= fenceLength && List.all isWhitespace afterFence


startsBlankLine : List Char -> Bool
startsBlankLine chars =
    case Tuple.second (spanWhile (\char -> char == ' ' || char == '\t') chars) of
        '\n' :: _ ->
            True

        _ ->
            False


startsWithDigit : List Char -> Bool
startsWithDigit chars =
    case chars of
        char :: _ ->
            Char.isDigit char

        [] ->
            False


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n' || char == '\u{000D}'


isEscapable : Char -> Bool
isEscapable char =
    String.contains (String.fromChar char) "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"


spanWhile : (Char -> Bool) -> List Char -> ( List Char, List Char )
spanWhile predicate chars =
    spanWhileHelp predicate [] chars


spanWhileHelp : (Char -> Bool) -> List Char -> List Char -> ( List Char, List Char )
spanWhileHelp predicate takenReversed chars =
    case chars of
        char :: rest ->
            if predicate char then
                spanWhileHelp predicate (char :: takenReversed) rest

            else
                ( List.reverse takenReversed, chars )

        [] ->
            ( List.reverse takenReversed, [] )
