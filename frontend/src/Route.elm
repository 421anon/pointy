module Route exposing (Highlight, HighlightTarget(..), LineRange, ProjectParams, Route(..), formatLineRange, fromUrl, highlightAnchor, highlightMatches, href, navigationTarget, parseLineRange, project, toString)

import Accessors exposing (Prism, prism)
import Html
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = Home
    | Project ProjectParams
    | NotFound


type alias ProjectParams =
    { projectId : Int
    , mHighlight : Maybe Highlight
    , mCommit : Maybe String
    }


type HighlightTarget
    = Output
    | Source


type alias Highlight =
    { id : Int
    , target : HighlightTarget
    , path : List String
    , range : Maybe LineRange
    }


type alias LineRange =
    { from : Int
    , to : Int
    }


project : Prism ls Route ProjectParams x y
project =
    prism ">Project"
        Project
        (\route ->
            case route of
                Project params ->
                    Ok params

                _ ->
                    Err route
        )


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map
            (\id basicHi mCommit mLines ->
                Project
                    { projectId = id
                    , mHighlight = Maybe.map (\hi -> { hi | range = mLines }) basicHi
                    , mCommit = mCommit
                    }
            )
            (Parser.s "project"
                </> Parser.int
                <?> Query.custom "hi" highlightParser
                <?> Query.string "commit"
                <?> Query.custom "lines" lineRangeParser
            )
        ]


highlightParser : List String -> Maybe Highlight
highlightParser strs =
    case Maybe.map (String.split "/") <| List.head strs of
        Just ("src" :: idStr :: rest) ->
            String.toInt idStr |> Maybe.map (\id -> { id = id, target = Source, path = rest, range = Nothing })

        Just (idStr :: rest) ->
            String.toInt idStr |> Maybe.map (\id -> { id = id, target = Output, path = rest, range = Nothing })

        _ ->
            Nothing


lineRangeParser : List String -> Maybe LineRange
lineRangeParser strs =
    List.head strs |> Maybe.andThen parseLineRange


parseLineRange : String -> Maybe LineRange
parseLineRange raw =
    case String.split "-" raw of
        [ a ] ->
            String.toInt a |> Maybe.map (\n -> { from = n, to = n })

        [ a, b ] ->
            Maybe.map2 (\f t -> { from = min f t, to = max f t })
                (String.toInt a)
                (String.toInt b)

        _ ->
            Nothing


formatLineRange : LineRange -> String
formatLineRange { from, to } =
    if from == to then
        String.fromInt from

    else
        String.fromInt from ++ "-" ++ String.fromInt to


highlightAnchor : HighlightTarget -> Int -> List String -> String
highlightAnchor target id path =
    let
        targetPrefix =
            case target of
                Output ->
                    "out"

                Source ->
                    "src"
    in
    String.join "/" (targetPrefix :: String.fromInt id :: path)


highlightMatches : HighlightTarget -> Int -> List String -> Highlight -> Bool
highlightMatches target recordId path highlight =
    highlight.target == target && highlight.id == recordId && highlight.path == path


navigationTarget : Route -> Route
navigationTarget route =
    case route of
        Project params ->
            Project { params | mHighlight = Maybe.map (\highlight -> { highlight | range = Nothing }) params.mHighlight }

        other ->
            other


fromUrl : Url -> Route
fromUrl url =
    case Parser.parse parser url of
        Just route ->
            route

        Nothing ->
            NotFound


href : Route -> Html.Attribute msg
href targetRoute =
    Attr.href (toString targetRoute)


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Project { projectId, mHighlight, mCommit } ->
            let
                baseUrl =
                    "/project/" ++ String.fromInt projectId

                hiStr =
                    Maybe.map
                        (\{ id, target, path } ->
                            let
                                hiPath =
                                    case target of
                                        Output ->
                                            String.fromInt id :: path

                                        Source ->
                                            "src" :: String.fromInt id :: path
                            in
                            "hi=" ++ String.join "/" hiPath
                        )
                        mHighlight

                commitStr =
                    Maybe.map (\c -> "commit=" ++ c) mCommit

                linesStr =
                    mHighlight
                        |> Maybe.andThen .range
                        |> Maybe.map (\r -> "lines=" ++ formatLineRange r)

                queryParts =
                    List.filterMap identity [ hiStr, commitStr, linesStr ]
            in
            if List.isEmpty queryParts then
                baseUrl

            else
                baseUrl ++ "?" ++ String.join "&" queryParts

        NotFound ->
            "/404"
