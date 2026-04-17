module Route exposing (Highlight, ProjectParams, Route(..), fromUrl, href, project, toString)

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


type alias Highlight =
    { id : Int
    , path : List String
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
        , Parser.map (\id hi mCommit -> Project { projectId = id, mHighlight = hi, mCommit = mCommit })
            (Parser.s "project" </> Parser.int <?> Query.custom "hi" highlightParser <?> Query.string "commit")
        ]


highlightParser : List String -> Maybe Highlight
highlightParser strs =
    case Maybe.map (String.split "/") <| List.head strs of
        Just (idStr :: rest) ->
            String.toInt idStr |> Maybe.map (\id -> { id = id, path = rest })

        _ ->
            Nothing


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
                        (\{ id, path } ->
                            "hi="
                                ++ String.fromInt id
                                ++ (if List.isEmpty path then
                                        ""

                                    else
                                        "/" ++ String.join "/" path
                                   )
                        )
                        mHighlight

                commitStr =
                    Maybe.map (\c -> "commit=" ++ c) mCommit

                queryParts =
                    List.filterMap identity [ hiStr, commitStr ]
            in
            if List.isEmpty queryParts then
                baseUrl

            else
                baseUrl ++ "?" ++ String.join "&" queryParts

        NotFound ->
            "/404"
