module Route exposing (ArtifactParams, ChatRef, CompareTarget, Comparison, Highlight, HighlightTarget(..), LineRange, ProjectParams, Route(..), chatFromUrl, chatHref, formatLineRange, fromUrl, highlightAnchor, highlightMatches, href, navigationTarget, project, toString, toStringWithChat)

import Accessors exposing (Prism, prism)
import Html
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Route
    = Home
    | Project ProjectParams
    | Artifact ArtifactParams
    | NotFound


type alias ProjectParams =
    { projectId : Int
    , mHighlight : Maybe Highlight
    , mCommit : Maybe String
    , mCompare : Maybe Comparison
    }


type alias ArtifactParams =
    { projectId : Int
    , stepId : Int
    , commit : String
    , path : List String
    }


type alias ChatRef =
    { sessionId : String
    , mTurnId : Maybe String
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


type alias Comparison =
    { left : CompareTarget
    , right : CompareTarget
    }


type alias CompareTarget =
    { id : Int
    , target : HighlightTarget
    , path : List String
    , commit : Maybe String
    , mimeType : Maybe String
    }


type alias CompareTargetRef =
    { id : Int
    , target : HighlightTarget
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
        , Parser.map
            (\id basicHi mCommit mLines leftRef leftCommit leftMime rightRef rightCommit rightMime ->
                Project
                    { projectId = id
                    , mHighlight = Maybe.map (\hi -> { hi | range = mLines }) basicHi
                    , mCommit = mCommit
                    , mCompare =
                        Maybe.map2
                            (\left right -> { left = left, right = right })
                            (compareTargetFromQuery leftRef leftCommit leftMime)
                            (compareTargetFromQuery rightRef rightCommit rightMime)
                    }
            )
            (Parser.s "project"
                </> Parser.int
                <?> Query.custom "hi" highlightParser
                <?> Query.string "commit"
                <?> Query.custom "lines" lineRangeParser
                <?> Query.custom "compareLeft" compareTargetParser
                <?> Query.string "compareLeftCommit"
                <?> Query.string "compareLeftMime"
                <?> Query.custom "compareRight" compareTargetParser
                <?> Query.string "compareRightCommit"
                <?> Query.string "compareRightMime"
            )
        , Parser.map
            (\projectId stepId commit mPath ->
                Artifact
                    { projectId = projectId
                    , stepId = stepId
                    , commit = commit
                    , path = Maybe.map (String.split "/") mPath |> Maybe.withDefault []
                    }
            )
            (Parser.s "artifact"
                </> Parser.int
                </> Parser.int
                </> Parser.string
                <?> Query.string "path"
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


compareTargetParser : List String -> Maybe CompareTargetRef
compareTargetParser strs =
    case Maybe.map (String.split "/") <| List.head strs of
        Just ("out" :: idStr :: rest) ->
            String.toInt idStr |> Maybe.map (\id -> { id = id, target = Output, path = rest })

        Just ("src" :: idStr :: rest) ->
            String.toInt idStr |> Maybe.map (\id -> { id = id, target = Source, path = rest })

        _ ->
            Nothing


compareTargetFromQuery : Maybe CompareTargetRef -> Maybe String -> Maybe String -> Maybe CompareTarget
compareTargetFromQuery mRef mCommit mMimeType =
    let
        nonEmpty value =
            if String.isEmpty value then
                Nothing

            else
                Just value
    in
    case mRef of
        Just ref ->
            let
                mimeType =
                    Maybe.andThen nonEmpty mMimeType
            in
            case ref.target of
                Output ->
                    mCommit
                        |> Maybe.andThen nonEmpty
                        |> Maybe.map (\commit -> { id = ref.id, target = ref.target, path = ref.path, commit = Just commit, mimeType = mimeType })

                Source ->
                    Just { id = ref.id, target = ref.target, path = ref.path, commit = Nothing, mimeType = mimeType }

        Nothing ->
            Nothing


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
            Project { params | mHighlight = Maybe.map (\highlight -> { highlight | range = Nothing }) params.mHighlight, mCompare = Nothing }

        other ->
            other


fromUrl : Url -> Route
fromUrl url =
    case Parser.parse parser url of
        Just route ->
            route

        Nothing ->
            NotFound


chatFromUrl : Url -> Maybe ChatRef
chatFromUrl url =
    Parser.parse (Parser.top <?> chatQuery) { url | path = "/", fragment = Nothing }
        |> Maybe.andThen identity


chatQuery : Query.Parser (Maybe ChatRef)
chatQuery =
    Query.map2 (\mSessionId mTurnId -> Maybe.map (\sessionId -> ChatRef sessionId mTurnId) mSessionId)
        (Query.string "chat")
        (Query.string "turn")


chatParts : ChatRef -> List String
chatParts chat =
    List.filterMap identity
        [ Just ("chat=" ++ chat.sessionId)
        , Maybe.map (\turnId -> "turn=" ++ turnId) chat.mTurnId
        ]


chatHref : ChatRef -> String
chatHref chat =
    "?" ++ String.join "&" (chatParts chat)


toStringWithChat : Maybe ChatRef -> Route -> String
toStringWithChat mChat route =
    let
        base =
            toString route

        separator =
            if String.contains "?" base then
                "&"

            else
                "?"
    in
    case mChat of
        Just chat ->
            base ++ separator ++ String.join "&" (chatParts chat)

        Nothing ->
            base


href : Route -> Html.Attribute msg
href targetRoute =
    Attr.href (toString targetRoute)


compareTargetRef : CompareTarget -> String
compareTargetRef compareTarget =
    let
        targetPrefix =
            case compareTarget.target of
                Output ->
                    "out"

                Source ->
                    "src"
    in
    String.join "/" (targetPrefix :: String.fromInt compareTarget.id :: compareTarget.path)


compareTargetQueryParts : String -> CompareTarget -> List (Maybe String)
compareTargetQueryParts prefix compareTarget =
    [ Just (prefix ++ "=" ++ compareTargetRef compareTarget)
    , Maybe.map (\commit -> prefix ++ "Commit=" ++ commit) compareTarget.commit
    , Maybe.map (\mimeType -> prefix ++ "Mime=" ++ mimeType) compareTarget.mimeType
    ]


toString : Route -> String
toString route =
    case route of
        Home ->
            "/"

        Project { projectId, mHighlight, mCommit, mCompare } ->
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

                compareStrs =
                    case mCompare of
                        Just compare_ ->
                            compareTargetQueryParts "compareLeft" compare_.left
                                ++ compareTargetQueryParts "compareRight" compare_.right

                        Nothing ->
                            []

                queryParts =
                    List.filterMap identity ([ hiStr, commitStr, linesStr ] ++ compareStrs)
            in
            if List.isEmpty queryParts then
                baseUrl

            else
                baseUrl ++ "?" ++ String.join "&" queryParts

        Artifact { projectId, stepId, commit, path } ->
            UrlBuilder.absolute
                [ "artifact", String.fromInt projectId, String.fromInt stepId, commit ]
                [ UrlBuilder.string "path" (String.join "/" path) ]

        NotFound ->
            "/404"
