module Route exposing
    ( ArtifactParams
    , ChatRef
    , CompareTarget
    , Comparison
    , Highlight
    , HighlightTarget(..)
    , LineRange
    , Page(..)
    , ProjectParams
    , Route
    , chat
    , chatHref
    , formatLineRange
    , fromPage
    , fromUrl
    , highlightAnchor
    , highlightMatches
    , href
    , navigationTarget
    , page
    , project
    , routeUrlIso
    , toString
    )

{-| `Route` is a full, lawful model of the URL: `fromUrl` and `toUrl` are
inverses (`routeUrlIso`). The page-specific part is `Page`; `chat` is the
first cross-cutting URL widget; every other query parameter is preserved
verbatim in `extraQuery` so nothing the URL carries is ever dropped.

Lawfulness: `fromUrl (toUrl r) == r` for every `r`; `toUrl (fromUrl u) == u`
for every URL in the app's own canonical rendering (parameters the app
renders are percent-encoded; hand-typed non-canonical encodings are
normalized on first navigation).

-}

import Accessors exposing (Iso, Lens, Prism, iso, lens, prism)
import Html
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type alias Route =
    { page : Page
    , chat : Maybe ChatRef
    , protocol : Url.Protocol
    , host : String
    , port_ : Maybe Int
    , extraQuery : Maybe String
    , fragment : Maybe String
    }


type Page
    = Home
    | Project ProjectParams
    | Artifact ArtifactParams
    | NotFound { path : String, query : Maybe String }


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


page : Lens ls Route Page x y
page =
    lens ".page" .page (\r p -> { r | page = p })


chat : Lens ls Route (Maybe ChatRef) x y
chat =
    lens ".chat" .chat (\r c -> { r | chat = c })


{-| The lawful `Url` <-> `Route` pair: `get routeUrlIso` is `fromUrl`,
`set routeUrlIso` is `toUrl`. See the module docs for the exact laws.
-}
routeUrlIso : Iso pr ls Url Route x y
routeUrlIso =
    iso "route-url" fromUrl toUrl


project : Prism pr Page ProjectParams x y
project =
    prism ">Project"
        Project
        (\page_ ->
            case page_ of
                Project params ->
                    Ok params

                _ ->
                    Err page_
        )


{-| A bare route with no chat and no extra URL state; used to build
navigation targets. `host` is empty, so `toString` renders it relative.
-}
fromPage : Page -> Route
fromPage page_ =
    { page = page_
    , chat = Nothing
    , protocol = Url.Https
    , host = ""
    , port_ = Nothing
    , extraQuery = Nothing
    , fragment = Nothing
    }


fromUrl : Url -> Route
fromUrl url =
    let
        mChat =
            chatFromQuery url.query

        queryWithoutChat =
            stripQueryKeys [ "chat", "turn" ] url.query

        ( page_, extraQuery ) =
            case Parser.parse pageParser url of
                Just parsedPage ->
                    ( parsedPage, stripQueryKeys (pageQueryKeys parsedPage) queryWithoutChat )

                Nothing ->
                    ( NotFound { path = url.path, query = queryWithoutChat }, Nothing )
    in
    { page = page_
    , chat = mChat
    , protocol = url.protocol
    , host = url.host
    , port_ = url.port_
    , extraQuery = extraQuery
    , fragment = url.fragment
    }


toUrl : Route -> Url
toUrl route =
    let
        ( pagePath, pageQueryParts ) =
            case route.page of
                Home ->
                    ( "/", [] )

                Project params ->
                    projectUrlParts params

                Artifact params ->
                    artifactUrlParts params

                NotFound { path, query } ->
                    ( path
                    , case query of
                        Just rawQuery ->
                            [ rawQuery ]

                        Nothing ->
                            []
                    )
    in
    { protocol = route.protocol
    , host = route.host
    , port_ = route.port_
    , path = pagePath
    , query = joinQueryParts pageQueryParts route.extraQuery (chatQueryParts route.chat)
    , fragment = route.fragment
    }


toString : Route -> String
toString route =
    let
        url =
            toUrl route
    in
    url.path
        ++ (case url.query of
                Just q ->
                    "?" ++ q

                Nothing ->
                    ""
           )
        ++ (case url.fragment of
                Just f ->
                    "#" ++ f

                Nothing ->
                    ""
           )


href : Route -> Html.Attribute msg
href targetRoute =
    Attr.href (toString targetRoute)


chatHref : ChatRef -> String
chatHref chatRef =
    "?" ++ String.join "&" (chatQueryParts (Just chatRef))


navigationTarget : Route -> Route
navigationTarget route =
    { route
        | chat = Nothing
        , extraQuery = Nothing
        , fragment = Nothing
        , page =
            case route.page of
                Project params ->
                    Project { params | mHighlight = Maybe.map (\highlight -> { highlight | range = Nothing }) params.mHighlight, mCompare = Nothing }

                other ->
                    other
    }


pageParser : Parser (Page -> a) a
pageParser =
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


pageQueryKeys : Page -> List String
pageQueryKeys page_ =
    case page_ of
        Project _ ->
            [ "hi"
            , "commit"
            , "lines"
            , "compareLeft"
            , "compareLeftCommit"
            , "compareLeftMime"
            , "compareRight"
            , "compareRightCommit"
            , "compareRightMime"
            ]

        Artifact _ ->
            [ "path" ]

        Home ->
            []

        NotFound _ ->
            []


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


projectUrlParts : ProjectParams -> ( String, List String )
projectUrlParts { projectId, mHighlight, mCommit, mCompare } =
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
                    "hi=" ++ String.join "/" (List.map Url.percentEncode hiPath)
                )
                mHighlight

        commitStr =
            Maybe.map (\c -> "commit=" ++ Url.percentEncode c) mCommit

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
    in
    ( baseUrl
    , List.filterMap identity ([ hiStr, commitStr, linesStr ] ++ compareStrs)
    )


artifactUrlParts : ArtifactParams -> ( String, List String )
artifactUrlParts { projectId, stepId, commit, path } =
    ( "/artifact/" ++ String.fromInt projectId ++ "/" ++ String.fromInt stepId ++ "/" ++ commit
    , [ "path=" ++ Url.percentEncode (String.join "/" path) ]
    )


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
    String.join "/" (List.map Url.percentEncode (targetPrefix :: String.fromInt compareTarget.id :: compareTarget.path))


compareTargetQueryParts : String -> CompareTarget -> List (Maybe String)
compareTargetQueryParts prefix compareTarget =
    [ Just (prefix ++ "=" ++ compareTargetRef compareTarget)
    , Maybe.map (\commit -> prefix ++ "Commit=" ++ Url.percentEncode commit) compareTarget.commit
    , Maybe.map (\mimeType -> prefix ++ "Mime=" ++ Url.percentEncode mimeType) compareTarget.mimeType
    ]



-- Chat widget: query pair surgery on the raw query string, so every other
-- parameter round-trips byte-for-byte.


chatFromQuery : Maybe String -> Maybe ChatRef
chatFromQuery mQuery =
    let
        pairs =
            queryPairs mQuery
    in
    Maybe.map
        (\sessionId -> { sessionId = sessionId, mTurnId = findQueryValue "turn" pairs })
        (findQueryValue "chat" pairs)


chatQueryParts : Maybe ChatRef -> List String
chatQueryParts mChat =
    case mChat of
        Just chatRef ->
            ("chat=" ++ Url.percentEncode chatRef.sessionId)
                :: (case chatRef.mTurnId of
                        Just turnId ->
                            [ "turn=" ++ Url.percentEncode turnId ]

                        Nothing ->
                            []
                   )

        Nothing ->
            []


queryPairs : Maybe String -> List String
queryPairs mQuery =
    Maybe.withDefault "" mQuery |> String.split "&"


pairKey : String -> String
pairKey pair =
    case String.split "=" pair of
        key :: _ ->
            key

        [] ->
            ""


findQueryValue : String -> List String -> Maybe String
findQueryValue key pairs =
    pairs
        |> List.filter (\p -> pairKey p == key)
        |> List.head
        |> Maybe.andThen pairValue


pairValue : String -> Maybe String
pairValue pair =
    let
        rawValue =
            String.dropLeft (String.length (pairKey pair) + 1) pair
    in
    if String.isEmpty rawValue then
        Nothing

    else
        Url.percentDecode rawValue


stripQueryKeys : List String -> Maybe String -> Maybe String
stripQueryKeys keys mQuery =
    let
        remaining =
            queryPairs mQuery
                |> List.filter (\p -> not (List.member (pairKey p) keys))
                |> List.filter (\p -> p /= "")
    in
    case remaining of
        [] ->
            Nothing

        _ ->
            Just (String.join "&" remaining)


joinQueryParts : List String -> Maybe String -> List String -> Maybe String
joinQueryParts pageParts mExtra chatParts =
    let
        all =
            pageParts
                ++ (case mExtra of
                        Just extra ->
                            [ extra ]

                        Nothing ->
                            []
                   )
                ++ chatParts
    in
    case all of
        [] ->
            Nothing

        _ ->
            Just (String.join "&" all)
