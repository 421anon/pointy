module Components.AgentMentions exposing (Resolver, mentionTarget, toHtml)

import Accessors exposing (try)
import Actions
import Api.ApiData as ApiData
import Browser.Dom as Dom
import Dict
import Extra.Accessors exposing (by)
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, title, type_)
import Html.Events as Events
import Markdown.Block as Block
import Markdown.Inline as Inline exposing (Inline)
import Model.Core as Model exposing (Model)
import Model.Lenses as Lenses
import Model.TableSpec as TableSpec
import Regex exposing (Regex)
import Route exposing (Route)
import Specs
import View.Icons


type EntityId
    = StepId Int
    | ProjectId Int


type alias ResolvedMention =
    { route : Route
    , runAction : Maybe (Flow Model ())
    , label : String
    , tooltip : String
    , suffixText : String
    }


type alias Resolver =
    EntityId -> Bool -> List String -> Maybe ResolvedMention


mentionTarget : Model -> EntityId -> Bool -> List String -> Maybe ResolvedMention
mentionTarget model entityId fixed candidates =
    let
        resolved route runAction mName =
            let
                name =
                    Maybe.withDefault (entityIdText entityId) mName

                label =
                    case entityId of
                        StepId _ ->
                            entityIdText entityId

                        ProjectId _ ->
                            name
            in
            { route = route
            , runAction = runAction
            , label = label
            , tooltip = name
            , suffixText = resolveSuffix fixed mName candidates
            }
    in
    case entityId of
        StepId stepId ->
            Actions.stepOutputRoute model stepId
                |> Maybe.map
                    (\route ->
                        case route.page of
                            Route.Project params ->
                                (resolved route
                                        (mentionRunAction model params.projectId stepId)
                                        (try (Lenses.projectStep (Just params.projectId) (Just stepId)) model |> Maybe.map .name)
                                    )

                            _ ->
                                (resolved route Nothing Nothing)
                    )

        ProjectId projectId ->
            Actions.knownProjectRoute model projectId
                |> Maybe.map
                    (\route ->
                        resolved route
                            Nothing
                            (try (Lenses.projects << Lenses.records << ApiData.success << by .id (Just projectId)) model |> Maybe.map .name)
                    )


resolveSuffix : Bool -> Maybe String -> List String -> String
resolveSuffix fixed mName candidates =
    let
        used =
            if fixed then
                List.length candidates

            else
                namePrefixLength
                    (Maybe.withDefault [] (Maybe.map String.words mName))
                    candidates
    in
    suffixFrom (List.drop used candidates)


suffixFrom : List String -> String
suffixFrom words =
    if List.isEmpty words then
        ""

    else
        " " ++ String.join " " words


namePrefixLength : List String -> List String -> Int
namePrefixLength nameTokens candidates =
    case ( nameTokens, candidates ) of
        ( token :: restName, candidate :: restCandidates ) ->
            if String.toLower token == String.toLower candidate then
                1 + namePrefixLength restName restCandidates

            else
                0

        _ ->
            0


mentionRunAction : Model -> Int -> Int -> Maybe (Flow Model ())
mentionRunAction model projectId stepId =
    try (Lenses.projectStep (Just projectId) (Just stepId)) model
        |> Maybe.andThen
            (\step ->
                try (Lenses.stepConfig << ApiData.success) model
                    |> Maybe.andThen (Dict.get step.type_)
                    |> Maybe.andThen
                        (\entry ->
                            let
                                spec =
                                    Specs.stepsInProject projectId step.type_ entry
                            in
                            case TableSpec.getStatus spec step |> ApiData.toMaybe of
                                Just Model.StatusSuccess ->
                                    Nothing

                                _ ->
                                    Just (Actions.runStep spec stepId)
                        )
            )


toHtml : Resolver -> String -> List (Html (Flow Model ()))
toHtml resolve body =
    Block.parse Nothing body
        |> List.concatMap (Block.defaultHtml Nothing (Just (inlineToHtml resolve)))


nameToken : String
nameToken =
    "[^\\s@\",.;:!?()\\[\\]{}\\u2014\\u2013-]+"


wholeQuotedRegex : Regex
wholeQuotedRegex =
    fromRegex "\"@\\[(step|project):([0-9]+)\\]\\s+([^\"]+)\""


structuredRegex : Regex
structuredRegex =
    fromRegex ("@\\[(step|project):([0-9]+)\\]\\s*(\"[^\"]+\"|" ++ nameToken ++ "(?:\\s+" ++ nameToken ++ "){0,4})")


bareStructuredRegex : Regex
bareStructuredRegex =
    fromRegex "@\\[(step|project):([0-9]+)\\]"


legacyRegex : Regex
legacyRegex =
    fromRegex "\\b(step|project)\\s+([0-9]+)\\b"


fromRegex : String -> Regex
fromRegex pattern =
    Maybe.withDefault Regex.never
        (Regex.fromStringWith { caseInsensitive = True, multiline = False } pattern)


type alias ParsedMention =
    { candidates : List String
    , fixedLabel : Bool
    , entityId : EntityId
    }


toEntityId : String -> Int -> EntityId
toEntityId keyword id_ =
    if String.toLower keyword == "step" then
        StepId id_

    else
        ProjectId id_


unquote : String -> String
unquote label =
    if String.startsWith "\"" label && String.endsWith "\"" label && String.length label >= 2 then
        String.slice 1 -1 label

    else
        label


parseWholeQuoted : Regex.Match -> Maybe ParsedMention
parseWholeQuoted match =
    decodeCommon match
        (\keyword digits label ->
            { candidates = String.words label
            , fixedLabel = True
            , entityId = toEntityId keyword digits
            }
        )


parseStructured : Regex.Match -> Maybe ParsedMention
parseStructured match =
    decodeCommon match
        (\keyword digits label ->
            { candidates = String.words (unquote label)
            , fixedLabel = String.startsWith "\"" label && String.endsWith "\"" label
            , entityId = toEntityId keyword digits
            }
        )


parseBareStructured : Regex.Match -> Maybe ParsedMention
parseBareStructured match =
    case match.submatches of
        [ Just keyword, Just digits ] ->
            String.toInt digits
                |> Maybe.map
                    (\id_ ->
                        { candidates = []
                        , fixedLabel = True
                        , entityId = toEntityId keyword id_
                        }
                    )

        _ ->
            Nothing


decodeCommon : Regex.Match -> (String -> Int -> String -> ParsedMention) -> Maybe ParsedMention
decodeCommon match build =
    case match.submatches of
        [ Just keyword, Just digits, Just label ] ->
            String.toInt digits
                |> Maybe.map (\id_ -> build keyword id_ label)

        _ ->
            Nothing


parseLegacy : Regex.Match -> Maybe ParsedMention
parseLegacy match =
    case match.submatches of
        [ Just keyword, Just digits ] ->
            String.toInt digits
                |> Maybe.map
                    (\id_ ->
                        { candidates = String.words match.match
                        , fixedLabel = True
                        , entityId = toEntityId keyword id_
                        }
                    )

        _ ->
            Nothing


type alias MentionSpan =
    { index : Int
    , end : Int
    , rawText : String
    , parsed : ParsedMention
    }


mentionSpans : String -> List MentionSpan
mentionSpans text =
    List.sortWith compareMentionSpans
        (spansOf parseWholeQuoted wholeQuotedRegex text
            ++ spansOf parseStructured structuredRegex text
            ++ spansOf parseBareStructured bareStructuredRegex text
            ++ spansOf parseLegacy legacyRegex text
        )


compareMentionSpans : MentionSpan -> MentionSpan -> Order
compareMentionSpans left right =
    case compare left.index right.index of
        EQ ->
            compare right.end left.end

        order ->
            order


spansOf : (Regex.Match -> Maybe ParsedMention) -> Regex -> String -> List MentionSpan
spansOf parseFor regex text =
    List.filterMap (toSpan parseFor) (Regex.find regex text)


toSpan : (Regex.Match -> Maybe ParsedMention) -> Regex.Match -> Maybe MentionSpan
toSpan parseFor match =
    parseFor match
        |> Maybe.map
            (\parsed ->
                { index = match.index
                , end = match.index + String.length match.match
                , rawText = match.match
                , parsed = parsed
                }
            )


type Segment
    = Plain String
    | Mention { rawText : String, candidates : List String, fixedLabel : Bool, entityId : EntityId }


toSegments : String -> List Segment
toSegments text =
    collectSpans text 0 (mentionSpans text) []
        |> List.reverse


collectSpans : String -> Int -> List MentionSpan -> List Segment -> List Segment
collectSpans text cursor spans segments =
    case spans of
        [] ->
            consPlain (String.dropLeft cursor text) segments

        span :: rest ->
            if span.index < cursor then
                collectSpans text cursor rest segments

            else
                collectSpans text
                    span.end
                    rest
                    (Mention { rawText = span.rawText, candidates = span.parsed.candidates, fixedLabel = span.parsed.fixedLabel, entityId = span.parsed.entityId }
                        :: consPlain (String.slice cursor span.index text) segments
                    )


consPlain : String -> List Segment -> List Segment
consPlain text segments =
    if String.isEmpty text then
        segments

    else
        Plain text :: segments


isMention : Segment -> Bool
isMention segment =
    case segment of
        Mention _ ->
            True

        Plain _ ->
            False


inlineToHtml : Resolver -> Inline i -> Html (Flow Model ())
inlineToHtml resolve inline =
    case inline of
        Inline.Link _ _ _ ->
            Inline.defaultHtml Nothing inline

        Inline.HtmlInline _ _ _ ->
            Inline.defaultHtml Nothing inline

        Inline.Text text ->
            viewText resolve text

        _ ->
            Inline.defaultHtml (Just (inlineToHtml resolve)) inline


viewText : Resolver -> String -> Html (Flow Model ())
viewText resolve text =
    let
        segments =
            toSegments text
    in
    if List.any isMention segments then
        Html.span [] (List.map (viewSegment resolve) segments)

    else
        Html.text text


viewSegment : Resolver -> Segment -> Html (Flow Model ())
viewSegment resolve segment =
    case segment of
        Plain text ->
            Html.text text

        Mention { rawText, candidates, fixedLabel, entityId } ->
            case resolve entityId fixedLabel candidates of
                Nothing ->
                    Html.text rawText

                Just target ->
                    Html.span []
                        (viewMention entityId target
                            :: (if String.isEmpty target.suffixText then
                                    []

                                else
                                    [ Html.text target.suffixText ]
                               )
                        )


entityIdText : EntityId -> String
entityIdText entity =
    case entity of
        StepId id_ ->
            "step " ++ String.fromInt id_

        ProjectId id_ ->
            "project " ++ String.fromInt id_


viewMention : EntityId -> ResolvedMention -> Html (Flow Model ())
viewMention entity target =
    Html.span [ class "agent-panel__mention" ]
        (Html.a
            [ class "agent-panel__mention-link"
            , Route.href target.route
            , title target.tooltip
            , Events.onClick
                (Flow.performTask Dom.getViewport
                    |> Flow.andThen
                        (\viewport ->
                            Flow.when (viewport.viewport.width <= 900) Actions.toggleAgentPanel
                        )
                )
            ]
            [ Html.text target.label ]
            :: viewMentionActions entity target
        )


viewMentionActions : EntityId -> ResolvedMention -> List (Html (Flow Model ()))
viewMentionActions entity target =
    case target.runAction of
        Just runAction ->
            [ viewRunAction entity runAction ]

        Nothing ->
            []


viewRunAction : EntityId -> Flow Model () -> Html (Flow Model ())
viewRunAction entity runAction =
    viewAction ("Run " ++ entityIdText entity) "Run" "play_arrow" runAction


viewAction : String -> String -> String -> Flow Model () -> Html (Flow Model ())
viewAction ariaLabel tooltip iconName action =
    Html.button
        [ class "agent-panel__mention-action"
        , type_ "button"
        , title tooltip
        , attribute "aria-label" ariaLabel
        , Events.onClick action
        ]
        [ View.Icons.iconCustom True iconName [ attribute "aria-hidden" "true" ] ]
