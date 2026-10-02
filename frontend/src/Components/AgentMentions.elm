module Components.AgentMentions exposing (Sources, sources, toHtml)

import Accessors exposing (try)
import Actions
import Api.ApiData as ApiData
import Browser.Dom as Dom
import Components.Markdown as Markdown
import Dict
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, title, type_)
import Html.Events as Events
import List.Extra as List
import Model.Core as Model exposing (Model, ProjectRecord)
import Model.Lenses as Lenses
import Model.Shadow exposing (StepConfig)
import Model.TableSpec as TableSpec
import Regex exposing (Regex)
import Route exposing (Route)
import Set
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


type alias Sources =
    { projects : List ProjectRecord
    , route : Route
    , stepConfig : StepConfig
    }


sources : Model -> Sources
sources model =
    { projects = ApiData.withDefault [] (Model.getProjects model).records
    , route = Model.getRoute model
    , stepConfig = ApiData.withDefault Dict.empty (Model.getStepConfig model)
    }


resolver : List ProjectRecord -> Route -> StepConfig -> String -> Resolver
resolver projects route stepConfig raw =
    let
        candidateStepIds =
            Regex.find digitsRegex raw
                |> List.filterMap (.match >> String.toInt)
                |> Set.fromList

        locations =
            if Set.isEmpty candidateStepIds then
                Dict.empty

            else
                Actions.stepLocations (\stepId -> Set.member stepId candidateStepIds) projects

        currentPath =
            try (Route.page << Lenses.projectRoute << Lenses.projectPath) route

        openProjectId =
            Maybe.map Route.pathProjectId currentPath

        canonicalPath =
            Model.canonicalProjectPath (Maybe.withDefault [] currentPath) projects
    in
    \entityId fixed candidates ->
        let
            resolved route_ runAction mName =
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
                { route = route_
                , runAction = runAction
                , label = label
                , tooltip = name
                , suffixText = resolveSuffix fixed mName candidates
                }
        in
        case entityId of
            StepId stepId ->
                Actions.stepOutputLocation openProjectId locations stepId
                    |> Maybe.map
                        (\location ->
                            resolved (Actions.stepOutputRoute (canonicalPath location.projectId) stepId)
                                (mentionRunAction stepConfig stepId location)
                                (Just location.step.name)
                        )

            ProjectId projectId ->
                List.find (\project -> project.id == Just projectId) projects
                    |> Maybe.map (\project -> resolved (Actions.projectPageRoute (canonicalPath projectId)) Nothing (Just project.name))


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


mentionRunAction : StepConfig -> Int -> Actions.StepLocation -> Maybe (Flow Model ())
mentionRunAction stepConfig stepId { projectId, step } =
    Dict.get step.type_ stepConfig
        |> Maybe.andThen
            (\entry ->
                let
                    spec =
                        Specs.stepsInProject projectId step.type_ entry
                in
                case TableSpec.getStatus spec step |> ApiData.toMaybe of
                    Just Model.StatusSuccess ->
                        Nothing

                    Just _ ->
                        Just (Actions.runStep spec stepId)

                    Nothing ->
                        Nothing
            )


toHtml : List ProjectRecord -> Route -> StepConfig -> String -> List (Html (Flow Model ()))
toHtml projects route stepConfig raw =
    Markdown.toHtml (viewText (resolver projects route stepConfig raw)) raw


digitsRegex : Regex
digitsRegex =
    fromRegex "[0-9]+"


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
