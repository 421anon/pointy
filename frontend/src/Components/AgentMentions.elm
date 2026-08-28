module Components.AgentMentions exposing (Resolver, mentionTarget, toHtml)

import Actions
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, title, type_)
import Html.Events as Events
import Markdown.Block as Block
import Markdown.Inline as Inline exposing (Inline)
import Model.Core exposing (Model)
import Regex exposing (Regex)
import Route exposing (Route)
import Specs
import View.Icons


type EntityId
    = StepId Int
    | ProjectId Int


type alias MentionTarget =
    { route : Route
    , runControl : Maybe Specs.StepRunControl
    }


type alias Resolver =
    EntityId -> Maybe MentionTarget


mentionTarget : Model -> EntityId -> Maybe MentionTarget
mentionTarget model entityId =
    case entityId of
        StepId stepId ->
            Actions.stepOutputRoute model stepId
                |> Maybe.map (\route -> { route = route, runControl = Specs.stepRunControl model stepId })

        ProjectId projectId ->
            Actions.knownProjectRoute model projectId
                |> Maybe.map (\route -> { route = route, runControl = Nothing })


toHtml : Resolver -> String -> List (Html (Flow Model ()))
toHtml resolve body =
    Block.parse Nothing body
        |> List.concatMap (Block.defaultHtml Nothing (Just (inlineToHtml resolve)))


mentionRegex : Regex
mentionRegex =
    Maybe.withDefault Regex.never
        (Regex.fromStringWith { caseInsensitive = True, multiline = False } "\\b(step|project)\\s+([0-9]+)\\b")


type Segment
    = Plain String
    | Mention { rawText : String, entityId : EntityId }


isMention : Segment -> Bool
isMention segment =
    case segment of
        Mention _ ->
            True

        Plain _ ->
            False


entityIdFromMatch : Regex.Match -> Maybe EntityId
entityIdFromMatch match =
    case match.submatches of
        [ Just keyword, Just digits ] ->
            String.toInt digits
                |> Maybe.map
                    (\id_ ->
                        if String.toLower keyword == "step" then
                            StepId id_

                        else
                            ProjectId id_
                    )

        _ ->
            Nothing


toSegments : String -> List Segment
toSegments text =
    collectSegments text 0 (Regex.find mentionRegex text) []
        |> List.reverse


collectSegments : String -> Int -> List Regex.Match -> List Segment -> List Segment
collectSegments text cursor matches segments =
    case matches of
        [] ->
            consPlain (String.dropLeft cursor text) segments

        match :: rest ->
            let
                end =
                    match.index + String.length match.match
            in
            case entityIdFromMatch match of
                Nothing ->
                    collectSegments text end rest (consPlain (String.slice cursor end text) segments)

                Just entityId ->
                    collectSegments text
                        end
                        rest
                        (Mention { rawText = match.match, entityId = entityId }
                            :: consPlain (String.slice cursor match.index text) segments
                        )


consPlain : String -> List Segment -> List Segment
consPlain text segments =
    if String.isEmpty text then
        segments

    else
        Plain text :: segments


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

        Mention { rawText, entityId } ->
            case resolve entityId of
                Nothing ->
                    Html.text rawText

                Just target ->
                    viewMention entityId rawText target


entityIdText : EntityId -> String
entityIdText entityId =
    case entityId of
        StepId id_ ->
            "step " ++ String.fromInt id_

        ProjectId id_ ->
            "project " ++ String.fromInt id_


mentionTitle : EntityId -> Route -> String
mentionTitle entityId linkRoute =
    case ( entityId, linkRoute.page ) of
        ( StepId _, Route.Project params ) ->
            "Open " ++ entityIdText entityId ++ " in project " ++ String.fromInt params.projectId

        _ ->
            "Open " ++ entityIdText entityId


viewMention : EntityId -> String -> MentionTarget -> Html (Flow Model ())
viewMention entityId rawText target =
    Html.span [ class "agent-panel__mention" ]
        (Html.a
            [ class "agent-panel__mention-link"
            , Route.href target.route
            , title (mentionTitle entityId target.route)
            ]
            [ Html.text rawText ]
            :: viewMentionActions entityId target
        )


viewMentionActions : EntityId -> MentionTarget -> List (Html (Flow Model ()))
viewMentionActions entityId target =
    case target.runControl of
        Just control ->
            [ viewRunControl entityId control ]

        Nothing ->
            []


viewRunControl : EntityId -> Specs.StepRunControl -> Html (Flow Model ())
viewRunControl entityId control =
    let
        ( tooltip, iconName, action ) =
            case control of
                Specs.Runnable run ->
                    ( "Run", "play_arrow", run )

                Specs.Stoppable stop ->
                    ( "Stop", "stop", stop )
    in
    viewAction (tooltip ++ " " ++ entityIdText entityId) tooltip iconName action


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
