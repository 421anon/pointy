module Components.AgentMentions exposing (Resolver, mentionTarget, toHtml)

import Accessors exposing (try)
import Actions
import Api.ApiData as ApiData
import Dict
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


type alias MentionTarget =
    { route : Route
    , runAction : Maybe (Flow Model ())
    }


type alias Resolver =
    EntityId -> Maybe MentionTarget


mentionTarget : Model -> EntityId -> Maybe MentionTarget
mentionTarget model entityId =
    case entityId of
        StepId stepId ->
            Actions.stepOutputRoute model stepId
                |> Maybe.map
                    (\route ->
                        { route = route
                        , runAction =
                            case route.page of
                                Route.Project params ->
                                    mentionRunAction model params.projectId stepId

                                _ ->
                                    Nothing
                        }
                    )

        ProjectId projectId ->
            Actions.knownProjectRoute model projectId
                |> Maybe.map (\route -> { route = route, runAction = Nothing })


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

mentionRegex : Regex
mentionRegex =
    Maybe.withDefault Regex.never
        (Regex.fromStringWith
            { caseInsensitive = True, multiline = False }
            "\"@\\[(step|project):([0-9]+)\\]\\s+([^\"]+)\"|@\\[(step|project):([0-9]+)\\]\\s*(\"[^\"]+\"|[^\\s\",.;:!?()\\[\\]{}]+)|\\b(step|project)\\s+([0-9]+)\\b"
        )



type alias MentionDetails =
    { label : String
    , entityId : EntityId
    }


type Segment
    = Plain String
    | Mention { rawText : String, label : String, entityId : EntityId }


isMention : Segment -> Bool
isMention segment =
    case segment of
        Mention _ ->
            True

        Plain _ ->
            False



mentionFromMatch : Regex.Match -> Maybe MentionDetails
mentionFromMatch match =
    let
        unquote label =
            if String.startsWith "\"" label && String.endsWith "\"" label && String.length label >= 2 then
                String.slice 1 -1 label

            else
                label

        fromParts keyword digits label =
            String.toInt digits
                |> Maybe.map
                    (\id_ ->
                        { label = unquote label
                        , entityId =
                            if String.toLower keyword == "step" then
                                StepId id_

                            else
                                ProjectId id_
                        }
                    )
    in
    case match.submatches of
        [ Just keyword, Just digits, Just label, _, _, _, _, _ ] ->
            fromParts keyword digits label

        [ _, _, _, Just keyword, Just digits, Just label, _, _ ] ->
            fromParts keyword digits label

        [ _, _, _, _, _, _, Just keyword, Just digits ] ->
            fromParts keyword digits match.match

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
            case mentionFromMatch match of
                Nothing ->
                    collectSegments text end rest (consPlain (String.slice cursor end text) segments)

                Just details ->
                    collectSegments text
                        end
                        rest
                        (Mention { rawText = match.match, label = details.label, entityId = details.entityId }
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

        Mention { rawText, label, entityId } ->
            case resolve entityId of
                Nothing ->
                    Html.text rawText

                Just target ->
                    viewMention entityId label target


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
viewMention entityId label target =
    Html.span [ class "agent-panel__mention" ]
        (Html.a
            [ class "agent-panel__mention-link"
            , Route.href target.route
            , title (mentionTitle entityId target.route)
            ]
            [ Html.text label ]
            :: viewMentionActions entityId target
        )


viewMentionActions : EntityId -> MentionTarget -> List (Html (Flow Model ()))
viewMentionActions entityId target =
    case target.runAction of
        Just runAction ->
            [ viewRunAction entityId runAction ]

        Nothing ->
            []


viewRunAction : EntityId -> Flow Model () -> Html (Flow Model ())
viewRunAction entityId runAction =
    viewAction ("Run " ++ entityIdText entityId) "Run" "play_arrow" runAction


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
