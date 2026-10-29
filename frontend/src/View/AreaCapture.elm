module View.AreaCapture exposing (view)

import Actions
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Maybe.Extra as Maybe
import Model.Core as Model exposing (Model)
import View.Icons exposing (icon, iconCustom)
import View.Lib exposing (boolText)


view : Model.AgentState -> Model.AreaCapture -> Html (Flow Model ())
view agentState capture =
    Html.div
        [ class "artifact-viewer-capture"
        , Events.preventDefaultOn "mousedown" (Decode.map (\drag -> ( Actions.beginAreaDrag drag, True )) dragStartDecoder)
        ]
        (case capture of
            Model.AreaCaptureArmed ->
                []

            Model.AreaCaptureDragging drag ->
                [ viewRect (Model.areaDragRect drag) ]

            Model.AreaCaptureAsking question ->
                [ viewRect question.rect, viewQuestion agentState question ]
        )


px : Float -> String
px value =
    String.fromFloat value ++ "px"


viewRect : Model.AreaRect -> Html msg
viewRect rect =
    Html.div
        [ class "artifact-viewer-capture-area"
        , style "left" (px rect.x)
        , style "top" (px rect.y)
        , style "width" (px rect.width)
        , style "height" (px rect.height)
        ]
        []


questionWidth : Float
questionWidth =
    360


questionHeight : Float
questionHeight =
    48


questionGap : Float
questionGap =
    8


questionPlacement : Model.AreaQuestion -> List (Html.Attribute msg)
questionPlacement { rect, overlaySize } =
    let
        width =
            min questionWidth (overlaySize.x - 2 * questionGap)

        selectionBottom =
            rect.y + rect.height

        vertical =
            if overlaySize.y - selectionBottom >= questionHeight + 2 * questionGap then
                style "top" (px (selectionBottom + questionGap))

            else if rect.y >= questionHeight + 2 * questionGap then
                style "bottom" (px (overlaySize.y - rect.y + questionGap))

            else
                style "bottom" (px questionGap)
    in
    [ style "left" (px (clamp questionGap (max questionGap (overlaySize.x - width - questionGap)) rect.x))
    , style "width" (px width)
    , vertical
    ]


viewQuestion : Model.AgentState -> Model.AreaQuestion -> Html (Flow Model ())
viewQuestion agentState question =
    let
        startingChat =
            agentState.request == Just Model.StartingAgentChat

        busyLabel =
            if Maybe.isNothing question.capturedArea then
                Just "Capturing the area…"

            else if startingChat then
                Just "Starting a new chat…"

            else
                Nothing
    in
    Html.form
        ([ class "artifact-viewer-question"
         , Events.stopPropagationOn "mousedown" (Decode.succeed ( Flow.pure (), True ))
         , Events.preventDefaultOn "submit" (Decode.succeed ( Actions.submitAreaQuestion, True ))
         ]
            ++ questionPlacement question
        )
        [ Html.textarea
            [ Html.Attributes.id Actions.areaQuestionInputId
            , class "artifact-viewer-question-input"
            , Html.Attributes.rows 1
            , Html.Attributes.placeholder "Ask about this area…"
            , Html.Attributes.attribute "aria-label" "Question about the selected area"
            , Html.Attributes.value question.prompt
            , Html.Attributes.readonly startingChat
            , Events.onInput Actions.updateAreaQuestion
            , Events.preventDefaultOn "keydown" (Decode.map (\send -> ( send, True )) sendOnEnter)
            ]
            []
        , Html.button
            [ class "icon-btn artifact-viewer-question-send"
            , Html.Attributes.type_ "submit"
            , Html.Attributes.disabled (Maybe.isNothing (Actions.areaQuestionToSend agentState question))
            , Html.Attributes.attribute "aria-busy" (boolText (Maybe.isJust busyLabel))
            , Html.Attributes.title (Maybe.withDefault "Ask in a new chat (Enter)" busyLabel)
            , Html.Attributes.attribute "aria-label" "Ask in a new chat"
            ]
            [ if startingChat then
                iconCustom True "progress_activity" [ class "artifact-viewer-question-spinner" ]

              else
                icon True "arrow_upward"
            ]
        ]


sendOnEnter : Decode.Decoder (Flow Model ())
sendOnEnter =
    Decode.map3 (\key shift composing -> key == "Enter" && not shift && not composing)
        (Decode.field "key" Decode.string)
        (Decode.field "shiftKey" Decode.bool)
        (Decode.field "isComposing" Decode.bool)
        |> Decode.andThen
            (\send ->
                if send then
                    Decode.succeed Actions.submitAreaQuestion

                else
                    Decode.fail "not a send key"
            )


dragStartDecoder : Decode.Decoder Model.AreaDrag
dragStartDecoder =
    Decode.when (Decode.field "button" Decode.int)
        ((==) 0)
        (Decode.map3
            (\client offset overlaySize ->
                { overlayOrigin = { x = client.x - offset.x, y = client.y - offset.y }
                , overlaySize = overlaySize
                , anchor = offset
                , cursor = offset
                }
            )
            Actions.clientPointDecoder
            (Model.areaPointDecoder [ "offsetX" ] [ "offsetY" ])
            (Model.areaPointDecoder [ "currentTarget", "clientWidth" ] [ "currentTarget", "clientHeight" ])
        )
