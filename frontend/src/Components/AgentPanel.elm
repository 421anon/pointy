module Components.AgentPanel exposing (view)

import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, placeholder, rows, title, value)
import Html.Events as Events
import Json.Decode as Decode
import Model.Core as Model exposing (Model)
import Markdown
import View.Icons


view : Model -> Html (Flow Model ())
view model =
    let
        agent =
            Model.getAgent model
    in
    Html.div []
        [ Html.button
            [ class "agent-panel-toggle"
            , Events.onClick Actions.toggleAgentPanel
            , title "Open AI agent"
            ]
            [ View.Icons.iconCustom False "smart_toy" []
            , Html.span [] [ Html.text "Agent" ]
            ]
        , if agent.isPanelOpen then
            viewPanel agent

          else
            Html.text ""
        ]


viewPanel : Model.AgentState -> Html (Flow Model ())
viewPanel agent =
    Html.div [ class "agent-panel" ]
        [ Html.div [ class "agent-panel__header" ]
            [ Html.div []
                [ Html.h2 [] [ Html.text "AI agent" ]
                ]
            , Html.button [ class "icon-btn", Events.onClick Actions.toggleAgentPanel, title "Close agent panel" ] [ View.Icons.icon False "close" ]
            ]
        , viewSessionBody agent
        ]


viewSessionBody : Model.AgentState -> Html (Flow Model ())
viewSessionBody agent =
    let
        loaded =
            ApiData.withDefault [] agent.sessions
    in
    Html.div [ class "agent-panel__split" ]
        [ viewSessionSidebar agent loaded
        , viewSessionDetail agent loaded
        ]


viewSessionSidebar : Model.AgentState -> List Model.AgentSessionView -> Html (Flow Model ())
viewSessionSidebar agent loaded =
    let
        listingStatus =
            case agent.sessions of
                Loading _ ->
                    Just "Loading drafts..."

                Error err ->
                    Just (Http.errorMessage err)

                _ ->
                    Nothing

        ( active, archived ) =
            List.partition (\v -> not (isArchivedStatus v.session.status)) loaded

        archivedToShow =
            if agent.showArchived then
                archived

            else
                []
    in
    Html.div [ class "agent-panel__sidebar" ]
        [ Html.div [ class "agent-panel__sidebar-header" ]
            [ Html.h3 [] [ Html.text "Drafts" ]
            , Html.button [ class "primary-btn", Events.onClick Actions.createAgentSession ] [ Html.text "New draft" ]
            ]
        , case listingStatus of
            Just msg ->
                Html.p [ class "agent-panel__loading" ] [ Html.text msg ]

            Nothing ->
                Html.text ""
        , Html.ul [ class "agent-panel__session-list" ]
            (List.map (viewSessionRow agent.selectedSessionId) active)
        , if List.isEmpty active && listingStatus == Nothing then
            Html.p [ class "agent-panel__empty-hint" ] [ Html.text "No drafts yet." ]

          else
            Html.text ""
        , if List.isEmpty archived then
            Html.text ""

          else
            Html.div [ class "agent-panel__archive-section" ]
                [ Html.button
                    [ class "link-btn agent-panel__archive-toggle"
                    , Events.onClick Actions.toggleAgentArchived
                    ]
                    [ Html.text
                        ((if agent.showArchived then
                            "Hide archived ("

                          else
                            "Show archived ("
                         )
                            ++ String.fromInt (List.length archived)
                            ++ ")"
                        )
                    ]
                , Html.ul [ class "agent-panel__session-list" ]
                    (List.map (viewSessionRow agent.selectedSessionId) archivedToShow)
                ]
        ]


isArchivedStatus : String -> Bool
isArchivedStatus s =
    s == "archived" || s == "discarded" || s == "applied"


viewSessionRow : Maybe String -> Model.AgentSessionView -> Html (Flow Model ())
viewSessionRow selectedId sessionView =
    let
        session =
            sessionView.session

        isSelected =
            selectedId == Just session.sessionId

        isArchived =
            isArchivedStatus session.status

        rowMainAttrs =
            if isArchived then
                [ class "agent-panel__session-row-main" ]

            else
                [ class "agent-panel__session-row-main"
                , Events.onClick (Actions.selectAgentSession session.sessionId)
                ]
    in
    Html.li
        [ classList
            [ ( "agent-panel__session-row", True )
            , ( "is-selected", isSelected )
            , ( "is-archived", isArchived )
            ]
        ]
        [ Html.div
            rowMainAttrs
            [ Html.div [ class "agent-panel__session-id" ] [ Html.text ("Draft " ++ shortSha session.sessionId) ]
            , Html.div [ class "agent-panel__session-meta" ]
                [ Html.text (draftStatusLabel session.status) ]
            , if sessionView.gitState.hasAgentCommits then
                Html.span [ class "agent-panel__pill" ] [ Html.text "changes" ]

              else
                Html.text ""
            ]
        , Html.div [ class "agent-panel__session-row-actions" ]
            [ if isArchived then
                Html.button
                    [ class "icon-btn"
                    , title "Delete permanently"
                    , Events.onClick (Actions.confirmDeleteAgentSession session.sessionId)
                    ]
                    [ View.Icons.icon False "delete_forever" ]

              else if session.status /= "running" then
                Html.button
                    [ class "icon-btn"
                    , title "Archive"
                    , Events.onClick (Actions.archiveAgentSession session.sessionId)
                    ]
                    [ View.Icons.icon False "archive" ]

              else
                Html.text ""
            ]
        ]


viewSessionDetail : Model.AgentState -> List Model.AgentSessionView -> Html (Flow Model ())
viewSessionDetail agent loaded =
    case Model.selectedSessionView agent of
        Just sessionView ->
            viewSession agent sessionView

        Nothing ->
            Html.div [ class "agent-panel__empty" ]
                [ Html.p []
                    [ Html.text
                        (if List.isEmpty loaded then
                            "No draft is open."

                         else
                            "Pick a draft on the left or start a new one."
                        )
                    ]
                , Html.button [ class "primary-btn", Events.onClick Actions.createAgentSession ] [ Html.text "New draft" ]
                ]


viewSession : Model.AgentState -> Model.AgentSessionView -> Html (Flow Model ())
viewSession agent sessionView =
    let
        session =
            sessionView.session

        gitState =
            sessionView.gitState

        runnerActive =
            session.activeTurnId /= Nothing || session.status == "running"

        closedDraft =
            isArchivedStatus session.status
    in
    Html.div [ class "agent-panel__body" ]
        [ viewError session
        , viewChat agent runnerActive
        , if closedDraft then
            viewClosedDraft session.status

          else
            viewPrompt agent runnerActive
        , viewDiffs gitState
        , if closedDraft then
            Html.text ""

          else
            viewCandidate sessionView runnerActive
        , Html.div [ class "agent-panel__footer" ]
            [ Html.button [ class "secondary-btn", Events.onClick (Actions.loadAgentSession session.sessionId) ] [ Html.text "Refresh" ]
            , Html.button [ class "danger-btn", Events.onClick Actions.discardAgentSession ] [ Html.text "Discard draft" ]
            ]
        ]


viewClosedDraft : String -> Html msg
viewClosedDraft status =
    Html.div [ class "agent-panel__section" ]
        [ Html.h3 [] [ Html.text (draftStatusLabel status) ]
        , Html.p [] [ Html.text "This draft is closed." ]
        ]


draftStatusLabel : String -> String
draftStatusLabel status =
    case status of
        "running" ->
            "Working"

        "applied" ->
            "Applied"

        "archived" ->
            "Archived"

        "discarded" ->
            "Discarded"

        "prepare_conflict" ->
            "Needs review"

        _ ->
            "Ready"


viewError : Model.AgentSession -> Html msg
viewError session =
    case session.lastError of
        Just err ->
            Html.pre [ class "agent-panel__error" ] [ Html.text err ]

        Nothing ->
            Html.text ""


viewPrompt : Model.AgentState -> Bool -> Html (Flow Model ())
viewPrompt agent runnerActive =
    let
        hasPrompt =
            not (String.isEmpty (String.trim agent.prompt))

        buttonText =
            if runnerActive then
                "Working..."

            else
                "Send"
    in
    Html.div [ class "agent-panel__section agent-panel__composer" ]
        [ Html.div [ class "agent-panel__composer-header" ]
            [ Html.h3 [] [ Html.text "Message" ]
            , Html.span [ class "agent-panel__shortcut" ] [ Html.text "Ctrl/⌘+Enter" ]
            ]
        , Html.div [ class "agent-panel__composer-row" ]
            [ Html.textarea
                [ class "agent-panel__prompt"
                , rows 3
                , value agent.prompt
                , placeholder "Ask for a change..."
                , disabled runnerActive
                , attribute "data-auto-resize" "true"
                , attribute "aria-label" "Agent prompt"
                , submitShortcut runnerActive
                , Events.onInput Actions.updateAgentPrompt
                ]
                []
            , Html.button
                [ class "primary-btn agent-panel__run-button"
                , disabled (runnerActive || not hasPrompt)
                , Events.onClick Actions.submitAgentPrompt
                , title "Send message"
                ]
                [ Html.text buttonText ]
            ]
        ]


submitShortcut : Bool -> Html.Attribute (Flow Model ())
submitShortcut runnerActive =
    let
        decoder =
            Decode.map3
                (\key ctrl meta -> ( key, ctrl, meta ))
                (Decode.field "key" Decode.string)
                (Decode.field "ctrlKey" Decode.bool)
                (Decode.field "metaKey" Decode.bool)
                |> Decode.andThen
                    (\( key, ctrl, meta ) ->
                        if not runnerActive && key == "Enter" && (ctrl || meta) then
                            Decode.succeed ( Actions.submitAgentPrompt, True )

                        else
                            Decode.fail "not an agent submit shortcut"
                    )
    in
    Events.preventDefaultOn "keydown" decoder


viewChat : Model.AgentState -> Bool -> Html (Flow Model ())
viewChat agent runnerActive =
    Html.div [ class "agent-panel__section agent-panel__conversation" ]
        [ Html.div [ class "agent-panel__section-header agent-panel__conversation-header" ]
            [ Html.h3 [] [ Html.text "Messages" ]
            , Html.button
                [ classList
                    [ ( "link-btn", True )
                    , ( "is-active", agent.showRawLog )
                    ]
                , Events.onClick Actions.toggleAgentLog
                ]
                [ Html.text
                    (if agent.showRawLog then
                        "Hide details"

                     else
                        "Show details"
                    )
                ]
            ]
        , if agent.showRawLog then
            Html.pre [ class "agent-panel__log" ]
                [ Html.text (emptyAs "No details yet." agent.turnLog) ]

          else
            viewChatTurns agent runnerActive
        ]


viewChatTurns : Model.AgentState -> Bool -> Html (Flow Model ())
viewChatTurns agent runnerActive =
    if List.isEmpty agent.chatTurns then
        Html.div [ class "agent-panel__chat agent-panel__chat--empty" ]
            [ Html.div [ class "agent-panel__empty-state" ]
                [ Html.strong []
                    [ Html.text
                        (if runnerActive then
                            "Agent is starting"

                         else
                            "No messages yet"
                        )
                    ]
                , Html.p []
                    [ Html.text
                        (if runnerActive then
                            "Waiting for the first reply."

                         else
                            "Send a message below to start."
                        )
                    ]
                ]
            ]

    else
        Html.div [ class "agent-panel__chat" ]
            (List.map viewChatTurn agent.chatTurns)


viewChatTurn : Model.ChatTurn -> Html msg
viewChatTurn turn =
    Html.div [ class "agent-panel__chat-turn" ]
        [ Html.div [ class "agent-panel__chat-message agent-panel__chat-message--user" ]
            [ Html.div [ class "agent-panel__chat-label" ] [ Html.text "You" ]
            , Html.div [ class "agent-panel__chat-bubble agent-panel__chat-bubble--user" ]
                [ Html.text turn.prompt ]
            ]
        , viewAgentMessage turn
        ]


viewAgentMessage : Model.ChatTurn -> Html msg
viewAgentMessage turn =
    let
        isEmptyAssistant =
            String.isEmpty (String.trim turn.assistant)

        ( statusLabel, body, failedMessage ) =
            case turn.status of
                Model.ChatPending ->
                    ( "Running", if isEmptyAssistant then "Waiting..." else turn.assistant, Nothing )

                Model.ChatDone ->
                    ( "Done", if isEmptyAssistant then "No reply." else turn.assistant, Nothing )

                Model.ChatFailed err ->
                    ( "Failed"
                    , if isEmptyAssistant then
                        "No reply before the task failed."

                      else
                        turn.assistant
                    , Just err
                    )
    in
    Html.div [ class "agent-panel__chat-message agent-panel__chat-message--agent" ]
        [ Html.div [ class "agent-panel__chat-label" ]
            [ Html.text "Agent"
            , Html.span
                [ classList
                    [ ( "agent-panel__chat-status", True )
                    , ( "is-running", turn.status == Model.ChatPending )
                    , ( "is-failed", failedMessage /= Nothing )
                    ]
                ]
                [ Html.text statusLabel ]
            ]
        , Html.div
            [ classList
                [ ( "agent-panel__chat-bubble", True )
                , ( "agent-panel__chat-bubble--agent", True )
                , ( "is-pending", turn.status == Model.ChatPending && isEmptyAssistant )
                , ( "is-failed", failedMessage /= Nothing )
                ]
            ]
            [ Html.div [ class "agent-panel__chat-content" ]
                (Markdown.toHtml Nothing body)
            , if turn.status == Model.ChatPending then
                Html.span [ class "agent-panel__chat-cursor" ] [ Html.text "\u{2588}" ]

              else
                Html.text ""
            ]
        , case failedMessage of
            Just err ->
                Html.div [ class "agent-panel__chat-error" ] [ Html.text ("Failed: " ++ err) ]

            Nothing ->
                Html.text ""
        ]


viewDiffs : Model.AgentGitState -> Html msg
viewDiffs gitState =
    Html.div [ class "agent-panel__section agent-panel__diffs" ]
        [ Html.h3 [] [ Html.text "Changes" ]
        , diffBlock "Proposed changes" gitState.branchDiff
        ]


diffBlock : String -> String -> Html msg
diffBlock label content =
    Html.div [ class "agent-panel__diff-block" ]
        [ Html.div [ class "agent-panel__diff-label" ] [ Html.text label ]
        , Html.pre [] [ Html.text (emptyAs "No changes yet." content) ]
        ]


viewCandidate : Model.AgentSessionView -> Bool -> Html (Flow Model ())
viewCandidate sessionView runnerActive =
    let
        hasAgentCommits =
            sessionView.gitState.hasAgentCommits
    in
    Html.div [ class "agent-panel__section" ]
        [ Html.h3 [] [ Html.text "Apply" ]
        , Html.p []
            [ Html.text
                (if hasAgentCommits then
                    "When you're ready, apply the changes to the target branch."

                 else
                    "No changes yet."
                )
            ]
        , Html.button
            [ class "primary-btn"
            , disabled (runnerActive || not hasAgentCommits)
            , Events.onClick Actions.applyAgentChanges
            ]
            [ Html.text "Apply changes" ]
        ]

shortSha : String -> String
shortSha sha =
    if String.isEmpty sha then
        "unknown"

    else
        String.left 12 sha


emptyAs : String -> String -> String
emptyAs fallback text =
    if String.isEmpty (String.trim text) then
        fallback

    else
        text
