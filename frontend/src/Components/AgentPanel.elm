module Components.AgentPanel exposing (view)

import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, rows, title, type_, value)
import Html.Events as Events
import Json.Decode as Decode
import Keyboard
import Markdown
import Model.Core as Model exposing (Model)
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
    Html.div [ classList [ ( "agent-panel", True ), ( "is-maximized", agent.isMaximized ) ] ]
        [ Html.div [ class "agent-panel__header" ]
            [ Html.div []
                [ Html.h2 [] [ Html.text "AI agent" ]
                ]
            , Html.div [ class "agent-panel__header-actions" ]
                [ Html.button [ class "icon-btn", Events.onClick Actions.toggleAgentPanel, title "Minimize agent panel" ] [ View.Icons.icon False "minimize" ]
                , Html.button
                    [ class "icon-btn"
                    , Events.onClick Actions.toggleAgentMaximized
                    , title
                        (if agent.isMaximized then
                            "Restore agent panel"

                         else
                            "Maximize agent panel"
                        )
                    ]
                    [ View.Icons.icon False
                        (if agent.isMaximized then
                            "close_fullscreen"

                         else
                            "open_in_full"
                        )
                    ]
                , Html.button [ class "icon-btn", Events.onClick Actions.toggleAgentPanel, title "Close agent panel" ] [ View.Icons.icon False "close" ]
                ]
            ]
        , viewSessionBody agent
        ]


viewSessionBody : Model.AgentState -> Html (Flow Model ())
viewSessionBody agent =
    let
        loaded =
            ApiData.withDefault [] agent.sessions
    in
    Html.div [ classList [ ( "agent-panel__split", True ), ( "is-desktop-sidebar-collapsed", agent.isDesktopSidebarCollapsed ) ] ]
        [ viewSessionSidebar agent loaded
        , viewSessionDetail agent loaded
        ]


viewSessionSidebar : Model.AgentState -> List Model.AgentSessionView -> Html (Flow Model ())
viewSessionSidebar agent loaded =
    Html.div
        [ classList
            [ ( "agent-panel__sidebar", True )
            , ( "is-mobile-sidebar-open", agent.isMobileSidebarOpen )
            ]
        ]
        [ Html.div [ class "agent-panel__sidebar-header" ]
            [ Html.h3 [] [ Html.text "Chats" ]
            , Html.button
                [ class "agent-panel__new-chat"
                , Events.onClick Actions.createAgentSession
                , title "New chat"
                ]
                [ View.Icons.icon False "add"
                , Html.span [] [ Html.text "New chat" ]
                ]
            ]
        , Html.button
            [ class "agent-panel__sidebar-toggle"
            , Events.onClick Actions.toggleAgentMobileSidebar
            , attribute "aria-controls" "agent-panel-sidebar-content"
            , attribute "aria-expanded"
                (if agent.isMobileSidebarOpen then
                    "true"

                 else
                    "false"
                )
            ]
            [ Html.span [] [ Html.text "Chats" ] ]
        , Html.div
            [ class "agent-panel__sidebar-content"
            , id "agent-panel-sidebar-content"
            ]
            (viewSessionSidebarContent agent loaded)
        , Html.button
            [ class "icon-btn agent-panel__sidebar-collapse"
            , Events.onClick Actions.toggleAgentDesktopSidebarCollapsed
            , title
                (if agent.isDesktopSidebarCollapsed then
                    "Show chats"

                 else
                    "Hide chats"
                )
            ]
            [ View.Icons.icon False
                (if agent.isDesktopSidebarCollapsed then
                    "left_panel_open"

                 else
                    "left_panel_close"
                )
            ]
        ]


viewSessionSidebarContent : Model.AgentState -> List Model.AgentSessionView -> List (Html (Flow Model ()))
viewSessionSidebarContent agent loaded =
    let
        listingStatus =
            case agent.sessions of
                Loading _ ->
                    Just "Loading chats..."

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
    [ case listingStatus of
        Just msg ->
            Html.p [ class "agent-panel__loading" ] [ Html.text msg ]

        Nothing ->
            Html.text ""
    , Html.ul [ class "agent-panel__session-list" ]
        (List.map (viewSessionRow agent.selectedSessionId) active)
    , if List.isEmpty active && listingStatus == Nothing then
        Html.p [ class "agent-panel__empty-hint" ] [ Html.text "No chats yet." ]

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


sessionDisplayName : Model.AgentSessionView -> String
sessionDisplayName sessionView =
    case sessionView.session.sessionName |> Maybe.andThen nonBlankName of
        Just name ->
            name

        Nothing ->
            sessionView.turns
                |> List.filterMap (.turnPrompt >> chatNameFromText)
                |> List.head
                |> Maybe.withDefault "New chat"


chatNameFromText : String -> Maybe String
chatNameFromText raw =
    nonBlankName raw
        |> Maybe.map (String.left chatNameMaxLength)


nonBlankName : String -> Maybe String
nonBlankName raw =
    let
        normalized =
            raw
                |> String.words
                |> String.join " "
    in
    if String.isEmpty normalized then
        Nothing

    else
        Just normalized


chatNameMaxLength : Int
chatNameMaxLength =
    80


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

        displayName =
            sessionDisplayName sessionView

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
            [ Html.div [ class "agent-panel__session-name", title session.sessionId ] [ Html.text displayName ]
            , Html.div [ class "agent-panel__session-meta" ]
                [ Html.text (chatStatusLabel session.status) ]
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
                            "No chat is open."

                         else
                            "Pick a chat on the left or start a new one."
                        )
                    ]
                , Html.button [ class "primary-btn", Events.onClick Actions.createAgentSession ] [ Html.text "New chat" ]
                ]


viewSession : Model.AgentState -> Model.AgentSessionView -> Html (Flow Model ())
viewSession agent sessionView =
    let
        session =
            sessionView.session

        runnerActive =
            session.activeTurnId /= Nothing || session.status == "running"

        closedChat =
            isArchivedStatus session.status
    in
    Html.div [ class "agent-panel__body" ]
        [ viewSessionTitle agent sessionView
        , viewError session
        , viewChat agent sessionView runnerActive
        , if closedChat then
            viewClosedChat session.status

          else
            viewPrompt runnerActive
        ]


viewSessionTitle : Model.AgentState -> Model.AgentSessionView -> Html (Flow Model ())
viewSessionTitle agent sessionView =
    let
        session =
            sessionView.session

        displayName =
            sessionDisplayName sessionView

        editing =
            case agent.sessionNameEdit of
                Just edit ->
                    if edit.sessionId == session.sessionId then
                        Just edit

                    else
                        Nothing

                Nothing ->
                    Nothing
    in
    Html.div [ class "agent-panel__session-title-card" ]
        [ case editing of
            Just edit ->
                viewSessionTitleEditor edit

            Nothing ->
                Html.div [ class "agent-panel__session-title-row" ]
                    [ Html.div []
                        [ Html.h3 [ class "agent-panel__session-title" ] [ Html.text displayName ]
                        , Html.div [ class "agent-panel__session-title-meta" ]
                            [ Html.span [] [ Html.text (chatStatusLabel session.status) ]
                            , Html.span [ title session.sessionId ] [ Html.text ("#" ++ shortSha session.sessionId) ]
                            , if sessionView.gitState.hasAgentCommits then
                                Html.span [ class "agent-panel__pill" ] [ Html.text "changes" ]

                              else
                                Html.text ""
                            ]
                        ]
                    , Html.button
                        [ class "icon-btn"
                        , title "Rename chat"
                        , Events.onClick (Actions.startAgentSessionNameEdit session.sessionId displayName)
                        ]
                        [ View.Icons.icon False "edit" ]
                    ]
        ]


viewSessionTitleEditor : Model.AgentSessionNameEdit -> Html (Flow Model ())
viewSessionTitleEditor edit =
    let
        trimmed =
            String.trim edit.value
    in
    Html.div [ class "agent-panel__session-title-editor" ]
        [ Html.input
            [ class "agent-panel__session-name-input"
            , type_ "text"
            , value edit.value
            , disabled edit.saving
            , attribute "maxlength" (String.fromInt chatNameMaxLength)
            , attribute "aria-label" "Chat name"
            , Events.onInput Actions.updateAgentSessionNameEdit
            , Events.on "keydown" <|
                Keyboard.decodeCombinations
                    [ ( Keyboard.enter, Decode.succeed Actions.saveAgentSessionName )
                    , ( Keyboard.escape, Decode.succeed Actions.cancelAgentSessionNameEdit )
                    ]
            ]
            []
        , Html.div [ class "agent-panel__session-title-actions" ]
            [ Html.button
                [ class "primary-btn"
                , disabled (edit.saving || String.isEmpty trimmed)
                , Events.onClick Actions.saveAgentSessionName
                ]
                [ Html.text
                    (if edit.saving then
                        "Saving..."

                     else
                        "Save"
                    )
                ]
            , Html.button
                [ class "secondary-btn"
                , disabled edit.saving
                , Events.onClick Actions.cancelAgentSessionNameEdit
                ]
                [ Html.text "Cancel" ]
            ]
        ]


viewClosedChat : String -> Html msg
viewClosedChat status =
    Html.div [ class "agent-panel__section" ]
        [ Html.h3 [] [ Html.text (chatStatusLabel status) ]
        , Html.p [] [ Html.text "This chat is closed." ]
        ]


chatStatusLabel : String -> String
chatStatusLabel status =
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


viewPrompt : Bool -> Html (Flow Model ())
viewPrompt runnerActive =
    let
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
                , id "agent-prompt"
                , rows 3
                , placeholder "Ask for a change..."
                , disabled runnerActive
                , attribute "aria-label" "Agent prompt"
                , submitShortcut runnerActive
                ]
                []
            , Html.button
                [ class "primary-btn agent-panel__run-button"
                , disabled runnerActive
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


viewChat : Model.AgentState -> Model.AgentSessionView -> Bool -> Html (Flow Model ())
viewChat agent sessionView runnerActive =
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
            viewChatTurns agent sessionView runnerActive
        ]


activeChangesetOperation : Model.AgentState -> String -> Maybe Model.ChangesetOperationKind
activeChangesetOperation agent sessionId =
    case agent.changesetOperation of
        Just operation ->
            if operation.sessionId == sessionId then
                Just operation.kind

            else
                Nothing

        Nothing ->
            Nothing


viewChatTurns : Model.AgentState -> Model.AgentSessionView -> Bool -> Html (Flow Model ())
viewChatTurns agent sessionView runnerActive =
    let
        activeOperation =
            activeChangesetOperation agent sessionView.session.sessionId

        pendingChangesetNodes =
            case pendingChangeset sessionView of
                Just changeset ->
                    [ viewChangesetBox runnerActive activeOperation changeset ]

                Nothing ->
                    []

        content =
            List.map (viewChatEntry runnerActive) agent.chatEntries ++ pendingChangesetNodes
    in
    if List.isEmpty content then
        Html.div [ class "agent-panel__chat agent-panel__chat--empty", id Actions.agentChatId ]
            [ Html.div [ id Actions.agentChatEndId ] []
            , Html.div [ class "agent-panel__empty-state" ]
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
        Html.div [ class "agent-panel__chat", id Actions.agentChatId ]
            (content ++ [ Html.div [ id Actions.agentChatEndId ] [] ])


viewChatEntry : Bool -> Model.ChatEntry -> Html (Flow Model ())
viewChatEntry runnerActive entry =
    case entry of
        Model.ChatTurnEntry turn ->
            viewChatTurn turn

        Model.ChatChangesetEntry changeset ->
            viewChangesetBox runnerActive Nothing changeset


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
                    ( "Running"
                    , if isEmptyAssistant then
                        "Waiting..."

                      else
                        turn.assistant
                    , Nothing
                    )

                Model.ChatDone ->
                    ( "Done"
                    , if isEmptyAssistant then
                        "No reply."

                      else
                        turn.assistant
                    , Nothing
                    )

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
                Html.span [ class "agent-panel__chat-cursor" ] [ Html.text "█" ]

              else
                Html.text ""
            ]
        , case failedMessage of
            Just err ->
                Html.div [ class "agent-panel__chat-error" ] [ Html.text ("Failed: " ++ err) ]

            Nothing ->
                Html.text ""
        ]


pendingChangeset : Model.AgentSessionView -> Maybe Model.ChatChangeset
pendingChangeset sessionView =
    let
        session =
            sessionView.session

        diff =
            String.trim sessionView.gitState.branchDiff
    in
    if sessionView.gitState.hasAgentCommits then
        if session.status == "prepare_conflict" then
            let
                err =
                    case session.lastError of
                        Just message ->
                            message

                        Nothing ->
                            "The changeset could not be prepared cleanly."

                state =
                    Model.ChatChangesetNeedsReview err
            in
            Just { state = state, description = defaultChangesetDescription state, diff = diff }

        else
            Just { state = Model.ChatChangesetProposed, description = defaultChangesetDescription Model.ChatChangesetProposed, diff = diff }

    else
        Nothing


defaultChangesetDescription : Model.ChatChangesetState -> String
defaultChangesetDescription state =
    case state of
        Model.ChatChangesetProposed ->
            "Review this changeset, then apply it to the target branch or discard the chat."

        Model.ChatChangesetNeedsReview _ ->
            "This changeset could not be prepared cleanly. Resolve the issue by continuing the conversation, or discard the chat."

        Model.ChatChangesetApplied ->
            "This changeset was applied. You can continue the conversation from the applied state."

        Model.ChatChangesetDiscarded ->
            "This changeset was discarded. No changes were applied."


viewChangesetBox : Bool -> Maybe Model.ChangesetOperationKind -> Model.ChatChangeset -> Html (Flow Model ())
viewChangesetBox runnerActive activeOperation changeset =
    let
        state =
            changeset.state

        diff =
            String.trim changeset.diff

        isApplying =
            activeOperation == Just Model.ApplyingChangeset

        isDiscarding =
            activeOperation == Just Model.DiscardingChangeset

        isBusy =
            isApplying || isDiscarding

        statusLabel =
            if isApplying then
                "Applying"

            else if isDiscarding then
                "Discarding"

            else
                case state of
                    Model.ChatChangesetProposed ->
                        "Proposed"

                    Model.ChatChangesetNeedsReview _ ->
                        "Needs review"

                    Model.ChatChangesetApplied ->
                        "Applied"

                    Model.ChatChangesetDiscarded ->
                        "Discarded"

        description =
            if String.isEmpty (String.trim changeset.description) then
                defaultChangesetDescription state

            else
                changeset.description

        canApply =
            case state of
                Model.ChatChangesetProposed ->
                    not runnerActive && not isBusy

                _ ->
                    False

        canDiscard =
            case state of
                Model.ChatChangesetProposed ->
                    not runnerActive && not isBusy

                Model.ChatChangesetNeedsReview _ ->
                    not runnerActive && not isBusy

                _ ->
                    False

        isProposed =
            case state of
                Model.ChatChangesetProposed ->
                    True

                _ ->
                    False

        isNeedsReview =
            case state of
                Model.ChatChangesetNeedsReview _ ->
                    True

                _ ->
                    False

        isApplied =
            case state of
                Model.ChatChangesetApplied ->
                    True

                _ ->
                    False

        isDiscarded =
            case state of
                Model.ChatChangesetDiscarded ->
                    True

                _ ->
                    False

        errorNode =
            case state of
                Model.ChatChangesetNeedsReview err ->
                    Html.pre [ class "agent-panel__changeset-error" ] [ Html.text err ]

                _ ->
                    Html.text ""

        applyLabel =
            if isApplying then
                "Applying..."

            else
                "Apply changes"

        discardLabel =
            if isDiscarding then
                "Discarding..."

            else
                "Discard changeset"
    in
    Html.div
        [ classList
            [ ( "agent-panel__changeset", True )
            , ( "is-proposed", isProposed )
            , ( "is-needs-review", isNeedsReview )
            , ( "is-applied", isApplied )
            , ( "is-discarded", isDiscarded )
            , ( "is-loading", isBusy )
            ]
        ]
        [ Html.div [ class "agent-panel__changeset-header" ]
            [ Html.h4 [] [ Html.text "Changeset" ]
            , Html.span [ class "agent-panel__changeset-status" ] [ Html.text statusLabel ]
            ]
        , Html.p [ class "agent-panel__changeset-description" ] [ Html.text description ]
        , if String.isEmpty diff then
            Html.text ""

          else
            Html.pre [ class "agent-panel__changeset-diff" ] [ Html.text diff ]
        , errorNode
        , Html.div [ class "agent-panel__changeset-actions" ]
            [ Html.button
                [ class "primary-btn"
                , disabled (not canApply)
                , Events.onClick Actions.applyAgentChanges
                ]
                [ Html.text applyLabel ]
            , Html.button
                [ class "danger-btn"
                , disabled (not canDiscard)
                , Events.onClick Actions.discardAgentSession
                ]
                [ Html.text discardLabel ]
            ]
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
