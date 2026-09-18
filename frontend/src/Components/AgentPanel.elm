module Components.AgentPanel exposing (view)

import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Components.AgentMentions as AgentMentions
import Extra.Http as Http
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, classList, disabled, id, placeholder, rows, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Html.Lazy
import Json.Decode as Decode
import Keyboard
import List.Extra as List
import Model.Core as Model exposing (Model)
import Model.Lib as Lib
import Route
import View.Icons
import View.Lib exposing (boolText, viewLoading)


view : Model -> Html (Flow Model ())
view model =
    let
        workspace =
            Lib.lastKnownWorkspace model

        resolveMention =
            AgentMentions.mentionTarget workspace

        mentionResolutionPending =
            awaitingData (Model.getStepConfig workspace)
                || awaitingData (Model.getPresets workspace)
                || awaitingData (Model.getProjects workspace).records
    in
    viewPanel mentionResolutionPending resolveMention (Model.getAgent model)


awaitingData : ApiData a -> Bool
awaitingData data =
    case data of
        NotAsked ->
            True

        Loading Nothing ->
            True

        _ ->
            False


viewPanel : Bool -> AgentMentions.Resolver -> Model.AgentState -> Html (Flow Model ())
viewPanel mentionResolutionPending resolveMention agent =
    Html.div
        [ classList
            [ ( "agent-panel", True )
            , ( "is-minimized", not agent.isPanelOpen )
            , ( "is-focus-mode", agent.isFocusMode )
            ]
        , id "agent-panel"
        , Events.on "keydown" <|
            Keyboard.decodeCombinations
                [ ( Keyboard.escape, Decode.succeed Actions.exitAgentFocusMode ) ]
        ]
        [ viewHeader agent
        , viewSessionBody mentionResolutionPending resolveMention agent
        ]


viewHeader : Model.AgentState -> Html (Flow Model ())
viewHeader agent =
    let
        ( sessionsLabel, sessionsIcon ) =
            if agent.isSessionListOpen then
                ( "Return to current chat", "arrow_back" )

            else
                ( "Open chat history", "history" )

        ( focusLabel, focusIcon ) =
            if agent.isFocusMode then
                ( "Exit focus mode (Esc)", "close_fullscreen" )

            else
                ( "Focus agent panel", "open_in_full" )
    in
    Html.div [ class "agent-panel__header" ]
        [ Html.h2 []
            [ Html.text
                (if agent.isSessionListOpen then
                    "Chats"

                 else
                    "AI agent"
                )
            ]
        , Html.div [ class "agent-panel__header-actions" ]
            [ viewIconButton sessionsLabel
                sessionsIcon
                [ class "icon-btn agent-panel__sessions-button"
                , Events.onClick Actions.toggleAgentSessionList
                , attribute "aria-controls" "agent-panel-sidebar"
                , attribute "aria-expanded" (boolText agent.isSessionListOpen)
                ]
            , Html.button
                [ class "icon-btn"
                , disabled (Model.agentInteractionsBlocked agent)
                , Events.onClick Actions.createAgentSession
                , title "New chat"
                , attribute "aria-label" "New chat"
                ]
                [ if isCreatingAgentSession agent then
                    viewButtonSpinner

                  else
                    View.Icons.icon False "add"
                ]
            , viewIconButton focusLabel
                focusIcon
                [ class "icon-btn agent-panel__focus-button"
                , Events.onClick Actions.toggleAgentFocusMode
                , attribute "aria-pressed" (boolText agent.isFocusMode)
                ]
            , viewIconButton "Close agent panel"
                "close"
                [ class "icon-btn"
                , Events.onClick Actions.toggleAgentPanel
                ]
            ]
        ]


viewIconButton : String -> String -> List (Html.Attribute (Flow Model ())) -> Html (Flow Model ())
viewIconButton label iconName attrs =
    Html.button (title label :: attribute "aria-label" label :: attrs)
        [ View.Icons.icon False iconName ]


viewRowAction : Bool -> Bool -> String -> String -> Flow Model () -> Html (Flow Model ())
viewRowAction busy blocked label iconName action =
    Html.button
        [ class "icon-btn"
        , title label
        , attribute "aria-label" label
        , disabled blocked
        , Events.stopPropagationOn "click" (Decode.succeed ( action, True ))
        ]
        [ if busy then
            viewButtonSpinner

          else
            View.Icons.icon False iconName
        ]


viewSessionBody : Bool -> AgentMentions.Resolver -> Model.AgentState -> Html (Flow Model ())
viewSessionBody mentionResolutionPending resolveMention agent =
    let
        loaded =
            Maybe.withDefault [] (ApiData.toMaybe agent.sessions)
    in
    Html.div
        [ classList
            [ ( "agent-panel__split", True )
            , ( "is-session-list-open", agent.isSessionListOpen )
            ]
        ]
        [ viewSessionSidebar agent loaded
        , if agent.isRestoringChat then
            viewSessionsLoading

          else
            case agent.sessions of
                NotAsked ->
                    viewSessionsBlank

                Loading Nothing ->
                    viewSessionsLoading

                _ ->
                    viewSessionDetail mentionResolutionPending resolveMention agent loaded
        ]


viewSessionsBlank : Html msg
viewSessionsBlank =
    Html.div [ class "agent-panel__empty" ] []


viewSessionsLoading : Html msg
viewSessionsLoading =
    Html.div [ class "agent-panel__empty agent-panel__empty--delayed" ]
        [ Html.span [ class "agent-panel__loading shimmer-text shimmer-text--low-contrast" ] [ Html.text "Loading chat..." ] ]


isCreatingAgentSession : Model.AgentState -> Bool
isCreatingAgentSession agent =
    agent.request == Just Model.CreatingAgentSession && agent.selectedSessionId == Nothing


viewButtonSpinner : Html msg
viewButtonSpinner =
    View.Icons.iconCustom True "progress_activity" [ class "agent-panel__button-spinner" ]


viewButtonContent : Bool -> String -> Html msg
viewButtonContent busy label =
    Html.span [ class "agent-panel__button-content" ]
        (if busy then
            [ viewButtonSpinner, Html.text label ]

         else
            [ Html.text label ]
        )


viewNewChatButtonContents : Model.AgentState -> List (Html msg)
viewNewChatButtonContents agent =
    if isCreatingAgentSession agent then
        [ viewButtonSpinner
        , Html.span [] [ Html.text "Creating..." ]
        ]

    else
        [ View.Icons.icon False "add"
        , Html.span [] [ Html.text "New chat" ]
        ]


viewSessionSidebar : Model.AgentState -> List Model.AgentSessionView -> Html (Flow Model ())
viewSessionSidebar agent loaded =
    Html.div
        [ class "agent-panel__sidebar"
        , id "agent-panel-sidebar"
        ]
        (viewSessionSidebarContent agent loaded)


viewSessionSidebarContent : Model.AgentState -> List Model.AgentSessionView -> List (Html (Flow Model ()))
viewSessionSidebarContent agent loaded =
    let
        listingStatus =
            case agent.sessions of
                Loading (Just _) ->
                    Nothing

                Loading Nothing ->
                    Just ( True, "Loading chats..." )

                Error err ->
                    Just ( False, Http.errorMessage err )

                _ ->
                    Nothing

        ( active, archived ) =
            List.partition (\v -> not (Model.agentSessionArchived v.session.status)) loaded

        activeRows =
            (if isCreatingAgentSession agent then
                [ viewCreatingSessionRow ]

             else
                []
            )
                ++ List.map (viewSessionRow agent) active
    in
    [ Html.viewMaybe
        (\( isLoading, msg ) ->
            Html.p
                [ classList
                    [ ( "agent-panel__loading", True )
                    , ( "shimmer-text", isLoading )
                    , ( "shimmer-text--low-contrast", isLoading )
                    ]
                ]
                [ Html.text msg ]
        )
        listingStatus
    , Html.ul [ class "agent-panel__session-list" ] activeRows
    , Html.viewIf (List.isEmpty activeRows && listingStatus == Nothing)
        (Html.p [ class "agent-panel__empty-hint" ] [ Html.text "No chats yet." ])
    , Html.viewIf (not (List.isEmpty archived)) <|
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
                (if agent.showArchived then
                    List.map (viewSessionRow agent) archived

                 else
                    []
                )
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


viewCreatingSessionRow : Html msg
viewCreatingSessionRow =
    Html.li
        [ class "agent-panel__session-row is-selected is-optimistic"
        , attribute "aria-busy" "true"
        ]
        [ Html.div [ class "agent-panel__session-row-main" ]
            [ Html.div [ class "agent-panel__session-name shimmer-text shimmer-text--low-contrast" ] [ Html.text "New chat" ]
            , Html.div [ class "agent-panel__session-meta shimmer-text shimmer-text--low-contrast" ] [ Html.text "Creating..." ]
            ]
        ]


viewSessionRow : Model.AgentState -> Model.AgentSessionView -> Html (Flow Model ())
viewSessionRow agent sessionView =
    let
        session =
            sessionView.session

        isSelected =
            agent.selectedSessionId == Just session.sessionId

        isArchived =
            Model.agentSessionArchived session.status

        displayName =
            sessionDisplayName sessionView

        interactionActive =
            Model.agentInteractionsBlocked agent

        isArchiving =
            agent.request == Just (Model.ArchivingAgentSession session.sessionId)

        isDeleting =
            agent.request == Just (Model.DeletingAgentSession session.sessionId)

        statusLabel =
            if isArchiving then
                "Archiving..."

            else if isDeleting then
                "Deleting..."

            else
                chatStatusLabel session.status

        rowAttrs =
            [ classList
                [ ( "agent-panel__session-row", True )
                , ( "is-selected", isSelected )
                , ( "is-archived", isArchived )
                , ( "is-updating", isArchiving || isDeleting )
                ]
            , title ("#" ++ session.sessionId)
            , attribute "aria-busy" (boolText (isArchiving || isDeleting))
            ]
    in
    Html.li
        (if isArchiving || isDeleting then
            rowAttrs

         else
            Events.onClick (Actions.selectAgentSession session.sessionId) :: rowAttrs
        )
        [ Html.div [ class "agent-panel__session-row-main" ]
            [ Html.div [ class "agent-panel__session-name" ] [ Html.text displayName ]
            , Html.viewIf (statusLabel /= "Ready" && statusLabel /= "Archived")
                (Html.div [ class "agent-panel__session-meta" ] [ Html.text statusLabel ])
            , Html.viewIf sessionView.gitState.hasAgentCommits
                (Html.span [ class "agent-panel__pill" ] [ Html.text "changes" ])
            ]
        , Html.div [ class "agent-panel__session-row-actions" ]
            [ viewIconButton "Copy link to chat"
                "share"
                [ class "icon-btn"
                , Events.stopPropagationOn "click" (Decode.succeed ( Actions.shareAgentChat session.sessionId, True ))
                ]
            , if isArchived then
                viewRowAction isDeleting interactionActive "Delete permanently" "delete_forever" (Actions.confirmDeleteAgentSession session.sessionId)

              else if session.status /= "running" then
                viewRowAction isArchiving interactionActive "Archive" "archive" (Actions.archiveAgentSession session.sessionId)

              else
                Html.nothing
            ]
        ]


viewSessionDetail : Bool -> AgentMentions.Resolver -> Model.AgentState -> List Model.AgentSessionView -> Html (Flow Model ())
viewSessionDetail mentionResolutionPending resolveMention agent loaded =
    case ( isCreatingAgentSession agent, Model.selectedSessionView agent ) of
        ( True, _ ) ->
            viewCreatingSession

        ( False, Just sessionView ) ->
            if mentionResolutionPending then
                viewSessionsLoading

            else
                viewSession resolveMention agent sessionView

        ( False, Nothing ) ->
            Html.div [ class "agent-panel__empty" ]
                [ Html.p []
                    [ Html.text
                        (if List.isEmpty loaded then
                            "No chat is open."

                         else
                            "Choose a chat above or start a new one."
                        )
                    ]
                , Html.button
                    [ class "btn"
                    , disabled (Model.agentInteractionsBlocked agent)
                    , Events.onClick Actions.createAgentSession
                    ]
                    (viewNewChatButtonContents agent)
                ]


viewCreatingSession : Html msg
viewCreatingSession =
    Html.div
        [ class "agent-panel__body"
        , attribute "aria-busy" "true"
        , attribute "aria-live" "polite"
        ]
        [ Html.div [ class "agent-panel__session-title-card" ]
            [ Html.div [ class "agent-panel__session-title-row" ]
                [ Html.div [ class "agent-panel__session-title-main" ]
                    [ Html.h3 [ class "agent-panel__session-title shimmer-text shimmer-text--low-contrast" ] [ Html.text "New chat" ]
                    , Html.div [ class "agent-panel__session-title-meta shimmer-text shimmer-text--low-contrast" ]
                        [ Html.span [] [ Html.text "Creating..." ] ]
                    ]
                ]
            ]
        , Html.div [ class "agent-panel__chat agent-panel__chat--empty" ]
            [ Html.div [ class "agent-panel__empty-state" ]
                [ Html.strong [ class "shimmer-text shimmer-text--low-contrast" ] [ Html.text "Creating chat..." ]
                , Html.p [] [ Html.text "Preparing agent workspace." ]
                ]
            ]
        ]


viewSession : AgentMentions.Resolver -> Model.AgentState -> Model.AgentSessionView -> Html (Flow Model ())
viewSession resolveMention agent sessionView =
    let
        session =
            sessionView.session

        runnerActive =
            (agent.activeTurnStream /= Nothing)
                || (session.activeTurnId /= Nothing)
                || (session.status == "running")

        detailBlocked =
            runnerActive || Model.agentInteractionsBlocked agent

        closedChat =
            Model.agentSessionArchived session.status
    in
    Html.div [ class "agent-panel__body" ]
        [ viewSessionTitle agent sessionView
        , viewError session
        , viewChatTurns resolveMention agent sessionView runnerActive detailBlocked
        , if closedChat then
            viewClosedChat session.status

          else
            let
                sendingPrompt =
                    agent.request == Just (Model.SendingAgentPrompt session.sessionId)

                stopping =
                    agent.request == Just (Model.StoppingAgentTurn session.sessionId)

                submitBlocked =
                    Model.agentSubmissionBlocked agent

                -- The textarea is uncontrolled, so its content survives re-renders.
                -- Keep it editable while the agent works so a steering prompt can
                -- be typed; lock it only while its content is being consumed
                -- by a send.
                composeBlocked =
                    sendingPrompt
            in
            viewPrompt runnerActive sendingPrompt stopping composeBlocked submitBlocked
        ]


viewSessionTitle : Model.AgentState -> Model.AgentSessionView -> Html (Flow Model ())
viewSessionTitle agent sessionView =
    let
        session =
            sessionView.session

        -- Rename is metadata-only and allowed in every session state; block
        -- only a pending delete of this session (the rename would queue
        -- behind the delete's lock and fail).
        renameBlocked =
            agent.request == Just (Model.DeletingAgentSession session.sessionId)

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
                viewSessionTitleEditor renameBlocked edit

            Nothing ->
                let
                    displayName =
                        sessionDisplayName sessionView
                in
                Html.div [ class "agent-panel__session-title-row" ]
                    [ Html.div [ class "agent-panel__session-title-main" ]
                        [ Html.h3 [ class "agent-panel__session-title" ] [ Html.text displayName ]
                        , Html.div [ class "agent-panel__session-title-meta" ]
                            [ Html.span [] [ Html.text (chatStatusLabel session.status) ]
                            , Html.span [ title session.sessionId ] [ Html.text ("#" ++ shortSha session.sessionId) ]
                            , Html.viewIf sessionView.gitState.hasAgentCommits
                                (Html.span [] [ Html.text "changes" ])
                            ]
                        ]
                    , viewIconButton "Copy link to chat"
                        "share"
                        [ class "icon-btn"
                        , Events.onClick (Actions.shareAgentChat session.sessionId)
                        ]
                    , viewIconButton "Rename chat"
                        "edit"
                        [ class "icon-btn"
                        , disabled renameBlocked
                        , Events.onClick (Actions.startAgentSessionNameEdit session.sessionId displayName)
                        ]
                    ]
        ]


viewSessionTitleEditor : Bool -> Model.AgentSessionNameEdit -> Html (Flow Model ())
viewSessionTitleEditor renameBlocked edit =
    let
        trimmed =
            String.trim edit.value

        saveButton =
            Html.button
                [ class "small-btn"
                , disabled (renameBlocked || edit.saving || String.isEmpty trimmed)
                , Events.onClick Actions.saveAgentSessionName
                ]
                [ Html.text "Save" ]
    in
    Html.div [ class "agent-panel__session-title-editor" ]
        [ Html.input
            [ class "agent-panel__session-name-input"
            , id Actions.agentSessionNameInputId
            , type_ "text"
            , value edit.value
            , disabled (renameBlocked || edit.saving)
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
            [ if edit.saving then
                viewLoading saveButton

              else
                saveButton
            , Html.button
                [ class "small-btn"
                , disabled edit.saving
                , Events.onClick Actions.cancelAgentSessionNameEdit
                ]
                [ Html.text "Cancel" ]
            ]
        ]


viewClosedChat : String -> Html msg
viewClosedChat status =
    Html.div [ class "agent-panel__closed" ]
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
    Html.viewMaybe (\err -> Html.pre [ class "agent-panel__error" ] [ Html.text err ]) session.lastError


viewPrompt : Bool -> Bool -> Bool -> Bool -> Bool -> Html (Flow Model ())
viewPrompt runnerActive sendingPrompt stopping composeBlocked submitBlocked =
    let
        steering =
            runnerActive || stopping
    in
    Html.div [ class "agent-panel__composer" ]
        [ Html.div [ class "agent-panel__composer-row" ]
            [ Html.textarea
                [ class "agent-panel__prompt"
                , id "agent-prompt"
                , rows 1
                , placeholder "Ask for a change..."
                , disabled composeBlocked
                , attribute "aria-label" "Agent prompt"
                , submitShortcut submitBlocked
                ]
                []
            , Html.div [ class "agent-panel__composer-actions" ]
                [ viewSubmitButton steering sendingPrompt submitBlocked
                , Html.viewIf steering (viewStopButton stopping)
                ]
            ]
        ]


viewSubmitButton : Bool -> Bool -> Bool -> Html (Flow Model ())
viewSubmitButton steering sendingPrompt submitBlocked =
    Html.button
        [ class "btn agent-panel__run-button"
        , disabled submitBlocked
        , attribute "aria-busy" (boolText sendingPrompt)
        , Events.onClick Actions.submitAgentPrompt
        , title
            (if steering then
                "Steer the running agent (Ctrl/⌘+Enter)"

             else
                "Send message (Ctrl/⌘+Enter)"
            )
        ]
        (if steering then
            [ viewButtonContent False "Steer" ]

         else if sendingPrompt then
            [ viewButtonContent False "Sending...", viewButtonSpinner ]

         else
            [ viewButtonContent False "Send"
            , Html.span [ class "agent-panel__run-hint" ] [ Html.text "Ctrl/⌘+Enter" ]
            ]
        )


viewStopButton : Bool -> Html (Flow Model ())
viewStopButton stopping =
    Html.button
        [ class "btn agent-panel__run-button agent-panel__stop-button"
        , disabled stopping
        , Events.onClick Actions.stopAgentTurn
        , title "Stop the agent"
        ]
        [ viewButtonContent False
            (if stopping then
                "Stopping..."

             else
                "Stop"
            )
        , if stopping then
            viewButtonSpinner

          else
            View.Icons.icon False "stop_circle"
        ]


submitShortcut : Bool -> Html.Attribute (Flow Model ())
submitShortcut submitBlocked =
    let
        decoder =
            Decode.map3
                (\key ctrl meta -> ( key, ctrl, meta ))
                (Decode.field "key" Decode.string)
                (Decode.field "ctrlKey" Decode.bool)
                (Decode.field "metaKey" Decode.bool)
                |> Decode.andThen
                    (\( key, ctrl, meta ) ->
                        if not submitBlocked && key == "Enter" && (ctrl || meta) then
                            Decode.succeed ( Actions.submitAgentPrompt, True )

                        else
                            Decode.fail "not an agent submit shortcut"
                    )
    in
    Events.preventDefaultOn "keydown" decoder


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


viewChatTurns : AgentMentions.Resolver -> Model.AgentState -> Model.AgentSessionView -> Bool -> Bool -> Html (Flow Model ())
viewChatTurns resolveMention agent sessionView runnerActive interactionsBlocked =
    let
        pendingChangesetNodes =
            case pendingChangeset sessionView of
                Just changeset ->
                    let
                        activeOperation =
                            activeChangesetOperation agent sessionView.session.sessionId
                    in
                    [ viewChangesetBox interactionsBlocked activeOperation changeset ]

                Nothing ->
                    []

        content =
            List.map (viewChatEntry resolveMention interactionsBlocked sessionView.session.sessionId agent.highlightTurnId) agent.chatEntries ++ pendingChangesetNodes
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


viewChatEntry : AgentMentions.Resolver -> Bool -> String -> Maybe String -> Model.ChatEntry -> Html (Flow Model ())
viewChatEntry resolveMention interactionsBlocked sessionId highlightTurnId entry =
    case entry of
        Model.ChatTurnEntry turn ->
            viewChatTurn resolveMention sessionId (highlightTurnId == Just turn.turnId) turn

        Model.ChatChangesetEntry changeset ->
            viewChangesetBox interactionsBlocked Nothing changeset


turnIdAttribute : Model.ChatTurn -> List (Html.Attribute (Flow Model ()))
turnIdAttribute turn =
    if String.isEmpty turn.turnId then
        []

    else
        [ id (Actions.agentTurnId turn.turnId) ]


viewChatTurn : AgentMentions.Resolver -> String -> Bool -> Model.ChatTurn -> Html (Flow Model ())
viewChatTurn resolveMention sessionId isHighlighted turn =
    Html.div
        (classList
            [ ( "agent-panel__chat-turn", True )
            , ( "is-highlighted", isHighlighted )
            ]
            :: turnIdAttribute turn
        )
        [ Html.div [ class "agent-panel__chat-message agent-panel__chat-message--user" ]
            [ Html.div [ class "agent-panel__chat-label" ]
                [ if String.isEmpty turn.turnId then
                    Html.text "You"

                  else
                    Html.a
                        [ class "agent-panel__chat-permalink"
                        , Html.Attributes.href (Route.chatHref { sessionId = sessionId, mTurnId = Just turn.turnId })
                        , title "Link to this message"
                        ]
                        [ Html.text "You" ]
                ]
            , Html.div [ class "agent-panel__chat-bubble agent-panel__chat-bubble--user" ]
                [ Html.text turn.prompt ]
            ]
        , viewAgentMessage resolveMention turn
        ]


viewAgentMessage : AgentMentions.Resolver -> Model.ChatTurn -> Html (Flow Model ())
viewAgentMessage resolveMention turn =
    let
        isEmptyAssistant =
            String.isEmpty (String.trim turn.assistant)

        ( statusLabel, emptyBody, failedMessage ) =
            case turn.status of
                Model.ChatPending ->
                    ( "Running", "Waiting...", Nothing )

                Model.ChatDone ->
                    ( "Done", "No reply.", Nothing )

                Model.ChatStopped ->
                    ( "Stopped", "Stopped before a reply.", Nothing )

                Model.ChatFailed err ->
                    ( "Failed", "No reply before the task failed.", Just err )

        body =
            if isEmptyAssistant then
                emptyBody

            else
                turn.assistant
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
                , attribute "role" "status"
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
            [ Html.div
                [ classList
                    [ ( "agent-panel__chat-content", True )
                    , ( "shimmer-text", turn.status == Model.ChatPending && isEmptyAssistant )
                    , ( "shimmer-text--low-contrast", turn.status == Model.ChatPending && isEmptyAssistant )
                    ]
                ]
                (AgentMentions.toHtml resolveMention body)
            , Html.viewIf (turn.status == Model.ChatPending)
                (Html.span [ class "agent-panel__chat-cursor", attribute "aria-hidden" "true" ] [ Html.text "█" ])
            ]
        , Html.viewMaybe
            (\err -> Html.div [ class "agent-panel__chat-error" ] [ Html.text ("Failed: " ++ err) ])
            failedMessage
        ]


pendingChangeset : Model.AgentSessionView -> Maybe Model.ChatChangeset
pendingChangeset sessionView =
    if sessionView.gitState.hasAgentCommits && not (Model.agentSessionArchived sessionView.session.status) then
        let
            session =
                sessionView.session

            diff =
                String.trim sessionView.gitState.branchDiff
        in
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
            "Review this changeset, then apply it to the target branch or discard it."

        Model.ChatChangesetNeedsReview _ ->
            "This changeset could not be prepared cleanly. Resolve the issue by continuing the conversation, or discard the changeset."

        Model.ChatChangesetApplied ->
            "This changeset was applied. You can continue the conversation from the applied state."

        Model.ChatChangesetDiscarded ->
            "This changeset was discarded. No changes were applied."


viewChangesetBox : Bool -> Maybe Model.ChangesetOperationKind -> Model.ChatChangeset -> Html (Flow Model ())
viewChangesetBox interactionsBlocked activeOperation changeset =
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

        actionsAllowed =
            not interactionsBlocked && not isBusy

        canApply =
            case state of
                Model.ChatChangesetProposed ->
                    actionsAllowed

                _ ->
                    False

        canDiscard =
            case state of
                Model.ChatChangesetProposed ->
                    actionsAllowed

                Model.ChatChangesetNeedsReview _ ->
                    actionsAllowed

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

        errorNode =
            case state of
                Model.ChatChangesetNeedsReview err ->
                    Html.pre [ class "agent-panel__changeset-error" ] [ Html.text err ]

                _ ->
                    Html.nothing
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
            , Html.Lazy.lazy viewChangesetTotals diff
            , Html.span [ class "agent-panel__changeset-status" ] [ Html.text statusLabel ]
            ]
        , Html.p [ class "agent-panel__changeset-description" ] [ Html.text description ]
        , Html.viewIf (not (String.isEmpty diff))
            (Html.Lazy.lazy viewChangesetDiff diff)
        , errorNode
        , Html.viewIf (isProposed || isNeedsReview) <|
            Html.div [ class "agent-panel__changeset-actions" ]
                [ Html.viewIf isProposed <|
                    Html.button
                        [ class "small-btn"
                        , disabled (not canApply)
                        , Events.onClick Actions.applyAgentChanges
                        ]
                        [ viewButtonContent isApplying applyLabel ]
                , Html.button
                    [ class "small-btn"
                    , disabled (not canDiscard)
                    , Events.onClick Actions.discardAgentSession
                    ]
                    [ viewButtonContent isDiscarding discardLabel ]
                ]
        ]


type alias ChangesetFile =
    { path : String
    , status : String
    , added : Int
    , removed : Int
    , lines : List String
    }


changesetFiles : String -> List ChangesetFile
changesetFiles diff =
    String.split "\ndiff --git " ("\n" ++ diff)
        |> List.drop 1
        |> List.map changesetFile


viewChangesetTotals : String -> Html msg
viewChangesetTotals diff =
    case changesetFiles diff of
        [] ->
            Html.nothing

        files ->
            viewChangesetCount (List.sum (List.map .added files)) (List.sum (List.map .removed files))


viewChangesetDiff : String -> Html msg
viewChangesetDiff diff =
    Html.div [ class "agent-panel__changeset-diff" ]
        (case changesetFiles diff of
            [] ->
                [ Html.text diff ]

            files ->
                List.map viewChangesetFile files
        )


changesetFile : String -> ChangesetFile
changesetFile chunk =
    let
        lines =
            String.lines chunk

        header =
            Maybe.withDefault "" (List.head lines)

        hunks =
            List.dropWhile (not << String.startsWith "@@") lines

        count prefix =
            List.length (List.filter (String.startsWith prefix) hunks)
    in
    { path = Maybe.withDefault header (List.last (String.split " b/" header))
    , status =
        if String.contains "\nnew file mode" chunk then
            "new"

        else if String.contains "\ndeleted file mode" chunk then
            "deleted"

        else if String.contains "\nrename to " chunk then
            "renamed"

        else
            ""
    , added = count "+"
    , removed = count "-"
    , lines = hunks
    }


viewChangesetFile : ChangesetFile -> Html msg
viewChangesetFile file =
    Html.details [ class "agent-panel__changeset-file", attribute "open" "" ]
        (Html.summary []
            [ Html.text file.path
            , Html.viewIf (not (String.isEmpty file.status))
                (Html.span [ class "agent-panel__changeset-tag" ] [ Html.text file.status ])
            , viewChangesetCount file.added file.removed
            ]
            :: List.map viewChangesetLine file.lines
        )


viewChangesetCount : Int -> Int -> Html msg
viewChangesetCount added removed =
    Html.span [ class "agent-panel__changeset-count" ]
        [ Html.span [ class "is-added" ] [ Html.text ("+" ++ String.fromInt added) ]
        , Html.span [ class "is-removed" ] [ Html.text ("-" ++ String.fromInt removed) ]
        ]


viewChangesetLine : String -> Html msg
viewChangesetLine line =
    Html.span [ class (changesetLineClass line) ] [ Html.text line ]


changesetLineClass : String -> String
changesetLineClass line =
    case String.left 1 line of
        "@" ->
            "is-hunk"

        "+" ->
            "is-added"

        "-" ->
            "is-removed"

        _ ->
            ""


shortSha : String -> String
shortSha sha =
    if String.isEmpty sha then
        "unknown"

    else
        String.left 12 sha
