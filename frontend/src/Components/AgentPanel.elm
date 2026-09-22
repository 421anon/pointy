module Components.AgentPanel exposing (view)

import Accessors exposing (get, just, try)
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
import Http
import Json.Decode as Decode
import Keyboard
import List.Extra as List
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (agentSessionBlank, agentSessionBlocked, agentSessionRunning, liveTurnAt, sessionEntries, sessionPendingQuestion, sessionPendingSteer)
import Model.Lib as Lib
import Route
import Set
import View.Icons
import View.Lib exposing (boolText)


view : Model -> Html (Flow Model ())
view model =
    viewPanel (mentionsPending model) (AgentMentions.mentionTarget (Lib.lastKnownWorkspace model)) (Model.getAgent model)


mentionsPending : Model -> Bool
mentionsPending model =
    not (ApiData.settled (Model.getStepConfig model))
        || not (ApiData.settled (Model.getProjects model).records)


viewPanel : Bool -> AgentMentions.Resolver -> Model.AgentState -> Html (Flow Model ())
viewPanel mentionsAwaited resolveMention agent =
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
        , viewSessionBody mentionsAwaited resolveMention agent
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

        openChatBlank =
            Model.selectedSessionSummary agent
                |> Maybe.map (\summary -> agentSessionBlank summary agent)
                |> Maybe.withDefault False
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
                , disabled (Model.agentMutationPending agent || openChatBlank)
                , Events.onClick Actions.createAgentSession
                , title "New chat"
                , attribute "aria-label" "New chat"
                ]
                [ View.Icons.icon False "add" ]
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


viewRowAction : Bool -> String -> String -> Flow Model () -> Html (Flow Model ())
viewRowAction blocked label iconName action =
    Html.button
        [ class "icon-btn"
        , title label
        , attribute "aria-label" label
        , disabled blocked
        , Events.stopPropagationOn "click" (Decode.succeed ( action, True ))
        ]
        [ View.Icons.icon False iconName ]


viewSessionBody : Bool -> AgentMentions.Resolver -> Model.AgentState -> Html (Flow Model ())
viewSessionBody mentionsAwaited resolveMention agent =
    let
        listed =
            Maybe.withDefault [] (ApiData.toMaybe agent.sessions)
    in
    Html.div
        [ classList
            [ ( "agent-panel__split", True )
            , ( "is-session-list-open", agent.isSessionListOpen )
            ]
        ]
        [ viewSessionSidebar agent listed
        , if agent.isRestoringChat then
            viewSessionsLoading

          else
            case agent.sessions of
                NotAsked ->
                    viewSessionsBlank

                Loading Nothing ->
                    viewSessionsLoading

                _ ->
                    viewSessionDetail mentionsAwaited resolveMention agent listed
        ]


viewSessionsBlank : Html msg
viewSessionsBlank =
    Html.div [ class "agent-panel__empty" ] []


viewSessionsLoading : Html msg
viewSessionsLoading =
    Html.div
        [ class "agent-panel__body agent-panel__empty--delayed"
        , attribute "aria-busy" "true"
        , attribute "role" "status"
        ]
        [ Html.div [ class "agent-panel__session-title-card" ]
            [ Html.div [ class "agent-panel__session-title-row" ]
                [ Html.div [ class "agent-panel__session-title shimmer-text shimmer-text--high-contrast" ]
                    [ Html.text "Loading chat" ]
                ]
            ]
        , viewChatSkeletonBody
        , Html.div [ class "agent-panel__composer" ]
            [ Html.div [ class "agent-panel__composer-row" ]
                [ Html.div [ class "agent-panel__prompt agent-panel__prompt--skeleton" ] []
                , Html.div [ class "agent-panel__composer-actions" ]
                    [ Html.div [ class "agent-panel__run-button agent-panel__run-button--skeleton" ] [] ]
                ]
            ]
        ]


viewChatSkeleton : Bool -> Int -> Html msg
viewChatSkeleton fromUser lines =
    let
        side =
            if fromUser then
                "user"

            else
                "agent"
    in
    Html.div
        [ class ("agent-panel__chat-message agent-panel__chat-message--" ++ side)
        , attribute "aria-hidden" "true"
        ]
        [ Html.div
            [ class ("agent-panel__chat-bubble agent-panel__chat-bubble--" ++ side ++ " agent-panel__chat-bubble--skeleton") ]
            (List.repeat lines (Html.div [ class "agent-panel__skeleton-line" ] []))
        ]


viewChatSkeletonBody : Html msg
viewChatSkeletonBody =
    Html.div [ class "agent-panel__chat", id Actions.agentChatId ]
        [ viewChatSkeleton True 2
        , viewChatSkeleton False 3
        , viewChatSkeleton True 1
        , viewChatSkeleton False 2
        , Html.div [ id Actions.agentChatEndId ] []
        ]


isCreatingAgentSession : Model.AgentState -> Bool
isCreatingAgentSession agent =
    agent.request == Just Model.CreatingAgentSession


viewSessionSidebar : Model.AgentState -> List Model.AgentSessionSummary -> Html (Flow Model ())
viewSessionSidebar agent listed =
    Html.div
        [ class "agent-panel__sidebar"
        , id "agent-panel-sidebar"
        ]
        (viewSessionSidebarContent agent listed)


viewSessionSidebarContent : Model.AgentState -> List Model.AgentSessionSummary -> List (Html (Flow Model ()))
viewSessionSidebarContent agent listed =
    let
        firstLoad =
            case agent.sessions of
                Loading Nothing ->
                    True

                NotAsked ->
                    True

                _ ->
                    False

        refreshing =
            case agent.sessions of
                Loading (Just _) ->
                    True

                _ ->
                    False

        listingError =
            case agent.sessions of
                Error err ->
                    Just (Http.errorMessage err)

                _ ->
                    Nothing

        ( active, archived ) =
            listed
                |> List.filter (\summary -> not (agentSessionBlank summary agent))
                |> List.partition (\summary -> not (Model.agentSessionArchived summary.session.status))

        activeRows =
            (if isCreatingAgentSession agent then
                [ viewCreatingSessionRow ]

             else
                []
            )
                ++ List.map (viewSessionRow agent) active
    in
    [ Html.viewMaybe
        (\msg ->
            Html.p [ class "agent-panel__loading" ] [ Html.text msg ]
        )
        listingError
    , Html.viewIf refreshing
        (Html.p [ class "agent-panel__refreshing shimmer-text shimmer-text--medium-contrast", attribute "role" "status" ]
            [ Html.text "Refreshing chats" ]
        )
    , if firstLoad then
        Html.ul [ class "agent-panel__session-list" ] (List.repeat 3 viewSessionRowSkeleton)

      else
        Html.ul [ class "agent-panel__session-list" ] activeRows
    , Html.viewIf (not firstLoad && List.isEmpty activeRows)
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


viewSessionRowSkeleton : Html msg
viewSessionRowSkeleton =
    Html.li
        [ class "agent-panel__session-row agent-panel__session-row--skeleton"
        , attribute "aria-hidden" "true"
        ]
        [ Html.div [ class "agent-panel__session-row-main" ]
            [ Html.div [ class "agent-panel__skeleton-line" ] []
            , Html.div [ class "agent-panel__skeleton-line" ] []
            ]
        ]


sessionDisplayName : Model.AgentSessionSummary -> String
sessionDisplayName =
    Model.displayName


viewCreatingSessionRow : Html msg
viewCreatingSessionRow =
    Html.li
        [ class "agent-panel__session-row is-selected is-optimistic"
        , attribute "aria-busy" "true"
        ]
        [ Html.div [ class "agent-panel__session-row-main" ]
            [ Html.div [ class "agent-panel__session-name" ] [ Html.text "New chat" ]
            , Html.div [ class "agent-panel__session-meta shimmer-text shimmer-text--medium-contrast" ] [ Html.text "Creating" ]
            ]
        ]


viewSessionRow : Model.AgentState -> Model.AgentSessionSummary -> Html (Flow Model ())
viewSessionRow agent summary =
    let
        session =
            summary.session

        isSelected =
            not (isCreatingAgentSession agent) && agent.selectedSessionId == Just session.sessionId

        isArchived =
            Model.agentSessionArchived session.status

        displayName =
            sessionDisplayName summary

        isRunning =
            agentSessionRunning session.sessionId agent

        interactionActive =
            agentSessionBlocked session.sessionId agent

        isArchiving =
            agent.request == Just (Model.ArchivingAgentSession session.sessionId)

        isDeleting =
            agent.request == Just (Model.DeletingAgentSession session.sessionId)

        statusLabel =
            if isArchiving then
                "Archiving"

            else if isDeleting then
                "Deleting"

            else
                Maybe.withDefault (chatStatusLabel session.status) (liveStatusLabel agent session.sessionId)

        rowAttrs =
            [ classList
                [ ( "agent-panel__session-row", True )
                , ( "is-selected", isSelected )
                , ( "is-archived", isArchived )
                , ( "is-updating", isArchiving || isDeleting )
                , ( "is-running", isRunning )
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
            , Html.viewIf summary.hasCommits
                (Html.span [ class "agent-panel__pill" ] [ Html.text "changes" ])
            ]
        , Html.div [ class "agent-panel__session-row-actions" ]
            [ viewIconButton "Copy link to chat"
                "share"
                [ class "icon-btn"
                , Events.stopPropagationOn "click" (Decode.succeed ( Actions.shareAgentChat session.sessionId, True ))
                ]
            , if isArchived then
                viewRowAction interactionActive "Delete permanently" "delete_forever" (Actions.confirmDeleteAgentSession session.sessionId)

              else if not isRunning then
                viewRowAction interactionActive "Archive" "archive" (Actions.archiveAgentSession session.sessionId)

              else
                Html.nothing
            ]
        ]


liveStatusLabel : Model.AgentState -> String -> Maybe String
liveStatusLabel agent sessionId =
    case get (liveTurnAt sessionId) agent of
        Just live ->
            if live.streamError /= Nothing then
                Just "Reconnecting"

            else if live.pendingQuestion /= Nothing then
                Just "Needs input"

            else if live.finished then
                Nothing

            else
                Just "Working"

        Nothing ->
            Nothing


viewSessionDetail : Bool -> AgentMentions.Resolver -> Model.AgentState -> List Model.AgentSessionSummary -> Html (Flow Model ())
viewSessionDetail mentionsAwaited resolveMention agent listed =
    case ( isCreatingAgentSession agent, Model.selectedSessionSummary agent ) of
        ( True, _ ) ->
            viewCreatingSession

        ( False, Just summary ) ->
            Model.selectedSessionView agent
                |> Maybe.map (viewSession mentionsAwaited resolveMention agent summary)
                |> Maybe.withDefault (viewSessionUnavailable agent)

        ( False, Nothing ) ->
            viewNoChat agent listed


viewSessionUnavailable : Model.AgentState -> Html (Flow Model ())
viewSessionUnavailable =
    viewSessionLoadError
        >> Maybe.map viewSessionLoadErrorNode
        >> Maybe.withDefault viewSessionsLoading


viewSessionLoadError : Model.AgentState -> Maybe Http.Error
viewSessionLoadError =
    Model.selectedSessionViewData >> Maybe.andThen (try ApiData.failure)


viewSessionLoadErrorNode : Http.Error -> Html (Flow Model ())
viewSessionLoadErrorNode err =
    Html.div [ class "agent-panel__empty" ] [ Html.p [] [ Html.text (Http.errorMessage err) ] ]


viewNoChat : Model.AgentState -> List Model.AgentSessionSummary -> Html (Flow Model ())
viewNoChat agent listed =
    Html.div [ class "agent-panel__empty" ]
        [ Html.p []
            [ Html.text
                (case agent.sessions of
                    Error err ->
                        Http.errorMessage err

                    _ ->
                        if List.isEmpty listed then
                            "No chats yet."

                        else
                            "Open a chat from the history, or start a new one."
                )
            ]
        , Html.div [ class "agent-panel__empty-actions" ]
            [ Html.button
                [ class "btn"
                , disabled (Model.agentMutationPending agent)
                , Events.onClick Actions.createAgentSession
                ]
                [ View.Icons.icon False "add"
                , Html.span [] [ Html.text "New chat" ]
                ]
            , Html.viewIf (not (List.isEmpty listed))
                (Html.button
                    [ class "btn"
                    , Events.onClick Actions.toggleAgentSessionList
                    ]
                    [ View.Icons.icon False "history"
                    , Html.span [] [ Html.text "Open chat history" ]
                    ]
                )
            ]
        ]


viewCreatingSession : Html (Flow Model ())
viewCreatingSession =
    viewChatBody [ attribute "aria-busy" "true" ]
        { title =
            Html.div [ class "agent-panel__session-title-card" ]
                [ Html.div [ class "agent-panel__session-title-row" ]
                    [ Html.div [ class "agent-panel__session-title-main" ]
                        [ Html.h3 [ class "agent-panel__session-title" ] [ Html.text "New chat" ]
                        , Html.div [ class "agent-panel__session-title-meta" ]
                            [ Html.span
                                [ class "agent-panel__creating shimmer-text shimmer-text--medium-contrast"
                                , attribute "role" "status"
                                ]
                                [ Html.text "Creating chat" ]
                            ]
                        ]
                    ]
                ]
        , error = Html.nothing
        , chat = viewEmptyChat EmptyCreating
        , composer = viewPrompt False ComposerIdle False True
        }


viewChatBody :
    List (Html.Attribute (Flow Model ()))
    ->
        { title : Html (Flow Model ())
        , error : Html (Flow Model ())
        , chat : Html (Flow Model ())
        , composer : Html (Flow Model ())
        }
    -> Html (Flow Model ())
viewChatBody attrs parts =
    Html.div (class "agent-panel__body" :: attrs)
        [ parts.title, parts.error, parts.chat, parts.composer ]


viewSession : Bool -> AgentMentions.Resolver -> Model.AgentState -> Model.AgentSessionSummary -> Model.AgentSessionView -> Html (Flow Model ())
viewSession mentionsAwaited resolveMention agent summary sessionView =
    let
        session =
            summary.session

        sessionId =
            session.sessionId

        runnerActive =
            agentSessionRunning sessionId agent

        detailBlocked =
            agentSessionBlocked sessionId agent

        closedChat =
            Model.agentSessionArchived session.status
    in
    viewChatBody []
        { title = viewSessionTitle agent summary
        , error = viewError session
        , chat =
            if mentionsAwaited then
                viewChatSkeletonBody

            else
                viewChatTurns resolveMention agent sessionView runnerActive closedChat detailBlocked
        , composer =
            if closedChat then
                viewClosedChat session.status

            else
                let
                    stopping =
                        agent.request == Just (Model.StoppingAgentTurn sessionId)

                    submitBlocked =
                        Model.agentMutationPending agent

                    busy =
                        if agent.request == Just (Model.SendingAgentPrompt sessionId) then
                            ComposerSending

                        else if agent.request == Just (Model.SteeringAgentTurn sessionId) then
                            ComposerSteering

                        else
                            ComposerIdle
                in
                viewPrompt runnerActive busy stopping submitBlocked
        }


viewSessionTitle : Model.AgentState -> Model.AgentSessionSummary -> Html (Flow Model ())
viewSessionTitle agent summary =
    let
        session =
            summary.session

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
                        sessionDisplayName summary
                in
                Html.div [ class "agent-panel__session-title-row" ]
                    [ Html.div [ class "agent-panel__session-title-main" ]
                        [ Html.h3 [ class "agent-panel__session-title" ] [ Html.text displayName ]
                        , Html.div [ class "agent-panel__session-title-meta" ]
                            [ Html.span [] [ Html.text (chatStatusLabel session.status) ]
                            , Html.span [ title session.sessionId ] [ Html.text ("#" ++ shortSha session.sessionId) ]
                            , Html.viewIf summary.hasCommits
                                (Html.span [] [ Html.text "changes" ])
                            , Html.viewMaybe
                                (\message ->
                                    Html.span
                                        [ class "agent-panel__stream-warning"
                                        , title message
                                        , attribute "role" "status"
                                        ]
                                        [ Html.text "Reconnecting" ]
                                )
                                (get (liveTurnAt session.sessionId) agent
                                    |> Maybe.andThen .streamError
                                )
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
    in
    Html.div [ class "agent-panel__session-title-editor" ]
        [ Html.input
            [ class "agent-panel__session-name-input"
            , id Actions.agentSessionNameInputId
            , type_ "text"
            , value edit.value
            , disabled (renameBlocked || edit.saving)
            , attribute "maxlength" (String.fromInt Model.chatNameMaxLength)
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
                [ class "small-btn"
                , disabled (renameBlocked || edit.saving || String.isEmpty trimmed)
                , Events.onClick Actions.saveAgentSessionName
                ]
                [ Html.text "Save" ]
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


type ComposerBusy
    = ComposerIdle
    | ComposerSending
    | ComposerSteering


viewPrompt : Bool -> ComposerBusy -> Bool -> Bool -> Html (Flow Model ())
viewPrompt runnerActive busy stopping submitBlocked =
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
                , attribute "aria-label" "Agent prompt"
                , submitShortcut submitBlocked
                ]
                []
            , Html.div [ class "agent-panel__composer-actions" ]
                [ viewSubmitButton steering busy submitBlocked
                , Html.viewIf steering (viewStopButton stopping)
                ]
            ]
        ]


viewSubmitButton : Bool -> ComposerBusy -> Bool -> Html (Flow Model ())
viewSubmitButton steering busy submitBlocked =
    let
        working =
            busy /= ComposerIdle

        label =
            case busy of
                ComposerSending ->
                    "Sending"

                ComposerSteering ->
                    "Steering"

                ComposerIdle ->
                    if steering then
                        "Steer"

                    else
                        "Send"
    in
    Html.button
        [ class "btn agent-panel__run-button"
        , disabled submitBlocked
        , attribute "aria-busy" (boolText working)
        , Events.onClick Actions.submitAgentPrompt
        , title
            (if steering then
                "Steer the running agent (Ctrl/⌘+Enter)"

             else
                "Send message (Ctrl/⌘+Enter)"
            )
        ]
        [ Html.text label
        , Html.viewIf (not working)
            (Html.span [ class "agent-panel__run-hint" ] [ Html.text "Ctrl/⌘+Enter" ])
        ]


viewStopButton : Bool -> Html (Flow Model ())
viewStopButton stopping =
    Html.button
        [ class "btn agent-panel__run-button agent-panel__stop-button"
        , disabled stopping
        , Events.onClick Actions.stopAgentTurn
        , title "Stop the agent"
        ]
        [ Html.text
            (if stopping then
                "Stopping"

             else
                "Stop"
            )
        , View.Icons.icon False "stop_circle"
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


viewChatTurns : AgentMentions.Resolver -> Model.AgentState -> Model.AgentSessionView -> Bool -> Bool -> Bool -> Html (Flow Model ())
viewChatTurns resolveMention agent sessionView runnerActive closedChat interactionsBlocked =
    let
        sessionId =
            sessionView.session.sessionId

        pendingChangesetNodes =
            case pendingChangeset sessionView of
                Just changeset ->
                    let
                        activeOperation =
                            activeChangesetOperation agent sessionId
                    in
                    [ viewChangesetBox interactionsBlocked activeOperation changeset ]

                Nothing ->
                    []

        questionNodes =
            sessionPendingQuestion sessionView agent
                |> Maybe.map
                    (viewAgentQuestion sessionId
                        (Model.agentMutationPending agent)
                        (agent.request == Just (Model.SteeringAgentTurn sessionId))
                        >> List.singleton
                    )
                |> Maybe.withDefault []

        pendingSteerNodes =
            sessionPendingSteer sessionView agent
                |> Maybe.map (viewPendingSteer >> List.singleton)
                |> Maybe.withDefault []

        content =
            List.map (viewChatEntry resolveMention interactionsBlocked sessionId agent.highlightTurnId) (sessionEntries sessionView agent) ++ pendingSteerNodes ++ pendingChangesetNodes ++ questionNodes
    in
    if List.isEmpty content then
        viewEmptyChat (EmptyMessages (not closedChat && not runnerActive))

    else
        Html.div [ class "agent-panel__chat", id Actions.agentChatId ]
            (content ++ [ Html.div [ id Actions.agentChatEndId ] [] ])


type EmptyChat
    = EmptyCreating
    | EmptyMessages Bool


viewEmptyChat : EmptyChat -> Html msg
viewEmptyChat state =
    let
        ( heading, detail ) =
            case state of
                EmptyCreating ->
                    ( "Creating chat", Just "Setting up your conversation." )

                EmptyMessages showPromptHint ->
                    ( "No messages yet"
                    , if showPromptHint then
                        Just "Send a message below to start."

                      else
                        Nothing
                    )
    in
    Html.div [ class "agent-panel__chat agent-panel__chat--empty", id Actions.agentChatId ]
        [ Html.div [ id Actions.agentChatEndId ] []
        , Html.div [ class "agent-panel__empty-state" ]
            [ Html.strong [] [ Html.text heading ]
            , Html.viewMaybe (\message -> Html.p [] [ Html.text message ]) detail
            ]
        ]


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


viewPendingSteer : String -> Html (Flow Model ())
viewPendingSteer prompt =
    Html.div [ class "agent-panel__chat-turn" ]
        [ Html.div [ class "agent-panel__chat-message agent-panel__chat-message--user" ]
            [ Html.div [ class "agent-panel__chat-label" ] [ Html.text "You" ]
            , Html.div [ class "agent-panel__chat-bubble agent-panel__chat-bubble--user is-sending" ] [ Html.text prompt ]
            , Html.div [ class "agent-panel__chat-sending shimmer-text shimmer-text--medium-contrast", attribute "role" "status" ]
                [ Html.text "Steering" ]
            ]
        ]


viewAgentMessage : AgentMentions.Resolver -> Model.ChatTurn -> Html (Flow Model ())
viewAgentMessage resolveMention turn =
    let
        isEmptyAssistant =
            String.isEmpty (String.trim turn.assistant)

        ( statusLabel, emptyBody, failedMessage ) =
            case turn.status of
                Model.ChatPending ->
                    ( "Running", "Waiting for a reply", Nothing )

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
                    , ( "shimmer-text--medium-contrast", turn.status == Model.ChatPending && isEmptyAssistant )
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


viewAgentQuestion : String -> Bool -> Bool -> Model.PendingQuestion -> Html (Flow Model ())
viewAgentQuestion sessionId answerBlocked answering question =
    Html.div [ class "agent-panel__question" ]
        [ Html.div [ class "agent-panel__question-options" ]
            (List.indexedMap (viewQuestionOption sessionId answerBlocked question) question.options)
        , Html.div [ class "agent-panel__question-actions" ]
            [ if answering then
                Html.span [ class "agent-panel__question-hint shimmer-text shimmer-text--medium-contrast", attribute "role" "status" ]
                    [ Html.text "Sending answer" ]

              else
                Html.span [ class "agent-panel__question-hint" ]
                    [ Html.text
                        (if question.multi then
                            "Pick any number of them, then send. Or type your own answer below."

                         else
                            "Or type your own answer below."
                        )
                    ]
            , Html.viewIf question.multi (viewQuestionSubmit sessionId answerBlocked question)
            ]
        ]


viewQuestionOption : String -> Bool -> Model.PendingQuestion -> Int -> String -> Html (Flow Model ())
viewQuestionOption sessionId answerBlocked question index option =
    let
        optionNumber =
            index + 1

        picked =
            Set.member optionNumber question.picked
    in
    Html.button
        [ classList
            [ ( "btn", True )
            , ( "agent-panel__question-option", True )
            , ( "is-picked", picked )
            ]
        , disabled answerBlocked
        , attribute "aria-pressed" (boolText picked)
        , Events.onClick (Actions.pickAgentQuestionOption sessionId question optionNumber)
        ]
        [ Html.span [ class "agent-panel__question-option-mark", attribute "aria-hidden" "true" ]
            [ Html.viewIf picked (View.Icons.icon False "check") ]
        , Html.text option
        ]


viewQuestionSubmit : String -> Bool -> Model.PendingQuestion -> Html (Flow Model ())
viewQuestionSubmit sessionId answerBlocked question =
    Html.button
        [ class "btn agent-panel__question-submit"
        , disabled (answerBlocked || Set.isEmpty question.picked)
        , Events.onClick (Actions.submitAgentQuestion sessionId question)
        ]
        [ Html.text "Send answer" ]


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
            Just { state = state, description = Model.defaultChangesetDescription state, diff = diff }

        else
            Just { state = Model.ChatChangesetProposed, description = Model.defaultChangesetDescription Model.ChatChangesetProposed, diff = diff }

    else
        Nothing


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
                Model.defaultChangesetDescription state

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
                "Applying"

            else
                "Apply changes"

        discardLabel =
            if isDiscarding then
                "Discarding"

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
                        [ Html.text applyLabel ]
                , Html.button
                    [ class "small-btn"
                    , disabled (not canDiscard)
                    , Events.onClick Actions.discardAgentSession
                    ]
                    [ Html.text discardLabel ]
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
