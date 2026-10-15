module Components.AgentPanel exposing (view, viewLightbox)

import Accessors exposing (get, just, try)
import Actions
import Api.ApiData as ApiData exposing (ApiData(..))
import Basics.Extra exposing (flip)
import Components.AgentMentions as AgentMentions
import Dict
import Extra.Http as Http
import File
import Flow exposing (Flow)
import Html exposing (Html)
import Html.Attributes exposing (alt, attribute, class, classList, disabled, id, placeholder, rows, src, title, type_, value)
import Html.Events as Events
import Html.Extra as Html
import Html.Lazy
import Http
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Keyboard
import List.Extra as List
import Model.Core as Model exposing (Model)
import Model.Lenses exposing (agentSessionBlank, agentSessionBlocked, agentSessionRunning, applying, lightbox, liveTurnAt, sessionEntries, sessionHasRunner, sessionPendingQuestion, sessionPendingSteer)
import Model.Lib as Lib
import Model.Shadow exposing (StepConfig)
import Route
import Set
import Time
import View.Icons
import View.Lib exposing (boolText)


view : Model -> Html (Flow Model ())
view model =
    let
        workspace =
            Lib.lastKnownWorkspace model
    in
    viewPanel (Model.getNow model) (mentionsPending workspace) (AgentMentions.sources workspace) (Maybe.map .branch (ApiData.toMaybe (Model.getUserRepoInfo model))) (Model.getAgent model)


mentionsPending : Model -> Bool
mentionsPending model =
    not (ApiData.settled (Model.getStepConfig model))
        || not (ApiData.settled (Model.getProjects model))


viewPanel : Time.Posix -> Bool -> AgentMentions.Sources -> Maybe String -> Model.AgentState -> Html (Flow Model ())
viewPanel now mentionsAwaited mentionSources branch agent =
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
        [ viewHeader branch agent
        , viewSessionBody now mentionsAwaited mentionSources agent
        ]


viewHeader : Maybe String -> Model.AgentState -> Html (Flow Model ())
viewHeader branch agent =
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

        autoApplyHelp =
            autoApplyDescription branch agent
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
            [ viewAutoApplySwitch autoApplyHelp agent
            , Html.span [ id autoApplyDescriptionId, Html.Attributes.hidden True ] [ Html.text autoApplyHelp ]
            , viewIconButton sessionsLabel
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


viewAutoApplySwitch : String -> Model.AgentState -> Html (Flow Model ())
viewAutoApplySwitch help agent =
    Html.button
        [ type_ "button"
        , class "agent-panel__auto-apply"
        , attribute "role" "switch"
        , attribute "aria-checked" (boolText agent.autoApply)
        , attribute "aria-describedby" autoApplyDescriptionId
        , title help
        , Events.onClick (Actions.setAgentAutoApply (not agent.autoApply))
        ]
        [ View.Icons.iconCustom True "fast_forward" [ class "agent-panel__auto-apply-icon", attribute "aria-hidden" "true" ]
        , Html.span [ class "agent-panel__auto-apply-label" ] [ Html.text "Auto-apply" ]
        , Html.span [ class "agent-panel__auto-apply-track", attribute "aria-hidden" "true" ] []
        ]


autoApplyDescriptionId : String
autoApplyDescriptionId =
    "agent-auto-apply-description"


autoApplyDescription : Maybe String -> Model.AgentState -> String
autoApplyDescription branch agent =
    let
        mode =
            if agent.autoApply then
                "When a turn succeeds, its changes are applied to "
                    ++ Maybe.withDefault "the shared branch" (Maybe.map (\name -> name ++ ", the shared branch") branch)
                    ++ ". Turn off to review them first."

            else
                "Changes wait on the changeset card until you apply or discard them."
    in
    if ownsRunningTurn agent then
        mode ++ " Turns already running follow this too."

    else
        mode


ownsRunningTurn : Model.AgentState -> Bool
ownsRunningTurn agent =
    ApiData.toMaybe agent.sessions
        |> Maybe.withDefault []
        |> List.any (\summary -> sessionHasRunner summary && ownsSession agent summary.session)


ownsSession : Model.AgentState -> Model.AgentSession -> Bool
ownsSession agent session =
    session.autoApplyClient == Just agent.clientId


turnAutoApplies : Model.AgentState -> Model.AgentSession -> Bool
turnAutoApplies agent session =
    if ownsSession agent session then
        agent.autoApply

    else
        session.autoApply


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


viewSessionBody : Time.Posix -> Bool -> AgentMentions.Sources -> Model.AgentState -> Html (Flow Model ())
viewSessionBody now mentionsAwaited mentionSources agent =
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
                    viewSessionDetail now mentionsAwaited mentionSources agent listed
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
                [ Html.div [ class "agent-panel__attach-button agent-panel__attach-button--skeleton" ] []
                , Html.div [ class "agent-panel__prompt agent-panel__prompt--skeleton" ] []
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

            else if agent.request == Just (Model.ApplyingAgentChanges session.sessionId) then
                "Applying"

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
                (Html.span [ class "agent-panel__pill" ] [ Html.text "not applied" ])
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

            else if live.applying /= Nothing then
                Just "Applying"

            else if live.finished then
                Nothing

            else
                Just "Working"

        Nothing ->
            Nothing


viewSessionDetail : Time.Posix -> Bool -> AgentMentions.Sources -> Model.AgentState -> List Model.AgentSessionSummary -> Html (Flow Model ())
viewSessionDetail now mentionsAwaited mentionSources agent listed =
    case ( isCreatingAgentSession agent, Model.selectedSessionSummary agent ) of
        ( True, _ ) ->
            viewCreatingSession agent

        ( False, Just summary ) ->
            Model.selectedSessionView agent
                |> Maybe.map (viewSession now mentionsAwaited mentionSources agent summary)
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


viewCreatingSession : Model.AgentState -> Html (Flow Model ())
viewCreatingSession agent =
    viewChatBody (Tuple.mapFirst ((::) (attribute "aria-busy" "true")) (imageDropZone agent.isDraggingFiles False))
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
        , composer = viewPrompt agent.autoApply False ComposerIdle False True agent.attachments
        }


viewChatBody :
    ( List (Html.Attribute (Flow Model ())), Html (Flow Model ()) )
    ->
        { title : Html (Flow Model ())
        , error : Html (Flow Model ())
        , chat : Html (Flow Model ())
        , composer : Html (Flow Model ())
        }
    -> Html (Flow Model ())
viewChatBody ( attrs, overlay ) parts =
    Html.div (class "agent-panel__body" :: attrs)
        [ parts.title, parts.error, parts.chat, parts.composer, overlay ]


imageDropZone : Bool -> Bool -> ( List (Html.Attribute (Flow Model ())), Html (Flow Model ()) )
imageDropZone dragging steering =
    ( [ Events.preventDefaultOn "dragenter" (carryingFiles (Decode.succeed ( Actions.setAgentFileDrag True, True )))
      , Events.preventDefaultOn "dragover" (carryingFiles (Decode.succeed ( Flow.none, True )))
      , Events.preventDefaultOn "drop" (carryingFiles (handled Actions.dropAgentImages (Decode.at [ "dataTransfer", "files" ] fileList)))
      ]
    , Html.viewIf dragging (viewDropOverlay steering)
    )


carryingFiles : Decode.Decoder a -> Decode.Decoder a
carryingFiles =
    Decode.when (Decode.at [ "dataTransfer", "types" ] (Decode.list Decode.string)) (List.member "Files")


fileList : Decode.Decoder (List File.File)
fileList =
    Decode.list File.decoder


handled : (a -> Flow Model ()) -> Decode.Decoder a -> Decode.Decoder ( Flow Model (), Bool )
handled action =
    Decode.map (action >> flip Tuple.pair True)


viewDropOverlay : Bool -> Html (Flow Model ())
viewDropOverlay steering =
    let
        ( iconName, label ) =
            if steering then
                ( "block", Actions.agentImagesWhileWorkingMessage )

            else
                ( "add_photo_alternate", "Drop images to attach" )
    in
    Html.div
        [ classList
            [ ( "agent-panel__drop-overlay", True )
            , ( "is-blocked", steering )
            ]
        , attribute "aria-hidden" "true"
        , Events.on "dragleave" (Decode.succeed (Actions.setAgentFileDrag False))
        ]
        [ View.Icons.icon False iconName
        , Html.span [] [ Html.text label ]
        ]


viewSession : Time.Posix -> Bool -> AgentMentions.Sources -> Model.AgentState -> Model.AgentSessionSummary -> Model.AgentSessionView -> Html (Flow Model ())
viewSession now mentionsAwaited mentionSources agent summary sessionView =
    let
        session =
            summary.session

        sessionId =
            session.sessionId

        applyingDiff =
            try (liveTurnAt sessionId << just << applying << just) agent

        runnerActive =
            agentSessionRunning sessionId agent

        closedChat =
            Model.agentSessionArchived session.status

        stopping =
            agent.request == Just (Model.StoppingAgentTurn sessionId)

        changeset =
            pendingChangeset agent runnerActive applyingDiff sessionView
    in
    viewChatBody
        (if closedChat then
            ( [], Html.nothing )

         else
            imageDropZone agent.isDraggingFiles (runnerActive || stopping)
        )
        { title = viewSessionTitle agent summary
        , error = Html.viewIf (not runnerActive && not (changesetRefused changeset)) (viewError session)
        , chat =
            if mentionsAwaited then
                viewChatSkeletonBody

            else
                let
                    changesetActions =
                        if applyingDiff == Nothing && not runnerActive then
                            [ viewChangesetActions sessionId (agentSessionBlocked sessionId agent) agent.request ]

                        else
                            []

                    autoApplyEcho =
                        if runnerActive && not stopping && applyingDiff == Nothing && ownsSession agent sessionView.session then
                            Just agent.autoApply

                        else
                            Nothing
                in
                viewChatTurns now mentionSources agent sessionView changeset changesetActions autoApplyEcho runnerActive closedChat
        , composer =
            if closedChat then
                viewClosedChat session.status

            else
                let
                    submitBlocked =
                        Model.agentMutationPending agent || applyingDiff /= Nothing

                    busy =
                        if agent.request == Just (Model.SendingAgentPrompt sessionId) then
                            ComposerSending

                        else if agent.request == Just (Model.SteeringAgentTurn sessionId) then
                            ComposerSteering

                        else
                            ComposerIdle
                in
                viewPrompt agent.autoApply (runnerActive && applyingDiff == Nothing) busy stopping submitBlocked agent.attachments
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
                                (Html.span [] [ Html.text "not applied" ])
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
            "Conflict"

        "evaluation_failed" ->
            "Needs fix"

        _ ->
            "Ready"


viewError : Model.AgentSession -> Html msg
viewError session =
    Html.viewMaybe (\err -> Html.pre [ class "agent-panel__error" ] [ Html.text err ]) session.lastError


changesetRefused : Maybe Model.ChatChangeset -> Bool
changesetRefused changeset =
    List.member (Maybe.map .state changeset) [ Just Model.ChatChangesetConflicted, Just Model.ChatChangesetRejected ]


type ComposerBusy
    = ComposerIdle
    | ComposerSending
    | ComposerSteering


viewPrompt : Bool -> Bool -> ComposerBusy -> Bool -> Bool -> List Model.AgentAttachment -> Html (Flow Model ())
viewPrompt autoApply runnerActive busy stopping submitBlocked attachments =
    let
        steering =
            runnerActive || stopping

        imagesBlockSteering =
            steering && not (List.isEmpty attachments)

        blocked =
            submitBlocked || imagesBlockSteering

        autoApplyState =
            if autoApply then
                "Auto-apply is on."

            else
                "Auto-apply is off."
    in
    Html.div [ class "agent-panel__composer" ]
        [ viewAttachments attachments
        , Html.div [ class "agent-panel__composer-row" ]
            [ viewAttachButton steering
            , Html.textarea
                [ class "agent-panel__prompt"
                , id "agent-prompt"
                , rows 1
                , placeholder "Ask for a change..."
                , attribute "aria-label" "Agent prompt"
                , attribute "aria-describedby" autoApplyStateId
                , submitShortcut blocked
                , pasteImages
                ]
                []
            , Html.div [ class "agent-panel__composer-actions" ]
                [ viewSubmitButton autoApplyState steering busy blocked imagesBlockSteering
                , Html.viewIf steering (viewStopButton stopping)
                ]
            ]
        , Html.span [ id autoApplyStateId, Html.Attributes.hidden True ] [ Html.text autoApplyState ]
        ]


viewAttachButton : Bool -> Html (Flow Model ())
viewAttachButton steering =
    viewIconButton
        (if steering then
            Actions.agentImagesWhileWorkingMessage

         else
            "Attach images"
        )
        "add_photo_alternate"
        [ class "icon-btn agent-panel__attach-button"
        , type_ "button"
        , disabled steering
        , Events.onClick Actions.pickAgentImages
        ]


viewAttachments : List Model.AgentAttachment -> Html (Flow Model ())
viewAttachments attachments =
    Html.viewIf (not (List.isEmpty attachments))
        (Html.ul
            [ class "agent-panel__attachments"
            , attribute "aria-label" "Attached images"
            ]
            (List.indexedMap (viewAttachment (List.map .dataUrl attachments)) attachments)
        )


viewAttachment : List String -> Int -> Model.AgentAttachment -> Html (Flow Model ())
viewAttachment sources index attachment =
    Html.li [ class "agent-panel__attachment" ]
        [ Html.button
            [ class "agent-panel__attachment-preview"
            , type_ "button"
            , title ("Preview " ++ attachment.name)
            , Events.onClick (Actions.openAgentLightbox sources index)
            ]
            [ Html.img [ src attachment.dataUrl, alt ("Preview " ++ attachment.name) ] [] ]
        , viewIconButton ("Remove " ++ attachment.name)
            "close"
            [ class "agent-panel__attachment-remove"
            , type_ "button"
            , Events.onClick (Actions.removeAgentAttachment index)
            ]
        ]


pasteImages : Html.Attribute (Flow Model ())
pasteImages =
    Events.preventDefaultOn "paste"
        (Decode.field "clipboardData"
            (Decode.when (Decode.field "types" (Decode.list Decode.string))
                (not << List.member "text/plain")
                (Decode.when (Decode.field "files" fileList)
                    (not << List.isEmpty)
                    (handled Actions.stageAgentImages (Decode.field "files" fileList))
                )
            )
        )


autoApplyStateId : String
autoApplyStateId =
    "agent-auto-apply-state"


viewSubmitButton : String -> Bool -> ComposerBusy -> Bool -> Bool -> Html (Flow Model ())
viewSubmitButton autoApplyState steering busy submitBlocked imagesBlockSteering =
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
            (if imagesBlockSteering then
                Actions.agentImagesSteeringMessage

             else if steering then
                "Steer the running agent (Ctrl/⌘+Enter). " ++ autoApplyState

             else
                "Send message (Ctrl/⌘+Enter). " ++ autoApplyState
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


viewChatTurns : Time.Posix -> AgentMentions.Sources -> Model.AgentState -> Model.AgentSessionView -> Maybe Model.ChatChangeset -> List (Html (Flow Model ())) -> Maybe Bool -> Bool -> Bool -> Html (Flow Model ())
viewChatTurns now mentionSources agent sessionView changeset changesetActions autoApplyEcho runnerActive closedChat =
    let
        sessionId =
            sessionView.session.sessionId

        pendingChangesetNodes =
            changeset
                |> Maybe.map (viewChangesetBox changesetActions >> List.singleton)
                |> Maybe.withDefault []

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

        activityNodes =
            get (liveTurnAt sessionId) agent
                |> Maybe.map (Model.visibleToolCalls now >> List.map (viewToolCall now))
                |> Maybe.withDefault []

        content =
            List.map (viewChatEntry mentionSources sessionId agent.highlightTurnId autoApplyEcho) (sessionEntries sessionView agent) ++ activityNodes ++ pendingSteerNodes ++ pendingChangesetNodes ++ questionNodes
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


viewChatEntry : AgentMentions.Sources -> String -> Maybe String -> Maybe Bool -> Model.ChatEntry -> Html (Flow Model ())
viewChatEntry mentionSources sessionId highlightTurnId autoApplyEcho entry =
    case entry of
        Model.ChatTurnEntry turn ->
            viewChatTurn mentionSources sessionId (highlightTurnId == Just turn.turnId) autoApplyEcho turn

        Model.ChatChangesetEntry changeset ->
            viewChangesetBox [] changeset


turnIdAttribute : Model.ChatTurn -> List (Html.Attribute (Flow Model ()))
turnIdAttribute turn =
    if String.isEmpty turn.turnId then
        []

    else
        [ id (Actions.agentTurnId turn.turnId) ]


viewChatTurn : AgentMentions.Sources -> String -> Bool -> Maybe Bool -> Model.ChatTurn -> Html (Flow Model ())
viewChatTurn mentionSources sessionId isHighlighted autoApplyEcho turn =
    Html.div
        (classList
            [ ( "agent-panel__chat-turn", True )
            , ( "is-highlighted", isHighlighted )
            ]
            :: turnIdAttribute turn
        )
        [ Html.viewMaybe
            (\prompt ->
                Html.div [ class "agent-panel__chat-message agent-panel__chat-message--user" ]
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
                    , viewChatImages turn.images
                    , Html.viewIf (not (String.isEmpty prompt))
                        (Html.div [ class "agent-panel__chat-bubble agent-panel__chat-bubble--user" ]
                            [ Html.text prompt ]
                        )
                    ]
            )
            turn.prompt
        , viewAgentMessage mentionSources autoApplyEcho turn
        ]


viewChatImages : List String -> Html (Flow Model ())
viewChatImages images =
    let
        count =
            List.length images
    in
    Html.viewIf (count > 0)
        (Html.div
            [ classList
                [ ( "agent-panel__chat-images", True )
                , ( "is-single", count == 1 )
                ]
            ]
            (List.indexedMap
                (\index source ->
                    Html.button
                        [ class "agent-panel__chat-image"
                        , type_ "button"
                        , title "View full size"
                        , Events.onClick (Actions.openAgentLightbox images index)
                        ]
                        [ Html.img
                            [ src source
                            , alt (imagePosition index count)
                            , attribute "loading" "lazy"
                            , attribute "decoding" "async"
                            ]
                            []
                        ]
                )
                images
            )
        )


imagePosition : Int -> Int -> String
imagePosition index count =
    "Image " ++ String.fromInt (index + 1) ++ " of " ++ String.fromInt count


viewLightbox : Model -> Html (Flow Model ())
viewLightbox model =
    Html.node "dialog"
        [ id Actions.agentLightboxId
        , class "agent-lightbox"
        , attribute "aria-label" "Image viewer"
        , Events.on "close" (Decode.succeed Actions.agentLightboxClosed)
        , Events.onClick Actions.closeAgentLightbox
        , Events.on "keydown"
            (Keyboard.decodeCombinations
                [ ( Keyboard.arrowLeft, Decode.succeed (Actions.stepAgentLightbox -1) )
                , ( Keyboard.arrowRight, Decode.succeed (Actions.stepAgentLightbox 1) )
                ]
            )
        ]
        [ Html.viewMaybe viewLightboxStage (try (Model.Lenses.agent << lightbox << just) model) ]


viewLightboxStage : Model.AgentLightbox -> Html (Flow Model ())
viewLightboxStage shown =
    let
        count =
            List.length shown.images
    in
    Html.div [ class "agent-lightbox__stage" ]
        (Html.viewMaybe
            (\source ->
                Html.img
                    [ class "agent-lightbox__image"
                    , src source
                    , alt (imagePosition shown.index count)
                    , Events.stopPropagationOn "click" (Decode.succeed ( Flow.none, True ))
                    ]
                    []
            )
            (List.getAt shown.index shown.images)
            :: viewLightboxButton "Close" "close" "agent-lightbox__close" Actions.closeAgentLightbox
            :: (if count > 1 then
                    [ viewLightboxButton "Previous image" "chevron_left" "agent-lightbox__nav agent-lightbox__nav--prev" (Actions.stepAgentLightbox -1)
                    , viewLightboxButton "Next image" "chevron_right" "agent-lightbox__nav agent-lightbox__nav--next" (Actions.stepAgentLightbox 1)
                    , Html.div [ class "agent-lightbox__counter", attribute "aria-live" "polite" ]
                        [ Html.text (String.fromInt (shown.index + 1) ++ " / " ++ String.fromInt count) ]
                    ]

                else
                    []
               )
        )


viewLightboxButton : String -> String -> String -> Flow Model () -> Html (Flow Model ())
viewLightboxButton label iconName className action =
    viewIconButton label
        iconName
        [ class ("icon-btn agent-lightbox__button " ++ className)
        , type_ "button"
        , Events.stopPropagationOn "click" (Decode.succeed ( action, True ))
        ]


viewToolCall : Time.Posix -> Model.AgentToolCall -> Html (Flow Model ())
viewToolCall now call =
    Html.div [ class "agent-panel__chat-message agent-panel__chat-message--agent" ]
        [ Html.div [ class "agent-panel__chat-bubble agent-panel__chat-bubble--agent" ]
            [ Html.span
                [ class "agent-panel__chat-status is-running"
                , attribute "role" "status"
                ]
                [ Html.text ("Running " ++ call.name ++ " · " ++ formatElapsed call.startedAt now) ]
            , Html.text " "
            , Html.code [] [ Html.text call.text ]
            ]
        ]


formatElapsed : Time.Posix -> Time.Posix -> String
formatElapsed startedAt now =
    let
        seconds =
            max 0 (Time.posixToMillis now - Time.posixToMillis startedAt) // 1000
    in
    String.fromInt (seconds // 60) ++ ":" ++ String.padLeft 2 '0' (String.fromInt (remainderBy 60 seconds))


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


viewAgentMessage : AgentMentions.Sources -> Maybe Bool -> Model.ChatTurn -> Html (Flow Model ())
viewAgentMessage mentionSources autoApplyEcho turn =
    let
        isEmptyAssistant =
            String.isEmpty (String.trim turn.assistant)

        ( statusLabel, emptyBody, failedMessage ) =
            case turn.status of
                Model.ChatPending ->
                    ( "Running"
                    , if turn.prompt == Nothing then
                        "The changes couldn't be applied, so the agent is fixing them."

                      else
                        "Waiting for a reply"
                    , Nothing
                    )

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
            , Html.viewMaybe viewAutoApplyEcho
                (if turn.status == Model.ChatPending then
                    autoApplyEcho

                 else
                    Nothing
                )
            ]
        , Html.div
            [ classList
                [ ( "agent-panel__chat-bubble", True )
                , ( "agent-panel__chat-bubble--agent", True )
                , ( "is-pending", turn.status == Model.ChatPending && isEmptyAssistant )
                , ( "is-failed", failedMessage /= Nothing )
                ]
            ]
            [ Html.Lazy.lazy6 viewAgentMessageContent
                mentionSources.projects
                mentionSources.steps
                mentionSources.route
                mentionSources.stepConfig
                (turn.status == Model.ChatPending && isEmptyAssistant)
                body
            , Html.viewIf (turn.status == Model.ChatPending)
                (Html.span [ class "agent-panel__chat-cursor", attribute "aria-hidden" "true" ] [ Html.text "█" ])
            ]
        , Html.viewMaybe
            (\err -> Html.div [ class "agent-panel__chat-error" ] [ Html.text ("Failed: " ++ err) ])
            failedMessage
        ]


viewAutoApplyEcho : Bool -> Html msg
viewAutoApplyEcho enabled =
    Html.span [ class "agent-panel__chat-auto-apply" ]
        [ Html.text
            (if enabled then
                "· Auto-apply on"

             else
                "· Auto-apply off"
            )
        ]


viewAgentMessageContent : Dict.Dict Int Model.ProjectRecord -> Dict.Dict Int Model.StepRecord -> Route.Route -> StepConfig -> Bool -> String -> Html (Flow Model ())
viewAgentMessageContent projects_ steps_ route_ stepConfig_ shimmer body =
    Html.div
        [ classList
            [ ( "agent-panel__chat-content", True )
            , ( "shimmer-text", shimmer )
            , ( "shimmer-text--medium-contrast", shimmer )
            ]
        ]
        (AgentMentions.toHtml { projects = projects_, steps = steps_, route = route_, stepConfig = stepConfig_ } body)


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


pendingChangeset : Model.AgentState -> Bool -> Maybe String -> Model.AgentSessionView -> Maybe Model.ChatChangeset
pendingChangeset agent runnerActive applyingDiff ({ session, gitState } as sessionView) =
    let
        changeset state diff =
            Just { state = state, description = changesetDescription agent runnerActive session state, diff = diff }
    in
    case applyingDiff of
        Just diff ->
            changeset Model.ChatChangesetApplying diff

        Nothing ->
            if not gitState.hasAgentCommits || Model.agentSessionArchived session.status then
                Nothing

            else if agent.request == Just (Model.ApplyingAgentChanges session.sessionId) then
                changeset Model.ChatChangesetApplying gitState.branchDiff

            else
                changeset (unappliedState sessionView) gitState.branchDiff


changesetDescription : Model.AgentState -> Bool -> Model.AgentSession -> Model.ChatChangesetState -> String
changesetDescription agent runnerActive session state =
    let
        branch =
            "`" ++ session.targetBranch ++ "`"
    in
    case state of
        Model.ChatChangesetPending ->
            if runnerActive && turnAutoApplies agent session then
                "Not applied yet. If this turn succeeds, these changes are auto-applied to " ++ branch ++ "."

            else if runnerActive then
                "Auto-apply is off for this turn. When it ends, review these changes here."

            else if agent.autoApply then
                "Not applied yet. Apply or discard these changes, or they're auto-applied when the next turn succeeds."

            else
                "Auto-apply is off. Review these changes, then apply them to " ++ branch ++ " or discard them."

        Model.ChatChangesetApplying ->
            "Applying these changes to " ++ branch ++ "."

        Model.ChatChangesetConflicted ->
            ""

        Model.ChatChangesetRejected ->
            ""

        Model.ChatChangesetApplied ->
            ""

        Model.ChatChangesetDiscarded ->
            ""


unappliedState : Model.AgentSessionView -> Model.ChatChangesetState
unappliedState { session, turns } =
    case session.status of
        "prepare_conflict" ->
            Model.ChatChangesetConflicted

        "evaluation_failed" ->
            Model.ChatChangesetRejected

        _ ->
            if List.any (\turn -> turn.turnAutomatic && Just turn.turnId == session.activeTurnId) turns then
                Model.ChatChangesetRejected

            else
                Model.ChatChangesetPending


viewChangesetActions : String -> Bool -> Maybe Model.AgentRequest -> Html (Flow Model ())
viewChangesetActions sessionId blocked request =
    Html.div [ class "agent-panel__changeset-actions" ]
        [ Html.button
            [ class "small-btn"
            , disabled blocked
            , Events.onClick (Actions.applyAgentChanges sessionId)
            ]
            [ Html.text
                (if request == Just (Model.ApplyingAgentChanges sessionId) then
                    "Applying"

                 else
                    "Apply changes"
                )
            ]
        , Html.button
            [ class "small-btn"
            , disabled blocked
            , Events.onClick (Actions.discardAgentChanges sessionId)
            ]
            [ Html.text
                (if request == Just (Model.DiscardingAgentChanges sessionId) then
                    "Discarding"

                 else
                    "Discard changeset"
                )
            ]
        ]


viewChangesetBox : List (Html msg) -> Model.ChatChangeset -> Html msg
viewChangesetBox actions changeset =
    let
        diff =
            String.trim changeset.diff

        ( stateClass, statusLabel ) =
            case changeset.state of
                Model.ChatChangesetPending ->
                    ( "is-pending", "Pending" )

                Model.ChatChangesetApplying ->
                    ( "is-applying", "Applying" )

                Model.ChatChangesetConflicted ->
                    ( "is-conflicted", "Conflict" )

                Model.ChatChangesetRejected ->
                    ( "is-rejected", "Not applied" )

                Model.ChatChangesetApplied ->
                    ( "is-applied", "Applied" )

                Model.ChatChangesetDiscarded ->
                    ( "is-discarded", "Discarded" )
    in
    Html.div [ class "agent-panel__changeset", class stateClass ]
        ([ Html.div [ class "agent-panel__changeset-header" ]
            [ Html.h4 [] [ Html.text "Changeset" ]
            , Html.Lazy.lazy viewChangesetTotals diff
            , Html.span [ class "agent-panel__changeset-status" ] [ Html.text statusLabel ]
            ]
         , Html.viewIf (not (String.isEmpty changeset.description))
            (Html.p [ class "agent-panel__changeset-description" ] (viewChangesetDescription changeset.description))
         , Html.viewIf (not (String.isEmpty diff))
            (Html.Lazy.lazy viewChangesetDiff diff)
         ]
            ++ actions
        )


viewChangesetDescription : String -> List (Html msg)
viewChangesetDescription description =
    case String.split "`" description of
        text :: code :: next :: rest ->
            let
                gluedLength =
                    String.indexes " " next |> List.head |> Maybe.withDefault (String.length next)
            in
            Html.text text
                :: Html.span [ class "agent-panel__changeset-code" ] [ Html.code [] [ Html.text code ], Html.text (String.left gluedLength next) ]
                :: viewChangesetDescription (String.join "`" (String.dropLeft gluedLength next :: rest))

        _ ->
            [ Html.text description ]


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
