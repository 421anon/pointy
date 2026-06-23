module Api.Agent exposing
    ( archive
    , confirmApply
    , createSession
    , delete_
    , discardSession
    , fetchSession
    , listSessions
    , prepareApply
    , renameSession
    , sendTurn
    , turnEvent
    )

import Flow exposing (Flow)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Model.Core as Model


baseUrl : String
baseUrl =
    "/backend/agent"


createSession : Flow s (Result Http.Error Model.AgentSessionView)
createSession =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/session"
            , body = Http.emptyBody
            , expect = Http.expectJson identity sessionViewDecoder
            }


listSessions : Flow s (Result Http.Error (List Model.AgentSessionView))
listSessions =
    Flow.lift <|
        Http.get
            { url = baseUrl ++ "/sessions"
            , expect = Http.expectJson identity (Decode.list sessionViewDecoder)
            }


fetchSession : String -> Flow s (Result Http.Error Model.AgentSessionView)
fetchSession sessionId =
    Flow.lift <|
        Http.get
            { url = baseUrl ++ "/session/" ++ sessionId
            , expect = Http.expectJson identity sessionViewDecoder
            }


sendTurn : String -> String -> Flow s (Result Http.Error Model.AgentTurn)
sendTurn sessionId prompt =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/turn"
            , body =
                Http.jsonBody <|
                    Encode.object
                        [ ( "sessionId", Encode.string sessionId )
                        , ( "prompt", Encode.string prompt )
                        ]
            , expect = Http.expectJson identity turnDecoder
            }


prepareApply : String -> Flow s (Result Http.Error Model.AgentSessionView)
prepareApply sessionId =
    postSessionView "/prepare-apply" (sessionIdBody sessionId)


confirmApply : String -> String -> String -> Flow s (Result Http.Error Model.AgentSessionView)
confirmApply sessionId targetHead candidateHead =
    postSessionView "/confirm-apply"
        (Encode.object
            [ ( "sessionId", Encode.string sessionId )
            , ( "targetHead", Encode.string targetHead )
            , ( "candidateHead", Encode.string candidateHead )
            ]
        )


discardSession : String -> Flow s (Result Http.Error Model.AgentSessionView)
discardSession sessionId =
    postSessionView "/discard" (sessionIdBody sessionId)


renameSession : String -> String -> Flow s (Result Http.Error Model.AgentSessionView)
renameSession sessionId name =
    postSessionView "/rename"
        (Encode.object
            [ ( "sessionId", Encode.string sessionId )
            , ( "name", Encode.string name )
            ]
        )


postSessionView : String -> Encode.Value -> Flow s (Result Http.Error Model.AgentSessionView)
postSessionView path body =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ path
            , body = Http.jsonBody body
            , expect = Http.expectJson identity sessionViewDecoder
            }


sessionIdBody : String -> Encode.Value
sessionIdBody sessionId =
    Encode.object [ ( "sessionId", Encode.string sessionId ) ]


sessionViewDecoder : Decoder Model.AgentSessionView
sessionViewDecoder =
    Decode.succeed Model.AgentSessionView
        |> required "session" sessionDecoder
        |> required "gitState" gitStateDecoder
        |> required "turns" (Decode.list turnDecoder)


sessionDecoder : Decoder Model.AgentSession
sessionDecoder =
    Decode.succeed Model.AgentSession
        |> required "sessionId" Decode.string
        |> optional "sessionName" (Decode.maybe Decode.string) Nothing
        |> required "targetBranch" Decode.string
        |> required "agentBranch" Decode.string
        |> required "baseCommit" Decode.string
        |> required "worktreePath" Decode.string
        |> required "status" Decode.string
        |> optional "preparedApply" (Decode.maybe preparedApplyDecoder) Nothing
        |> optional "activeTurnId" (Decode.maybe Decode.string) Nothing
        |> optional "lastError" (Decode.maybe Decode.string) Nothing


preparedApplyDecoder : Decoder Model.AgentPreparedApply
preparedApplyDecoder =
    Decode.succeed Model.AgentPreparedApply
        |> required "targetHead" Decode.string
        |> required "agentHead" Decode.string
        |> required "candidateHead" Decode.string
        |> required "candidateWorktree" Decode.string


gitStateDecoder : Decoder Model.AgentGitState
gitStateDecoder =
    Decode.succeed Model.AgentGitState
        |> required "headCommit" Decode.string
        |> required "commitLog" Decode.string
        |> required "branchDiff" Decode.string
        |> required "hasAgentCommits" Decode.bool


turnDecoder : Decoder Model.AgentTurn
turnDecoder =
    Decode.succeed Model.AgentTurn
        |> required "turnId" Decode.string
        |> required "turnSessionId" Decode.string
        |> optional "turnPrompt" Decode.string ""
        |> required "turnStatus" Decode.string
        |> optional "turnExitCode" (Decode.maybe Decode.int) Nothing
        |> required "turnLogPath" Decode.string
        |> optional "turnLog" Decode.string ""


turnEvent : Decoder Model.AgentTurnEvent
turnEvent =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "chunk" ->
                        Decode.field "data"
                            (Decode.succeed (\turnId chunk -> Model.AgentTurnChunk { turnId = turnId, chunk = chunk })
                                |> required "turnId" Decode.string
                                |> required "chunk" Decode.string
                            )

                    "done" ->
                        Decode.field "data"
                            (Decode.map Model.AgentTurnDone (Decode.field "turnId" Decode.string))

                    "heartbeat" ->
                        Decode.succeed Model.AgentTurnHeartbeat

                    "error" ->
                        Decode.field "data" Decode.string |> Decode.map Model.AgentTurnError

                    _ ->
                        Decode.fail ("Unknown agent turn event: " ++ type_)
            )


archive : String -> Flow s (Result Http.Error Model.AgentSessionView)
archive sessionId =
    postSessionView "/archive" (sessionIdBody sessionId)


delete_ : String -> Flow s (Result Http.Error ())
delete_ sessionId =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/delete"
            , body = Http.jsonBody (sessionIdBody sessionId)
            , expect = Http.expectWhatever identity
            }
