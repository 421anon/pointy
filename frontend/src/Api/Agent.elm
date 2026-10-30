module Api.Agent exposing
    ( applyChanges
    , archive
    , compact
    , createSession
    , delete_
    , discardSession
    , fetchSession
    , imageMimeTypes
    , listSessions
    , renameSession
    , sendTurn
    , setAutoApply
    , steer
    , stop
    , turnEvent
    )

import Flow exposing (Flow)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode
import Maybe.Extra as Maybe
import Model.Core as Model
import Route


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


listSessions : Flow s (Result Http.Error (List Model.AgentSessionSummary))
listSessions =
    Flow.lift <|
        Http.get
            { url = baseUrl ++ "/sessions"
            , expect = Http.expectJson identity (Decode.list sessionSummaryDecoder)
            }


sessionSummaryDecoder : Decoder Model.AgentSessionSummary
sessionSummaryDecoder =
    Decode.succeed Model.AgentSessionSummary
        |> required "session" sessionDecoder
        |> required "title" Decode.string
        |> required "turnCount" Decode.int
        |> required "hasCommits" Decode.bool


fetchSession : String -> Flow s (Result Http.Error Model.AgentSessionView)
fetchSession sessionId =
    Flow.lift <|
        Http.get
            { url = baseUrl ++ "/session/" ++ sessionId
            , expect = Http.expectJson identity sessionViewDecoder
            }


type alias AutoApplySetting a =
    { a | autoApply : Bool, clientId : String }


autoApplyFields : AutoApplySetting a -> List ( String, Encode.Value )
autoApplyFields setting =
    [ ( "autoApply", Encode.bool setting.autoApply )
    , ( "clientId", Encode.string setting.clientId )
    ]


sendTurn : String -> String -> List Model.ComposerAttachment -> AutoApplySetting a -> Maybe Int -> Maybe Route.ArtifactParams -> Flow s (Result Http.Error Model.AgentTurn)
sendTurn sessionId prompt attachments setting mCurrentProjectId mViewedArtifact =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/turn"
            , body =
                Http.jsonBody <|
                    Encode.object
                        ([ ( "sessionId", Encode.string sessionId )
                         , ( "prompt", Encode.string prompt )
                         , ( "images", Encode.list encodeImage (Model.composerPlainImages attachments) )
                         , ( "areaCaptures", Encode.list encodeCapturedArea (Model.composerCapturedAreas attachments) )
                         , ( "currentProjectId", Maybe.unwrap Encode.null Encode.int mCurrentProjectId )
                         , ( "viewedArtifact", Maybe.unwrap Encode.null encodeViewedArtifact mViewedArtifact )
                         ]
                            ++ autoApplyFields setting
                        )
            , expect = Http.expectJson identity turnDecoder
            }


encodeViewedArtifact : Route.ArtifactParams -> Encode.Value
encodeViewedArtifact artifact =
    Encode.object
        [ ( "stepId", Encode.int artifact.stepId )
        , ( "commit", Encode.string artifact.commit )
        , ( "path", Encode.list Encode.string artifact.path )
        ]


encodeImage : Model.ComposerImage -> Encode.Value
encodeImage image =
    Encode.object
        [ ( "mimeType", Encode.string image.mimeType )
        , ( "data", Encode.string (String.dropLeft (String.length ("data:" ++ image.mimeType ++ ";base64,")) image.url) )
        ]


encodeCapturedArea : Model.CapturedArea -> Encode.Value
encodeCapturedArea area =
    Encode.object
        [ ( "overview", encodeImage area.overview )
        , ( "selection", encodeImage area.selection )
        , ( "documentSize"
          , Encode.object
                [ ( "width", Encode.int (round area.documentSize.width) )
                , ( "height", Encode.int (round area.documentSize.height) )
                ]
          )
        , ( "selectionRect"
          , Encode.object
                [ ( "x", Encode.int (round area.selectionRect.x) )
                , ( "y", Encode.int (round area.selectionRect.y) )
                , ( "width", Encode.int (round area.selectionRect.width) )
                , ( "height", Encode.int (round area.selectionRect.height) )
                ]
          )
        , ( "artifact", encodeCapturedArtifact area.artifact )
        ]


encodeCapturedArtifact : Route.ArtifactParams -> Encode.Value
encodeCapturedArtifact artifact =
    Encode.object
        [ ( "projectPath", Encode.list Encode.int artifact.projectPath )
        , ( "stepId", Encode.int artifact.stepId )
        , ( "commit", Encode.string artifact.commit )
        , ( "path", Encode.list Encode.string artifact.path )
        ]


imageMimeTypes : List String
imageMimeTypes =
    [ "image/png", "image/jpeg", "image/gif", "image/webp" ]


steer : String -> String -> Flow s (Result Http.Error ())
steer sessionId prompt =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/steer"
            , body =
                Http.jsonBody <|
                    Encode.object
                        [ ( "sessionId", Encode.string sessionId )
                        , ( "prompt", Encode.string prompt )
                        ]
            , expect = Http.expectWhatever identity
            }


applyChanges : String -> AutoApplySetting a -> Flow s (Result Http.Error Model.AgentSessionView)
applyChanges sessionId setting =
    postSessionView "/apply" (Encode.object (( "sessionId", Encode.string sessionId ) :: autoApplyFields setting))


discardSession : String -> Flow s (Result Http.Error Model.AgentSessionView)
discardSession sessionId =
    postSessionView "/discard" (sessionIdBody sessionId)


setAutoApply : AutoApplySetting a -> Flow s (Result Http.Error ())
setAutoApply setting =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/auto-apply"
            , body = Http.jsonBody (Encode.object (autoApplyFields setting))
            , expect = Http.expectWhatever identity
            }


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
        |> optional "activeTurnId" (Decode.maybe Decode.string) Nothing
        |> optional "lastError" (Decode.maybe Decode.string) Nothing
        |> required "updatedAt" updatedAtDecoder
        |> optional "autoApply" Decode.bool True
        |> optional "autoApplyClient" (Decode.maybe Decode.string) Nothing


updatedAtDecoder : Decoder Model.SessionTimestamp
updatedAtDecoder =
    Decode.string
        |> Decode.andThen
            (\raw ->
                case decodeSessionTimestamp raw of
                    Just timestamp ->
                        Decode.succeed timestamp

                    Nothing ->
                        Decode.fail ("Invalid session updatedAt: " ++ raw)
            )


decodeSessionTimestamp : String -> Maybe Model.SessionTimestamp
decodeSessionTimestamp raw =
    let
        ( basePart, fraction ) =
            case String.indexes "." raw of
                dotIdx :: _ ->
                    ( String.left dotIdx raw ++ "Z", String.dropLeft (dotIdx + 1) (String.dropRight 1 raw) )

                [] ->
                    ( raw, "" )
    in
    case Iso8601.toTime basePart of
        Ok posix ->
            Maybe.map (Model.SessionTimestamp posix) (String.toInt (String.padRight 12 '0' (String.left 12 fraction)))

        Err _ ->
            Nothing


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
        |> custom turnAttachmentsDecoder
        |> optional "turnAutomatic" Decode.bool False
        |> required "turnStatus" Decode.string
        |> optional "turnExitCode" (Decode.maybe Decode.int) Nothing
        |> required "turnLogPath" Decode.string
        |> optional "turnLog" Decode.string ""


turnAttachmentsDecoder : Decoder (List Model.Attachment)
turnAttachmentsDecoder =
    Decode.map3 turnAttachments
        (Decode.field "turnId" Decode.string)
        (Decode.field "turnImages" (Decode.list Decode.string))
        (Decode.field "turnAreaCaptures" (Decode.list storedAreaCaptureDecoder))


type alias StoredAreaCapture =
    { overviewImage : Int
    , selectionImage : Int
    , artifact : Maybe Route.ArtifactParams
    }


storedAreaCaptureDecoder : Decoder StoredAreaCapture
storedAreaCaptureDecoder =
    Decode.succeed StoredAreaCapture
        |> required "overviewImage" Decode.int
        |> required "selectionImage" Decode.int
        |> optional "artifact" (Decode.map Just capturedArtifactDecoder) Nothing


capturedArtifactDecoder : Decoder Route.ArtifactParams
capturedArtifactDecoder =
    Decode.succeed Route.ArtifactParams
        |> required "projectPath" (Decode.list Decode.int)
        |> required "stepId" Decode.int
        |> required "commit" Decode.string
        |> required "path" (Decode.list Decode.string)


turnAttachments : String -> List String -> List StoredAreaCapture -> List Model.Attachment
turnAttachments turnId mimeTypes areaCaptures =
    let
        capturedIndexes =
            List.concatMap (\capture -> [ capture.overviewImage, capture.selectionImage ]) areaCaptures

        plainImages =
            List.range 0 (List.length mimeTypes - 1)
                |> List.filter (\index -> not (List.member index capturedIndexes))
                |> List.map (Model.ImageAttachment << turnImageUrl turnId)

        capturedAreas =
            List.map
                (\capture ->
                    Model.CapturedAreaAttachment
                        { overviewUrl = turnImageUrl turnId capture.overviewImage
                        , selectionUrl = turnImageUrl turnId capture.selectionImage
                        , artifact = capture.artifact
                        }
                )
                areaCaptures
    in
    plainImages ++ capturedAreas


turnImageUrl : String -> Int -> String
turnImageUrl turnId index =
    baseUrl ++ "/turn/" ++ turnId ++ "/image/" ++ String.fromInt index


turnEvent : Decoder Model.AgentTurnEvent
turnEvent =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\type_ ->
                case type_ of
                    "chunk" ->
                        Decode.field "data"
                            (Decode.succeed (\sessionId chunk -> Model.AgentTurnChunk { sessionId = sessionId, chunk = chunk })
                                |> required "sessionId" Decode.string
                                |> required "chunk" Decode.string
                            )

                    "done" ->
                        Decode.field "data"
                            (Decode.succeed (\sessionId turnId -> Model.AgentTurnDone { sessionId = sessionId, turnId = turnId })
                                |> required "sessionId" Decode.string
                                |> required "turnId" Decode.string
                            )

                    "heartbeat" ->
                        Decode.succeed Model.AgentTurnHeartbeat

                    "error" ->
                        Decode.field "data"
                            (Decode.succeed (\sessionId message -> Model.AgentTurnError { sessionId = sessionId, message = message })
                                |> required "sessionId" Decode.string
                                |> required "message" Decode.string
                            )

                    _ ->
                        Decode.fail ("Unknown agent turn event: " ++ type_)
            )


archive : String -> Flow s (Result Http.Error Model.AgentSessionView)
archive sessionId =
    postSessionView "/archive" (sessionIdBody sessionId)


compact : String -> Flow s (Result Http.Error Model.AgentSessionView)
compact sessionId =
    postSessionView "/compact" (sessionIdBody sessionId)


stop : String -> Flow s (Result Http.Error Model.AgentSessionView)
stop sessionId =
    postSessionView "/stop" (sessionIdBody sessionId)


delete_ : String -> Flow s (Result Http.Error ())
delete_ sessionId =
    Flow.lift <|
        Http.post
            { url = baseUrl ++ "/delete"
            , body = Http.jsonBody (sessionIdBody sessionId)
            , expect = Http.expectWhatever identity
            }
