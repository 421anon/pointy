module Channels exposing (agentTurn, stepStatus)

import Flow.Channel as Channel exposing (Channel)
import Json.Decode
import Ports


stepStatus : Int -> Maybe String -> Channel s Json.Decode.Value
stepStatus projectId commit =
    Channel.connect Ports.stepStatusIn (\_ -> Ports.openStepStatusStream { projectId = projectId, commit = commit })



agentTurn : String -> Channel s Json.Decode.Value
agentTurn turnId =
    Channel.connect Ports.agentTurnIn (\_ -> Ports.openAgentTurnStream { turnId = turnId })