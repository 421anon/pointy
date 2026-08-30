module Channels exposing (agentTurn, clusterStatus, stepStatus)

import Flow.Channel as Channel exposing (Channel)
import Json.Decode
import Ports


stepStatus : Channel s Json.Decode.Value
stepStatus =
    Channel.connect Ports.stepStatusIn (\_ -> Ports.openStepStatusStream {})


agentTurn : String -> Channel s Json.Decode.Value
agentTurn turnId =
    Channel.connect Ports.agentTurnIn (\_ -> Ports.openAgentTurnStream { turnId = turnId })


clusterStatus : Channel s Json.Decode.Value
clusterStatus =
    Channel.connect Ports.clusterStatusIn (\_ -> Ports.openClusterStatusStream {})
