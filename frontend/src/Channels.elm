module Channels exposing (agentTurns, clusterStatus, stepStatus)

import Flow.Channel as Channel exposing (Channel)
import Json.Decode
import Ports


stepStatus : Channel s Json.Decode.Value
stepStatus =
    Channel.connect Ports.stepStatusIn (\_ -> Ports.openStepStatusStream {})


agentTurns : Channel s Json.Decode.Value
agentTurns =
    Channel.join Ports.agentTurnIn


clusterStatus : Channel s Json.Decode.Value
clusterStatus =
    Channel.connect Ports.clusterStatusIn (\_ -> Ports.openClusterStatusStream {})
