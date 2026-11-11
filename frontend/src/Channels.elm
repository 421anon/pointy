module Channels exposing (agentTurns, clusterStatus, ingestJobs, stepStatus)

import Flow exposing (ChannelSpec)
import Json.Decode
import Ports


stepStatus : ChannelSpec Json.Decode.Value
stepStatus =
    Flow.connect Ports.stepStatusIn (\_ -> Ports.openStepStatusStream {})


agentTurns : ChannelSpec Json.Decode.Value
agentTurns =
    Flow.listen Ports.agentTurnIn


clusterStatus : ChannelSpec Json.Decode.Value
clusterStatus =
    Flow.connect Ports.clusterStatusIn (\_ -> Ports.openClusterStatusStream {})


ingestJobs : ChannelSpec Json.Decode.Value
ingestJobs =
    Flow.connect Ports.ingestJobsIn (\_ -> Ports.openIngestStream {})
