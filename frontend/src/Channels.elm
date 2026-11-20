module Channels exposing (agentTurns, backendConnection, clusterStatus, ingestJobs, stepStatus)

import Flow exposing (ChannelSpec)
import Json.Decode
import Ports


stepStatus : ChannelSpec String
stepStatus =
    backendEvent "step-status"


clusterStatus : ChannelSpec String
clusterStatus =
    backendEvent "cluster-status"


ingestJobs : ChannelSpec String
ingestJobs =
    backendEvent "ingest-jobs"


backendConnection : ChannelSpec Bool
backendConnection =
    Flow.listen Ports.backendConnectionIn


agentTurns : ChannelSpec Json.Decode.Value
agentTurns =
    Flow.listen Ports.agentTurnIn


backendEvent : String -> ChannelSpec String
backendEvent name =
    Flow.connect
        (\toMsg ->
            Ports.backendEventIn
                (\{ event, data } ->
                    if event == name then
                        toMsg data

                    else
                        Nothing
                )
        )
        (\_ -> Ports.openBackendEvents {})
