module Channels exposing (stepStatus)

import Flow.Channel as Channel exposing (Channel)
import Json.Decode
import Ports


stepStatus : Int -> Maybe String -> Channel s Json.Decode.Value
stepStatus projectId commit =
    Channel.connect Ports.stepStatusIn (\_ -> Ports.openStepStatusStream { projectId = projectId, commit = commit })
