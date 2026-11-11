module Api.AgentPaths exposing (baseUrl, shownImageUrl)


baseUrl : String
baseUrl =
    "/backend/agent"


shownImageUrl : String -> String -> String
shownImageUrl turnId file =
    baseUrl ++ "/turn/" ++ turnId ++ "/shown-image/" ++ file
