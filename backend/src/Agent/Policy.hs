{-# LANGUAGE OverloadedStrings #-}

module Agent.Policy (
    embeddedAgentModeMarker,
    agentOutputPathPatterns,
    isAgentOutputPath,
    appliedProjectId,
    appliedStepId,
    renderEmbeddedBootstrapPrompt,
    promptWithEvaluationFailure,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

embeddedAgentModeMarker :: Text
embeddedAgentModeMarker = "POINTY_AGENT_MODE=embedded"

agentOutputPathPatterns :: [Text]
agentOutputPathPatterns =
    [ "projects/<numeric-id>.nix"
    , "steps/<numeric-id>.nix"
    , "srcFiles/<numeric-step-id>/<relative-path>"
    ]

isAgentOutputPath :: Text -> Bool
isAgentOutputPath path =
    isJust (appliedProjectId path) || isJust (appliedStepId path)

renderEmbeddedBootstrapPrompt :: Text -> Text
renderEmbeddedBootstrapPrompt configuredPrompt =
    T.unlines
        ( [ embeddedAgentModeMarker
          , "The backend applies changes only to these path patterns:"
          ]
            ++ map ("- " <>) agentOutputPathPatterns
            ++ [ "Do not edit outside this allowlist; those changes will be discarded."
               , "The backend refuses a changeset that introduces evaluation failures in the projects or in the steps it changes; failures that already exist on the target branch do not block it. When it refuses one, the failures are sent to you with the next message."
               , "Before ending a turn that edits steps/<id>.nix or projects/<id>.nix, run `nix-instantiate --parse <file>` on each edited file and fix any error it reports."
               , "Follow the Embedded agents only section in AGENTS.md."
               , "Use only these entity-reference formats in every response:"
               , "- Step: step <id>. This is the entire step reference; never include the step name."
               , "- Project: @[project:<id>] <name>. Quote the name when it contains spaces."
               , "Keep entity references as ordinary plain text: no inline code, no Markdown links, and no parentheses around an id."
               , ""
               , configuredPrompt
               ]
        )

promptWithEvaluationFailure :: Text -> Text -> Text
promptWithEvaluationFailure failures prompt =
    T.unlines
        ( "The backend refused to apply your last changeset because it introduces evaluation failures:"
            : map ("- " <>) (filter (not . T.null) (T.lines failures))
            ++ ["Fix these problems so the changeset can be applied.", "", "User message:"]
        )
        <> prompt

appliedProjectId :: Text -> Maybe Int
appliedProjectId path =
    case T.splitOn "/" path of
        ["projects", file] -> numberedNixId file
        _ -> Nothing

appliedStepId :: Text -> Maybe Int
appliedStepId path =
    case T.splitOn "/" path of
        ["steps", file] -> numberedNixId file
        "srcFiles" : stepId : _ : _ -> decimalId stepId
        _ -> Nothing

numberedNixId :: Text -> Maybe Int
numberedNixId file = T.stripSuffix ".nix" file >>= decimalId

decimalId :: Text -> Maybe Int
decimalId text_ =
    case TR.decimal text_ of
        Right (n, rest) | T.null rest -> Just n
        _ -> Nothing
