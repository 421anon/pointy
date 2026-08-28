{-# LANGUAGE OverloadedStrings #-}

module Agent.Policy (
    embeddedAgentModeMarker,
    agentOutputPathPatterns,
    isAgentOutputPath,
    appliedProjectId,
    appliedStepId,
    renderEmbeddedBootstrapPrompt,
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
               , "Follow the Embedded agents only section in AGENTS.md."
               , "When a known project or step has a name, refer to it in replies as @[step:<id>] <name> or @[project:<id>] <name> (e.g. @[step:201] script); quote multi-word names (e.g. @[step:156] \"Oligopool QC alignment\"). Never use a bare name without its id."
               , "Keep the whole mention as ordinary plain text: no inline code, no Markdown link, no parentheses around the id."
               , ""
               , configuredPrompt
               ]
        )

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
