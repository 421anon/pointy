{-# LANGUAGE OverloadedStrings #-}

module Agent.Policy (
    embeddedAgentModeMarker,
    agentOutputPathPatterns,
    isAgentOutputPath,
    appliedProjectId,
    appliedStepId,
    renderEmbeddedBootstrapPrompt,
    renderCurrentProject,
    promptWithEvaluationFailure,
    promptWithApplyConflict,
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
               , "A project file projects/<id>.nix holds `name`, exactly one of `preset` or `templates`, and `children`: a list of entries `{ step = { hidden = false; id = <step-id>; sortKey = null; }; }` for steps and `{ project = { hidden = false; id = <project-id>; sortKey = null; }; }` for subprojects. The `hidden` and `sortKey` of an entry set how that child shows inside this project. A step or project can be listed in several parents, but the project graph must stay a DAG: never link a project under one of its own descendants."
               , "projects/0.nix is the root project, Home. The root is never a child of another project and cannot be deleted. A new project file only shows up once an entry for it is added to the `children` of a parent project; to place it at the top level, add the entry to projects/0.nix."
               , "The backend refuses a changeset that introduces evaluation failures in the projects or in the steps it changes; failures that already exist on the target branch do not block it. When it refuses one, the failures are sent to you with the next message."
               , "Before ending a turn that edits steps/<id>.nix or projects/<id>.nix, run `nix-instantiate --parse <file>` on each edited file and fix any error it reports."
               , "Every bash command without an explicit `timeout` is stopped after 120 seconds and returns the output it produced so far. Set `timeout` in seconds when you expect a command to run longer, and expect a search through /nix/store, /data or the git history to be stopped at that limit: narrow it with a path, -maxdepth, or a name pattern instead of scanning everything."
               , "Follow the Embedded agents only section in AGENTS.md."
               , "Use only these entity-reference formats in every response:"
               , "- Step: step <id>. This is the entire step reference; never include the step name."
               , "- Project: @[project:<id>] <name>. Quote the name when it contains spaces."
               , "Keep entity references as ordinary plain text: no inline code, no Markdown links, and no parentheses around an id."
               , ""
               , configuredPrompt
               ]
        )

renderCurrentProject :: Int -> Text
renderCurrentProject projectId =
    T.intercalate
        "\n"
        [ "The currently open project is:"
        , "id: " <> T.pack (show projectId)
        , "file: projects/" <> T.pack (show projectId) <> ".nix"
        ]

promptWithEvaluationFailure :: Text -> Text -> Text
promptWithEvaluationFailure failures prompt =
    T.unlines
        ( "The backend refused to apply your last changeset because it introduces evaluation failures:"
            : map ("- " <>) (filter (not . T.null) (T.lines failures))
            ++ ["Fix these problems so the changeset can be applied.", "", "User message:"]
        )
        <> prompt

promptWithApplyConflict :: Text -> FilePath -> Text -> Text -> Text
promptWithApplyConflict target applyWorktree conflictSummary prompt =
    T.unlines
        ( [ "Your changeset cannot be applied: `" <> target <> "` gained commits that conflict with it."
          , "The apply worktree " <> T.pack applyWorktree <> " holds the latest `" <> target <> "` with your changeset merged in. Git state:"
          ]
            ++ map ("  " <>) (filter (not . T.null) (T.lines conflictSummary))
            ++ [ "Resolve the conflict by editing files in the apply worktree only; its git metadata is read-only, and the backend stages and commits the result when this turn ends."
               , "The result must keep every change from `" <> target <> "` and every change from your changeset."
               , "When both sides added the same steps/<id>.nix, projects/<id>.nix or srcFiles/<id>/, they are different records: keep the `" <> target <> "` version at that id, move yours to an id no file in the apply worktree uses, and update every reference to it in the files of your changeset, including path strings such as \"<id>/file\"."
               , "Remove every conflict marker. Do not edit the session worktree for this; a commit there discards the apply worktree."
               , ""
               , "User message:"
               ]
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
