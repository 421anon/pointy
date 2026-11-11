---
description: "Drive the embedded agent from your own client."
---

# Agent API

## Embedded agent integration

The `/agent/*` routes are intentionally excluded from the public OpenAPI operation list. They serve the built-in chat UI and are not a separately versioned public API. If you use them in a custom interface, keep it on a matching backend version and handle the chat's states, not just its replies. The [AI Agent guide](agent.md) explains how researchers use the chat.

All POST bodies below are JSON unless the body is shown as “none”. `sessionId` and `turnId` are opaque strings, unlike numeric project/step ids.

| Method and route | Body | Result |
| --- | --- | --- |
| `POST /agent/session` | none | New session view with status `open`. |
| `GET /agent/sessions` | — | Array of session **summaries** `{session, title, turnCount, hasCommits}`, newest first. |
| `GET /agent/session/<id>` | — | One session view, including turns and full turn logs. |
| `POST /agent/turn` | `{ "sessionId": "…", "prompt": "…", "currentProjectId": 2, "images": […], "areaCaptures": […], "viewedArtifact": {…}, "autoApply": true, "clientId": "…" }` | Started turn; execution continues asynchronously. Everything but `sessionId` and `prompt` is optional: `currentProjectId` tells the agent which folder page is open, `autoApply` defaults to true, and `clientId` tags the client that started the turn. |
| `POST /agent/steer` | `{ "sessionId": "…", "prompt": "…" }` | Empty 204 after accepted steering or question answer. |
| `POST /agent/stop` | `{ "sessionId": "…" }` | Session view after requesting termination; also succeeds when idle. |
| `GET /agent/turn/<id>/stream` | — | Turn-log SSE stream. |
| `GET /agent/turn/<id>/image/<index>` | — | Raw image attached to the turn, by zero-based index; 404 `image not found`. |
| `GET /agent/turn/<id>/shown-image/<file>` | — | Raw image the agent showed during the turn, named by an `[image]` log line; 404 `image not found`. |
| `POST /agent/apply` | `{ "sessionId": "…", "autoApply": true, "clientId": "…" }` | Applies the changes that are not applied yet and returns the session view; `clientId` is optional. A refused changeset leaves `prepare_conflict` or `evaluation_failed` and starts a corrective turn. |
| `POST /agent/auto-apply` | `{ "clientId": "…", "autoApply": false }` | Empty 204; changes the auto-apply choice of running turns this client started. |
| `POST /agent/compact` | `{ "sessionId": "…" }` | Session view after summarising the conversation to free context; a failure is recorded in the latest turn log. |
| `POST /agent/discard` | `{ "sessionId": "…" }` | Session reset to the target branch; chat remains open. |
| `POST /agent/archive` | `{ "sessionId": "…" }` | Closed session retained in history. |
| `POST /agent/rename` | `{ "sessionId": "…", "name": "…" }` | Renamed session view; whitespace is collapsed and the name is capped at 80 characters. |
| `POST /agent/delete` | `{ "sessionId": "…" }` | Empty response; permanently removes a session. An active runner must be stopped first. |
| `GET /agent/usage` | — | Session counts: `totalSessions`, `openSessions`, `runningSessions`, `appliedSessions`, `discardedSessions`. |
| `POST /agent/chat-search` | `{ "query": "…", "exclude": ["<sessionId>"] }` | `{ "results": [ { "session_id", "similarity", "relevance" } ] }`, most relevant first: chats whose title and conversation match the query semantically. `exclude` skips chats your interface already shows; `relevance` is null without a judge. Without [semantic search](providers.md#semantic-search), or for a blank query, `results` is empty. |

A session view contains `session`, `gitState`, and `turns`. Relevant session fields are `sessionId`, `sessionName`, `status`, `baseCommit`, `activeTurnId`, `preparedApply`, `lastError`, `agentCurrentProjectId`, `autoApply`, and `autoApplyClient`. Git state includes `headCommit`, `commitLog`, `branchDiff`, and `hasAgentCommits`. A turn has `turnId`, `turnSessionId`, `turnPrompt`, `turnImages`, `turnAreaCaptures`, `turnAutomatic`, `turnStatus`, `turnExitCode`, `turnStartedAt`, `turnFinishedAt`, `turnLogPath`, and `turnLog`. Finished turn statuses are `succeeded`, `failed`, and `stopped`; an automatic turn is one the backend started to fix a refused changeset. Session statuses include `open`, `running`, `prepare_conflict`, `evaluation_failed`, and `archived`.

`images` in a turn request are `{ "mimeType": "image/png", "data": "<base64>" }` objects; PNG, JPEG, GIF, and WebP are accepted. `areaCaptures` describe a selected area of the document embedded in the Pointy page, not of Pointy itself: `overview` and `selection` images in the same shape, `documentSize` (`width`, `height`), `selectionRect` (`x`, `y`, `width`, `height`), and the `artifact` it came from (`projectPath`, `stepId`, `commit`, `path`). The backend stores the plain images first, then each capture's overview and selection, and numbers them from zero; `turnImages` lists the MIME types in that order, and each `turnAreaCaptures` entry records `overviewImage`, `selectionImage`, `documentSize`, `selectionRect`, and `artifact`. `/agent/turn/<id>/image/<index>` serves any of them by that zero-based index. `viewedArtifact` (`stepId`, `commit`, `path`) tells the agent which output file the user is looking at.

Errors use plain-text codes: 400 `empty_session_name` and `empty_prompt`; 404 `session_not_found`, and `turn not found` for a missing turn stream; 409 `session_applied`, `session_discarded`, `session_archived`, `runner_active`, `runner_not_active`, `runner_stopping`, `steering_failed`, `compaction_active`, and `nothing_to_compact`; other failures are 500 with the raw text.

One runner may be active per session. A second turn conflicts with 409 `runner_active`; steer the existing runner instead. Separate sessions can run concurrently. Steering can return 409 with `runner_not_active`, `runner_stopping`, or `steering_failed`; an empty prompt is 400 `empty_prompt`, missing sessions are 404 `session_not_found`, and archived, applied, or discarded sessions refuse new turns with the corresponding 409 codes. Missing sessions that fail to load are 404; other failures are 500 with the raw text. Re-read session state after stop or an error rather than guessing the next state from the button that was clicked.

### Turn streaming and questions

Turn SSE events are `chunk` with `{ "turnId": "…", "chunk": "…" }`, `heartbeat` with the turn id after about five idle seconds, and `done` with the turn id. `done` closes the stream; it does not mean the turn succeeded. Fetch the session for its final status and changeset.

Every connection replays the log from the beginning. There is no offset query parameter or `Last-Event-ID` resume support. Replace/reset your accumulated log on reconnect, or discard the already-delivered prefix as the built-in client does. Otherwise a reconnect duplicates the conversation.

Chunks contain the backend's labeled log lines, not just assistant prose. In particular, `[question]` is followed by JSON `{ "multi": true, "options": ["…"] }`, and `[steering]` carries a JSON string and clears the pending question. `[image]` carries JSON `{ "turnId": "…", "file": "0.png" }` for an image the agent showed the user at that point of its reply; fetch it from `/agent/turn/<turnId>/shown-image/<file>`. Preserve partial lines between chunks. Answer through `/agent/steer`: a single selection is its 1-based option number; a multi-selection is a comma- or space-separated list of numbers; free text is also accepted. Only one question is pending at a time.

### Applying changes safely

An agent turn's changeset is applied to the session's target branch when the turn ends if its `autoApply` is true, or when you call `/agent/apply`. Before applying, the backend merges the agent branch into the current target head and checks the result: a changeset that would introduce evaluation failures not already on the shared branch is refused, and a merge conflict leaves a candidate for the agent to resolve. A refusal is recorded as `evaluation_failed` or `prepare_conflict` with the reason in `lastError`, and `/agent/apply`, or the end of an auto-applied turn, starts a corrective turn carrying that reason (at most two automatic turns run in a row at turn end).

The apply response is the session view; re-read `status`, `lastError`, `preparedApply`, and `turns` rather than assuming the changeset landed. `preparedApply` remains visible while a conflicted merge waits, and a later apply reuses it if the heads are still current. The backend's path allowlist and review checks still apply to a custom client: only changes under `projects/<id>.nix`, `steps/<id>.nix`, and `srcFiles/<id>/` are applied, and changes to reviewed steps and their source files are dropped from the changeset with a note in a lifecycle turn before the rest is checked and applied. Reload affected records and request fresh statuses after a successful apply.
