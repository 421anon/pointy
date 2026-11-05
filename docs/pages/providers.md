---
description: "Connect AI providers for the agent's coder and answerer models, message routing, chat titles, and search."
---

# Providers

## AI providers and the chat models

The backend has no model access by default. Each `[[providers]]` entry defines a named API endpoint. `base-url` is the API root; the embedding client appends `/embeddings`, and the judge and router clients `/systemone`. `api-key-env` names the environment variable holding the bearer token, so all keys come from the backend's environment (for example through `services.pointy-backend.agentEnvFile`). `timeout-seconds` (default `10`) bounds the embedding, judge, and router HTTP calls.

`[models.coder]` selects the model the agent panel runs on, and the optional `[models.answerer]` a second model that answers questions. Both tables take the same keys:

- `provider` must name an entry in `[[providers]]`, or startup fails with `Failed to parse config: [models.coder] names provider "…", which is not in [providers]` (or `[models.answerer]`).
- `protocol` is one of `openai-completions`, `openai-responses`, `anthropic-messages`, or `google-generative-ai`.
- `model` is the provider's model identifier.
- `thinking` is optional and becomes the level suffix of `--model <provider>/<model>:<level>`.
- `hosted-tools` is optional: Neuralwatt hosted tools, such as `nw_web_search`, that the model may call on every request; the gateway runs them inside the response. It needs `protocol = "openai-completions"`, or startup fails, and a model whose `/v1/models` entry reports `hosted_tools = true`.
- `[models.<role>.pi]` supplies Pi metadata: `name`, `contextWindow`, `maxTokens`, `input`, `reasoning`, `thinkingLevelMap`, `disabledThinkingLevels`, `cost` (all four prices required), `compat` flags such as `supportsStore`, `supportsDeveloperRole`, `supportsReasoningEffort`, and `requiresReasoningContentOnAssistantMessages`, and `headers` sent with every request. Pi expands `$NAME` in header values from the runner's environment, which holds the chat's id in `POINTY_AGENT_SESSION_ID`; Neuralwatt keeps a conversation's prompt cache on one server while `X-NW-Conversation-ID` stays the same.

The backend rewrites Pi's `models.json` and `settings.json` from these tables before a turn, and passes `--model` itself. Without `[models.coder]`, the agent starts with no chat model. A provider referenced by `[models.coder]`, `[models.answerer]`, `[models.router]`, `[search.embedding]`, or `[search.judge]` whose key is not set at startup produces `Warning: … reads its key from …, which is not set`; turns or searches that use it then fail.

## Routing messages to the answerer

`[models.answerer]` needs `[models.coder]`, and `[models.answerer]` and `[models.router]` must be set together; otherwise startup fails. `[models.router]` names a `provider`, `protocol = "systemone"`, and the decision `model`. With all three set, the router reads each message the user writes, together with the earlier messages, the model each went to, and its reply, and hands the message to the answerer when it needs a heavy-weight specialist and to the coder otherwise. It also sees how many images and area selections came with each message and which step output file was open, with that step's definition and source file names. Turns the backend starts itself, such as automatic fixes, go to the coder, and so do chat titles and compaction. If routing fails, the coder takes the turn. The agent panel labels each reply **Coder** or **Answerer**.

Both agents share one conversation and the same tools, and each keeps its own model, thinking level, and system prompt, so each model's prompt cache stays valid for the whole conversation.

## Model selection and chat titles

The chat models come from `[models.coder]` and `[models.answerer]`, not from `runner-args`. The backend writes Pi's `models.json` and `settings.json` from `[[providers]]` and those tables before a turn and passes `--model <provider>/<model>[:<thinking>]` for the agent that takes it. A `--model`, `--thinking`, or `--provider` in `runner-args` is discarded.

`title-prompt` controls automatic naming of unnamed chats. A blank value disables it. Title generation is a separate short Pi request on the coder model with `--no-session --no-tools` and a 60-second timeout; it accepts at most 60 characters and 10 words from the last non-empty output line, and does not overwrite a user-chosen name.

## Semantic search

Step and chat search are lexical by default. Adding `[search.embedding]` enables the **Related** section in the step search box and in the chat history search; `[search.judge]` then filters the embedding candidates.

- `[search]`: `limit` (default `10`) is the number of results; `judge-candidates` (default `24`, at most `64`) is how many similarity hits reach the judge.
- `[search.embedding]`: `provider` names a `[[providers]]` entry; `protocol` must be `openai-embeddings`; `model` is the embedding model. `dimensions` is optional and sent to the provider. `query-template` and `chat-query-template` (both default `{query}`) wrap step and chat queries, and `document-template` (default `{document}`) wraps step and chat text. `batch-size` (default `128`) sets embedding request size, and `min-similarity` (default `0.4`) is the cosine cutoff.
- `[search.judge]`: `provider` names a `[[providers]]` entry; `protocol` must be `systemone`; `model` is the judge model. `instructions` is the per-step question and `chat-instructions` the per-chat question; both may use `{id}` and `{query}`. `min-relevance` (default `0.5`) is the cutoff.

A provider named by any of these sections must exist in `[[providers]]`, and `judge-candidates` above `64` fails startup. Step embeddings are cached under `/home/backend/.local/state/pointy/embeddings/` and chat embeddings under `/home/backend/.local/state/pointy/chat-embeddings/`, keyed by model, dimensions, and document template. The in-memory step index is per repository commit; the chat index is rebuilt when chats or their turns change. Both caches are safe to delete; they are rebuilt by calling the embedding provider.
