---
description: "Connect AI providers for the agent's chat model, chat titles, and step search."
---

# Providers

## AI providers and the chat model

The backend has no model access by default. Each `[[providers]]` entry defines a named API endpoint. `base-url` is the API root; the embedding client appends `/embeddings` and the judge client `/systemone`. `api-key-env` names the environment variable holding the bearer token, so all keys come from the backend's environment (for example through `services.pointy-backend.agentEnvFile`). `timeout-seconds` (default `10`) bounds the embedding and judge HTTP calls.

`[models.chat]` selects the model used by the agent panel:

- `provider` must name an entry in `[[providers]]`, or startup fails with `Failed to parse config: [models.chat] names provider "…", which is not in [providers]`.
- `protocol` is one of `openai-completions`, `openai-responses`, `anthropic-messages`, or `google-generative-ai`.
- `model` is the provider's model identifier.
- `thinking` is optional and becomes the level suffix of `--model <provider>/<model>:<level>`.
- `[models.chat.pi]` supplies Pi metadata: `name`, `contextWindow`, `maxTokens`, `input`, `reasoning`, `thinkingLevelMap`, `disabledThinkingLevels`, `cost` (all four prices required), and `compat` flags such as `supportsStore`, `supportsDeveloperRole`, `supportsReasoningEffort`, and `requiresReasoningContentOnAssistantMessages`.

The backend rewrites Pi's `models.json` and `settings.json` from these tables before a turn, and passes `--model` itself. Without `[models.chat]`, the agent starts with no chat model. A provider referenced by `[models.chat]`, `[search.embedding]`, or `[search.judge]` whose key is not set at startup produces `Warning: … reads its key from …, which is not set`; turns or searches that use it then fail.

## Model selection and chat titles

The chat model comes from `[models.chat]`, not from `runner-args`. The backend writes Pi's `models.json` and `settings.json` from `[[providers]]` and `[models.chat]` before a turn and passes `--model <provider>/<model>[:<thinking>]`. A `--model`, `--thinking`, or `--provider` in `runner-args` is discarded. Without `[models.chat]` the agent has no chat model.

`title-prompt` controls automatic naming of unnamed chats. A blank value disables it. Title generation is a separate short Pi request with `--no-session --no-tools` and a 60-second timeout; it accepts at most 60 characters and 10 words from the last non-empty output line, and does not overwrite a user-chosen name.

## Semantic search

Step and chat search are lexical by default. Adding `[search.embedding]` enables the **Related** section in the step search box and in the chat history search; `[search.judge]` then filters the embedding candidates.

- `[search]`: `limit` (default `10`) is the number of results; `judge-candidates` (default `24`, at most `64`) is how many similarity hits reach the judge.
- `[search.embedding]`: `provider` names a `[[providers]]` entry; `protocol` must be `openai-embeddings`; `model` is the embedding model. `dimensions` is optional and sent to the provider. `query-template` and `chat-query-template` (both default `{query}`) wrap step and chat queries, and `document-template` (default `{document}`) wraps step and chat text. `batch-size` (default `128`) sets embedding request size, and `min-similarity` (default `0.4`) is the cosine cutoff.
- `[search.judge]`: `provider` names a `[[providers]]` entry; `protocol` must be `systemone`; `model` is the judge model. `instructions` is the per-step question and `chat-instructions` the per-chat question; both may use `{id}` and `{query}`. `min-relevance` (default `0.5`) is the cutoff.

A provider named by any of these sections must exist in `[[providers]]`, and `judge-candidates` above `64` fails startup. Step embeddings are cached under `/home/backend/.local/state/pointy/embeddings/` and chat embeddings under `/home/backend/.local/state/pointy/chat-embeddings/`, keyed by model, dimensions, and document template. The in-memory step index is per repository commit; the chat index is rebuilt when chats or their turns change. Both caches are safe to delete; they are rebuilt by calling the embedding provider.
