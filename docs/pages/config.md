---
description: "The backend's config.toml and environment variables."
---

# Config

## Backend configuration

The backend reads `/home/backend/config.toml` by default; set `POINTY_CONFIG_PATH` when the deployment keeps this instance-specific configuration elsewhere. A config file that does not parse crashes startup with `Failed to parse config: …`, and runtime requests that re-read it fail until it is fixed.

The current configuration format is:

```toml
[user-repo]
url = "git@example.com:org/user-repo.git"
keyfile = "/path/to/deploy-key"
branch = "main"

[slurm]
# Optional. partition defaults to ""; omit account and time-limit, or leave them empty, to use Slurm defaults.
partition = ""
account = ""
time-limit = ""
extra = []
# "scheduler" passes --mem and --cpus-per-task; "metadata-only" records the requirements in the job environment only.
enforcement = "scheduler"

[agent]
# A turn uses two sandboxes. sbox-command starts both; sbox-args configure the
# tool sandbox. runner-command runs Pi in the separate runner sandbox.
sbox-command = "sbox"
sbox-args = ["--bind", "{home}", "{home}"]
runner-command = "pi"
runner-args = []
timeout-seconds = 1800
output-limit-bytes = 1048576
# Parsed for configuration compatibility; automatic age-based pruning is not currently implemented.
session-retention-days = 7
title-prompt = "Name the chat that opens with the request in this message. Reply with the title only: five words at most, no quotes, no punctuation, no explanation."

# AI providers. Each entry names the environment variable that holds its API key,
# so keys never live in this file. With no provider tables there is no model access.
[[providers]]
name = "neuralwatt"
base-url = "https://api.neuralwatt.com/v1"
api-key-env = "NEURALWATT_API_KEY"
timeout-seconds = 60

# The chat model the agent panel runs on. The backend writes it into Pi's
# models.json and settings.json before a turn and passes --model explicitly.
[models.chat]
provider = "neuralwatt"
protocol = "openai-completions"
model = "deepseek-v4.1-flash-speed"
thinking = "high"

[models.chat.pi]
name = "DeepSeek V4.1 Flash (Speed)"
contextWindow = 1000000
maxTokens = 384000
input = ["text", "image"]
reasoning = true
thinkingLevelMap = { minimal = "low", low = "low", medium = "high", high = "high", xhigh = "xhigh", max = "max" }
disabledThinkingLevels = ["off"]
# Prices per million tokens; all four are required and must be written as decimals.
cost = { input = 0.14, output = 0.28, cacheRead = 0.028, cacheWrite = 0.0 }
compat = { supportsStore = false, supportsDeveloperRole = false, supportsReasoningEffort = true, requiresReasoningContentOnAssistantMessages = true }

# Optional semantic step and chat search. Without [search.embedding] the search
# boxes stay lexical; with [search.judge] the judge filters the embedding candidates.
# [search]
# limit = 10
# judge-candidates = 24
#
# [search.embedding]
# provider = "neuralwatt"
# protocol = "openai-embeddings"
# model = "qwen3-embedding-8b"
# query-template = "Instruct: Given a search typed into a research notebook, retrieve the analysis steps it refers to\nQuery: {query}"
# chat-query-template = "Instruct: Given a search typed into a research notebook's chat history, retrieve the conversations it refers to\nQuery: {query}"
# document-template = "{document}"
# batch-size = 128
# min-similarity = 0.4
#
# [search.judge]
# provider = "neuralwatt"
# protocol = "systemone"
# model = "clef-flash"
# instructions = "Is step {id} something the person searching for \"{query}\" wants to find?"
# chat-instructions = "Is chat {id} something the person searching for \"{query}\" wants to find?"
# min-relevance = 0.5
```

These settings tell the backend:

- which Git remote to use for the user repository
- which SSH key to use for Git operations
- which branch contains the live Pointy state

The backend re-reads `config.toml` for every repository transaction, Slurm submission, and `/user-repo-info` request, so changes to the branch, keyfile, and `[slurm]` settings apply without a restart. The `[agent]`, `[[providers]]`, `[models.chat]`, and `[search]` sections are re-read when they are used, so changes apply to the next turn or request. A changed repository `url` applies only at startup: the backend moves `/home/backend/user-repo.git` to `/home/backend/user-repo.git-replaced` and clones the new remote fresh.

The optional `[slurm]` section controls how the backend submits build jobs:

- `partition`: passed as `--partition` when non-empty.
- `account`: passed as `--account` when non-empty.
- `time-limit`: passed as `--time` when non-empty.
- `extra`: appends raw `sbatch` flags after the computed flags; a later duplicate flag wins.
- `enforcement`: sets the Slurm resource-enforcement mode. `"scheduler"` (the default) passes `--mem` and `--cpus-per-task` to `sbatch`. `"metadata-only"` omits those two flags but still exports the requirements as `POINTY_REQ_CPU`, `POINTY_REQ_RAM`, `POINTY_REQ_IOR`, and `POINTY_REQ_IOW` in the job environment, like `"scheduler"` does. I/O requirements are never enforced, and any other value behaves as `"scheduler"`.

There is no `[nix-evaluator]` table any more; a table with that name is ignored. Evaluation memory is not configurable: each `nix repl` session is released when it has been idle above a fixed 2 GiB RSS threshold, and the next request starts a fresh session. There is no warm-up and no sharded cache.

The `[agent]` section configures the embedded agent runner. When this section is absent, the backend uses the defaults shown above. Runner secrets are provisioned through the NixOS module, not through `config.toml`; see [NixOS module options](nixos.md#nixos-module-options). For the user-facing agent workflow, see the [AI Agent guide](agent.md).

### Environment variables

| Variable | Set by | Effect |
| --- | --- | --- |
| `POINTY_CONFIG_PATH` | admin (optional) | Config file path instead of `/home/backend/config.toml`. |
| `POINTY_LISTEN_HOST` | `services.pointy-backend.listenHost` | Address the backend HTTP listener binds; default `127.0.0.1`. |
| `POINTY_LISTEN_PORT` | `services.pointy-backend.listenPort` | Backend listener port; default `8081`, also used by the supplied vhost and job hook. |
| `NIX_REMOTE` | `services.pointy-backend.storeUrl` | Store used by every backend `nix` call; read once at startup. |
| `POINTY_REALISE_COMMAND` | `services.pointy-backend.slurm.realiseCommand` | Executable each Slurm build job runs as `<command> <derivation>`. |
| `POINTY_SCRATCH_DIR` | `services.pointy-backend.scratchDirectory` | Enables `/scratch*` and gives the agent tool sandbox a read-only bind. |
| `POINTY_UPLOAD_DIR` | `services.pointy-backend.uploadDirectory` | Roots upload staging; when unset, the canonical system temporary directory. |
| `POINTY_COLD_STORE_SOCKET` | `services.pointy-backend.coldStorage.ingestSocket` | Ingest socket passed to the cold-storage command for status and prefetch. |
| `POINTY_AGENT_MEMORY_MAX` | `services.pointy-backend.agentMemoryMax` | Memory limit shared by the agent sandboxes; default `24G`. |
| `POINTY_TOKEN_AUDIENCE` | `services.pointy-host.tokenAuth.audience` | Required `aud` claim of session tokens. |
| `POINTY_TOKEN_PUBLIC_KEY` | `services.pointy-host.tokenAuth.publicKeyFile` | Ed25519 public key used to verify session tokens. |
| `POINTY_LINKED_DATA_BASE` | `services.pointy-host.linkedDataBase` | Origin used for `@base` in artifact Turtle. |
| `SLURM_CONF` | `services.pointy-backend.slurm.confFile` | Slurm client configuration. |
| `HOME` | the `backend` account | Root of every state path under `/home/backend`. |
| `NIX_CACHE_HOME` | the development VM | `/var/cache/nix`, shared from the host. |
