---
description: "How the agent runner isolates its working copies, tools, and credentials."
---

# Sandbox

## Agent runner configuration

The embedded agent runner is configured through the `[agent]` table in `config.toml`; its models come from `[[providers]]`, `[models.coder]`, and `[models.answerer]` (see [Providers](providers.md)). Every field is optional; the backend falls back to the defaults shown in the [configuration reference](config.md#backend-configuration).

### Working copies and permitted files

Each chat has a separate Git working copy, or *worktree*. The backend commits the agent's edits there after each request; they reach the shared branch only when a user applies them.

Before a request, Pointy fetches the configured target branch. A failed fetch does not stop the turn; it is logged as `Warning: could not fetch latest repo state: …`. If the chat has no local commits, its working copy is cleaned and reset to the current branch. Otherwise Pointy attempts to merge the branch into it with the commit message `Merge latest <branch> into agent session`. A merge conflict is aborted and logged as a `[system]` line, which the chat does not display; the agent continues from its previous base. Conflicts are resolved later, during apply preparation.

Only changes to these paths can be committed and applied:

- `projects/<numeric-id>.nix`
- `steps/<numeric-id>.nix`
- `srcFiles/<numeric-step-id>/<relative-path>`

Other changed paths are reported in the log and excluded. In particular, the embedded agent cannot update `flake.nix` or templates; administrators make those changes with their repository tools. Applied changes are ordinary Git commits, available to other instances using that repository after their next fetch.

Applying a changeset uses a temporary worktree at the current target-branch commit. Pointy combines the agent's edits into one commit, removes changes to reviewed steps (recording the proposed changeset as discarded if nothing else remains), evaluates the result, and refuses it if it introduces evaluation failures that are not already on the shared branch. It checks that neither side has changed during preparation, then pushes it. When preparation finds conflicts, the agent resolves them in that worktree and you prepare again. The [API guide](agent-api.md#applying-changes-safely) describes the preparation and confirmation requests.

### Sandbox and credential boundaries

Each turn runs two `sbox` sandboxes. The **tool sandbox** runs `pi-glovebox-server` from the per-session tool home. It binds the session worktree read-write (the tools' working directory), the user-repository Git directory read-only, reviewed steps read-only, conflict worktrees read-write while resolving an apply conflict, and a socket directory read-write. `sbox-command` starts both sandboxes; `sbox-args` configure this tool sandbox, and by default `{home}` is bind-mounted too. The tool sandbox also gets the Nix daemon compatibility socket when present and, with `storeUrl`, the store socket read-write and `ROOT` read-only.

The **runner sandbox** runs Pi in RPC mode with its own `{sessionRoot}/pi` home, Pi config, and session history. It sees an empty directory in place of the worktree, reaches the tools through the glovebox socket, and mounts the extensions directory read-only. Its binds are fixed; `runner-args` only changes Pi's arguments. Pi starts with `--no-context-files`, `--no-skills`, `--no-extensions`, `--no-prompt-templates`, `--no-themes`, `--no-mcp`, and `--no-approve`; the backend then adds the configured extensions, the `.pi/skills` and `AGENTS.md` from the session's base commit, and a system prompt containing its built-in rules. The backend's own `HOME`, SSH keys, and Git credentials are not passed to either sandbox.

The runner receives `PATH`, `USER`, `LANG`, `LOCALE_ARCHIVE`, and the keys named by the `api-key-env` of the coder's and answerer's providers. It also receives `HOME`, `POINTY_AGENT_WORKTREE`, `POINTY_AGENT_OUTPUT_MARKER`, `POINTY_AGENT_SESSION_ID`, `PI_GLOVEBOX_SOCKET`, and `TMPDIR`. The tool sandbox receives `PATH`, `USER`, `LANG`, `LOCALE_ARCHIVE`, `HOME`, `POINTY_AGENT_WORKTREE`, and `POINTY_AGENT_SESSION_ID`, but no provider keys. Both receive `NIX_REMOTE` when `storeUrl` is set. Other backend environment variables are not passed through. The sandboxes do not see the host's system profile, so `LANG` resolves only through `LOCALE_ARCHIVE`; without it, programs fall back to the C locale, and some, such as gnuplot, crash. Supply provider secrets through `services.pointy-backend.agentEnvFile`, not `config.toml`.

The module puts an `sbox` on the backend's `PATH` that always runs with `--network isolated --history off`; it does not install `sbox` system-wide. Network isolation here is a sandbox setting, not a promise that the agent cannot contact a model provider. Check the sandbox's connectivity and any extra binds as part of your deployment policy.

`services.pointy-backend.agentMemoryMax` (default `24G`) limits the memory shared by every agent sandbox and the processes it starts, including files in the sandbox `/tmp`. On exceeding the limit the kernel kills the largest process and the backend keeps running; agents are told the limit. `null` leaves the sandboxes unlimited. When a limit is set, the module delegates the memory controller to the backend service and sets `OOMPolicy = continue`; the backend requires a cgroup v2 hierarchy and fails to start otherwise.

The module grants the `backend` user a narrow polkit rule for managing systemd units whose names start with `nix-build-`, and declares the `pointy-builds` slice. That is separate from ordinary step scheduling through Slurm. Treat custom sandbox binds and privilege overrides as security-sensitive configuration.

### Placeholder expansion

For a normal turn, placeholders in `sbox-args` (tool sandbox) and `runner-args` (runner sandbox) are expanded before the process starts:

- `{worktree}`: absolute path to the session worktree (the tools' working directory)
- `{home}`: per-session tool HOME (`{worktree}/../home`), created before the runner starts
- `{sessionRoot}`: `{worktree}/..`, the session root directory
- `{sessionId}`: short opaque session identifier

Title generation expands `runner-args` only; it does not use `sbox-args`.

The backend manages RPC mode and session continuation itself. Do not put `--mode`, `--session`, `--fork`, `--model`, `--thinking`, `--provider`, `-c`/`--continue`, `--no-session`, or `-p`/`--print` in `runner-args`. There is no `{prompt}` substitution; that legacy token is stripped, and prompts go over the runner's RPC input.

### Time, output, and session storage

- `timeout-seconds` (default `1800`): active-time budget for a single agent turn, sampled every second and paused while a question waits for an answer. On expiry the backend logs `Runner timed out; aborting the turn`, sends pi an abort request, waits 10 seconds, then terminates the process.
- `output-limit-bytes` (default `1048576`, 1 MiB): per-turn log size cap. Once the log reaches this limit, further output lines are discarded.
- `session-retention-days` (default `7`): accepted by the current configuration schema but not yet used for automatic pruning. Users remove stored sessions with **Delete permanently** after archiving them.

The `[agent]` section is re-read when a turn starts, when a session is created, and when a changeset is applied, so edits apply to the next turn without a restart.
