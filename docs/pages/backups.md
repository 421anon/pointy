---
description: "What to back up and how to recover an instance."
---

# Backups

## Backups and recovery

Git preserves definitions, not all the data. A useful backup covers:

- The remote repository **and** `/home/backend/user-repo.git`, including unpublished commits and agent branches (`agent/<session-id>`), plus `/home/backend/user-repo.git-replaced` if a repository URL change created it.
- `/home/backend/agent-sessions/`, with its worktrees, Pi conversation history, per-session tool homes, and turn logs. Each session directory holds `session.json`, `turns/<turn-id>.json` and `.log`, `worktree/`, `home/` (tool scratch), and `pi/` (Pi home and `.pi/agent/sessions`); `instructions/`, `tool-socket/`, `pi-tmp/`, and `apply-worktree/` are transient. Capture it consistently with the repository it points into.
- The required Nix store closures, especially uploaded input objects and results that cannot simply be fetched again. Store hashes in a step file are not copies of the bytes, and an ingested path has no GC root until a successful run roots it. If your ingest program references its source, also back up `uploadDirectory` and every wrapped scratch directory, because the store paths read those bytes.
- The external cold store, if cold storage is configured, plus its ingest socket path. Pointy only reports and prefetches from it; it does not hold those bytes.
- GC-root state under `/home/backend/.local/state/pointy/gc-roots/`, restored together with its store targets and Nix registrations; its `derivations/` subdirectory is transient.
- Runtime configuration, deploy keys, and provider secrets: `/home/backend/config.toml`, `/home/backend/agent-env` (a symlink when `agentEnvFile` is set), `/home/backend/.ssh/known_hosts`, and your NixOS configuration, stored with appropriate access protection.

The diff reports under `/home/backend/.local/state/pointy/diff-reports/`, the certificate store under `/home/backend/.local/state/pointy/certificates/`, the embedding cache under `/home/backend/.local/state/pointy/embeddings/`, and `/home/backend/.pi` can be regenerated; they are not substitutes for the inputs above. Preserve Nix logs too if you need historical failure diagnostics.

For a consistent recovery point, stop new edits and agent turns, allow or stop scheduled builds, then snapshot the repository, sessions, and store using your normal system backup tooling. A filesystem copy of `/nix/store` alone is not a complete Nix store backup: preserve its database and registrations, or export/import the required closures through Nix.

Test restoration on a separate instance with the same paths and ownership. Check an old shared result, a reviewed step, an uploaded input, and a saved chat before considering the backup usable. Do not let a recovery test push changes to the production branch.
