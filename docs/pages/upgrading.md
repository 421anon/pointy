---
description: "Upgrade Pointy safely and migrate older repositories."
---

# Upgrading

## Before upgrading

Back up first and let active agent turns finish. Pin a matching notebook, stdlib, and language revision, then evaluate the user repository and build a representative step before deploying the new frontend and backend together. Keep the old configuration and input lock files so you can reproduce the previous state.

## Storage history cache migration

The storage panel keeps its history evaluations in an append-only binary file, `~/.local/state/pointy/storage/evaluations.bin`, instead of legacy JSON cache files. To preserve existing history instead of rescanning from Git, convert the JSON cache before the new backend starts:

```sh
nix shell .#backend -c pointy-migrate-storage-history ~/.local/state/pointy/storage/evaluations-*.json
```

Run this as the `backend` user, then remove the old JSON files in that directory. The tool refuses to run once `evaluations.bin` exists; skipping migration costs only a background rescan.

## Folder tree (`children`) migration

Pointy expects folders with tagged `children` and a root `projects/0.nix`. Repositories using the legacy `steps` format remain readable, but every create and organize write fails with `projects/<id>.nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it.`

1. Run `nix shell .#backend -c pointy-migrate-project-tree <user-repo-worktree>` on a checkout of the branch. The tool refuses when `projects/` is missing or `projects/0.nix` already exists, and it neither commits nor pushes.
2. Update `pointy-stdlib` to a revision that reads `children` (`353a8d1` or later) in the same commit, then push.
3. Remove any `[nix-evaluator]` table from `config.toml`; it is ignored because the evaluator has no warm-up or sharded cache to configure.

## NixOS modules and AI provider configuration

- **Module rename:** `nixosModules.shared` was removed; import `pointy.nixosModules.pointy-host` instead. `services.pointy-backend` is declared there, and `sbox` is now placed on the backend's `PATH` rather than enabled system-wide via `programs.sbox`.
- **Model tables:** Chat model selection moved from `runner-args` to `[[providers]]` and `[models.coder]` (and optional `[models.answerer]` + `[models.router]`; see [Routing messages to the answerer](providers.md#routing-messages-to-the-answerer)). `--model`, `--thinking`, and `--provider` in `runner-args` are discarded, `[agent] bootstrap-prompt` and warm sessions were removed, and Pi's `models.json` and `settings.json` are now regenerated before each turn.
- **Renamed `[models.chat]`:** Rename `[models.chat]` and `[models.chat.pi]` to `[models.coder]` and `[models.coder.pi]`. A leftover `[models.chat]` is ignored, leaving the agent with no chat model.

## Template and API breaking changes

- Templates are plain attribute sets; the legacy typed-template (`compile.options`) API is gone.
- Step-input bindings take no `allowedTypes` key (the picker follows the parameter's declared type), and there is no `visible = false` option (`args.id`, uploads, and downloads are handled by the stdlib).
- `PATCH /projects` replaces only `name`, `preset`, and `templates`; clients no longer convert evaluated `steps[].def` back into references before saving.

## Restart behavior

On restart, the backend recovers running Slurm jobs from their recorded metadata: it re-attaches `kind: step` jobs whose comment, name, and commit match and `kind: extras` jobs with a valid comment that `squeue` still lists. Jobs without a usable comment are skipped and their derivation roots swept. Agent sessions marked running are reset, and unfinished turns get `runner exited while backend was offline`. Even so, avoid upgrading while people are editing or agents are working. A NixOS rollback restores software, not repository commits, deleted chats, or store objects removed by garbage collection.
