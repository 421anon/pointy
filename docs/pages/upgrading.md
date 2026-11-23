---
description: "Upgrade Pointy safely and migrate older repositories."
---

# Upgrading

## Upgrading

Back up first and let active agent turns finish. Pin a matching notebook, stdlib, and language revision, then evaluate the user repository and build a representative step before deploying the new frontend/backend together. Keep the old configuration and input lock files so you can reproduce the previous state.

The current platform expects folders: projects with tagged `children` and a root `projects/0.nix`. Repositories written in the former `steps` format can still be read, but every create and organize write refuses them with `projects/<id>.nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it.` Run `nix shell .#backend -c pointy-migrate-project-tree <user-repo-worktree>` on a checkout of the branch, commit and push the result, and move the stdlib to a revision that reads `children` (353a8d1 or later) in the same change. The tool refuses to run when `projects/` is missing or when `projects/0.nix` already exists, and it neither commits nor pushes. Remove any `[nix-evaluator]` table from `config.toml`; it is ignored, and the evaluator has no warm-up or sharded cache to configure.

The storage panel now keeps its history evaluations in an append-only binary file, `~/.local/state/pointy/storage/evaluations.bin`, instead of a JSON cache. To keep the evaluated history instead of scanning it again, convert the JSON cache before the new backend first starts: run `nix shell .#backend -c pointy-migrate-storage-history ~/.local/state/pointy/storage/evaluations-*.json` as the backend user, then delete the JSON files in that directory. The tool refuses to run once `evaluations.bin` exists, so run it before the switch. Skipping it costs only a rescan.

This revision removes the `nixosModules.shared` module; import `pointy.nixosModules.pointy-host` instead. `services.pointy-backend` is declared there, and the module no longer enables `programs.sbox` system-wide: it places its own `sbox` wrapper on the backend's `PATH`. The `[agent] bootstrap-prompt` key and the warm-session directory are gone. Chat model selection moved from `runner-args` to `[models.coder]` and `[[providers]]`; `--model`, `--thinking`, and `--provider` in `runner-args` are discarded, and an instance without `[models.coder]` has no chat model. Existing Pi configuration files keep their extensions, but `models.json` and `settings.json` are now generated and overwritten.

An instance that already configured `[models.chat]` and `[models.chat.pi]` renames them to `[models.coder]` and `[models.coder.pi]`. A leftover `[models.chat]` is ignored, so an instance that keeps it has no chat model. The optional `[models.answerer]` and `[models.router]` tables add a second model for questions; see [Routing messages to the answerer](providers.md#routing-messages-to-the-answerer).

Templates and API clients written for earlier revisions need attention too. Templates are plain attribute sets; the old typed-template API is gone. A step-input binding takes no `allowedTypes` key: the picker follows the parameter's declared type. There is no `visible = false` option: values Pointy supplies, such as `args.id` and the upload and download store references, are handled by the stdlib rather than declared as hidden form options. `PATCH /projects` replaces only `name`, `preset`, and `templates`, so a client no longer converts an evaluated folder's `steps[].def` back into references before saving.

On restart, the backend recovers running Slurm jobs from their recorded metadata: it re-attaches `kind: step` jobs whose comment, name, and commit match and `kind: extras` jobs with a valid comment that `squeue` still lists. Jobs without a usable comment are skipped and their derivation roots swept. Agent sessions marked running are reset, and unfinished turns get `runner exited while backend was offline`. Even so, avoid upgrading while people are editing or agents are working. A NixOS rollback restores software, not repository commits, deleted chats, or store objects removed by garbage collection.
