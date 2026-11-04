---
description: "Upgrade Pointy safely and migrate older repositories."
---

# Upgrading

## Upgrading

Back up first and let active agent turns finish. Pin a matching notebook, stdlib, and language revision, then evaluate the user repository and build a representative step before deploying the new frontend/backend together. Keep the old configuration and input lock files so you can reproduce the previous state.

The current platform expects folders: projects with tagged `children` and a root `projects/0.nix`. Repositories written in the former `steps` format can still be read, but every create and organize write refuses them with `projects/<id>.nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it.` Run `nix shell .#backend -c pointy-migrate-project-tree <user-repo-worktree>` on a checkout of the branch, commit and push the result, and move the stdlib to a revision that reads `children` (353a8d1 or later) in the same change. The tool refuses to run when `projects/` is missing or when `projects/0.nix` already exists, and it neither commits nor pushes. Remove any `[nix-evaluator]` table from `config.toml`; it is ignored.

This revision removes the `nixosModules.shared` module; import `pointy.nixosModules.pointy-host` instead. `services.pointy-backend` is declared there, and the module no longer enables `programs.sbox` system-wide: it places its own `sbox` wrapper on the backend's `PATH`. The `[agent] bootstrap-prompt` key and the warm-session directory are gone. Chat model selection moved from `runner-args` to `[models.chat]` and `[[providers]]`; `--model`, `--thinking`, and `--provider` in `runner-args` are discarded, and an instance without `[models.chat]` has no chat model. Existing Pi configuration files keep their extensions, but `models.json` and `settings.json` are now generated and overwritten.

On restart, the backend recovers running Slurm jobs from their recorded metadata: it re-attaches `kind: step` jobs whose comment, name, and commit match and `kind: extras` jobs with a valid comment that `squeue` still lists. Jobs without a usable comment are skipped and their derivation roots swept. Agent sessions marked running are reset, and unfinished turns get `runner exited while backend was offline`. Even so, avoid upgrading while people are editing or agents are working. A NixOS rollback restores software, not repository commits, deleted chats, or store objects removed by garbage collection.
