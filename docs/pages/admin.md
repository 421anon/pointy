# Instance Admin Guide

This guide is for instance admins. If you are looking for everyday workflow tasks in the web UI, start with [Managing Projects](projects.md), [Building Workflows (Steps)](steps.md), and [Execution and Data Management](execution.md).

## Runtime architecture

A Pointy deployment has five moving parts:

1. **Frontend** — the browser UI where users manage projects, steps, runs, outputs, and share links.
2. **Backend** — a service that reads and writes the user repository, serves API endpoints, streams live step statuses, accepts uploads, and starts or stops builds.
3. **Backend-owned Nix REPL child** — the backend keeps a persistent `nix repl` child process for `nix eval` requests, reuses loaded flake bindings across requests, and restarts the child if it crashes.
4. **User repository** — a Git-backed Nix flake that stores templates, optional presets, step definitions, project definitions, and source files.
5. **Nix / Slurm runtime** — builds are submitted as Slurm batch jobs that call `nix build` against a pinned commit of the user repository.

When a user edits a project or step in the UI, the backend rewrites the corresponding `.nix` file in the user repository, commits the change, and pushes it back to the configured remote.

When a user runs a step, the backend evaluates the step from a specific commit and broadcasts status changes to every project that contains that step.

## Backend configuration

The backend reads its configuration from `/home/backend/config.toml`. Override the path by setting the `POINTY_CONFIG_PATH` environment variable.

The current configuration format is:

```toml
[user-repo]
url = "git@example.com:org/user-repo.git"
keyfile = "/path/to/deploy-key"
branch = "main"

[slurm]
# Optional. Empty fields defer to Slurm defaults.
partition = ""
account = ""
time-limit = ""
extra = []
```

These settings tell the backend:

- which Git remote to use for the user repository
- which SSH key to use for Git operations
- which branch contains the live Pointy state

The optional `[slurm]` section controls how the backend submits build jobs. `partition` selects a Slurm partition when non-empty; `account` and `time-limit` become `sbatch --account` and `--time` flags when non-empty; `extra` appends raw `sbatch` flags for site-specific policy. Leave these fields empty to use the local Slurm defaults.

## What the user repository must contain

At minimum, the user repository needs:

- `flake.nix`
- `flake.lock`
- `templates/`
- optional `presets/`
- `steps/`
- `projects/`
- `srcFiles/`

Responsibilities are split like this:

- `templates/`, optional `presets/`, and `srcFiles/` are admin-authored
- `steps/` and `projects/` are backend-managed
- `flake.nix` wires everything together through `pointy-stdlib.lib.mkFlake`

You can keep any additional repo content you want — for example helper Nix files or a `packages/` directory — but that layout is your own convention, not something Pointy discovers automatically.

For the concrete flake setup, see [Setting Up the User Repository](user-repo-setup.md).

Project definitions must choose either a preset or a custom template list. If templates, presets, or step ids disappear from the user repository, Pointy keeps the project loadable where possible and surfaces validation errors in the UI.

## Repository synchronization behaviour

The backend keeps its local clone synchronized with the configured remote.

### Fetch path

When the backend fetches updates and the fetch is rejected as non-fast-forward, it:

1. tries to push any unpublished local commits
2. retries the fetch if that push succeeds
3. falls back to a force-fetch if the push also fails

This guarantees that the backend eventually converges on a definite state, even in the presence of conflicting local and remote histories.

### Push path

When the backend pushes a UI-originated commit and the remote has moved, it retries by running `git pull --rebase` and then pushing again.

## Live status updates

The frontend listens for live step-status snapshots on:

- `/backend/step-status-stream?project_id=<id>`
- optionally `&commit=<hash>` for pinned read-only views

This is a Server-Sent Events (SSE) endpoint. Pointy sends:

- an initial snapshot for the requested project
- periodic heartbeat events
- replacement snapshots when project status changes

When the stream is opened without `commit`, it follows the live head of the configured user-repository branch. When `commit=<hash>` is present, the stream is pinned to that historical read-only view and only emits snapshots for that commit.

If you place Pointy behind a reverse proxy, make sure this endpoint is allowed to stay open for a long time and that response buffering is disabled.

## External process limit and Nix evaluation child

The backend runs non-evaluation external commands through a global semaphore of **40 concurrent processes**.
That limit applies to subprocess work such as `git`, Slurm commands (`sbatch`, `squeue`, `scancel`), `nix path-info`, `nix log`, and file-type probing. Backend `nix eval` calls go through a backend-owned persistent `nix repl` child instead of this process launcher.
The REPL child keeps loaded flake bindings warm across requests. If the child exits or stops responding, the backend discards it, starts a fresh child, and retries the request once.

## Build execution and GC roots

Each build runs as a Slurm batch job whose job name is derived from the step output path. The default NixOS module configures a single-node Slurm runtime; production admins can override Slurm settings for remote workers, provided those workers have access to the same Nix store and user repository.

When a build succeeds, the backend registers a GC root under the backend user's home directory, for example:

- `/home/backend/.local/state/pointy/gc-roots/`

This keeps successful step outputs alive across `nix-collect-garbage`. To allow a specific output to be collected, remove the matching GC-root entry first.

## Related admin docs

- [Setting Up the User Repository](user-repo-setup.md)
- [Type Reference](type-reference.md)
- [CLI Reference](cli-reference.md)
