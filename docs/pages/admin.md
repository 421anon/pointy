# Instance Admin Guide

This guide is for instance admins. If you are looking for everyday workflow tasks in the web UI, start with [Managing Projects](projects.md), [Building Workflows (Steps)](steps.md), and [Execution and Data Management](execution.md).

## Runtime architecture

A Pointy deployment has four moving parts:

1. **Frontend** — the browser UI where users manage projects, steps, runs, outputs, and share links.
2. **Backend** — a service that reads and writes the user repository, serves API endpoints, streams live step statuses, accepts uploads, and starts or stops builds.
3. **User repository** — a Git-backed Nix flake that stores templates, step definitions, project definitions, and source files.
4. **Nix / systemd runtime** — builds are executed as `systemd-run` units that call `nix build` against a pinned commit of the user repository.

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
```

These settings tell the backend:

- which Git remote to use for the user repository
- which SSH key to use for Git operations
- which branch contains the live Pointy state

## What the user repository must contain

At minimum, the user repository needs:

- `flake.nix`
- `flake.lock`
- `templates/`
- `steps/`
- `projects/`
- `srcFiles/`

Responsibilities are split like this:

- `templates/` and `srcFiles/` are admin-authored
- `steps/` and `projects/` are backend-managed
- `flake.nix` wires everything together through `trotter.lib.mkFlake`

You can keep any additional repo content you want — for example helper Nix files or a `packages/` directory — but that layout is your own convention, not something Pointy discovers automatically.

For the concrete flake setup, see [Setting Up the User Repository](user-repo-setup.md).

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

- an initial snapshot
- periodic heartbeat events
- replacement snapshots when project status changes

If you place Pointy behind a reverse proxy, make sure this endpoint is allowed to stay open for a long time and that response buffering is disabled.

## External process limit

The backend runs external commands through a global semaphore of **20 concurrent processes**.

That limit applies to subprocess work in general — including `nix`, `git`, `systemctl`, and file-type probing — so in practice it also caps how many builds can be active at once.

## Build execution and GC roots

Each build runs as a `systemd-run` unit whose name is derived from the step output path.

When a build succeeds, the backend registers a GC root under the backend user's home directory, for example:

- `/home/backend/.local/state/pointy/gc-roots/`

This keeps successful step outputs alive across `nix-collect-garbage`. To allow a specific output to be collected, remove the matching GC-root entry first.

## Related admin docs

- [Setting Up the User Repository](user-repo-setup.md)
- [Type Reference](type-reference.md)
- [CLI Reference](cli-reference.md)
