---
description: "How the backend fetches and pushes the user repository, streams live status, and evaluates Nix."
---

# Sync

## Repository synchronization

The backend commits and pushes on every user edit. It does not fetch periodically. A fetch happens only when one of these triggers fires:

- backend startup;
- every unpinned `GET /step-config`, which the UI sends on each app load;
- every unpinned `POST /autocomplete`;
- a read of a commit that is missing locally;
- a write that hits `Concurrent modification detected…`;
- agent session creation, refresh, preparation, or confirmation, which always force-fetches.

Until one of these runs, a direct push to the remote is invisible to unpinned reads. When histories diverge, the backend resolves conflicts automatically where it can.

Writes run in a temporary worktree under `/tmp/pointy-worktree*` and take one exclusive lock; reads wait for writes, including while a push runs.

### On fetch

When a fetch is rejected as non-fast-forward, the backend:

1. tries to push any unpublished local commits
2. retries the fetch if that push succeeds
3. falls back to a force-fetch if the push also fails

The force-fetch fallback makes the remote authoritative; it is not a guarantee that unpublished local work is preserved. Back up the local repository as well as the remote, and investigate repeated synchronization errors before changing refs by hand.

### On push

When a push is rejected because the remote moved ahead, the backend runs `git pull --rebase` in the temporary worktree and pushes again. A rebase conflict aborts the write with `Concurrent modification detected. The remote repository was updated by another process. Please retry your operation.` The status code depends on the route: 409 for `PATCH /step`, `/projects/batch`, and `/src-files`; 400 for `POST /step` and `POST /projects`; 500 for `PATCH /projects`.

## Live status updates

The frontend opens one app-global SSE connection at `/backend/step-status-stream`. It takes **no project or commit query parameters**. Its `snapshot` events identify a `projectId`, `commit`, and a partial map of step statuses. Snapshots are partial, so merge them per `stepId`. A step's `projectId` is a project that links the step directly: a step linked from several folders produces several snapshots, ancestors do not receive their descendants' steps, and steps not linked from any project never appear on the stream.

Status strings are `not-started`, `running`, `success`, `failure`, `built-not-certified`, and `certification-failed`. A step whose output is valid but whose certificate is not shows `built-not-certified`; when a failure log for the certificate exists it shows `certification-failed` with the log line as `error`. Slurm PENDING jobs count as `running`; there is no queued status. While a build of a step at a commit is tracked, its other statuses are dropped before broadcast and before the replay buffer.

To request a fresh snapshot, the client posts to `/backend/project-status?project_id=<id>`, optionally with `&commit=<hash>`. The response returns before evaluation finishes; the results arrive on the global stream as partial snapshots. New connections replay up to 256 recent snapshots, not a guaranteed full inventory of every project.

`/backend/cluster-status-stream` separately sends cluster availability, a `detail` string, and an `activeSteps` map, which feed the status bar. Each active step carries a phase (`preparing`, `queued`, or `running`), the time that phase started, an optional Slurm reason, and the commits being built. The backend probes `sinfo` at startup and every 30 seconds. `unavailable` means the probe failed or no compute nodes are registered, `degraded` means some node or partition is not healthy and the `detail` names it, and `available` is neither. Both streams send 30-second heartbeats. The [API guide](streams.md#live-status-streams) gives payloads and reconnection details.

If you place Pointy behind a reverse proxy, make sure these endpoints are allowed to stay open for a long time and that response buffering is disabled.

## Nix evaluation

User-repository expressions are evaluated by two long-lived `nix repl` sessions: a **pure** session for pinned `git+file` revisions and an **impure** session for write worktrees and raw expressions. Each session serves one request at a time from a FIFO queue and binds each distinct installable once, reusing it for later requests. When a session has been idle, the backend reads its memory; above **2 GiB RSS** it closes the session and logs `Warning: <PureRepl|ImpureRepl> nix repl released at <N> MiB RSS`. The next request starts a fresh session, and a request whose session died is retried once. There is no warm-up and no configurable memory limit.

Bulk certificate work runs short-lived `nix eval` processes, with step keys evaluated in batches of 512. After every successful write the backend forks a certificate refresh at the new head; a refresh is also scheduled when a status request meets step keys that are not stored yet. Only one refresh runs at a time and the newest pending commit is queued; the journal logs `Certificate refresh for <c> skipped|failed: …`.

The certificate store at `/home/backend/.local/state/pointy/certificates/` maps a step key to its certificate path and `<key>.output` to its output path. Entries are write-once and never removed; they are a cache that evaluation can warm again.
