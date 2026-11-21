---
description: "Follow step statuses, cluster activity, and ingest jobs as they change."
---

# Streams

## Live event stream

Open **one** `GET /events` connection for the application. It has no parameters and carries three named SSE events: `step-status`, `cluster-status`, and `ingest-jobs`. On connect it sends the current `cluster-status` and `ingest-jobs`, then replays recent `step-status` snapshots, then sends each event as its state changes.

`step-status` events carry partial folder snapshots:

```json
{
  "projectId": 1,
  "commit": "a-full-git-commit-hash",
  "steps": [
    { "stepId": 42, "status": "success" },
    { "stepId": 43, "status": "failure", "error": "Build failure detail" }
  ]
}
```

Status strings are `not-started`, `running`, `success`, `failure`, `inputs-cold`, `built-not-certified`, and `certification-failed`. `built-not-certified` means the output is valid but the certificate is not; `certification-failed` is the same with a failure log, kept in `error`. `inputs-cold` means the run was refused because an already-built input step has files offloaded to cold storage; `error` names them. Slurm PENDING jobs count as `running`; there is no queued status. Loading is a client-side state.

Snapshots are **partial**: a folder broadcast carries one snapshot with the `success` and `running` steps, then, when other statuses still need resolving, one further snapshot with all of those resolved steps; step-level broadcasts carry exactly one step. Merge each snapshot per `stepId` instead of replacing a folder's whole status map. A step entry may also carry `certificate`, the step's certificate store path. A snapshot's `projectId` names a folder that links the step **directly**, so a step linked from several folders produces several snapshots; ancestors never receive their descendants' steps, and steps that no folder links never appear on the stream. While a build of a step at a commit is tracked, only its `running` and `success` statuses are broadcast or replayed.

After subscribing, call `POST /project-status?project_id=1&commit=<hash>` to request a fresh snapshot. That empty response also precedes the asynchronous result, which arrives as partial snapshots. On connection or reconnection the bus replays up to 256 recent snapshots in order. This is bounded in-memory history, not an event log or a guaranteed complete initial snapshot.

A minimal browser listener is:

```js
const events = new EventSource("/backend/events");
events.addEventListener("step-status", event => {
  const snapshot = JSON.parse(event.data);
  console.log(snapshot.projectId, snapshot.commit, snapshot.steps);
});
// Call events.close() when this application no longer needs it.
```

`cluster-status` events carry the cluster's availability and the builds in flight:

```json
{
  "status": "degraded",
  "detail": "node2 drain",
  "activeSteps": [
    { "stepId": 42, "phase": "running", "since": "2026-11-03T10:15:30Z", "reason": null, "commits": ["a-full-git-commit-hash"] }
  ]
}
```

`status` is `available`, `degraded`, or `unavailable`, and `detail` is a string or null. The backend derives cluster status from the authoritative `sinfo` snapshot that its Slurm event watcher delivers on every connection and lifecycle event. A missing or failed watcher stream is `unavailable` with a short disconnect detail; an empty node list is `unavailable` with `no compute nodes registered`; and a node whose state is not one of Slurm's active states (idle, alloc/allocated, mix/mixed, comp/completing, res/reserved) or that carries an impairment flag, or a partition that is not up, makes the cluster `degraded` and is named in `detail` (at most three nodes and three partitions, then `+N more`, with `; no usable nodes` when none are healthy).

`activeSteps` lists the builds this backend process tracks, one entry per step id, including restored jobs. The backend publishes `preparing` as soon as it accepts a run request, `queued` when `sbatch` returns the job id, and then follows the watcher's snapshots: `queued` for a Slurm PENDING job (`reason` carries Slurm's reason), `running` while the job runs, and `saving` for the runner's saving marker. `since` is the start of the current phase, using Slurm's elapsed time for a running job and the first observation time for a saving job; `commits` lists the commits with a build tracked for the step. There is no periodic Slurm poll. Ingest jobs never appear. Status is not a promise that a particular job fits the available resources.

`ingest-jobs` events carry every ingest job the backend tracks; [Ingest progress](files.md#ingest-progress) describes them.

The stream sends a `heartbeat` event with `{}` after about 30 idle seconds. A client that hears nothing for longer than that has lost the connection even if the browser has not noticed, and should reconnect. Ignore SSE comments, including connection padding. Responses use `Cache-Control: no-transform` and `X-Accel-Buffering: no`; preserve unbuffered, long-lived connections through your proxy. There are no SSE `id:` or `retry:` fields, and `Last-Event-ID` is not a resume cursor: a reconnect receives the current cluster status and ingest jobs and the replayed step statuses. Request fresh folder status after reconnecting when you need to establish current state beyond that replay.
