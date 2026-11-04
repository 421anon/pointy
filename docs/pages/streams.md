---
description: "Follow step statuses and cluster activity as they change."
---

# Streams

## Live status streams

Open **one** `GET /step-status-stream` connection for the application. It has no project or commit parameters. Named SSE `snapshot` events carry JSON:

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

Status strings are `not-started`, `running`, `success`, `failure`, `built-not-certified`, and `certification-failed`. `built-not-certified` means the output is valid but the certificate is not; `certification-failed` is the same with a failure log, kept in `error`. Slurm PENDING jobs count as `running`; there is no queued status. Loading is a client-side state.

Snapshots are **partial**: a project broadcast carries one snapshot with the `success` and `running` steps, then, when other statuses still need resolving, one further snapshot with all of those resolved steps; step-level broadcasts carry exactly one step. Merge each snapshot per `stepId` instead of replacing a folder's whole status map. A step entry may also carry `certificate`, the step's certificate store path. A snapshot's `projectId` is a project that links the step **directly**, so a step linked from several folders produces several snapshots; ancestors never receive their descendants' steps, and steps that no folder links never appear on the stream. While a build of a step at a commit is tracked, only its `running` and `success` statuses are broadcast or replayed.

After subscribing, call `POST /project-status?project_id=1&commit=<hash>` to request a fresh snapshot. That empty response also precedes the asynchronous result, which arrives as partial snapshots. On connection or reconnection the bus replays up to 256 recent snapshots in order. This is bounded in-memory history, not an event log or a guaranteed complete initial snapshot.

A minimal browser listener is:

```js
const statusStream = new EventSource("/backend/step-status-stream");
statusStream.addEventListener("snapshot", event => {
  const snapshot = JSON.parse(event.data);
  console.log(snapshot.projectId, snapshot.commit, snapshot.steps);
});
// Call statusStream.close() when this application no longer needs it.
```

`GET /cluster-status-stream` immediately sends a `cluster-status` event, then updates on change:

```json
{
  "status": "degraded",
  "detail": "node2 drain",
  "activeSteps": [
    { "stepId": 42, "phase": "running", "since": "2026-11-03T10:15:30Z", "reason": null, "commits": ["a-full-git-commit-hash"] }
  ]
}
```

`status` is `available`, `degraded`, or `unavailable`, and `detail` is a string or null. The backend probes `sinfo` at startup and every 30 seconds: a failed probe is `unavailable` with the first line of the probe error (up to 200 characters), an empty node list is `unavailable` with `no compute nodes registered`, and a node whose state is not one of Slurm's active states (idle, alloc/allocated, mix/mixed, comp/completing, res/reserved) or that carries an impairment flag, or a partition that is not up, makes the cluster `degraded` and is named in `detail` (at most three nodes and three partitions, then `+N more`, with `; no usable nodes` when none are healthy).

`activeSteps` lists the builds this backend process tracks, one entry per step id, including restored jobs. `phase` is `preparing` while the backend is still evaluating and submitting the build, `queued` for a Slurm PENDING job (`reason` carries Slurm's reason), and `running` otherwise; `since` approximates when that phase began, and `commits` lists the commits with a build tracked for the step. The backend polls Slurm about every five seconds while builds are tracked. Ingest jobs never appear. Status is not a promise that a particular job fits the available resources.

The project, cluster, and ingest streams (`/step-status-stream`, `/cluster-status-stream`, and `/ingest-stream`) send `heartbeat` events with `{}` after about 30 idle seconds. Ignore SSE comments, including connection padding. Responses use `Cache-Control: no-transform` and `X-Accel-Buffering: no`; preserve unbuffered, long-lived connections through your proxy. There are no SSE `id:` or `retry:` fields, and `Last-Event-ID` is not a resume cursor. Request fresh project status after reconnecting when you need to establish current state.
