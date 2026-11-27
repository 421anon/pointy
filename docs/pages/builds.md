---
description: "How the backend submits builds and which store paths it keeps as GC roots."
---

# Builds

## Build execution and GC roots

Each build target is a derivation the backend already evaluated. A Slurm batch job runs `<realiseCommand> <derivation>`. The NixOS module defaults `realiseCommand` to the generated `pointy-realise`, which runs `nix build --no-link -- "$1^*"` and adds `--store <storeUrl>` when configured; without the module, the backend looks for `pointy-realise` on its `PATH`. Exit code 0 means every output of that derivation is valid in the backend's store and its build log is readable there; any other exit is a failed build. The command must exist at the configured path on every compute node, so a store path from the module only works there when the nodes share a store; use [the hub/node pair](clusters.md#multi-node-clusters) when they do not.

The job name is `pointy-nix-build-` plus the sanitized build-target store path plus a 16-character FNV-1a hash, at most 128 characters in total. The job comment is a JSON restore record, `{"kind":"step"|"extras","step":<id>,"commit":"<c>","outPath":"<target>"}`, and every job exports `POINTY_REQ_CPU`, `POINTY_REQ_RAM`, `POINTY_REQ_IOR`, and `POINTY_REQ_IOW`. Job stdout and stderr go to `/dev/null`.

### Slurm event watcher and job recovery

`single-node-slurm` configures a single-node runtime. A multi-node deployment needs matching scheduler configuration and access to the required repository and store paths.

The backend runs `pointy-slurm-watch` for the whole process lifetime and consumes newline-delimited snapshot and heartbeat frames. Slurm lifecycle hooks notify the host event broker, which takes an event-triggered `squeue` and `sinfo` snapshot and pushes it to the watcher; the broker also supplies heartbeats without running Slurm commands.

Snapshots are authoritative:

- A tracked job missing from a snapshot is rechecked with one targeted `squeue` query before its completion waiters are woken, so a job ending around waiter registration cannot be missed and a stale snapshot cannot complete a just-submitted job.
- Brokers may include bounded `terminalJobs` hints (`{"id","name","state"}`) for jobs whose terminal event arrived while `squeue` still reports `COMPLETING`. A hint ends only a waiter registered against that exact accepted Slurm job id, never a newer retry with the same name; recent hints are cached so late waiters still observe them.
- On startup, one query restores running `kind: step` jobs whose JSON comment, name, and commit match, plus running `kind: extras` jobs with a valid comment. Jobs without a usable comment or with another kind are skipped and their derivation roots swept.
- There is no periodic reconciler. If the watcher exits or stops sending heartbeats, the cluster is marked unavailable and the watcher reconnects with bounded backoff; the fresh snapshot on every connection resynchronizes job progress and completions.

### Termination reasons

A realise command reports only success or failure; Slurm reports why a job ended. A hint's `state` is Slurm's base terminal state from the job-completion hook, such as `COMPLETED`, `FAILED`, `OUT_OF_MEMORY`, `TIMEOUT`, or `NODE_FAIL`, and is null when the hook supplied none. When the latest job submitted for a step ends `OUT_OF_MEMORY`, `TIMEOUT`, or `NODE_FAIL`, the step fails with that reason in place of the build log's last line, and the step log carries it as `reason` above the log text. The backend keeps these states in memory and the broker repeats a hint for an hour, so a backend restarted more than an hour after a job ended shows only the build log.

On a cluster the build runs on a compute node, so the log is not in the backend's store; the runner publishes it to the configured substituters. When a job ends without a valid certificate, the backend fetches the failing derivation's log from a substituter and keeps it in `services.pointy-backend.buildLogMirror` (`POINTY_BUILD_LOG_MIRROR`). Status probes read that mirror like a local log, so a remotely failed step stays failed with its last log line across status refreshes, and the step log opens without contacting the cache. Without the option, only the case where the substituter itself publishes the log to the backend's store works.

The activity bar shows **Starting**, **Queued**, **Running**, and **Saving results**. **Saving results** means the calculation has finished and its outputs and logs are being made available to the notebook; it is not additional calculation time.

### Dependencies and certificates

Running a step expands its complete transitive step-reference graph. Pointy submits one Slurm job for each unbuilt step in dependency order, or reuses the step's valid build target and in-flight jobs with the same name. Dependent jobs are connected with `--dependency=afterok:` over every enqueued dependency and `--kill-on-invalid-dep=yes`; a dependent is not submitted when one of its prerequisites cannot be evaluated or scheduled, and the stream reports `dependency step(s) [<ids>] could not be scheduled`.

On a revision whose stdlib publishes `pointy.schemaVersion`, the build target is the step's certificate (`#pointy.steps.<id>.certificate`) rather than its output. A step whose output is already valid but whose certificate is not is reported as `built-not-certified` and still gets a certificate build; a certificate whose log shows a failure is reported as `certification-failed`.

### GC roots

Before submitting a build, the backend creates a temporary GC root for its derivation under `/home/backend/.local/state/pointy/gc-roots/derivations/<job-name>/`, so a garbage collection cannot remove the derivation while the job waits. It removes that root when the job ends, and sweeps leftovers at startup.

When a watched run ends successfully, the backend registers output roots with `nix-store --add-root /home/backend/.local/state/pointy/gc-roots/<store-basename> --realise <path>`:

- a run that finds a valid certificate roots the certificate;
- an enqueued or re-attached job that ends with a valid certificate roots the certificate and the step output;
- extras outputs are rooted.

Output roots are created only on run paths. A build that finished while nothing watched it, a status probe, a review, and an ingested upload create no output root. Output roots remain until an explicit revision delete (`POST /storage/delete` or **Delete** in the [Storage panel](storage-panel.md)) removes both the output root and its matching certificate root before calling `nix-store --delete`. A GC root is a reference, not a backup: removing one permits collection and can make old share links or reviewed outputs unavailable.

The agent's step shell needs the user-repo flake's inputs in the store, because its sandbox has no network or Git credentials. When the target branch's `flake.lock` changes, the backend runs `nix flake archive` against the new head and registers a root for every input under `/home/backend/.local/state/pointy/flake-inputs/<flake-lock-blob>/`. It keeps the current and previous lock's roots and removes older ones. The archive runs before a turn, outside the sandbox.
