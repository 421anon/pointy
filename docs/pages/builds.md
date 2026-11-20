---
description: "How the backend submits builds and which store paths it keeps as GC roots."
---

# Builds

## Build execution and GC roots

Each build target is a derivation the backend already evaluated. A Slurm batch job runs `<realiseCommand> <derivation>`. The NixOS module defaults `realiseCommand` to the generated `pointy-realise`, which runs `nix build --no-link -- "$1^*"` and adds `--store <storeUrl>` when configured; without the module, the backend looks for `pointy-realise` on its `PATH`. Exit code 0 means every output of that derivation is valid in the backend's store and its build log is readable there; any other exit is a failed build. The command must exist at the configured path on every compute node, so a store path from the module only works there when the nodes share a store; use [the hub/node pair](clusters.md#multi-node-clusters) when they do not.

The job name is `pointy-nix-build-` plus the sanitized build-target store path plus a 16-character FNV-1a hash, at most 128 characters in total. The job comment is a JSON restore record, `{"kind":"step"|"extras","step":<id>,"commit":"<c>","outPath":"<target>"}`, and every job exports `POINTY_REQ_CPU`, `POINTY_REQ_RAM`, `POINTY_REQ_IOR`, and `POINTY_REQ_IOW`. Job stdout and stderr go to `/dev/null`.

`single-node-slurm` configures a single-node runtime. A multi-node deployment needs matching scheduler configuration and access to the required repository and store paths. The backend runs `pointy-slurm-watch` for the whole process lifetime and consumes newline-delimited snapshot and heartbeat frames. Slurm lifecycle hooks notify the host event broker, which takes an event-triggered `squeue` and `sinfo` snapshot and pushes it to the watcher; the broker also supplies heartbeats without running Slurm commands. Snapshots are authoritative: a tracked job missing from a snapshot is rechecked with one targeted `squeue` query and then its completion waiters are woken, so a job ending before or around waiter registration cannot be missed and a stale snapshot cannot complete a job that has just been submitted. Brokers may also include bounded `terminalJobs` hints, each an `{"id","name","state"}` object, for jobs whose authoritative terminal event arrived while `squeue` still reports COMPLETING. A hint ends only a waiter registered against that exact accepted Slurm job id, never a newer retry with the same name; the backend caches recent hints so a waiter that registers after the hint still observes it. Startup restores the jobs still listed by one query. The backend re-attaches running step jobs whose JSON comment, name, and commit all match, plus running extras jobs with a valid comment; jobs without a usable comment or with another kind are skipped and their derivation roots swept. There is no periodic reconciler. If the watcher exits or stops sending heartbeats, the cluster is marked unavailable and the watcher reconnects with bounded backoff; the fresh snapshot on every connection resynchronises job progress and completions.

A realise command reports only success or failure; Slurm reports why a job ended. A hint's `state` is Slurm's base terminal state from the job-completion hook, such as `COMPLETED`, `FAILED`, `OUT_OF_MEMORY`, `TIMEOUT`, or `NODE_FAIL`, and is null when the hook supplied none. When the latest job submitted for a step ends `OUT_OF_MEMORY`, `TIMEOUT`, or `NODE_FAIL`, the step fails with that reason in place of the build log's last line, and the step log carries it as `reason` above the log text. The backend keeps these states in memory and the broker repeats a hint for an hour, so a backend restarted more than an hour after a job ended shows only the build log.

The activity bar shows **Starting**, **Queued**, **Running**, and **Saving results**.
Saving results means the calculation has finished and its outputs and logs are being
made available to the notebook; it is not additional calculation time.

Running a step expands its complete transitive step-reference graph. Pointy submits one Slurm job for each unbuilt step in dependency order, or reuses the step's valid build target and in-flight jobs with the same name. Dependent jobs are connected with `--dependency=afterok:` over every enqueued dependency and `--kill-on-invalid-dep=yes`; a dependent is not submitted when one of its prerequisites cannot be evaluated or scheduled, and the stream reports `dependency step(s) [<ids>] could not be scheduled`.

On a revision whose stdlib publishes `pointy.schemaVersion`, the build target is the step's certificate (`#pointy.steps.<id>.certificate`) rather than its output. A step whose output is already valid but whose certificate is not is reported as `built-not-certified` and still gets a certificate build; a certificate whose log shows a failure is reported as `certification-failed`.

Before submitting a build, the backend creates a temporary GC root for its derivation under `/home/backend/.local/state/pointy/gc-roots/derivations/<job-name>/`, so a garbage collection cannot remove the derivation while the job waits. It removes that root when the job ends, and sweeps leftovers at startup.

When a watched run ends successfully, the backend registers output roots with `nix-store --add-root /home/backend/.local/state/pointy/gc-roots/<store-basename> --realise <path>`:

- a run that finds a valid certificate roots the certificate;
- an enqueued or re-attached job that ends with a valid certificate roots the certificate and the step output;
- extras outputs are rooted.

Output roots are created only on run paths. A build that finished while nothing watched it, a status probe, a review, and an ingested upload create no output root. The backend never removes output roots; it does remove the temporary derivation roots. A GC root is a reference, not a backup: removing one permits collection and can make old share links or reviewed outputs unavailable.
