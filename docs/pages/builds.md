---
description: "How the backend submits builds and which store paths it keeps as GC roots."
---

# Builds

## Build execution and GC roots

Each build target is a derivation the backend already evaluated. A Slurm batch job runs `<realiseCommand> <derivation>`. The NixOS module defaults `realiseCommand` to the generated `pointy-realise`, which runs `nix build --no-link -- "$1^*"` and adds `--store <storeUrl>` when configured; without the module, the backend looks for `pointy-realise` on its `PATH`. Exit code 0 means every output of that derivation is valid in the backend's store and its build log is readable there; any other exit is a failed build. The command must exist at the configured path on every compute node, so a store path from the module only works there when the nodes share a store; use [the hub/node pair](clusters.md#multi-node-clusters) when they do not.

The job name is `pointy-nix-build-` plus the sanitized build-target store path plus a 16-character FNV-1a hash, at most 128 characters in total. The job comment is a JSON restore record, `{"kind":"step"|"extras","step":<id>,"commit":"<c>","outPath":"<target>"}`, and every job exports `POINTY_REQ_CPU`, `POINTY_REQ_RAM`, `POINTY_REQ_IOR`, and `POINTY_REQ_IOW`. Job stdout and stderr go to `/dev/null`.

`single-node-slurm` configures a single-node runtime. A multi-node deployment needs matching scheduler configuration and access to the required repository and store paths. The job-completion hook, `services.pointy-backend.slurm.jobCompletionHook`, POSTs the job name to `http://127.0.0.1:<listenPort>/job-ended` for `pointy-nix-build-*` jobs; the backend only wakes waiters after `squeue` confirms the job has ended. A 30-second reconciler covers job ends missed while the backend is running, and startup restores the jobs that are still listed. At startup the backend re-attaches running step jobs whose JSON comment, name, and commit all match, plus running extras jobs with a valid comment; jobs without a usable comment or with another kind are skipped and their derivation roots are swept.

Running a step expands its complete transitive step-reference graph. Pointy submits one Slurm job for each unbuilt step in dependency order, or reuses the step's valid build target and in-flight jobs with the same name. Dependent jobs are connected with `--dependency=afterok:` over every enqueued dependency and `--kill-on-invalid-dep=yes`; a dependent is not submitted when one of its prerequisites cannot be evaluated or scheduled, and the stream reports `dependency step(s) [<ids>] could not be scheduled`.

On a revision whose stdlib publishes `pointy.schemaVersion`, the build target is the step's certificate (`#pointy.steps.<id>.certificate`) rather than its output. A step whose output is already valid but whose certificate is not is reported as `built-not-certified` and still gets a certificate build; a certificate whose log shows a failure is reported as `certification-failed`.

Before submitting a build, the backend creates a temporary GC root for its derivation under `/home/backend/.local/state/pointy/gc-roots/derivations/<job-name>/`, so a garbage collection cannot remove the derivation while the job waits. It removes that root when the job ends, and sweeps leftovers at startup.

When a watched run ends successfully, the backend registers output roots with `nix-store --add-root /home/backend/.local/state/pointy/gc-roots/<store-basename> --realise <path>`:

- a run that finds a valid certificate roots the certificate;
- an enqueued or re-attached job that ends with a valid certificate roots the certificate and the step output;
- extras outputs are rooted.

Output roots are created only on run paths. A build that finished while nothing watched it, a status probe, a review, and an ingested upload create no output root. The backend never removes output roots; it does remove the temporary derivation roots. A GC root is a reference, not a backup: removing one permits collection and can make old share links or reviewed outputs unavailable.
