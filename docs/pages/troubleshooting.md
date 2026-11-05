---
description: "Where the logs are and what common failures mean."
---

# Troubleshooting

## Logs and troubleshooting

On the host:

```sh
systemctl status backend nginx slurmctld slurmd munged
journalctl -u backend -f
journalctl -u slurmctld -u slurmd -f
sinfo
squeue
scontrol show job <job-id>
```

Use the step's log popover or `nix log <output-or-derivation-path>` for Nix build output. On certificate revisions the logged target is the certificate derivation. Slurm submissions send their own stdout/stderr to `/dev/null`; do not expect `slurm-<job-id>.out` to contain the build log. Agent turn logs are at `/home/backend/agent-sessions/<session-id>/turns/<turn-id>.log`.

| Symptom | Where to look |
| --- | --- |
| Projects or templates do not load | Evaluate `pointy.projects` and `pointy.stepConfig` from the same pinned repository revision. Partial failures do not block loading: look for placeholder folders named `Project <id>` with `This project does not evaluate: …`, and for `Projects at <sha8> are served with evaluation failures: …` in the journal. A 500 containing `projects/<id>.nix cannot be read: …` means a project file does not import, and `… uses the legacy steps format. Run pointy-migrate-project-tree to convert it.` or `Project 0 does not exist.` means the repository was never migrated. |
| Saving fails | Check deploy-key permissions, branch protection, and push/rebase errors in the backend journal (`Concurrent modification detected…`, `git push failed with exit code <n>: …`, `git pull --rebase failed: …`). |
| A step stays queued | Step status has no queued value; a PENDING Slurm job appears as `running` there, while the cluster stream shows its phase as `queued`. Inspect `squeue` and `scontrol show job`; CPU/memory requests may exceed advertised resources, or a dependency may not have completed. An `sbatch` rejection appears as `failure` with `sbatch failed (exit …): …`, and invalid requirements as `Invalid #pointy.steps.<id>.requirements: …`. |
| Cluster unavailable or degraded | Check `sinfo`, `munged`, controller/worker journals, and node memory configuration. The status bar shows the `detail` text behind degraded or unavailable states. |
| Status updates arrive in bursts or stop | Statuses other than `running` and `success` are suppressed while a build is tracked; steps not linked from any project never stream, and per-step snapshots arrive after log probes. Check proxy buffering, read timeouts, and browser origin. The API guide describes the stream's restricted CORS policy. |
| Agent fails before replying | Check that `[models.coder]` (and `[models.answerer]`, if set) names a configured provider, that provider's key is in the environment file, the sandboxes start, and the provider is reachable. A turn whose routing failed goes to the coder and logs `Routing failed, so the coder takes this turn: …`. |
| Related search is empty | `GET /backend/step-search/info` reports whether semantic search and the judge are configured; step and chat search share that configuration. Check `[search.embedding]`, its provider entry and key, and the journal warnings about failed embeddings or judging. |
| A cold file never becomes local | Check that both `coldStorage.package` and `coldStorage.ingestSocket` are set, that the package's cold-storage command is on the backend's `PATH`, and that the socket is reachable. A failing status query is treated as nothing cold. |
| Login redirects repeatedly or returns 401 | Check `tokenAuth.loginUrl`, `audience`, and that `publicKeyFile` is the Ed25519 public key matching the issuer's private key. Restart the backend after changing the key or audience, and remember that the session cookie requires HTTPS. |
| Reviewed output is missing | Check the store and GC roots; recording a review creates no GC root, so the reviewed output is kept only by roots from the run that built it. Rebuild the reviewed revision with `POST /run-step?id=<id>&commit=<reviewedRevision>`, or remove the review. |

The backend loads the config, warns about missing provider keys, prepares the agent memory cgroup, ensures or clones the repository, resets stale agent sessions, and fetches before it listens. After it listens, it restores Slurm jobs and running statuses, runs the first cluster probe, and starts the 30-second cluster poller and job reconciler. There is no project warm-up and no dedicated readiness endpoint that certifies all of these operations. Monitor service health, Git access, disk/store capacity, cold storage if used, and Slurm separately.
