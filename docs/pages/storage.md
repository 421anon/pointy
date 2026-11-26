---
description: "Ingest data, provide scratch space, and offload outputs to cold storage."
---

# Storage

## Data ingest and scratch space

Uploads and scratch wraps run as background ingest jobs. For an upload, the backend stages the files under `uploadDirectory` (or the system temporary directory) and runs `pointy-ingest DIR store-ref` as the `backend` user. For a wrap, it runs the same program on the chosen scratch directory. The resulting NAR hash goes into the step's `args.uploaded.hash`, which is committed like any other record change. A browser tab that started or saw the job shows its progress and can run the step once the job succeeds.

The default `pointy-ingest` copies the directory into the store with `nix store add --mode nar --hash-algo sha256`, then prints the result from `nix path-info --json`. It prints no progress lines and deletes nothing. The backend deletes the staged upload after a success that reports `references_source: false`, and after any failure whatever `references_source` says.

A host can replace `pointy-ingest` through `ingestPackage`, for example with a program that indexes the bytes where they already are instead of copying them. The program prints JSON lines: optional `{"progress":{"done":N,"total":N}}` lines, then either `{"ok":true,"store_path":…,"nar_hash":…,"nar_size":N,"references_source":B}` or `{"ok":false,"error":…}`. Non-JSON lines are ignored, and the last `ok` line wins. Success needs `ok: true` and exit code 0; only `nar_hash` is used. With `references_source = true`, the store path keeps reading the source bytes, so the backend never deletes them. In that case `uploadDirectory` must be persistent, and a scratch directory that has been wrapped must not be changed or removed.

An upload request may contain at most 100 file fields; more than 100 fails request parsing with an internal server error. Any field name is accepted, names are reduced to their base name, and the backend enforces no file-size limit. The supplied nginx vhost, however, inherits nginx's default 10 MiB `client_max_body_size`, so larger request bodies fail at the proxy unless you raise it. An upload never writes `srcFiles/`: only `args.uploaded.hash` changes, and it is committed before the job reports success.

Anything inside the canonical `scratchDirectory` can be wrapped, except the root itself and plain files. Readability is checked when browsing; a wrap of an unreadable directory fails when the job runs. The agent's tool sandbox sees the scratch directory read-only. When `storeUrl = "unix://SOCKET?root=ROOT"`, the backend serves store files from `ROOT/nix/store`, and the tool sandbox gets the socket and `ROOT` bound into it.

Ingested store paths get no GC root of their own; only a later successful run of the step roots its output. Until such a run happens, a garbage collection can remove the uploaded data.

## Storage capacity

The Storage panel has a warm and a cold tier. The warm tier holds step outputs on this server; its capacity is the filesystem at `services.pointy-backend.storageDiskPath` by default, or an account quota from `services.pointy-backend.storageCapacityCommand` (`POINTY_STORAGE_CAPACITY_COMMAND`). Pointy runs the command without arguments as its own user and reads one JSON object:

```json
{
  "kind": "quota",
  "label": "account quota",
  "totalBytes": 2199023255552
}
```

`kind` is required and is `filesystem` or `quota`. `totalBytes` is required and must be positive. A quota must report only its total; Pointy fills it with the warm step-output bytes, so offloading or deleting output releases quota. A filesystem must report exactly one of `usedBytes` and `availableBytes`; Pointy uses its free bytes. `label` defaults to `disk` for a filesystem and `quota` for a quota. A non-zero exit, output that does not parse, or a non-positive total hides that tier's bar instead of falling back to the filesystem. The backend reuses the command's result for up to 30 seconds, and a Storage request refreshes it immediately.

On a filesystem the bar divides warm step-output bytes by those bytes plus the filesystem's actual free bytes. Space used outside the Notebook is not a separate bar segment and is not part of the denominator. On a quota it divides warm step-output bytes by the quota total and prints the quota left and total.

The cold tier appears only when cold storage is configured. It has no Notebook-specific quota unless the provider supplies one. When the provider supplies `availableBytes`, measured across all of its cold boxes, the bar divides this Notebook's cold bytes by those bytes plus that actual free space. Otherwise the panel shows cold usage without a bar or an invented amount left. Other users' data, retained cold copies, and compression are not inferred away.

## Cold storage

A deployment can keep large step outputs outside the backend's local Nix store with a cold storage provider: any program that implements the interface below. Set `services.pointy-backend.coldStorage.command` to its executable, which the backend sees as `POINTY_COLD_STORAGE_COMMAND`. Unset, the feature is off. Pointy never moves cold bytes itself; it asks the provider for status, for offloads, and for onloads.

### Provider interface

The backend runs the command as its own user with one of four subcommands. `<output-path>` is a step output's store path; the provider resolves it to whatever it keeps, and reports paths relative to it with `/` separators.

`<command> status <output-path>` prints one JSON object and exits 0:

```json
{
  "capacityBytes": 5497558138880,
  "availableBytes": 3839679791104,
  "files": [
    { "path": "reads_R1.fastq.gz", "size": 52428800, "state": "offloaded" },
    { "path": "reads_R2.fastq.gz", "size": 52428800, "state": "offloading", "done": 10485760, "total": 52428800 },
    { "path": "notes.txt", "size": 4096, "state": "onloading", "done": 1024, "total": 4096 },
    { "path": "aligned.bam", "size": 2097152, "state": "failed", "error": "storage box unreachable" }
  ]
}
```

`files` lists only the files that are not local: `offloaded`, `offloading` or `onloading` with `done` and `total` bytes, or `failed` with the error of the last onload. Every entry carries `size`, the file's bytes; Pointy sums it for the copy-back and cold-storage totals. `capacityBytes` is optional metadata for the cold tier, not a Notebook-specific free-space entitlement. `availableBytes` is optional and, when present, is the real free space across all cold boxes. A provider that cannot measure free space omits it; Pointy retains its last valid measurement rather than erasing it because another path's status failed. An output the provider does not track, or one that is entirely local, gives `{"files": []}`. A non-zero exit, output that does not parse, or no answer within two seconds means the status is unknown.

`<command> offload <output-path>` moves every local file of the output to cold storage and stays attached until it is done. While it moves files it writes one JSON object per line to stdout, `{"done": 10485760, "total": 52428800}`, counting bytes; `total` is what this run moves and stays fixed, and a run with nothing left to move may write no lines. It exits 0 once every file of the output is offloaded, including when it already was, so repeating it is safe; on failure it exits non-zero and the first line of stderr is shown to the user. If Pointy stops waiting, for example because the backend restarts, the offload continues in the provider, and later `status` calls report each file as `offloading` with its progress while it moves and then as `offloaded`. An `offloading` file is still local. A provider must keep an offloaded file's bytes out of the backend's local store, so `status` returning `offloaded` means the bytes are really gone.

`<command> onload <output-path> <relative-path>` starts onloading one file and exits without waiting for it; exit 0 means the onload started, is already running, or the file is already local. Pointy does not deduplicate concurrent requests for the same file, so a second `onload` while one runs must also exit 0 and must not copy the file twice. On a non-zero exit, the first line of stderr is shown to the user. The onload continues without Pointy, and later `status` calls report its progress, its failure, or the file's absence once it is local.

`<command> delete <output-path>` permanently deletes every cold copy and provider-side state of that output and exits 0. It must be idempotent for an output the provider no longer tracks, refuse an active offload or onload with a non-zero exit, and leave unrelated outputs alone; it may also invalidate the named output's provider-managed store registration so no valid path is left pointing at deleted bytes. Pointy checks running and queued readers and serializes its own path mutations before invoking it; the provider serializes deletion with its transfers. On a non-zero exit, the first line of stderr is shown to the user and Pointy leaves the local output alone.

The interface says nothing about what reading an offloaded file through the store does; Pointy never reads a file the provider reports as not local.

### What Pointy does with it

Listings mark files that are not local and do not read them; when the status is unknown, a listing reads no file of that output. A download, seek, raw, or bundle request for a frozen file returns `409` (`This file is frozen in cold storage. Rewarm it, then try again.`), and `503` when the status is unknown. Pointy asks for an onload only when explicitly requested — through the file browser's snowflake (**Rewarm file**), **Rewarm** in the [Storage panel](storage-panel.md), **Rewarm** in the step's status popover, `POST /step-files/onload`, `POST /step-inputs/onload`, `POST /storage/onload`, or `POST /storage/onload-step`, which rewarms every cold or failed revision of a step. Any onload started this way follows provider status updates into the Storage panel and clears stale `inputs-cold` refusals once the path is local.

#### Storage panel and deletion lifecycle

The status bar's [Storage panel](storage-panel.md) lists every step output revision still present in the store, grouped by step and ordered by the larger of each step's warm and cold bytes. See [Storage endpoints](api-guide.md#storage-endpoints) for the `/storage*` routes, response fields, and HTTP error codes.

- **Freeze** (`POST /storage/offload` or `POST /storage/offload-step`) moves local output files to cold storage; step-level freezes run one revision at a time in a single background thread so they do not hammer the provider in parallel.
- **Rewarm** (`POST /storage/onload` for one revision, `POST /storage/onload-step` for every cold or failed revision of a step) starts copying every frozen file back and follows provider-status checks until progress stops, pushing updated local and cold byte totals to the panel.
- **Delete** (`POST /storage/delete`) removes cold files through the provider (`delete <output-path>`) first when present; if provider deletion fails (`502`), Pointy leaves the local output alone. Once cold copies are gone, Pointy removes the output's GC root and matching certificate root, then runs `nix-store --delete`. If the store refuses the local delete (`409`), Pointy restores both roots and reports the refusal without recreating the provider's copy. A later Run always requires the output path itself rather than accepting a surviving certificate alone.
- **Concurrency guards**: freeze, rewarm, and delete are refused with `409` while any owner of the output, or a step that directly reads it, is running or queued (steps further upstream are not treated as readers), or while an offload of that output is active. Step-level freezes recheck before each revision.

#### Live updates and caching

The panel never polls. The backend keeps the storage view in memory and pushes it as a `storage` event on the [live event stream](streams.md#live-event-stream) whenever the branch moves, a build finishes, the Slurm watcher reports a step starting or stopping, an offload started by Pointy reports progress, or another part of Pointy observes a provider status change.

Opening the panel triggers a refresh and pushes a `storage` event even when nothing changed, advancing `updatedAt` (**Checked**). New paths are fetched once; builds and deletions invalidate sizes, while freezes, rewarms, deletions, and observed provider reads refresh cold status. A rewarm started from the file browser or a cold-inputs popover appears here as its provider status changes, and a transfer started outside Pointy appears after the next observed status change or when the panel is reopened.

#### History scan

The backend builds the storage view from Git history by scanning commits that change step definitions, source files, templates, packages, or the flake, evaluating each step's output path at those commits, and appending the results to `~/.local/state/pointy/storage/evaluations.bin`.

- The scan evaluates four commits at a time on dedicated Nix evaluators, separate from the app's request evaluators.
- On a large repository, the first scan covers at most 5,000 changes (newest first) in the background and streams progress to the panel; until it finishes, totals and revisions reflect the commits evaluated so far.
- A commit whose flake does not evaluate is cached as having no outputs. Transient failures (such as a failed download or crashed evaluator) are not cached; the panel shows the reason and the scan resumes after a minute.
- Sizes are the NAR sizes Nix records for the output paths, read once and refreshed after builds finish.

#### Cold-input guards before runs

Before submitting a step's build, Pointy checks the cold status of the files the build reads. A step whose output is not built reads each direct input step that is already built (not steps further upstream, whose files the build never reads). A step whose output is built but not certified builds only its certificate, which reads every file of that output and none of its inputs.

- If any of those files is not local, the build is refused before submission: the step — and any dependent steps waiting on it — get status `inputs-cold` naming the frozen files and the steps holding them. If cold status is unknown, submission is also refused.
- Comparing a step against a reviewed output fails with a message naming any frozen files, unless the comparison is already cached. When cold storage status is unavailable, the comparison fails closed.
- `GET /step-inputs/cold` and `POST /step-inputs/onload` drive the **Rewarm** card in the step's status popover, which lists the frozen files and the steps holding them, refreshes every three seconds while open, and offers **Run step** once every file is local; see [API](api-guide.md#running-work).
