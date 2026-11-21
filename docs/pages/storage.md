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

## Cold storage

A deployment can keep large step outputs outside the backend's local Nix store with a cold storage provider: any program that implements the interface below. Set `services.pointy-backend.coldStorage.command` to its executable, which the backend sees as `POINTY_COLD_STORAGE_COMMAND`. Unset, the feature is off. Pointy never moves cold bytes itself; it asks the provider for status, for offloads, and for onloads.

### Provider interface

The backend runs the command as its own user with one of three subcommands. `<output-path>` is a step output's store path; the provider resolves it to whatever it keeps, and reports paths relative to it with `/` separators.

`<command> status <output-path>` prints one JSON object and exits 0:

```json
{
  "files": [
    { "path": "reads_R1.fastq.gz", "size": 52428800, "state": "offloaded" },
    { "path": "reads_R2.fastq.gz", "size": 52428800, "state": "offloading", "done": 10485760, "total": 52428800 },
    { "path": "notes.txt", "size": 4096, "state": "onloading", "done": 1024, "total": 4096 },
    { "path": "aligned.bam", "size": 2097152, "state": "failed", "error": "storage box unreachable" }
  ]
}
```

`files` lists only the files that are not local: `offloaded`, `offloading` or `onloading` with `done` and `total` bytes, or `failed` with the error of the last onload. Every entry carries `size`, the file's bytes; Pointy sums it for the copy-back and cold-storage totals. An output the provider does not track, or one that is entirely local, gives `{"files": []}`. A non-zero exit, output that does not parse, or no answer within two seconds means the status is unknown.

`<command> offload <output-path>` moves every local file of the output to cold storage and stays attached until it is done. While it moves files it writes one JSON object per line to stdout, `{"done": 10485760, "total": 52428800}`, counting bytes; `total` is what this run moves and stays fixed, and a run with nothing left to move may write no lines. It exits 0 once every file of the output is offloaded, including when it already was, so repeating it is safe; on failure it exits non-zero and the first line of stderr is shown to the user. If Pointy stops waiting, for example because the backend restarts, the offload continues in the provider, and later `status` calls report each file as `offloading` with its progress while it moves and then as `offloaded`. An `offloading` file is still local. A provider must keep an offloaded file's bytes out of the backend's local store, so `status` returning `offloaded` means the bytes are really gone.

`<command> onload <output-path> <relative-path>` starts onloading one file and exits without waiting for it; exit 0 means the onload started, is already running, or the file is already local. Pointy does not deduplicate concurrent requests for the same file, so a second `onload` while one runs must also exit 0 and must not copy the file twice. On a non-zero exit, the first line of stderr is shown to the user. The onload continues without Pointy, and later `status` calls report its progress, its failure, or the file's absence once it is local.

The interface says nothing about what reading an offloaded file through the store does; Pointy never reads a file the provider reports as not local.

### What Pointy does with it

Listings mark files that are not local and do not read them; when the status is unknown, a listing reads no file of that output. A download, seek, raw, or bundle request for such a file returns 409 with `This file is offloaded to cold storage. Copy it back from its snowflake in the file browser, then try again.`, and 503 when the status is unknown. Pointy asks for an onload only when someone requests one, through the UI's snowflake (**Copy back to this server**), `POST /step-files/onload`, or `POST /step-inputs/onload`; the file browser polls that file's status until it is local and then shows its usual actions, or shows the provider's error.

The **Storage center** in the status bar (its drive icon) groups the step output revisions still present in the store by step. Each step is a parent node with the bytes its revisions keep on this server, the bytes they keep in cold storage, and how many revisions it has; expanding it lists the revisions that are in the store-wide list, with the step's name at that revision, its short Git hash, how long ago it was committed, and its local bytes. The store-wide list holds the 200 largest local revisions, largest first; steps with the most local bytes come first, then those with the most cold bytes, and a step whose revisions are not all listed says how many are missing. The store-wide totals cover every revision, not only the listed ones, and sit beside the store filesystem's free and total bytes. With a cold storage provider configured, each step also has **Move to cold storage**, which offloads every local or failed revision of that step (`POST /storage/offload-step?id=<id>`), and each listed revision has its own **Move to cold storage** (`POST /storage/offload?id=<id>&commit=<hash>`); without a provider, the panel shows no cold storage column or buttons. A step move runs the revisions one output after another in a single background thread instead of hammering the provider in parallel. A button is disabled while the step, or a step that reads its output, is running or queued, with the reason in the tooltip; the backend checks again when the request arrives and answers 409. Revisions being moved or copied back show progress, a fully offloaded revision shows **In cold storage** instead of its button, and a move that failed shows the provider's reason on its revision. The view's `updatedAt` (shown as **Updated**) is when the backend last finished checking, not when the content last changed, so it also advances while nothing is moving.

The panel never polls. The backend keeps the view in memory and pushes it as a `storage` event on the [live event stream](streams.md#live-event-stream) whenever it changes: when the branch moves, when a build finishes, when the Slurm watcher reports a step starting or stopping, on every progress line of an offload Pointy started, and whenever another part of Pointy reads a provider status, such as a file browser following a copy-back. Opening the panel asks the provider once more for the status of every listed output, which picks up offloads and copy-backs started outside Pointy, and the refresh that request triggers is pushed even when it finds nothing new so the **Updated** time advances; a move started elsewhere does not update the open panel until it is reopened.

The backend builds this view from its Git history. It scans the commits that change step definitions, source files, templates, packages, or the flake, evaluates each step's output path at those commits, and caches the results under `~/.local/state/pointy/storage/evaluations.json`, so later views are cheap. The scan evaluates four commits at a time in its own Nix evaluators, apart from the ones the rest of the app uses. The first view of a large repository can take a while: it scans at most 5000 changes, newest first, pushes its progress to the panel, and the scan continues in the background; until it finishes, the totals and step summaries cover only the revisions evaluated so far. A commit whose flake does not evaluate is cached as having no outputs. When an evaluation fails for a reason that can pass, such as a failed download or an evaluator that crashed, nothing is cached for that commit, the panel shows the reason, and the scan resumes after a minute. Sizes are the NAR sizes Nix records for the output paths, not the sum of what the files would weigh after unpacking. The backend reads a path's size once and again only after a build finishes or the panel is opened.

Before submitting a step's build, Pointy asks for the status of each input step that is already built: the steps its parameters name, not the steps further upstream, whose files the build never reads. If any of them has files that are not local, the step is not submitted: its status is `inputs-cold` and names the files and the steps holding them. A step that depends on a refused step gets that status too, naming the steps that read the files, itself included when it also reads offloaded files directly. If a status is unknown, the step is not submitted and its status says so. Comparing with a reviewed output fails with a message naming its files that are not local, unless the comparison is already cached.

`GET /step-inputs/cold?id=<id>&commit=<hash>` lists what the run of that step needs copied back: the steps that cannot start because of offloaded inputs, and the files grouped by the upstream step whose output holds them, with each file's size and onload state, plus the files of those steps that came back since the backend started. `POST /step-inputs/onload` on the same parameters starts copying those files back and returns the new status; files already copying are skipped, so repeating it is safe. The UI shows this as a card in the step's status popover: the totals, a **Copy back** button, the files and bytes back so far, and a **Run step** button once everything is local. While the popover is open and files are copying, it refreshes every three seconds. Nothing copies back without that request.
