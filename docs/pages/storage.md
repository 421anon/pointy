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

A deployment can keep large step outputs outside the backend's local Nix store with a cold storage provider: any program that implements the interface below. Set `services.pointy-backend.coldStorage.command` to its executable, which the backend sees as `POINTY_COLD_STORAGE_COMMAND`. Unset, the feature is off. Pointy never moves cold bytes itself; it asks the provider for status and for onloads.

### Provider interface

The backend runs the command as its own user with one of two subcommands. `<output-path>` is a step output's store path; the provider resolves it to whatever it keeps, and reports paths relative to it with `/` separators.

`<command> status <output-path>` prints one JSON object and exits 0:

```json
{
  "files": [
    { "path": "reads_R1.fastq.gz", "size": 52428800, "state": "offloaded" },
    { "path": "reads_R2.fastq.gz", "size": 52428800, "state": "onloading", "done": 1048576, "total": 52428800 },
    { "path": "notes.txt", "size": 4096, "state": "failed", "error": "storage box unreachable" }
  ]
}
```

`files` lists only the files that are not local: `offloaded`, `onloading` with `done` and `total` bytes, or `failed` with the error of the last onload. Every entry carries `size`, the file's bytes; Pointy sums it for the copy-back totals. An output the provider does not track, or one that is entirely local, gives `{"files": []}`. A non-zero exit, output that does not parse, or no answer within two seconds means the status is unknown.

`<command> onload <output-path> <relative-path>` starts onloading one file and exits without waiting for it; exit 0 means the onload started, is already running, or the file is already local. Pointy does not deduplicate concurrent requests for the same file, so a second `onload` while one runs must also exit 0 and must not copy the file twice. On a non-zero exit, the first line of stderr is shown to the user. The onload continues without Pointy, and later `status` calls report its progress, its failure, or the file's absence once it is local.

The interface says nothing about what reading an offloaded file through the store does; Pointy never reads a file the provider reports as not local.

### What Pointy does with it

Listings mark files that are not local and do not read them; when the status is unknown, a listing reads no file of that output. A download, seek, raw, or bundle request for such a file returns 409 with `This file is offloaded to cold storage. Copy it back from its snowflake in the file browser, then try again.`, and 503 when the status is unknown. Pointy asks for an onload only when someone requests one, through the UI's snowflake (**Copy back to this server**), `POST /step-files/onload`, or `POST /step-inputs/onload`; the file browser polls that file's status until it is local and then shows its usual actions, or shows the provider's error.

Before submitting a step's build, Pointy asks for the status of each input step that is already built: the steps its parameters name, not the steps further upstream, whose files the build never reads. If any of them has files that are not local, the step is not submitted: its status is `inputs-cold` and names the files and the steps holding them. A step that depends on a refused step gets that status too, naming the steps that read the files, itself included when it also reads offloaded files directly. If a status is unknown, the step is not submitted and its status says so. Comparing with a reviewed output fails with a message naming its files that are not local, unless the comparison is already cached.

`GET /step-inputs/cold?id=<id>&commit=<hash>` lists what the run of that step needs copied back: the steps that cannot start because of offloaded inputs, and the files grouped by the upstream step whose output holds them, with each file's size and onload state, plus the files of those steps that came back since the backend started. `POST /step-inputs/onload` on the same parameters starts copying those files back and returns the new status; files already copying are skipped, so repeating it is safe. The UI shows this as a card in the step's status popover: the totals, a **Copy back** button, the files and bytes back so far, and a **Run step** button once everything is local. While the popover is open and files are copying, it refreshes every three seconds. Nothing copies back without that request.
