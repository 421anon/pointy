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

A deployment can keep large step outputs outside the backend's local Nix store. Set both `services.pointy-backend.coldStorage.package`, which provides the cold-storage command, and `services.pointy-backend.coldStorage.ingestSocket`. With either unset, the feature is off.

The backend runs the command's `status` subcommand with the ingest socket and the output's store path to ask whether a step output is local and which files are not. The query times out after two seconds. Listings mark cold files and do not read them; when the query fails, a listing reads no file of that output. A file is copied back only when someone asks for it: the UI's snowflake or `POST /step-files/prefetch` runs the command's `prefetch` subcommand with the socket, the store path, and the file's relative path. The store copies it in the background, so the copy finishes even if the page is closed; the UI polls until the file is local and then shows its usual actions. A copied-back file stays local until the output is offloaded again. The prefetch endpoint returns 503 when cold storage is not configured and reports the store program's error when it fails. A download, seek, raw, or bundle request for a cold file returns 409 with `This file is offloaded to cold storage. Copy it back from its snowflake in the file browser, then try again.` and fetches nothing.

Reading an offloaded file never copies it back, whoever reads it: the store fails the read instead. A build that reads an offloaded input fails; its log starts with a note naming the file and saying to copy it back from its snowflake. Comparing a reviewed output whose files are offloaded fails with a message naming them. The agent is told that an `Input/output error` on a file under `/nix/store` means the file is offloaded, and to tell the user which file and how to copy it back.

Pointy does not copy the cold bytes itself; it only reads status and asks for prefetches. Back up the cold store with the same consistency rules as the local Nix store, and keep its ingest socket path stable.
