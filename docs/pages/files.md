---
description: "Read, upload, and edit files, fetch cold outputs and Turtle, and search steps through the API."
---

# Files

## Files, previews, and uploads

`GET /step-files?id=…` and `GET /src-files?id=…` list a directory; `path` selects a relative subdirectory and `commit` pins the revision. Path traversal rules apply to every store-backed read, source files included: a normalised `path` with any `..` component, or one that does not stay inside the base, returns 400 `Path traversal not allowed`; a base outside `/nix/store/` returns 400 `Invalid store path`. An absolute `path` inside the output is accepted, and symlinks are followed wherever they point. Entries contain `name`, `isDir`, `size`, `viewable`, `seekable`, `mimeType`, and `cold`. Listings are unsorted and include dotfiles. A cold entry is not read while listing: its MIME type comes from the file extension, and its `viewable`/`seekable` flags from the recorded size. Zip files appear as expandable directories with a size and `application/zip` MIME type; append their internal paths to browse or download entries (seek inside a zip gives 404).

`GET /step-files/out-path?id=…&commit=…` returns the store path of the step's output at that commit as plain text, whether or not it is built; a step that does not evaluate there gives 500. The artifact viewer compares these paths to tell whether the branch head has a different output from the one shown.

Use these routes for file contents:

| Route | Response |
| --- | --- |
| `GET /step-files/download?id=…&path=…&commit=…` | Full file as `application/octet-stream`, with attachment `Content-Disposition` and `Content-Length`. |
| `GET /src-files/download?id=…&path=…&commit=…` | The equivalent source-file download; 404 `Step <id> has no source files` when none exist. |
| `GET /step-files/raw/<path>?id=…&commit=…` | Raw output bytes with a detected MIME type. |
| `GET /step-files/bundle/<step-id>/<commit>/<path>` | Raw output bytes at a path that keeps relative HTML/CSS/image requests in the same pinned bundle. |
| `GET /src-files/raw?id=…&path=…&commit=…` | Raw source bytes, without attachment disposition. This route may appear without a GET operation in the generated reference. |

Raw and bundle routes are raw handlers, so any method works, and they send no sandboxing headers. An unbuilt output gives 500 `Something went wrong` when listing and 404 with an empty body from download, raw, and bundle. Reading a file that is still in cold storage—through download, seek, raw, or bundle—returns **409** `This file is in cold storage. It is being fetched; try again once it is local.` and starts a prefetch of the enclosing cold file. `GET /src-files` returns `[]` when the step has no source directory and 500 `Failed to evaluate the source files of step <id>: …` when the revision cannot be evaluated. Source files can be read at past revisions; on the current stdlib this works for `derivation` templates with `withSrcFiles` and an existing `srcFiles/<id>`.

Encode paths and query values. Raw HTML is executable content: a custom client should sandbox its previews, as Pointy's UI does, rather than inserting it into the application's DOM.

For large text files, use `/step-files/seek` or `/src-files/seek`. Supply `id`, `path`, optional `commit`, exactly one of `line` (1-based) or `offset` (0-based bytes), and a nonzero signed `bytes` value with absolute value at most **2 MiB**. Positive bytes read forward; negative bytes read backward to the anchor. The JSON response contains `content`, `startOffset`, `endOffset`, `startLine`, `endLine`, and `eof`. Use the returned boundaries: UTF-8 alignment can move a requested boundary, and a `line` past the end returns an empty chunk with `eof: true`. Invalid requests return 400 with `bytes must not be zero`, `abs(bytes) must be <= 2097152`, `Specify exactly one of line or offset`, `line must be >= 1`, `offset must be >= 0`, or `seek offset beyond end of file`. File-listing preview flags distinguish readable files up to 5 MiB from larger seekable files. The download route returns the complete file, not this window.

### Cold-stored outputs

Instances can offload step outputs to a cold store. When `services.pointy-backend.coldStorage.ingestSocket` (exported as `POINTY_COLD_STORE_SOCKET`) points at a running cold store, and `services.pointy-backend.coldStorage.package` puts the cold-storage command on the backend's PATH, two routes report and drive it:

- `GET /step-files/cold?id=…&commit=…` returns `{ "cold": <bool>, "onloading": <bool>, "files": [ { "path", "done", "total" } ] }`. `cold` says the output lives in cold storage, `onloading` that it is being fetched, and `files` lists the output's files with their fetch progress in bytes (0 when the store reports none). Without a configured cold store, or when its status command fails, times out, or cannot be read, the answer is `{"cold": false, "onloading": false, "files": []}`.
- `POST /step-files/prefetch?id=…&commit=…&path=<relative file>` asks the cold store to fetch one file. It returns `{ "ok": true, "started": […], "skipped": […], "unknown": […] }` naming the paths it started, skipped, and did not recognise. Without a cold store it returns **503** `cold storage is not configured`; a failing command or unreadable answer returns **502** with the reason.

Poll `/step-files/cold` while `onloading` is true; the file listing flips an entry's `cold` to false once the file is local. Source files are never cold.

### Artifacts as Turtle

`GET /rdf/artifact/<project path>/<step-id>/<full-commit>` serves an artifact permalink's linked data as `text/turtle; charset=utf-8`, with `Cache-Control: public, max-age=31536000, immutable` and `Vary: Accept`. Write the project path as folder ids (omit it for Home), then the step id and a full 40-character lowercase hex commit. Without `?path` the document describes the step's output and the artifacts upstream of it; `?path=<file>` selects one output file instead, as the permalink's query does. The supplied nginx vhost rewrites `/artifact/...` requests whose `Accept` includes `text/turtle` to this route, so the HTML permalink keeps serving the frontend. A malformed route or a step that is not in that folder at the commit returns 404 with text; a commit the backend cannot find after one fetch returns 404 `Git commit <c> is unavailable after fetching the user repository`, and a revision from before the stdlib published linked data returns 404 naming the commit. Set `services.pointy-host.linkedDataBase` to an origin to prefix the response with `@base <origin/> .`.

### Editing source files

`POST /src-files?id=42&path=script.py` creates a file, `PUT` updates it, and `DELETE` removes it. POST and PUT take **`text/plain`** contents, not a JSON object. Each successful operation commits and pushes separately, with the message `Create|Update|Delete source file <id>/<path>`, and a PUT with identical content makes no commit. Writes have no `commit` parameter.

Absolute, empty, or `.`/`..` paths are rejected with 400 `Invalid source file path`. Creating an existing path returns 409 `Source file already exists` (also for a directory); updating or deleting a missing file returns 404 `Source file does not exist` (also for a directory under PUT or DELETE). Reviewed steps reject mutations with 409 `Reviewed steps cannot be edited. Remove the review first.`, and other transaction failures are 409 too. A custom UI must distinguish its local draft from changes already saved by these requests; multi-file saves are not atomic.

### Uploading input data

`POST /upload?id=42` takes multipart files for an upload step. It stages the files, starts an ingest job, and returns **202** with the plain-text body `Ingest job started`. Any file field name is accepted; names are reduced to their base name, so duplicates overwrite each other. It never writes the repository's `srcFiles/` directory: only `args.uploaded.hash` changes, and that commit (`Upload files for step <id>`) happens before the ingest job reports success.

Submit at least one file; zero gives 400 `No files found`. A request with more than **100** files fails request parsing with an internal server error. The backend itself enforces no file-size limit, but the supplied nginx vhost inherits nginx's default **10 MiB** `client_max_body_size`, so larger request bodies fail at the proxy unless you raise it. Do not assume unlimited uploads.

Neither the upload nor the job runs the resulting step. Pointy's UI waits for the job to succeed, then requests a build; a custom client should do the same if it wants the output realized. An upload to a reviewed step is refused, and so is a new upload or wrap while a job for that step is still running (409 `An ingest job is already running for this step.`).

### Wrapping scratch directories

`GET /scratch` returns `{"root": "/data"}` (the canonicalised configured path), or `{"root": null}` when no scratch directory is configured. `GET /scratch/list?path=<relative>` lists one directory as `{"path", "entries": [{"name", "directory", "size"}]}`, directories first; `size` is null for directories and for entries that cannot be stat'd, and dotfiles are included. `POST /scratch/wrap?id=42` with the JSON body `{"path": "<relative directory>"}` starts an ingest job for that whole directory and returns **202** with `Ingest job started`. Paths are relative to the scratch root and must stay inside it after symlinks are resolved; the root itself and plain files cannot be wrapped. The commit message is `Wrap scratch directory <rel> for step <id>`, and the scratch directory is never deleted.

An unconfigured scratch space returns 404 `Scratch directory is not configured`. Bad paths return 400 with `Path must not be empty`, `Path must be relative to the scratch root`, `Invalid scratch path`, `Path escapes the scratch root`, `Not a directory`, or `The scratch root itself cannot be wrapped`. An unreadable directory returns 403 `Scratch directory is not readable` from `/scratch/list` only; a wrap of an unreadable directory fails when the job runs. A reviewed step or a running job returns 409.

### Ingest progress

`GET /ingest-stream` is a server-sent event stream. It sends an `ingest-jobs` event on connect and whenever a job changes: `{"jobs": [{"id", "stepId", "source", "path", "state", "done", "total", "hash", "error"}]}`, sorted by id. The stream keeps the latest job for each step. `source` is `upload` or `scratch`. `state` is `running`, `succeeded` or `failed`. `done` and `total` are whatever the ingest program reports, throttled to one update per second; the default program reports no progress, so both stay `null`. Jobs live in memory only, and their ids restart at 1 after a backend restart. A 30-second `heartbeat` arrives while nothing changes.

### Extras and autocomplete

`GET /step-files/extras?id=…&path=<directory>&commit=…` returns that folder's metadata object. `{}` can mean no extras, no metadata for that folder, or a scan queued in the background. Invalid JSON or evaluation errors are failures, not empty metadata. A `meta.json` over **10 MiB** is rejected with 400 `extras meta.json exceeds 10 MiB size limit`; parse failures return 500. See [Extras](extras.md#injecting-typed-column-metadata-via-extras).

`POST /autocomplete?commit=…` accepts:

```json
{
  "template": "script",
  "autocomplete": "packages",
  "context": {},
  "query": "py",
  "limit": 25
}
```

Use hook names actually exported by your repository. `limit` defaults to 25 and is clamped to 1–100 before calling the hook. Template/hook/context-key identifiers use ASCII letters, digits, `_`, and `-`. Query and context values may additionally contain `.` and `+`; whitespace is not accepted, and a rejected value returns 400 `<template|autocomplete|query|context key <k>|context value <k>> contains unsafe characters`. The response is an array of suggestion strings. An unpinned request fetches from the remote first.

### Step search

Two routes expose the semantic search behind the “Related” list:

- `GET /step-search/info` returns `{ "semantic": <bool>, "judge": <bool> }`, saying whether an embedding provider and a judge are configured.
- `POST /step-search?commit=…` takes `{ "query": "…", "exclude": [42] }` and returns `{ "results": [ { "step_id", "similarity", "relevance" } ] }`, most relevant first. `exclude` skips steps your interface already shows.

The backend embeds each step definition linked in a folder—its name, type, folder path, note, and arguments—and keeps up to `limit` results (10 by default) above `min-similarity` (0.4 by default). When a judge is configured it ranks the best `judge-candidates` candidates (24 by default) and drops those below `min-relevance` (0.5 by default); `relevance` is null without a judge. Configure search under `[search]`, `[search.embedding]`, and `[search.judge]` in `config.toml`. A blank query, missing search configuration, or a failed index build or embedding call returns an empty `results` array instead of an error; the failure is logged. `commit` is used verbatim, so pass a full hash.
