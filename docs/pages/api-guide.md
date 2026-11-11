---
description: "Read a consistent revision, change folders and steps, start runs, and record reviews from your own client."
---

# API

As an instance administrator, you may want to connect a script or a custom interface to Pointy. A sample-entry form, for example, could create the steps for an experiment while Pointy stores them and runs the analysis. This guide explains how to read a consistent version of the folder tree, save changes, and follow a run's progress.

Use the generated [API Reference](api/) for route names and request schemas. It lists every route except `/agent/*`, which [Agent API](agent-api.md) covers; a few raw routes appear without a GET operation, and handler-level 404, 409, and 500 responses are not declared there. You do not need the API to work on an analysis: use the [Agent](agent.md) or [step controls](steps.md).

## Connecting to an instance

The supplied nginx host serves the API under **`/backend/`**. The backend itself listens with no prefix; the host module binds it to `127.0.0.1` on port 8081 by default (`services.pointy-backend.listenHost` and `listenPort`). Thus `/backend/projects` through nginx and `/projects` on the backend refer to the same route. The current host module has no `/api/` proxy.

Deploy a browser client on the same origin, or give it a same-origin reverse proxy. When token auth is configured, Pointy verifies the caller's identity but does not authorize per user. The supplied host can use nginx basic authentication or the optional token gate below; handle access at your deployment boundary and use HTTPS. A commit hash or share URL does not grant access on its own.

### Session tokens

The supplied host can require an EdDSA-signed JWT session token for its requests; the login page and `/docs/` stay open. Configure it with `services.pointy-host.tokenAuth` (`audience`, `publicKeyFile`, and the external `loginUrl`) or the equivalent `POINTY_TOKEN_AUDIENCE` and `POINTY_TOKEN_PUBLIC_KEY` environment variables. The token's `aud` claim must equal the configured audience, its `exp` claim must be in the future, and its `name` claim—or `sub` when there is no name—is shown as the signed-in user (null when neither is present). The backend checks the signature against the configured Ed25519 public key and stores no accounts.

The supplied nginx vhost calls `GET /token-auth` through `auth_request` for every gated request. It returns **204** when the request's `pointy-session` cookie holds a valid token and **401** otherwise, including when the backend is not configured for token auth. Unauthenticated page navigations are redirected to `loginUrl`; background requests get 401. After login, the external page POSTs a form-encoded `token` field to the host's `/token-login` (proxied to the backend route of the same name), which answers **303** with `Location: /` and sets the `pointy-session` cookie (`Path=/`, `Secure`, `HttpOnly`, `SameSite=Lax`, expiring with the token), or 401 for an invalid token. A POST to the host's `/token-logout` clears the cookie and redirects to `/` the same way; the backend route returns 404 when token auth is not configured. `GET /token-session` (under `/backend/` like the rest of the API) returns `{"account": "<aud>", "user": "<name or sub, or null>"}` for the cookie, 401 without a valid token, and 404 when token auth is not configured.

These routes identify the caller; they do not make the API per-user, and a caller that reaches the backend directly bypasses the gate. Keep the backend on loopback behind the supplied vhost.

### Cross-origin clients

CORS is route-specific, not a general permission to build a cross-origin client:

- `/step-status-stream` permits only `http://localhost:3000`, with credentials.
- `/agent/*`, `/ingest-stream`, `/cluster-status-stream`, `/step-files/cold`, `/step-files/prefetch`, and many ordinary resource routes allow any origin, without a credentialed cross-origin policy.
- Review routes, `/project-status`, `/notices`, `/projects/batch`, `/project-rollup`, `/step-review-diff`, `/src-files/seek`, `/job-ended`, `/step-search`, `/rdf/artifact`, and the `/token-*` routes have no explicit CORS policy in the current backend.

The ordinary policies allow `Content-Type`; the stream and agent policies also allow `Last-Event-ID`. None of them provides a general cross-origin `Authorization` header policy. CORS does not protect the backend from non-browser callers. See the [admin guide](admin.md#setting-up-a-host) before exposing an instance.

## Read one consistent revision

Start with `GET /commit-hash`, which returns plain text: the 40-character hash of the backend's local branch head. `GET /user-repo-info` returns the configured repository `url` and `branch`. Pass the hash as `commit` on reads that support it, such as `/projects`, `/project-rollup`, `/project-review`, `/step-config`, `/presets`, `/step-log`, `/step-search`, file routes, notices, autocomplete, and status requests. Omitting it reads the backend's local repository state for that request; several unpinned requests need not see the same revision.

The backend does not fetch periodically. Unpinned `GET /step-config` and unpinned `POST /autocomplete` fetch from the remote first and return 500 if that fetch fails. A fetch otherwise happens only on startup, on a commit that is missing locally (including one requested by `/rdf/artifact`), on a write that hits a concurrency conflict, and on agent session operations.

Most routes put the `commit` string verbatim into the evaluated flake reference, so supply a full commit hash. `/src-files*` and the review routes resolve it with `git rev-parse`; an unknown commit there fails with `Commit <c> is not in the local user repository.` `/rdf/artifact` takes the full hash in its path, tries one fetch when the commit is missing, and otherwise reports `Git commit <c> is unavailable after fetching the user repository`. Bundle URLs carry the commit as a path segment instead.

For example, from a terminal, with `BASE` set to your instance:

```sh
BASE=https://pointy.example.org/backend
COMMIT=$(curl --fail --silent --show-error "$BASE/commit-hash")
curl --fail --get "$BASE/projects" --data-urlencode "commit=$COMMIT"
curl --fail --get "$BASE/step-config" --data-urlencode "commit=$COMMIT"
```

Supply your deployment's credentials separately if required. Use full commit hashes for reproducible links, not a moving name such as `HEAD`.

`GET /projects` returns an object keyed by folder id, including `"0"` (the root folder, **Home**) and any orphaned folders. Each value is a project record with `id`, `name`, `preset` (string or null), `templates` (array or null), `children`, and `validationErrors`; other attributes of the file pass through. `lastModifiedAt` and `createdAt`, when available, come from Git history as UTC timestamps.

Each entry in `children` is exactly one of:

```json
{ "step": { "id": 42, "hidden": false, "sortKey": null, "def": { "type": "hello", "name": "First greeting", "args": {} } } }
{ "project": { "id": 2, "hidden": false, "sortKey": 3 } }
```

Subfolders are never expanded. `hidden` and `sortKey` belong to the link, so the same step or folder can appear in several parents with different placement. The effective order used by writes and rollups is: entries with an integer `sortKey` first, ascending, ties broken by id; entries with a null `sortKey` follow, by id; duplicates collapse to their first occurrence. A revision still written in the former `steps` format is converted on read: each entry becomes `{ "step": … }`, project-level `hidden`/`sortKey` are dropped, and a root `projects/0.nix` is synthesized when it is missing.

`validationErrors` explains references to missing presets, templates, or steps, and unknown child ids. A folder whose header fails to evaluate is served as a placeholder `{ "name": "Project <id>", "children": [], "validationErrors": ["This project does not evaluate: <error>"] }`, and failing children are dropped and reported there too.

`GET /project-rollup?project_id=<id>` returns `{ "project_id": <id>, "children": [ { "id", "steps", "statuses": { "<status>": <count>, … } } ] }` for each direct child folder, counting every step reachable below it once per child and all six statuses, defaulting to 0. An unknown folder id gives an empty `children` array. A step can appear in several folders without becoming a different step. A reviewed step's status comes from its reviewed revision; reviews are read from the branch tip. There is no route that lists steps no folder links.

`GET /step-config` returns `{ "version": 4, "templates": { ... } }`. Consume the fields' widgets and shapes rather than hard-coding a form per template. The [Type Reference](forms.md#the-generated-form-document) describes that document. `GET /presets` returns named template bundles; each preset has `templates`, `description` (default null), and `sortKey` (default null), and it returns `{}` when the repository defines none. These responses depend on the user repository, so their OpenAPI schemas are deliberately less specific than the JSON you will receive.

## Creating and editing workflow records

Folder and step ids are integers. Most routes take them as query parameters (`id`, `project_id`, `parent_id`, `source_id`), not path segments. A create request allocates the id and returns the evaluated record.

A folder body is `ProjectFields`:

```json
{
  "name": "Example analysis",
  "templates": ["hello"]
}
```

Use exactly one of `templates` or `preset`. Only create-time evaluation enforces this: a `PATCH /projects` or batch `update` that sets both commits the combination, and the folder is then served as a placeholder with a validation error. Every other key (`hidden`, `sortKey`, `steps`, `children`, `id`) is silently ignored, and the new file gets `children = [ ];`. Send it to `POST /projects?parent_id=<id>`; `parent_id` defaults to the root folder 0. The request creates the file and links it under the parent in one commit, `Create project <id> in project <parent>`. Without `projects/0.nix` the default parent fails with 400 `Project 0 does not exist.`, and a legacy parent file fails with 400 `projects/<p>.nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it.`

Then send a step body to `POST /step?project_id=<returned-id>`; without `project_id` the step is linked nowhere, so no folder lists it and no status broadcast covers it:

```json
{
  "type": "hello",
  "name": "First greeting",
  "note": "A small build to check the instance.",
  "args": { "message": "Hello, research!" }
}
```

These requests write and push repository commits. Try client development against a disposable user repository or instance, not a production analysis.

Important distinctions:

- **`PATCH /step?id=…` replaces the saved step record from the submitted JSON; nothing is stripped.** `PATCH /projects?id=…` replaces only `name`, `preset`, and `templates`; its `children` and every other attribute are left unchanged. Neither is JSON Merge Patch. Send all authored fields you intend to keep, and omit computed fields such as `lastModifiedAt` and `validationErrors`. The old advice to convert an evaluated folder's `steps[].def` back into references no longer applies.
- `POST /step?source_id=…` copies `srcFiles/<source_id>/` recursively when it exists. The body still supplies the new step's definition; `source_id` is not a request to infer it all for you.
- Folders are organized with `POST /projects/batch`, an ordered JSON array of tree operations. `REF` is `{ "step": { "id": N } }` or `{ "project": { "id": N } }`:
  - `{ "op": "update", "project": P, "fields": { …ProjectFields } }` replaces name, preset, and templates.
  - `{ "op": "link", "parent": P, "child": REF }` appends a link with `hidden: false` and a null `sortKey`; linking an already-linked child is a no-op.
  - `{ "op": "unlink", "parent": P, "child": REF }` removes the link; it never deletes anything.
  - `{ "op": "order", "parent": P, "children": [REF…] }` places the listed linked children first in the given order; the rest keep their effective order. It need not be a full permutation, and unknown refs are ignored.
  - `{ "op": "hide", "parent": P, "child": REF, "hidden": true|false }`.
  - `{ "op": "delete", "child": REF }` unlinks the child from every parent and deletes it: a step removes `steps/<id>.nix` and `srcFiles/<id>`; a folder removes only its project file, orphaning its children.
  Operations apply in order; the first error aborts the whole batch, and the result is one commit. Parents whose children changed are rewritten in effective order with dense `sortKey` values, and project statuses are broadcast to them after success.
  Errors: 400 `Empty project tree batch` for `[]`, and a servant 400 for an unknown op. The first validation failure returns **409** with plain text: `Step <id> does not exist.`, `Project <id> does not exist.`, `projects/<id>.nix cannot be read: <reason>` (any unreadable project file blocks every delete), `projects/<id>.nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it.`, `Step <id> was deleted earlier in this batch.` or `Project <id> was deleted earlier in this batch.`, `The root project 0 cannot become a child or be deleted.`, `That would introduce a project cycle: a -> b -> a.` (only newly introduced cycles are refused), `Reviewed steps cannot be edited. Remove the review first.`, `Step <a> depends on step <b>, so it cannot be deleted.`, `Failed to evaluate step dependencies: …`, `git rm failed for <path>: …`, and git or push errors.
- Edits target the current repository branch. Appending `commit` to a write route that does not declare it does **not** make the write historical or conditional. There is no general `If-Match` or compare-and-swap edit contract. A request that changes nothing creates no commit and still reports success.
- Status codes differ by route: `POST /projects` and `POST /step` map every transaction failure, including push conflicts, to **400**; `PATCH /projects` maps every failure to **500**; `/projects/batch` and `PATCH /step` use **409** for transaction failures, and `PATCH /step` adds 400 for invalid JSON or a bad download URL and 500 for step-config decode errors.

Inputs from other steps are JSON objects such as `{ "step": 42 }`; several inputs are an array of those objects. Download steps take `args.url`, while the backend supplies the recorded file hash and download time in `args.downloaded` (`url`, `hash`, `downloadedAt`). Uploads use a separate multipart request. Do not treat generated download details or review annotations as ordinary editable form fields.

## Running work

`POST /run-step?id=42&commit=<hash>` starts asynchronous evaluation and scheduling. A successful empty response means the request was dispatched, **not** that scheduling or execution succeeded; it is returned before the step or commit is even read. The backend builds missing upstream dependencies too, reusing valid build targets and in-flight jobs.

Listen for status snapshots, and use `GET /step-log?id=42&commit=<hash>` for a build log. On revisions whose stdlib publishes `pointy.schemaVersion`, the logged derivation is the step's certificate; a failed prerequisite appears as `Build prerequisite failed: <drv>` followed by `-----` and the log. A missing log returns 404 with `No build log available for step <id>`; it does not establish that the step succeeded.

`POST /stop-step?id=42&commit=<hash>` targets the build for that revision. It raises a per-step stop flag that halts an in-flight run of that id at any commit, cancels the build target's Slurm job and the extras job with `scancel --name=`, and, when the step is not tracked as building, broadcasts its projects. It runs synchronously and always returns 200. Use the same commit you used to run or inspect it. A client displaying a reviewed step must explicitly use its reviewed revision to read or rebuild the approved output; ordinary file/build endpoints do not automatically substitute that revision for you.

## Recording a review

`GET /project-review?project_id=…&commit=…` returns a map keyed by step-id strings, covering only the folder's direct step children at the viewed commit. Each report has `reviewedRevision`, `reviewedBy`, `reviewComments`, `reviewedStatus`, `reviewedStatusError`, `reviewedCertificate`, `comparison`, and `comparisonDetail`; an unreviewed step's review fields are null. Reviews themselves are read from the branch tip; `commit` selects the revision being compared with them. `reviewedStatus` can be any of the six step statuses, computed at the reviewed revision, and `reviewedCertificate` is that revision's certificate store path, or null when it cannot be resolved.

Comparison values are `no-review`, `same-out-path`, `same-content`, `different-content`, `viewed-output-unbuilt`, `reviewed-output-unbuilt`, and `unresolvable`, decided in this order: no review; either side unresolvable; the reviewed output missing from the store; the same output path; the viewed output missing from the store; the same content; otherwise different content. Do not collapse missing outputs or evaluation failures into “different”.

`GET /step-comparison?id=42&revision=<hash>&commit=<hash>` makes the same comparison for one step between any two revisions and returns `{ "comparison": …, "comparisonDetail": … }`: `revision` takes the reviewed side and `commit` the viewed side, so `viewed-output-unbuilt` means the output at `commit` is not in the store, and `no-review` never occurs. The artifact viewer uses it with the displayed commit as `revision` and the branch head as `commit`.

`POST /step-review?id=42&commit=<viewed-hash>` takes:

```json
{ "reviewedBy": "Researcher name", "reviewComments": "Checked the report and inputs." }
```

The name must be nonblank (`Name who reviewed the step.` otherwise). It is entered text, not a verified identity. The JSON boolean response has a specific meaning: **`false` means the review was recorded; `true` means the output differs and the existing review was left unchanged**. A new review requires an available built output; missing or unresolvable output returns 409, including `Build the step before reviewing it. Output not in the store: <path>`, `Build the output at <rev> before reviewing it.`, and `Step <id>: the reviewed revision has no built output (<path>). Rebuild the reviewed revision or remove the review.` Unlike other routes, `commit` defaults to the literal `HEAD` and is resolved to a full hash before storing; the commit message is `review step <id> by <name>`.

`DELETE /step-review?id=42` removes the review and unlocks the step; it returns 200 and does nothing when there is no review. `GET /step-review-diff?id=42&commit=<viewed-hash>` serves the reviewed-vs-viewed diffoscope report as `text/html; charset=utf-8` with `Content-Security-Policy: sandbox allow-same-origin; default-src 'none'; img-src data:; style-src 'unsafe-inline'` and `X-Content-Type-Options: nosniff`. It requires both outputs to exist and differ; preparation failures return 500 with text such as `This step has no review, so there is nothing to compare it with.`, `The viewed output is not built (<path>). Build the step to compare it.`, `The reviewed and viewed outputs are identical.`, or `Output comparison failed: …`. This raw route is another gap in the generated operation list. Use it only after the comparison reports `different-content`.

## Errors and empty responses

Check HTTP status before decoding. Successful record/configuration reads return JSON, but commit hashes, build logs, uploads, and most error bodies are text. There is no shared JSON error format, and servant error bodies carry no Content-Type. Operations documented as returning no content return **200** with an empty body; `/agent/steer`, `/agent/auto-apply`, and `/token-auth` answer **204**, and `/token-login` and `/token-logout` answer **303** on success. The real status mapping varies by handler: `POST /projects` and `POST /step` map transaction failures to 400, `PATCH /projects` to 500, `/projects/batch`, `PATCH /step`, `/src-files` writes, and `DELETE /step-review` to 409, and uncaught exceptions return 500 `Something went wrong`. Cold prefetch adds 502 for a failing cold store and 503 when none is configured; `/rdf/artifact` returns 404 for a malformed route or unavailable commit and 500 when evaluation fails; the token routes return 401 for a missing or invalid token or cookie.

Typical failures include 400 for malformed input or invalid download URLs, 404 for missing files/sessions/logs, 409 for review locks, cold files, or conflicting workflow state, and 500 for evaluation or operational failures. Keep the error text for diagnosis. Do not automatically replay a mutating request merely because the connection failed; first check whether the write reached the repository. Commits land on the backend's local branch before the push, so a commit whose push failed can still appear in unpinned reads until the next fetch reconciles it.

## Links into Pointy

These are **frontend** routes, not API endpoints:

| URL or parameter | Meaning |
| --- | --- |
| `/` | The root folder, Home. |
| `/project/<p1>/…/<pn>` | Folder page; the current folder is the last id, and `/project/1?commit=<hash>` opens it read-only at a revision. |
| `hi=42/results.csv` | Highlight a step's output file; `hi=42` selects the step. There is no `out/` prefix here. |
| `hi=src/42/script.py` | Highlight a source file. |
| `lines=6-9` | Select 1-based text lines on the highlighted file; a single line can be `lines=6`. Reversed ranges are normalised, and `lines` applies only together with `hi`. |
| `compareLeft=out/42/a.csv` and `compareRight=src/43/b.csv` | The two comparison targets. Both sides are required. An output target also needs its `compareLeftCommit` or `compareRightCommit`; source targets may carry an optional commit. Optional `…Mime` values select MIME handling. |
| `/artifact/<p1>/…/<pn>/<stepId>/<commit>?path=report/index.html` | Full-window output preview with a link back to Pointy; the project path may be empty, as in `/artifact/42/<hash>?path=report/index.html`, and `path` is omitted when the whole output is shown, as in `/artifact/42/<hash>`. |
| `chat=<session-id>&turn=<turn-id>` | Open a chat and optionally highlight a turn on the current page. Unknown query parameters are preserved. |

URL-encode path segments and query values. Prefer the built-in Share and Compare controls when you are not writing a client: they select the correct revision and preserve the surrounding folder context.
