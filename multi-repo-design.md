# Multi-Repo Pointy: Interdependent Repositories with Separate LLM Visibility

Design for giving Pointy a multi-repo capability:

- Repos live side-by-side in one running instance, each with a user-facing name binding.
- A repo declares pointy repos it depends on **via its flake inputs + an additional stdlib declaration**.
- Rules for a declared dependency:
  1. The dependent repo can refer to the dependency's steps, templates, and all domain objects **under a namespace**.
  2. If the dependency is **co-managed** in this running instance, Pointy **ignores the dependent's flake pin** and always evaluates the dependent **against the fresh version** of the dependency.
  3. Whenever Pointy **commits in the dependent**, Pointy **updates the declared dependency's flake pin** to the latest version.
- Step ids become **namespaced**.
- A repo can be marked **confidential**; any repo that is confidential, or is downstream of a confidential repo, **cannot be seen by the configured LLM agent**.

This document covers the backend model, the stdlib contract, evaluation, commit-time pinning, confidentiality, frontend impact, migration, and an appendix with the empirically verified Nix mechanisms.

---

## 1. Goals and Non-Goals

Goals:

- Multiple user repos per running instance, managed under one backend.
- First-class "repo A is a dependency of repo B" relationships with live freshness while co-managed, and durable pins on commit.
- LLM agent isolation for confidential repos and everything downstream of them.
- Namespaced, unambiguous step identity across repos.

Non-goals (out of scope for this design):

- Cross-repo *projects* (a project lives in one repo; project data stays per-repo).
- User-facing (non-LLM) access control between repos — visibility to the web UI is unchanged; confidentiality applies only to the LLM agent, per the requirement.
- Pointy hosting/enriching non-pointy flakes; dependencies are pointy user repos.
- Solving Nix input override at `builtins.getFlake` time (Nix does not support it — see §3.1); we route around it with view flakes.

---

## 2. Current Architecture (one repo)

Everything today is a single bare clone `$HOME/user-repo.git` (config `[user-repo] {url, keyfile, branch}`), guarded by `$HOME/user-repo.lock`.

- **Reads**: `withReadRepoTransaction` (UserRepo.hs:323-337) — shared flock, `git rev-parse <branch>`, yields `ReadRepoContext repoPath commitHash`. Handlers evaluate flake attrs `#pointy.<…>` against `git+file://<repoPath>?rev=<commit>&allRefs=true` via the pooled `nix repl` evaluator (`repoSource`/`mutableRepoSource`, NixEvaluator.hs). Results are cached per `RepoSource` in `RevisionResultCache` (max 8 revisions).
- **Writes**: `withWriteRepoTransactionRaw` (UserRepo.hs:349-377) — exclusive flock, temp git worktree on `<branch>`, then `commitAndPushChanges` (add -A → commit → push with rebase retry).
- **Step identity**: integers. `steps/<id>.nix`, `projects/<id>.nix`. Attrs: `#pointy.stepDefs.<id>`, `#pointy.steps.<id>` (+ `.outPath`, `.requirements`, `.meta.pointy.extras.*`), `#pointy.dependencies.<id>` (`[Int]`, same-repo), `#pointy.projects`, `#pointy.projectOutPaths.<pid>`, `#pointy.stepConfig`, `#pointy.presets`, `#pointy.autocomplete.<t>.<f>`, `#pointy.srcFiles`. Source files: `srcFiles/<stepId>/<rel>`.
- **Agent**: runs `pi` (numtide/llm-agents) inside an `sbox` sandbox with CWD = a worktree of the single repo; the repo's own content (AGENTS.md, steps, srcFiles, flake) reaches the model via the agent's own file tools. Sessions live under `~/agent-sessions/<sid>/`; warm sessions under `~/agent-sessions/warm-template`.
- **flake.lock**: never read or written by the backend today (verified: zero references).

---

## 3. Registry and Evaluation — the Core Design

### 3.1 Why a view flake (and not `--override-input`)

Nix offers `--override-input <input> <flake-url>` (implies `--no-write-lock-file`) for evaluating a flake with an input replaced. We verified it works for one-shot `nix eval` (Appendix A.2). But Pointy evaluates through **pooled, long-lived `nix repl` sessions** that call `builtins.getFlake "<installable>"`. We verified empirically that **`--override-input` on a `nix repl` process does NOT propagate into `builtins.getFlake` calls** inside the repl (Appendix A.3). `builtins.getFlake` is the only evaluation hook Pointy has, and its `--override-input`-equivalent requires *re-locking* the repo's input graph, which the pooled repl cannot express dynamically.

The mechanism that **does** work inside `builtins.getFlake` is a **generated view flake** that re-exports a repo's `pointy.*` outputs while rewiring selected inputs through Nix's native `follows` mechanism (Appendix A.4-verified):

```nix
# ~/views/<repo>.git — generated per (repo rev, fresh-dependency set), committed, content-addressed.
{
  inputs = {
    B.url = "git+file:///home/backend/repos/<repo>.git?rev=<commit>&allRefs=true";
    # Each co-managed declared dependency is re-pinned to the LIVE local clone:
    B.inputs.a.follows = "A";                                    # ignore repo's pin for input a
    A.url = "git+file:///home/backend/repos/<dep>.git?rev=<fresh>&allRefs=true";
  };
  # Re-export ONLY the pointy output: everything else is delegated to B's own flake.
  outputs = { self, B, A, ... }: {
    pointy = B.outputs.pointy;
  };
}
```

Pointy evaluates `#pointy.steps.<id>` etc. against the view's installable `git+file://~/views/<repo>?rev=<viewCommit>&allRefs=true` instead of against the repo directly. The view is itself a tiny git repo so it is **immutable and content-addressable**, exactly like the `RepoSource` cache expects. Evaluation runs entirely offline:

- B's own inputs resolve from **B's committed flake.lock** (unchanged behavior, fetch-from-cache).
- Dependencies with `follows` resolve from the view's pinned local clones.
- No flake.lock is written anywhere (`builtins.getFlake` on a pinned rev does not modify the source tree; verified Appendix A.4).

### 3.2 Repo registry and layout

`Config` replaces the single `[user-repo]` with a `repos` map plus a `default-repo` pointer.

```toml
[repos."edit-tooling"]
url = "git@example.com:org/edit-tooling.git"
keyfile = "/home/backend/.ssh/id_ed25519_deploy"   # per-repo optional; defaults to first/global key
branch = "main"
confidential = false                                # LLM visibility flag, admin-controlled (§7)

[repos."datasets"]
url = "git@example.com:org/datasets.git"
branch = "main"

default-repo = "edit-tooling"                       # optional; legacy single-repo default
```

Layout on disk (names are the user-facing bindings):

- `~/repos/<name>.git` — bare clone (replaces `~/user-repo.git`)
- `~/repos/<name>.lock` — per-repo flock (replaces the single `~/user-repo.lock`; independent repos commit in parallel)
- `~/views/<name>.git` — the evaluation view repo for `<name>` (one view commit per (repo rev, fresh-dep set))
- `~/agent-sessions/<sid>/` — unchanged, but sessions record the target `RepoName`

Locks become per-repo. Cross-repo coordination is read-only and cheap:

- Committing in B reads A's **HEAD** (shared lock on A) to decide freshness and to compute the pin update — it never needs A's write lock.
- A's head is stable for the duration of a B commit because any commit to A itself takes A's exclusive lock; B reads the committed HEAD (never worktrees).

`RepoContext`/`ReadRepoContext`/`WriteRepoContext` gain a `repoName :: RepoName` field, and `withReadRepoTransaction`/`withWriteRepoTransaction{…}` take a `RepoName` parameter. `RepoSource` becomes structured:

```haskell
data RepoSource = RepoSource
  { srcCacheable  :: Bool
  , srcRepo       :: RepoName        -- which repo this evaluates
  , srcCommit     :: CommitHash      -- the pinned rev of srcRepo
  , srcOverrides  :: Map FlakeInputName (RepoName, CommitHash) -- fresh co-managed deps (empty => direct eval)
  , srcInstallable:: String          -- derived: direct URL or view URL
  }
```

### 3.3 The stdlib declaration (dependency contract)

The repo declares dependencies in the **pointy stdlib config** consumed by its flake. The stdlib exposes the mapping to the backend as a JSON attribute **`#pointy.deps`**:

```nix
# repo B's flake, using the pointy stdlib
{
  inputs = {
    a.url = "git@example.com:org/edit-tooling.git";   # flake wiring
    b.url = "git@example.com:org/datasets.git";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };
  outputs = { self, a, b, ... }@inputs:
    pointyStd.lib.pointy { inherit inputs; } {
      deps = {
        # namespace → { flake input, pointy repo identity }
        edit-tooling = { input = "a"; repo = "edit-tooling"; };
        datasets     = { input = "b"; repo = "datasets"; };
      };
    };
}
```

Backend contract — evaluating `#pointy.deps` in B yields:

```json
{
  "edit-tooling":   { "input": "a", "repo": "edit-tooling" },
  "datasets":       { "input": "b", "repo": "datasets" }
}
```

- Keys are **namespaces** (the name B uses when referring to the dependency).
- `input` — the flake-input key in B's `flake.nix` (the "under the hood" translation: user-facing name binding → repo-level flake-input name).
- `repo` — the dependency's registry name. Optional: if absent, Pointy resolves the input's locked git URL against the registry to find the repo (robust to renames).
- `#pointy.deps` must evaluate from **B's own committed state only** (no forced evaluation of dependency content), so it resolves offline against B's lock.

Granting ("refer to its steps, templates, all domain objects under this namespace") is a **stdlib-side** feature:

- The stdlib mounts each dependency's `pointy` output under a `namespaces.<ns>` attrset inside B's evaluation, e.g. `pointy.namespaces.edit-tooling.steps`, `.templates/.stepConfig`, `.projects`, `.presets`, `.srcFiles` — "all domain objects".
- Cross-repo references in B's step JSON (templates, step dependencies) are namespaced strings (`edit-tooling/aln`, `edit-tooling/12`) that the stdlib resolves through the mounts.

Pointy reads `#pointy.deps` for every registered repo at each committed head (same pattern as `#pointy.projects` today, OutPaths.hs:111-115) to build the **dependency graph**: namespace → input → repo → behind the scenes, the view wiring.

### 3.4 Fresh evaluation (co-managed ⇒ ignore pin)

- Define **co-managed**: the dependency's `repo` is registered in this instance's config (its local clone exists). Everything else keeps B's committed pin (direct eval, exactly today's behavior).
- For each co-managed dependency, read its **live HEAD** (`git rev-parse <branch>` under A's shared lock).
- If every co-managed dep already matches B's *committed pin for that input* (i.e. the lock node for `a` has `rev == fresh`), evaluate B **directly** (pin is already fresh — no view churn).
- Otherwise, generate/refresh B's **view** commit encoding the override set and evaluate through the view (§3.1).

Because the view is content-addressed by `(B-rev, overrides)`, the existing `RevisionResultCache` (max 8) and `rewarmRevision` paths keep working with zero changes. Advancing A's head, or B's head, merely produces a new view commit → new `RepoSource` → fresh evaluation. The **entire** dependent evaluation (stepDefs, steps, projects, templates, presets) sees the fresh dependency — this is the requirement "always evaluate me against the fresh version."

### 3.5 Commit-time pin update

"Whenever you commit in me, update my declared dependency's flake pin to the latest version."

Inside `commitAndPushChanges` for repo B (under B's exclusive lock), after `git add -A` and **before** commit:

1. For each declared, **co-managed** dependency input `a` with live head `h`:
   ```
   nix flake lock --update-input a \
     --override-input a "git+file://~/repos/<dep>.git?rev=<h>&allRefs=true"
   ```
   (run in the B worktree). Verified behavior (Appendix A.5): this rewrites B's `flake.lock` node for `a` to `rev = h`, **preserves the `original`/flake.nix declaration** (so the lock remains self-consistent and a non-overridden eval honors it), computes `narHash`/`lastModified`/`revCount`, and works fully offline because the override pins the local clone.
2. **Normalize** the lock node's `locked.url` back to the dependency's real remote (from the registry), keeping `rev = h`. Motivation: the raw command records `locked.url = file://~/repos/<dep>.git` (machine-specific); rewriting it to the fetchable remote keeps B's lock portable for clones outside Pointy. `original` stays equal to B's `flake.nix` declaration, so plain Nix eval elsewhere resolves the same rev from the remote.
3. `git add flake.lock` and continue the normal commit+push.

Semantics: the committed state of B is always self-contained and reproducible (evaluating B *not* co-managed, or after copying the repo elsewhere, reproduces the exact dependency revisions that live evaluation used). Freshness only ever lags by the uncommitted interval, during which Pointy still evaluates B through the view (fresh). For co-managed deps whose remote is behind the local head, the pin update may fail or pin a not-yet-pushed rev — see Risks (§8).

If the pin update fails (e.g. lock write error), the whole B transaction fails exactly like today's eval-on-commit validation — no partial commits.

---

## 4. Namespaced Step Ids

Step identity becomes `<namespace>/<id>` where the leading segment is the owning repo's name binding (default repo mentioned when creating steps in it).

### 4.1 Wire format

- Newtype `StepId :: StepId RepoName Int`, serialized as the string `"<repo>/<id>"` (and `"<id>"` → the default repo, kept for back-compat in API inputs).
- Step id appears as one opaque `Text`/string in:
  - API query params: `/step?id=<repo>/<id>&commit=…`, `/run-step`, `/step-log`, `/step-files`, `/src-files`, `/notices`, `/step-status-stream?project_id=…`, agent referents.
  - JSON/SSE payloads: `stepRecord.id`, `stepRef`, run/status snapshots (currently Int fields in Decode.elm:45-59, Encode.elm:12-13).
  - Project step refs: `projectDef.steps[].ref`.
- Internal Int ids remain per-repo (files `steps/<id>.nix`), scoped by the owning repo. `getNextStepId` (Steps.hs:242-251) becomes per-repo (max+1 over that repo's `steps/` dir).

### 4.2 Resolution to evaluation and storage

A `StepId (RepoName r) i` resolves **within repo r's context**:

- Evaluation: repo r's `RepoSource` (`r`, commit, overrides) + attr `#pointy.steps.<i>`, `#pointy.stepDefs.<i>`, etc. — i.e. through r's view when r has co-managed deps.
- Source files: `srcFiles/<i>/<rel>` in r.
- Outputs/store: `StepId` namespaced into the existing `BuildKey`/`JobComment` (BuildRunner.hs:40-70) so Slurm and build logs disambiguate.

### 4.3 Cross-repo step dependencies

`#pointy.dependencies.<id>` becomes a list of **namespaced** ids: `[{"repo": "edit-tooling", "id": 12}, …]` (today: `[Int]`, RunStep.hs:313-340). Consumers:

- `RunStep.getDependencies` — parse namespaced ids, ensure the dependency's repo is co-managed (dependency steps are only resolvable against a fresh/cached evaluation of the dep), evaluate the dep's step within its own context, and schedule accordingly.
- The stdlib resolves B's in-flake references to `edit-tooling/12` via the namespace mounts, so B's own `.nix` files carry namespaced *stepDef* references as plain Nix.

---

## 5. Frontend Changes

From the frontend audit (Model/Core.elm, Api/Api.elm, Route.elm, Actions.elm):

- **Model**: replace the two scalar `ApiData UserRepoInfo` / `ApiData commitHash` fields with per-repo maps (`Dict RepoName …`), initialized by a new `GET /repos` that returns `[{name, url, branch, confidential, isDefault}]` for the workspace UI (replaces `GET /user-repo-info`).
- **Identity**: step id keys (`uploadProgress`, `stepLogs`, `notices`, `stepStatusHooks/Buffer`, `runningStepIds`, SSE snapshots) switch from Int to the namespaced string id; `Dict Int` → `Dict String`. `TStep` step refs change to the string form.
- **Routes**: add an optional repo segment `/:repo` before `/project/:id` etc. (Route.elm:263-330, 412-444). ~10-15 mechanical sites per the audit.
- **Status bar**: show `repo/branch @ short-commit` (StatusBar.elm:109-152); repo switcher in the workspace header.
- **Workspace init**: `initializeWorkspace` (Main.elm:58-65) runs once per workspace and loads all repos instead of gating on a single `userRepoInfo`; default repo drives the initial landing.
- **Confidential repos** still appear in the UI (confidentiality is LLM-only) but are **not offered as agent session targets**.

---

## 6. Change Plan by Backend Module

| Module | Change |
|---|---|
| `Config.hs` | `repos :: Map RepoName RepoConfig` (+`confidential`, optional per-repo keyfile), `defaultRepo`; keep `[user-repo]` as an alias for `default-repo` during migration. |
| `UserRepo.hs` | Per-repo paths (`~/repos/<name>.git`, `.lock`), per-repo locks, `RepoName` in contexts, `repoHead :: RepoName -> IO CommitHash` (shared-lock read), commit timeout/push using the repo's keyfile. `commitAndPushChanges` gains the pin-refresh step (§3.5). |
| `RepoGraph.hs` (new) | Load `#pointy.deps` per repo at committed head; build dependency DAG; cache per (repo, commit); acyclicity check; `coManaged :: RepoName -> Set InputName`; `isEffectiveConfidential :: RepoName -> Bool` (transitive closure over deps). |
| `EvalViews.hs` (new) | Generate/commit view flakes (`~/views/<name>.git`) from (repo, commit, overrides); content-addressed; `viewInstallable :: RepoSource -> String`. Decide direct-vs-view (§3.4). |
| `NixEvaluator.hs` | `RepoSource` gains repo/overrides; unchanged repl/worker machinery — the installable already carries the full identity. |
| `ApiTypes.hs` | `StepId`, `RepoInfo`, namespaced request/response types; `Repos` endpoint. |
| `Api.hs` | `/repos` (replaces `/user-repo-info`); step-id params become namespaced strings; `?id=<repo>/<id>` parse. |
| `Handlers/{Steps,RunStep,Store,SrcFiles,Upload,ProjectEntities,Statuses,StatusStream}` | Route `StepId` → `(RepoName, Int)`; target repo drives `withRead/WriteRepoTransaction repo`; `#pointy.*` attrs unchanged within the repo's view context. `dependencies` decode namespaced ids. |
| `Handlers/Projects.hs` | Project CRUD scoped to one repo (project id already global-ish, but owned by a repo; `#pointy.projects` evaluated in that repo's context). |
| `Handlers/Agent.hs`, `Agent/*` | Session binds a target `RepoName`; refusal for effective-confidential targets; sandbox binds only the target's repo + view; write whitelist stays per-repo; apply/commit targets the session's repo. |
| `OutPaths.hs` | Warm/outPath eval per repo (repo id in warm state key). |

---

## 7. Confidentiality Model (LLM Visibility)

**Admin policy flag** `confidential` lives in the Pointy instance config (`repos.<name>.confidential`), **not** in the repo's own flake — a repo cannot mark/unmark itself. The configured LLM is the external `pi` process.

### 7.1 Rule

- A repo is **effectively confidential** iff it is marked confidential, or any repo it depends on (transitively, over the declared `deps` DAG) is effectively confidential — i.e. the **downstream closure** of every confidential repo.
- Effective-confidential repos **cannot be seen by the LLM agent**. Everything else is visible.

Caveat the requirement makes explicit: the closure is over the pointy-repo dependency `deps` graph. External inputs (nixpkgs, etc.) never make a repo confidential, and invisible-for-LLM does not mean hidden from the web UI.

### 7.2 Enforcement surfaces (from the agent audit)

| Surface | Enforcement |
|---|---|
| Session creation (`Agent/Git.hs:104-131`) | Reject `createSession` when the target `RepoName` is effectively confidential (HTTP 403). |
| Warm session / bootstrap (`Agent/WarmSession.hs`) | Only ever built for a non-confidential target; sandbox never binds a confidential repo. |
| Sandbox binds (`Agent/Runner.hs:283-298`) | Bind only the **target repo's** bare clone + its view. Other repos' bare clones are not mounted, so the process (and the model) cannot read them. |
| Evaluation for agent sessions | The target's dependency closure contains **no** confidential repo (closure rule), so the agent's view is the same view as interactive — no redaction needed. Confidential views never exist. |
| Apply / agent commits (`Agent/Git.hs:277-396`) | Target repo only; cannot touch confidential repos. |
| `pi` config / API keys | Not repo-specific; unchanged. |

Because any repo that could *reach* confidential content (via `deps`) is itself effectively confidential, the agent's entire reachable universe is confined to the visible subgraph. The one residual channel is the nix **store**: an agent on target C could in principle build outputs of C derived from a confidential dep — but that cannot happen: if C depends on confidential A, C is effectively confidential and no session is allowed. The visible subgraph never references confidential store paths (§8 risk on this).

---

## 8. Risks and Open Questions

1. **`nix flake lock` dependency on nix version.** `--update-input` + `--override-input` semantics (original-preserving) were verified on Nix 2.28.4. Pin this as the minimum backend nix version in the deploy module; add a startup version check.
2. **View churn.** Each (repo commit, fresh-dep-set) change writes a new view commit. Bounded by the 8-rev cache; acceptable — view files are a few lines. Optimization gate in §3.4 avoids churn when deps are already fresh-pinned.
3. **Pin update when a dep's remote is behind the local head.** Pointy's own commits push immediately, so local head ≈ remote; edge cases (failed push) surface as a commit-time error, not silent divergence. The normalization step (§3.5.2) makes the final lock portable; if the remote truly lacks the rev, the next plain eval outside Pointy errors loudly instead of silently pinning to a wrong rev. Consider auto-fetch+verify before normalizing.
4. **Confidentiality ≠ secrecy from local operators.** The rule is "not visible to the *configured LLM agent*." A repo dependent on confidential A is also LLM-invisible even if A itself is fine — operators must mark dependencies accordingly. Document this in the operator guide.
5. **Stdlib contract is new surface.** `#pointy.deps` and the namespace mounts must be implemented in the (external) pointy stdlib and versioned with the backend; the backend treats a missing `#pointy.deps` as "no dependencies" for back-compat.
6. **Step-id migration breaks old URLs/agents.** `/backend/…?id=<int>` still parses (default repo), but `"/<repo>/<id>"` is the canonical form going forward; update frontend + docs in the same release.

---

## 9. Migration & Rollout

1. **Phase 0 — registry + no deps (behavior-preserving).** Introduce `repos` config, per-repo locks/paths, `RepoName`-scoped transactions, direct-eval path (view disabled when `overrides` empty). Single repo = symmetric with today. `[user-repo]` auto-maps to `repos."default"`/`default-repo`.
2. **Phase 1 — namespaced ids.** `StepId` plumbing through API/frontend; `dependencies` decode both shapes.
3. **Phase 2 — deps + views.** `#pointy.deps` reading, view-flake generation, fresh eval, commit-time pin refresh.
4. **Phase 3 — confidentiality.** Config flag + effective-confidential closure + agent refusal + sandbox binds + UI hiding of the agent entry point for confidential repos.
5. **Phase 4 — stdlib rollout** (external): `deps` config, namespace mounts, namespaced template/stepDef resolution.

Operational migration: stop backend → copy bare repos into `~/repos/<name>.git` → write `repos` config → start backend (runs the existing clone/ensure logic per repo). Existing `~/user-repo.git` becomes `~/repos/<default>.git`.

---

## Appendix A — Empirically Verified Nix Mechanisms

Environment: `nix (Nix) 2.28.4`, `nix --extra-experimental-features 'nix-command flakes'` (global flags **before** the subcommand; several earlier failures traced to flag placement/quoting).

Setup: `B` is a flake with `inputs.a = git+file://<A1>` (locked rev = old `A1`, original matches), `A` is the live repo at fresh rev `AH`, outputs `{ pointy.version = "A-v2" }` (A1 says `A-v1`).

**A.1 Pinned eval honors the committed lock.**
`nix eval --raw --no-write-lock-file 'git+file://B?rev=…&allRefs=true#pointy.depVersion'` → `A-v1`.

**A.2 One-shot override works on `nix eval`.**
`nix eval --raw --override-input a 'git+file://A?allRefs=true' 'git+file://B?…###pointy.depVersion'` → `A-v2`.

**A.3 `--override-input` does NOT reach `builtins.getFlake` inside `nix repl`.**
Opened `nix repl --override-input a <A>`; ran `:p builtins.toJSON (builtins.getFlake "git+file://B…").pointy` → `A-v1` (pin used). Conclusion: CLI overrides bind to the session's installable only; Pointy's repl is `getFlake`-based, so a different mechanism is required.

**A.4 View flake + `follows` works through `builtins.getFlake` (Pointy's path).** View `V`:
```nix
{ inputs = {
    B.url = "git+file://B?rev=<BH>&allRefs=true";
    B.inputs.a.follows = "A";
    A.url = "git+file://A?rev=<AH>&allRefs=true"; };
  outputs = { self, B, A }: { pointy = B.outputs.pointy; }; }
```
- `nix eval --raw 'git+file://V?rev=<VH>&allRefs=true#pointy.depVersion'` → `A-v2`.
- `builtins.getFlake "git+file://V?rev=<VH>&allRefs=true"` in a plain `nix repl` → `{"depVersion":"A-v2"}`.
- No flake.lock needed in the view; inputs resolve offline (B's other inputs from B's lock, A/B local).

**A.5 Commit-time pin refresh via `nix flake lock --update-input` + `--override-input`.**
In B's worktree: `nix flake lock --update-input a --override-input a 'git+file://A?rev=<AH>&allRefs=true'` → rewrites B's `flake.lock` node `a` to `rev = AH` with computed `narHash`/`lastModified`/`revCount`; **`original` stays equal to B's `flake.nix` declaration**; afterwards `nix eval` of B **without any override** → `A-v2` (pin holds standalone). Note: `locked.url` records the local override path — Pointy normalizes it to the registry remote (§3.5.2) for portability.

---

## Appendix B — Glossary

- **Repo / repo name** — a pointy user repo registered in the instance config; the user-facing binding.
- **Flake input name** — the key under `inputs` in a repo's `flake.nix` (e.g. `a`).
- **Namespace** — the name a dependent repo uses for a dependency's domain objects (defaults to the dep's repo name); the leading segment of namespaced step ids.
- **Co-managed** — the dependency's repo is registered (cloned) in this running instance ⇒ Pointy overrides/evaluates fresh and updates pins.
- **View flake** — generated flake re-exporting a repo's `pointy` output with selected inputs rewired via `follows`.
- **Effective-confidential** — marked confidential or transitively downstream of one (LLM-invisible).
