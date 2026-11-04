---
description: "Commands for inspecting the user repository with nix: building steps, viewing folders and configuration, and debugging build outputs."
---

# CLI

These commands help administrators inspect a repository, troubleshoot a step, or reproduce a result outside the browser. The user repository is a Nix flake, and the commands use the same definitions as Pointy. Run them from that repository unless a section says otherwise, replacing `42` with your step or folder id.

This is also a useful way to separate a repository problem from a web or backend problem. See [Repository](user-repo-setup.md) for compatible inputs and [Types](type-reference.md) for the generated version 4 form document.

## Flake outputs

### Definitions and form configuration

| <div style="width: 220px">Attribute</div> | What it contains |
| -------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `pointy.schemaVersion` | The version of the output shape, `1`. The backend uses it to detect that this repository publishes step keys and certificates; an older stdlib does not have it. |
| `pointy.agentSchema` | Markdown reference for the embedded agent: the editable step and project shapes, dependency references, apply behavior, and the output list below. The backend does not read it; `nix eval --raw .#pointy.agentSchema` prints it. |
| `pointy.stepConfig` | Version 4 form document: `{ version; templates; }`, generated from language declarations and template bindings. |
| `pointy.presets` | Preset bundles read from `presets/`: display labels, descriptions, sort order, and template lists. |
| `pointy.projects` | Evaluated folder definitions from `projects/`: `name`, the active preset or template list, tagged `children` with each entry's `hidden` and `sortKey`, and `validationErrors`. Includes the root `projects/0.nix` under id `0`. |

### Builds and helper functions

| <div style="width: 220px">Attribute</div> | What it contains |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `pointy.steps.<id>` | The fully evaluated derivation for step `<id>`. A run builds the step's certificate, which builds this output; the backend submits it from a pinned Git commit through Slurm. |
| `pointy.steps.<id>.def` | The step definition from `steps/<id>.nix`: type, name, raw argument values, and review fields, with its integer `id`. |
| `pointy.steps.<id>.meta.pointy` | Resolved step metadata: `id` (string), `type`, `requirements`, and `args`, with `notices` and `extras` when present. |
| `pointy.steps.<id>.key` | The step's input address: a hash of the Nix version, `flake.lock`, the global source, the step file, its `srcFiles/` directory, and its producers' keys. |
| `pointy.steps.<id>.certificate` | The certificate derivation that checks the step's output against every promise its program makes. The backend builds and reports this for the step's status. |
| `pointy.steps.<id>.dependencies` | The step's transitive upstream step ids, as strings, excluding the step itself. |
| `pointy.steps.<id>.srcFiles` | The store copy of `srcFiles/<id>/` that the build links in. Present only for templates with `withSrcFiles` when that directory exists. |
| `pointy.autocomplete.<template>.<key>` | Named suggestion function selected by a field's autocomplete hook. |
| `pointy.steps.<id>.requirements` | Resolved resource requirements for step `<id>`: `cpu`, `ram`, `ior`, and `iow`. Reflects the step-level override when present, otherwise the template-level function. |

### Linked data and dependencies

| <div style="width: 220px">Attribute</div> | What it contains |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `pointy.root` | The folder tree, one node per project record starting at the root project `0`. Each step node is `pointy.steps.<id>` plus `turtle` and `fileTurtle` documents for artifact permalinks. |
| `pointy.staleDependencies` | Dependency references whose recorded `path` no longer holds the referenced step at this commit, sorted by dependent step id. Each entry is `{ step; dependency; }`. |

These attributes live directly under `pointy`, not under a system suffix. The current stdlib builds for `x86_64-linux`.

## Building steps

### Build a step from your current checkout

```sh
nix build .#pointy.steps.42
```

This builds the step and creates `./result` as a symlink to its output. It invokes Nix directly: it does not submit a Slurm job or apply the backend's scheduler resource policy. A Pointy run targets the step's certificate instead, which builds this output and then checks it.

### Build the same output from a pinned commit

```sh
nix build "git+file://$PWD?rev=<commit-hash>&allRefs=true#pointy.steps.42"
nix build "git+file://$PWD?rev=<commit-hash>&allRefs=true#pointy.steps.42.certificate"
```

This uses the workflow definition and flake inputs saved at the named Git commit. The second form is the one the backend submits for a run; the first builds the step's output alone. Unsaved changes in your checkout do not affect either.

### Get a step's output path without building

```sh
nix eval --raw .#pointy.steps.42.outPath
```

The command returns the store path expected for step `42` without building that step. Schema generation may itself build compiler/schema dependencies during evaluation. The step's output exists locally only once it has been built or substituted.

### Check whether that output already exists in the store

```sh
nix path-info "$(nix eval --raw .#pointy.steps.42.outPath)"
```

## Inspecting configuration

### Show all template schemas

```sh
nix eval --json .#pointy.stepConfig | jq .
```

The top-level object contains `version = 4` and `templates`, keyed by template name. Each field carries a `widget` and a `shape`; names and defaults come from the language declaration, while labels and controls come from Nix bindings.

To check compatibility or inspect just the fields:

```sh
nix eval --json .#pointy.stepConfig.version
nix eval --json .#pointy.stepConfig.templates.hello.fields | jq .
```

Replace `hello` with a template key in your repository.

### Show all preset bundles

```sh
nix eval --json .#pointy.presets | jq .
```

### Show one step definition

```sh
nix eval --json .#pointy.steps.42.def
```

### Show all folder definitions

```sh
nix eval --json .#pointy.projects | jq .
```

The object is keyed by folder id; each value carries `name`, the active `preset` or `templates`, the tagged `children` entries, and `validationErrors`. A file in the legacy format fails this evaluation until it is migrated.

### Show a step's input key

```sh
nix eval --raw .#pointy.steps.42.key
```

The key covers the step's own inputs: the repository lock, the global source, the step file, its source directory, and its producers. Pointy caches certificates per key.

### Inspect a step's Pointy metadata

```sh
nix eval --json .#pointy.steps.42.meta.pointy
```

This shows the stdlib's exported metadata, including `type`, `id`, `requirements`, resolved `args`, and any template-supplied `notices` or extras.

### Inspect a step's resolved resource requirements

```sh
nix eval --json .#pointy.steps.42.meta.pointy.requirements
```

Returns the resolved `{ cpu; ram; ior; iow; }` for step `42`. A step-level override takes precedence; otherwise the template's attribute set or function result is merged over the stdlib defaults.

### Show a step's evaluated `srcFiles` path

```sh
nix eval --raw .#pointy.steps.42.srcFiles
```

This is the build-time copy of `srcFiles/42/` the step links in; it is missing when the template does not enable `withSrcFiles` or the directory does not exist. If you want to **edit** source files, use your repository checkout, not the `/nix/store/...` path returned by this command.

### Try an autocomplete function

```sh
nix eval --json --apply 'f: f { query = "py"; limit = 10; }' '.#pointy.autocomplete.<template>.<key>'
```

Autocomplete functions may use additional string or enum fields as context. Pass those fields in the attrset when reproducing a UI request from the CLI.

### Inspect dependencies and reviews

```sh
nix eval --json .#pointy.steps.42.dependencies
nix eval --json .#pointy.steps.42.def --apply 'd: d.reviewedRevision or null'
```

The dependency list includes indirect upstream steps. `reviewedRevision` is absent on unreviewed steps, so read it with the fallback above. A non-null value identifies the approved revision; it does not change what `nix build .#pointy.steps.42` builds from your current checkout. To reproduce the reviewed output, use that revision with the pinned-commit command above.

## Inspecting build behavior and outputs

### List files in a step's output

```sh
nix build .#pointy.steps.42 && ls ./result
```

### Inspect the evaluated `installPhase`

```sh
nix eval --raw .#pointy.steps.42.drvAttrs.installPhase
```

`drvAttrs` exposes the evaluated `stdenv.mkDerivation` attributes; it is not a source reconstruction. The evaluated `installPhase` is often the most informative first stop when a template builds differently from what its source appears to imply. Helper scripts created elsewhere in the template may still be represented as Nix store paths rather than expanded inline.

### Show a folder's steps and their output paths

```sh
nix eval --json .#pointy.projects.7 --apply 'p: map (c: c.step.id) (builtins.filter (c: c ? step) p.children)'
nix eval --raw .#pointy.steps.42.outPath
nix eval --raw .#pointy.steps.42.certificate.outPath
```

A folder record lists its children, not their outputs; read the output and certificate paths from the step attributes directly. The first command lists the step ids linked directly in folder `7`. When a folder has validation errors, inspect `.#pointy.projects.7.validationErrors` first: paths are only meaningful for steps whose folder membership and templates still evaluate coherently.

### Check the language's applications

```sh
nix build .#checks.x86_64-linux.pointy-applications
```

This runs `pointy check --applications` over every step record, using each template's interface, its arguments, and any step-level `refine`. It fails on a `Bottom` edge, never on a residual.

## Day-to-day patterns

Once you are comfortable with the commands above, a few patterns make day-to-day work smoother:

- Use `nix eval --json ... | jq .` for readable interactive inspection.
- `nix derivation show .#pointy.steps.42` shows the full `.drv` for a step.
- The web UI's share links and read-only commit views are backed by the same commit-pinned flake state shown above.
