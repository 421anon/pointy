---
description: "Prepare the Git repository that stores templates, folders, and steps."
---

# Repository

The user repository stores the available analysis tools, the steps researchers create, and the folders that collect them. As the administrator, you set it up and maintain its templates. Researchers use those templates through the agent or the step forms; they do not need to edit the setup described here.

A template has two parts. A Pointy language declaration describes the program's inputs and output. A Nix file chooses how the inputs appear in the browser and how the program runs. The standard library combines them into the configuration Pointy reads.

## Choose compatible inputs

The current frontend reads **step-config version 4**. This guide describes the `main` branch of [pointy-stdlib](https://github.com/421anon/pointy-stdlib), which produces that format. Its schema comes from `pointy-lang`. Older examples using `compile.options` and `pointyLib.types` describe a different API; selecting the stdlib's old branches is not a compatible substitute.

Declare `pointy-stdlib` and let its own lock file choose the language inputs. The stdlib's lock pins `pointy-lang` and `pointy-lang-stdlib`, so a repository that takes its language inputs from `pointy-stdlib` uses the compiler and language library the stdlib was tested with. The `nixpkgs` follow in the example below keeps the repository on the same nixpkgs as the compiler. You normally do not need to override the language inputs.

The two language repositories are private, and the lock file records them as `git+ssh` URLs. Every machine that locks, evaluates, or builds the repository — your workstation and the Pointy host — needs SSH read access to `421anon/pointy-lang` and `421anon/pointy-lang-stdlib`.

For local language development, declare `pointy-lang` yourself and override its `fixtures` and `pointy-lang-stdlib` inputs with absolute paths. Keep the language, stdlib, and language-library revisions together, and do not commit a lock file that depends on paths available only on your laptop.

Use Nix with `nix-command`, `flakes`, and `pipe-operators` enabled. Pointy uses the Nix installed on its host instead of a bundled one, and it leaves the host's `nixpkgs` flake registry alone; pin the inputs your repository needs in `flake.lock` instead of relying on a registry entry. This stdlib fixes the target system to `x86_64-linux`. Generating the argument schema uses import-from-derivation: evaluation may build the language compiler and schema, and must permit that operation.

## What the user repository must contain

Keep the flake and templates separate from the step and folder files that Pointy writes. You can evolve the available tools without hand-editing every step. Source files can be edited through the UI as well as directly in the repository.

At minimum, the user repository needs:

- `flake.nix`
- `flake.lock`
- `pointy.nix`
- `templates/`
- `main.pointy`, the language entry program
- `steps/`, with at least one step file
- `projects/`, including `projects/0.nix` to create or organize folders
- optional `presets/`
- optional `srcFiles/`

`flake.nix` calls `pointy-stdlib.lib.mkFlake { inherit inputs; }`; `pointy.nix` supplies `pkgs` and the language `source` as `{ pkgs; source = ./main.pointy; }`. The stdlib copies `flake.nix`, `pointy.nix`, `templates/`, `packages/`, and `main.pointy` into the store as global sources, so helper files reachable from templates must live under `packages/`; changing any global source changes every step key. The current frontend requires step-config version 4. A stdlib that publishes `pointy.schemaVersion` enables key-based certificate evaluation; older locks fall back to the slower per-project evaluation. Upgrade the notebook and its user-repository inputs together rather than assuming every stdlib branch emits the same format.

For the concrete flake setup, see [Minimal `flake.nix` and `pointy.nix`](#minimal-flake-nix-and-pointy-nix).

Folder definitions must choose either a preset or a custom template list; the root's empty `templates = [ ]` counts as a list. If templates, presets, or step ids disappear from the user repository, Pointy keeps the folder loadable where possible and shows validation errors in the UI. A folder whose header fails to evaluate is served as a placeholder named `Project <id>`.

## Minimal repository structure

Start with:

```text
flake.nix
flake.lock
pointy.nix
main.pointy
templates/hello.nix
steps/1.nix
projects/0.nix
projects/1.nix
```

The examples on this page, [Templates](templates.md), and [Step files](step-files.md) form one small working repository.

- `pointy.nix` is required. It supplies the package set and the entry program.
- `main.pointy` must have that exact name at the repository root. It must be a single file: only this file is copied for evaluation, so it cannot import other paths.
- Every entry in `templates/` is loaded as a template, and the filename is the template key. Put helper Nix files in `packages/`, not here.
- Every regular `<id>.nix` file in `steps/` is a step. Do not put a placeholder such as `.gitkeep` there; it would be read as a step named ".gitkeep" and fail evaluation.
- `projects/` holds folders, including the root `projects/0.nix` ("Home").
- `srcFiles/` and `presets/` are optional; they can be missing until you need them.

`loadDir` imports every entry in `templates/`, `projects/`, and `presets/` as Nix, so do not put placeholder or unrelated files in those directories.

You author the flake, language declarations, and templates. Pointy writes steps, folders, and source files in response to UI actions. You can edit those files outside Pointy too, provided you preserve their structure and push your changes to the configured branch.

## Minimal `flake.nix` and `pointy.nix`

```nix
{
  inputs = {
    pointy-stdlib.url = "github:421anon/pointy-stdlib";
    nixpkgs.follows = "pointy-stdlib/nixpkgs";
  };

  outputs = inputs@{ pointy-stdlib, ... }:
    pointy-stdlib.lib.mkFlake { inherit inputs; };
}
```

```nix
{
  inputs,
  ...
}:
{
  pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
  source = ./main.pointy;
}
```

`source` points to the language file that declares the template inputs. `pkgs` supplies the Nix packages used to prepare those declarations and run the steps. Both are required. Pointy generates the forms from these declarations; you do not maintain a second set of field definitions in the frontend.

The stdlib copies `flake.nix`, `pointy.nix`, `templates/`, `packages/`, and `main.pointy` into one store path and reads them from there, so `pointy.nix` and your templates may reach those paths and nothing else. The steps, projects, and presets are read from the repository itself, one store path per step; a step definition holds everything it needs and cannot import a sibling file.

A repository that needs flake-parts configuration of its own passes it as `modules`, where the loaded `pointy.nix` result is available as `pointyRepo.cfg`:

```nix
outputs = inputs@{ pointy-stdlib, ... }:
  pointy-stdlib.lib.mkFlake {
    inherit inputs;
    modules = [
      ({ pointyRepo, ... }: {
        perSystem = { system, ... }: {
          _module.args.pkgs = pointyRepo.cfg.pkgs;
        };
      })
    ];
  };
```

## Migrating to the folder format

A repository locked to an older stdlib stores its folders as a flat `steps = [ ... ]` list in `projects/<id>.nix`. That shape no longer evaluates: with the current stdlib, `nix eval .#pointy.projects` fails with `error: attribute 'children' missing`. The backend still reads revisions in the old format, so past commits remain viewable, but it refuses to organize such files:

> `projects/<id>.nix uses the legacy steps format. Run pointy-migrate-project-tree to convert it.`

The migration tool is part of the Pointy backend package, not a flake app. It needs `nix` on `PATH`. On a clean checkout of the repository, on the configured branch, update the `pointy-stdlib` input to a revision that reads `children` (the `main` branch this guide describes), run the tool, and check the result:

```sh
nix flake update pointy-stdlib
nix build github:421anon/pointy#backend
result/bin/pointy-migrate-project-tree .
nix eval --json .#pointy.projects
```

The tool:

- refuses to run when `projects/0.nix` already exists (`... already exists; the projects are already in the children format.`) or when the checkout has no `projects/` directory;
- rewrites each `projects/<id>.nix`: the old `steps` entries become tagged `children`, and the project-level `hidden` and `sortKey` are dropped;
- writes `projects/0.nix` ("Home") listing every existing folder with its old placement;
- does not commit or push. It re-renders the files from JSON, so comments are lost and keys are sorted.

Commit the lock bump and the migrated files together, then push.

## After you commit template changes

Evaluate `.#pointy.stepConfig` and build a representative step before pushing a template change. Keep the language declaration and its Nix bindings in the same change, and commit the lock file when inputs change. Once the backend has fetched the revision, reload the UI to pick up the new form configuration.

A change to `flake.nix`, `pointy.nix`, `templates/`, `packages/`, `main.pointy`, or `flake.lock` moves the key of every step. A step file or the `srcFiles/` directory its template links in moves that step's key and the keys of the steps that consume it, and upgrading the Nix on the Pointy host moves every key because the key includes the Nix version. Pointy caches certificates and outputs by key, and resolves a revision's missing certificates in the background; a changed key therefore starts out uncertified and needs a fresh build.

[Forms](forms.md) covers bindings and widgets in detail. [CLI](cli-reference.md) gives commands for inspecting configurations, dependencies, and outputs without going through the browser.
