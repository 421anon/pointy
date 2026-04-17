# CLI Reference

The user repository is a regular Nix flake, so you can inspect it directly with `nix`.

All commands below assume you are in the root of the user repository. Replace `<id>` with a numeric step or project identifier such as `42`.

For how these outputs are created, see [Setting Up the User Repository](user-repo-setup.md). For the meaning of template types, see the [Type Reference](type-reference.md).

---

## Flake outputs

### System-independent outputs (`.#pointy.*`)

| <div style="width: 220px">Attribute</div> | What it contains                                                                                                                                                                     |
| -------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `pointy.stepConfig` | Template schema derived from `templates/`: step kinds, argument names, types, and display hints.                                                                                     |
| `pointy.stepDefs`   | Evaluated step definitions from `steps/`: type, name, and raw argument values for every step.                                                                                        |
| `pointy.projects`   | Evaluated project definitions from `projects/`: project metadata plus assigned steps, per-project hidden flags, and sort order.                                                      |
| `pointy.srcFiles`   | The evaluated source-files path used by builds. Because flakes copy local sources into the store, this is usually a `/nix/store/...-srcFiles` path, not your editable checkout path. |

### Per-system outputs (`.#pointy.*`)

| <div style="width: 220px">Attribute</div> | What it contains                                                                                                                                                               |
| ------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `pointy.steps.<id>`      | The fully evaluated derivation for step `<id>`. This is the same flake output the backend builds; the backend simply uses a pinned Git commit and runs it under `systemd-run`. |
| `pointy.projectOutPaths` | A nested map of `project-id → step-id → store-path`. Steps that cannot be evaluated are reported as `/invalid`.                                                                |

---

## Building steps

### Build a step from your current checkout

```sh
nix build .#pointy.steps.42
```

This creates a `./result` symlink pointing to the built output in the Nix store.

### Build the same output from a pinned commit

```sh
nix build "git+file://$PWD?rev=<commit-hash>&allRefs=true#pointy.steps.42"
```

This mirrors the backend's commit-pinned evaluation model more closely than building from the working tree.

### Get a step's output path without building

```sh
nix eval --raw .#pointy.steps.42.outPath
```

This returns the expected store path for step `42`. The path is deterministic, but it will only exist in the local store after a successful build.

### Check whether that output already exists in the store

```sh
nix path-info "$(nix eval --raw .#pointy.steps.42.outPath)"
```

---

## Inspecting configuration

### Show all template schemas

```sh
nix eval --json .#pointy.stepConfig | jq .
```

This returns a JSON object keyed by template name. It is the same schema the frontend reads to decide which widgets to render.

### Show all step definitions

```sh
nix eval --json .#pointy.stepDefs | jq .
```

### Show one step definition

```sh
nix eval --json .#pointy.stepDefs.42
```

### Show all project definitions

```sh
nix eval --json .#pointy.projects | jq .
```

### Inspect a step's Pointy metadata

```sh
nix eval --json .#pointy.steps.42.meta.pointy
```

This shows the `type` and `id` exported through `passthru.meta.pointy`.

### Show the evaluated `srcFiles` path

```sh
nix eval --raw .#pointy.srcFiles
```

This is useful for seeing what build-time path the flake exposes. If you want to **edit** source files, use your repository checkout, not the `/nix/store/...` path returned by this command.

---

## Inspecting build behaviour and outputs

### List files in a step's output

```sh
nix build .#pointy.steps.42 && ls ./result
```

### Inspect the evaluated `installPhase`

```sh
nix eval --raw .#pointy.steps.42.config.mkDerivation.installPhase
```

This shows the evaluated `installPhase` string for step `42`. It is a good first stop when debugging template behaviour.

Note that helper scripts created elsewhere in the template may still appear here as store paths rather than inline shell code.

### Show all project output paths

```sh
nix eval --json .#pointy.projectOutPaths | jq .
```

This returns a nested map of `project-id → step-id → store-path`. Paths for steps that cannot be evaluated are reported as `/invalid`.

---

## Tips

- Use `nix eval --json ... | jq .` for readable interactive inspection.
- `nix show-derivation .#pointy.steps.42` shows the full `.drv` for a step.
- The web UI's share links and read-only commit views are backed by the same commit-pinned flake state shown above.
