# Type Reference

This page documents the option types that template authors use inside `options.pointy.<name>`. They are available as `config._pointy.lib.types` inside a module, so templates typically open a `with config._pointy.lib.types;` block in their `options` section.

Pointy serializes these types into `.#pointy.stepConfig`, which the frontend uses to decide which widgets to render. The current UI uses option names as field labels; `description` values are still carried through the schema, but they are not currently rendered as on-screen labels.

## Step type declarations

These are set at the top level of a template file, outside `module`, to tell Pointy what kind of step the template represents.

### `pointy.type.derivation`

```nix
pointy.type.derivation = { };
# or, to make repository source files available at build time:
pointy.type.derivation = { withSrcFiles = true; };
```

This declares a runnable derivation step.

If `withSrcFiles = true`, Pointy symlinks every top-level entry from `srcFiles/<step-id>/` into the build working directory before the build runs, and the frontend shows a **Source Files** section for that step type.

### `pointy.type.fileUpload`

```nix
pointy.type.fileUpload = {
  allowedExtensions = [ ".fastq.gz" ".fastq" ];
  description = "sequencing reads";
};
```

This declares a file upload step.

- `allowedExtensions` restricts the frontend file picker.
- `description` is carried in the generated step schema.
- file-upload templates usually also declare an `uploaded` option of type `lib.types.package`; Pointy fills that option automatically.

At build time, `cfg.uploaded` is a Nix store path pointing at the uploaded payload directory.

---

## Option types

These are used inside `options.pointy.<name>` to declare the arguments that users fill in through the UI form.

### `pointy.string`

```nix
lib.mkOption {
  type = pointy.string {
    description = "schema metadata";
    display = { ... }; # optional, see below
  };
  default = ""; # optional
}
```

By default this renders a plain text input. The optional `display` attribute changes the widget:

| `display` value                 | Widget rendered                  |
| ------------------------------- | -------------------------------- |
| _(omitted)_                     | Plain single-line text input     |
| `display.command = "tool-name"` | Command-prefixed argument box    |
| `display.textarea = { }`        | Auto-growing multi-line textarea |

### `pointy.step`

```nix
lib.mkOption {
  type = pointy.step {
    description = "schema metadata";
    allowedTypes = [ "typeA" "typeB" ]; # optional
  };
}
```

This renders a step selector filtered by `allowedTypes`. If `allowedTypes` is omitted, any step type is allowed.

Important frontend behaviour: the selector offers steps that are already assigned to the **current project**. If you need to reference a step from another project, first add it to the current project with **Add Existing**. See [Building Workflows (Steps)](steps.md#creating-steps).

At build time the selected value resolves to the Nix store path of the chosen step's output, so it can be used directly in `installPhase`.

### `pointy.listOf`

```nix
lib.mkOption {
  type = pointy.listOf (pointy.step { description = "dependency"; });
  default = [];
}
# or a list of strings:
lib.mkOption {
  type = pointy.listOf (pointy.string { description = "nixpkgs attribute"; });
  default = [];
}
```

This wraps another Pointy type to make it repeatable. The UI renders an add/remove list, and the final value becomes a Nix list of the resolved inner values.

---

## Hidden options

Fields with `visible = false` are omitted from the generated step schema and therefore do not appear in the UI form.

## The `id` option

Every template should declare an `id` option as follows:

```nix
id = lib.mkOption {
  type = pointy.string { description = ""; };
  visible = false;
};
```

Pointy populates `id` automatically with the numeric step identifier.

Templates normally forward it into `passthru.meta.pointy`:

```nix
passthru.meta.pointy = {
  type = "<template-name>";
  inherit (cfg) id;
};
```

This keeps the step id available to templates and to downstream tooling such as scripts that inspect `meta.pointy`.
