# Type Reference

This page documents the option types that template authors use inside `options.pointy.<name>`. They are available as `config._pointy.lib.types` inside a module, so templates typically open a `with config._pointy.lib.types;` block in their `options` section.

Pointy serializes these types into `.#pointy.stepConfig`, which the frontend uses to decide which widgets to render.

## Top-level template metadata

These fields sit at the top level of a template file, alongside `sortKey`, `pointy.type`, and `module`:

- `displayName` (optional) — short label shown in table headings, modal titles, and the type chip in step selectors. When omitted, the raw attribute name is used.
- `description` (optional) — intro paragraph rendered in the create/edit modal.
- `sortKey` (optional) — integer that orders step-type sections in the project view.

Example:

```nix
{
  sortKey = 3;
  displayName = "Alignment";
  description = "Align reads to a library with STAR.";

  pointy.type.derivation = { };

  module = { ... };
}
```

For per-option labels and hints, see `pointy.string` / `pointy.step` below.

## Step type declarations

These are set at the top level of a template file, outside `module`, to tell Pointy what kind of step the template represents.

### `pointy.type.derivation`

```nix
pointy.type.derivation = {
  withSrcFiles = false; # default
};
```

This declares a runnable derivation step.

- `withSrcFiles = true` makes Pointy symlink every top-level entry from `srcFiles/<step-id>/` into the build working directory before the build runs, and the frontend shows a **Source Files** section for that step type.

### `pointy.type.fileUpload`

```nix
pointy.type.fileUpload = {
  allowedExtensions = [ ".fastq.gz" ".fastq" ];
};
```

This declares a file upload step.

- `allowedExtensions` restricts the frontend file picker.
- file-upload templates usually also declare an `uploaded` option of type `lib.types.package`; Pointy fills that option automatically.

At build time, `cfg.uploaded` is a Nix store path pointing at the uploaded payload directory.

---

## Option types

These are used inside `options.pointy.<name>` to declare the arguments that users fill in through the UI form.

### `pointy.string`

```nix
lib.mkOption {
  type = pointy.string {
    displayName = "Extra STAR args"; # optional, used as form label
    description = "Help text shown under the field.";
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
    displayName = "Reads"; # optional, used as form label
    description = "Help text shown under the field.";
    allowedTypes = [ "typeA" "typeB" ]; # optional
  };
}
```

This renders a step selector filtered by `allowedTypes`. If `allowedTypes` is omitted, any step type is allowed.

Important frontend behaviour: the selector offers steps that are already assigned to the **current project**. If you need to reference a step from another project, first add it to the current project with **Add from other project**. See [Building Workflows (Steps)](steps.md#creating-steps).

At build time the selected value resolves to the Nix store path of the chosen step's output, so it can be used directly in `installPhase`.

### `pointy.listOf`

```nix
lib.mkOption {
  type = pointy.listOf (pointy.step {
    displayName = "Step deps";
    description = "Other steps to symlink into the build directory.";
  });
  default = [];
}
# or a list of strings:
lib.mkOption {
  type = pointy.listOf (pointy.string {
    displayName = "nixpkgs deps";
    description = "Attribute paths into pkgs.";
  });
  default = [];
}
```

This wraps another Pointy type to make it repeatable. The UI renders an add/remove list, and the final value becomes a Nix list of the resolved inner values. The inner type's `displayName` and `description` apply to the whole list field.

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
