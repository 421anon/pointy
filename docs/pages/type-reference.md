---
description: "Template metadata, step kinds, contracts, and builder helpers."
---

# Types

This reference is for administrators writing templates. A template should not have to describe the same input twice: the language declaration sets the argument names, types, order, and defaults; the Nix template chooses form controls and runs the program. For a complete example, start with [Repository](user-repo-setup.md).

This reference describes the `main` branch of [pointy-stdlib](https://github.com/421anon/pointy-stdlib) and **step-config version 4**, which the current frontend requires. It does not describe the older `compile.options` API. Keep the stdlib, language, and language-library revisions together; the setup guide explains the current input prerequisites.

## Top-level template metadata

A template is an attribute set loaded from `templates/<name>.nix`. Its filename is the key used by step definitions, preset template lists, and folder template lists.

| Attribute | Meaning |
| --- | --- |
| `displayName` | Human-readable template label. Required; `null` lets the UI use the template key. |
| `description` | Explanation shown with the template. Required; a string or `null`. |
| `sortKey` | Section ordering. Required; an integer or `null`. |
| `icon` | Optional icon name for the template's controls. |
| `contract.interface` | Required interface name declared in the language entry program. |
| `bindings` | Presentation overrides keyed by language parameter name; defaults to `{ }`. |
| `pointy.type` | Exactly one of the step kinds below. |
| `compile` | Function receiving `{ lib, pkgs, pointyLib }` and returning `build`. |
| `requirements` | Optional resource attribute set or function of resolved args. |
| `extras` | Optional `"csv"` or `"fastq"` metadata scan. |
| `autocomplete` | Optional function receiving `{ pkgs, lib }` and returning named suggestion functions. |

The renderer reads `displayName`, `description`, and `sortKey` directly, so all three attributes must exist even when their value is `null`.

Unlike the old typed-template API, these templates are plain attribute sets.

## Step type declarations

### `pointy.type.derivation`

```nix
pointy.type.derivation = { withSrcFiles = true; };
```

This is the Nix build behind an analysis step. `withSrcFiles` defaults to `false`; when enabled, the stdlib links `srcFiles/<step-id>/` entries into the build working directory and the UI shows them beside the step.

### `pointy.type.fileUpload`

```nix
pointy.type.fileUpload = {
  allowedExtensions = [ ".fastq.gz" ".fastq" ];
};
```

The file picker uses `allowedExtensions`; supply `null` to leave the selection unrestricted. This attribute is required. It is a picker hint, not content validation. Pointy fills `args.uploaded` from the uploaded payload hash and resolves it to a store object for the builder. Do not add an `uploaded` language parameter or binding. Upload templates normally have a parameterless interface describing the output.

### `pointy.type.download`

```nix
pointy.type.download = { };
```

Declare a `url : String` language parameter and optionally give `bindings.url` a label or help text. Saving the step prefetches the URL and records a trusted `args.downloaded` object with `url`, `hash`, and `downloadedAt`. The builder receives `args.downloaded` resolved through `pkgs.fetchurl`, not the raw object.

The renderer appends a read-only **Downloaded at** field using `path = [ "downloaded" "downloadedAt" ];`. Do not add a binding for that synthetic field. The URL must be HTTP or HTTPS; see [Download steps](steps.md#download-steps) for the save and reuse behavior.

## Contracts and argument values

For example, an entry program can contain:

```pointy
program example {
  type Layout = "single" | "paired"

  struct Sample
    (name : String)
    (layout : Layout)
    (packages : List String)
    : Sample

  struct Analysis
    (body : String = "")
    (threads : Int = 2)
    (keepIntermediate : Bool = False)
    (reads : @fulfill Directory)
    (dependencies : List (@fulfill Directory))
    (samples : List Sample = [])
    : Directory
}
```

Finite choices are declared with `type`, as in `type Layout = "single" | "paired"`. `data` does not declare a choice: a `data` name used as a parameter type leaves the form unrenderable.

`contract.interface = "Analysis";` selects these parameters. Data arguments become ordinary Nix strings, integers, booleans, lists, and records. An input from another step, such as `reads`, is stored as `{ step = 42; }` and resolved to that step's derivation for the builder; the UI may add `path = [ ... ];` recording the folder that held the producer at that point. The build reads only `.step`; the recorded path is for artifact links and is kept current as folders move, with a stale path treated like a reference without one. Multiple step inputs are stored as a list of those references; an omitted list of step inputs resolves to an empty list.

The language declaration determines defaults and required arguments. The stdlib rejects unknown names and missing required values with `pointy.<type>: unknown arg(s): ...` or `pointy.<type>: missing required arg(s): ...`, then passes resolved arguments to `build`. Evaluating the form document builds nothing; when a step runs, Pointy builds its certificate against the declared output type. A step record may also carry a top-level `refine = "<type expression>"`; it is part of the declared output type for certificates and never reaches the build derivation.

## Builder helpers

`pointy-stdlib.lib` provides `mkFlake`, `loadDir`, `csvExtras`, `fastqExtras`, `nixDepPkgs`, and `stepLinkCommands`.

- `nixDepPkgs { pkgs; nixDeps; }` resolves a list such as `[ "python3" "jq" ]` to packages. Unknown attribute paths fail evaluation with ``Unknown Nix package `<name>`.``
- `stepLinkCommands { stepDeps; }` produces shell commands linking resolved upstream outputs into the working directory under their step ids.
- `csvExtras` and `fastqExtras` take `{ pkgs; baseDrv; requirements?; }` and return metadata scanner derivations.

For resource precedence, notices, metadata, source files, and extras layout, see [Resources](resources.md), [Step files](step-files.md), and [Extras](extras.md).
