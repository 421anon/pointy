---
description: "Pass source files and typed column metadata into a build."
---

# Extras

## Injecting `srcFiles` into a build

Enable repository-backed files at the template's top level:

```nix
pointy.type.derivation = {
  withSrcFiles = true;
};
```

The stdlib symlinks each top-level entry from `srcFiles/<step-id>/` into the build working directory during unpacking. If the directory is absent, there is nothing to unpack. `srcFiles/` is optional; create it when you first add a source file. The UI exposes the same files for [browsing and editing](outputs.md#source-files). Treat them as read-only build inputs; write generated files elsewhere and copy results into `$out`.

## Injecting typed column metadata via `extras`

For CSV/TSV output, add this at the template's top level:

```nix
extras = "csv";
```

The stdlib attaches a separate scanner derivation as `meta.pointy.extras`. It mirrors output directories with `meta.json` files describing column types and nullability. This lets the [grid viewer](outputs.md#csv-and-tsv-grid-preview) sort numbers as numbers. `extras = "fastq";` instead scans FASTQ files for read counts; a line count not divisible by four fails that scan.

Metadata is requested per folder. Pointy can build missing extras in the background, and a failed scan does not prevent ordinary file browsing. Each returned `meta.json` is limited to 10 MiB. CSV scans default to 2 CPUs and 2 GiB of RAM; FASTQ scans default to 1 CPU and 1 GiB.

For a custom extras builder, omit the top-level shorthand and attach your derivation under `mkDerivation.passthru.meta.pointy.extras`. It should expose `requirements = { cpu; ram; ior; iow; }` for scheduling. The public `pointyLib.csvExtras` and `pointyLib.fastqExtras` helpers accept `{ pkgs; baseDrv; requirements?; }`, where `baseDrv` can be the builder's `public` argument.

A CSV folder's metadata looks like this, keyed by filename with columns in header order:

```json
{
  "results.csv": {
    "delimiter": ",",
    "hasHeader": true,
    "columns": [
      { "type": "string", "nullable": false },
      { "type": "float", "nullable": true }
    ]
  }
}
```

Supported column types are `int`, `float`, and `string`. Unknown types and columns beyond the supplied schema fall back to strings.
