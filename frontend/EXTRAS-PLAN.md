# Extras Plan

## Scope

Implement step output sidecar metadata (“extras”) for directory-level embellishments, then use it to provide CSV/TSV column metadata and FASTQ read counts.

## Contracts

### Nix output contract

For each step derivation:

```nix
pointy.steps.<id>
```

allow optional sidecar fields:

```nix
pointy.steps.<id>.meta.pointy.extras
pointy.steps.<id>.meta.pointy.extras.requirements
```

- `extras`: derivation whose output mirrors the base output by directory metadata files.
- `extras.requirements`: `{ ram, cpu, ior, iow }`, same shape as existing step requirements.
- Missing `extras` means no extras.
- Existing `meta.pointy` must be preserved when stdlib injects `id`, `type`, `requirements`, and `args`.

### Extras directory layout

For an output directory path:

```text
<dir>/
```

metadata lives at:

```text
<dir>/meta.json
```

Examples:

```text
base output:  a/b/c.csv
base output:  a/b/d.txt
extras file:  a/b/meta.json
```

`a/b/meta.json` is a JSON object keyed by direct child file name:

```json
{
  "c.csv": { "columns": [] },
  "d.txt": { "readCount": 123 }
}
```

Rules:

- Directory-level metadata only; at most one JSON file per directory.
- Missing extras derivation -> backend returns `{}`.
- Missing `meta.json` -> backend returns `{}`.
- Existing malformed/non-object `meta.json` -> backend returns an error.
- Extras scanners follow symlinks.

## pointy-stdlib changes

1. Preserve template-provided `result.meta.pointy` in `evalSteps`:

```nix
meta = (result.meta or { }) // {
  pointy = (result.meta.pointy or { }) // {
    inherit id type;
    requirements = resolvedRequirements;
    args = resolvedArgs;
  };
};
```

2. Add stdlib helpers for extras derivations:
   - generic CSV/TSV metadata generator;
   - generic FASTQ read-count generator;
   - merge helper for multiple extras generators.

3. Merge semantics:
   - merge `meta.json` objects per directory;
   - fail the extras build on duplicate child-file keys within one directory;
   - fail on invalid JSON emitted by a generator.

4. Add `extras.requirements` support:
   - template-provided value wins;
   - otherwise default to conservative low resource values in the stdlib-provided generator.

5. Commit and push stdlib changes to `pointy-stdlib` `main` when implementation begins.

## Generic CSV/TSV extras

### Input detection

Scan the base output recursively, following symlinks, for:

- `*.csv`
- `*.tsv`

Assume every detected table has a header row.

### Output shape

For each table file, write metadata under its parent directory’s `meta.json`:

```json
{
  "data.csv": {
    "delimiter": ",",
    "hasHeader": true,
    "columns": [
      { "type": "string", "nullable": false },
      { "type": "int", "nullable": true },
      { "type": "float", "nullable": false }
    ]
  }
}
```

Types:

- `int`: all non-empty values parse as integers.
- `float`: all non-empty values parse as floats, and at least one non-empty value is not an int.
- `string`: otherwise.
- `nullable: true`: at least one empty/blank value appears.
- all-empty column: `type = "string"`, `nullable = true`.

### Parser

duckdb's `sniff_csv` function.

## FASTQ extras

Scan recursively, following symlinks, for:

- `*.fastq`
- `*.fq`
- `*.fastq.gz`
- `*.fq.gz`

For each file, emit:

```json
{
  "reads.fastq.gz": {
    "readCount": 1234567
  }
}
```

Counting rule:

- read count = decompressed line count / 4;
- if line count is not divisible by 4, emit a warning or fail the extras build; prefer fail for now.

## Backend changes

### Build flow

Observed current flow builds only `#pointy.steps.<id>`.

Change `runStep` flow:

1. Build the main step as today.
2. Resolve optional `#pointy.steps.<id>.meta.pointy.extras.outPath`.
3. If extras exists, resolve `#pointy.steps.<id>.meta.pointy.extras.requirements`.
4. Submit extras to the build queue with its own requirements.
5. Register a GC root for the extras out path.
6. Keep main step status semantics unchanged.

### Stop/cancel flow

When stopping a step, cancel both:

- extras build key, if resolvable;
- main build key.

In this order, because extras build would restart main build via nix dependency resolution.

### Endpoint

Add:

```http
GET /backend/step-files/extras?id=<stepId>&commit=<commit>&path=<directoryPath>
```

Behavior:

1. Resolve main step and extras against the same commit.
2. If no extras attr: `200 {}`.
3. If extras attr exists but its `meta.json` for the requested directory is missing: `200 {}`.
4. If `meta.json` exists: validate it is a JSON object and return it.
5. Reject traversal and assert the resolved path stays inside the extras store path.
6. Enforce a reasonable JSON size limit before reading.

Because Nix is build-on-read, the endpoint may trigger realization indirectly if needed. The response should still distinguish real JSON errors from absence.

### MIME cleanup

Add extension MIME mappings:

- `.csv` -> `text/csv`
- `.tsv` -> `text/tab-separated-values`

Also add `Content-Type` headers to download responses if the frontend keeps using download endpoints for text content.

## Frontend changes

### Dependencies

Add `BrianHicks/elm-csv` and use `Csv.Parser.parse`:

- CSV: `{ fieldSeparator = ',' }`
- TSV: `{ fieldSeparator = '\t' }`

Observed from the installed package:

- supports CSV and TSV via custom separator;
- supports quoted fields, escaped quotes, CRLF/LF, quoted newlines;
- returns parse errors for quote problems.

### Directory extras loading

When loading an output directory:

1. fetch directory entries;
2. fetch `/step-files/extras` for that same directory path;
3. store extras on `DirectoryFolder` as `ApiData (Dict String Json.Value)`.

Source-file directories do not need extras unless later required.

### CSV/TSV viewer

On opening a CSV/TSV file:

1. parse rows with `Csv.Parser.parse`;
2. assume first row is header;
3. look up table metadata by direct file name in the parent folder extras;
4. build Grid columns using backend metadata instead of frontend value inference;
5. if metadata missing, render columns as strings and show a low-noise “metadata unavailable” indicator.

### Nullable numeric Grid solution

`elm-advanced-grid` does not expose `Grid.Filters`, so the frontend cannot construct honest custom nullable numeric filters directly.

Use this first-pass solution:

- for non-null numeric columns: use `Grid.intColumnConfig` / `Grid.floatColumnConfig`;
- for nullable numeric columns: use `Grid.stringColumnConfig`, then override `comparator`, `renderer`, and `toString` to:
  - render original text;
  - sort numerically when both cells parse;
  - place blanks after numbers;
  - keep string filtering rather than lying that blanks are `0`.

Do not use sentinel values for blanks. They would make filters and equality semantically false.

If numeric filtering for nullable columns becomes required, fork/patch `elm-advanced-grid` to expose nullable numeric filter constructors or add `nullableIntColumnConfig` / `nullableFloatColumnConfig` upstream-style.

## Branching and verification instructions

Allowed when implementation begins:

- Commit and push `pointy-stdlib` on `main`.
- Work on `pointy-welker` `dev-backend`.
- Update `pointy-welker` flake input after stdlib push.
- For experimentation, use `--override-input` before changing locks.
- Backend verification: `nix build -L --no-link .#backend`.
- Frontend verification: `elm make` only.
- Do not use `npm` or `npx`.
- Clean up temporary outputs, generated scripts, and test litter before yielding.
