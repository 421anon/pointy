# Setting Up the User Repository

The user repository is a Git repository and Nix flake that Pointy treats as the source of truth for:

- step templates
- optional template presets
- step instances
- project membership, ordering, and active template selection
- optional per-step source files

Pointy expects this repository to be wired through `pointy-stdlib.lib.mkFlake`, which generates the flake outputs the backend and frontend read. See [Architecture & Configuration](admin.md) for the runtime overview.

## Minimal repository structure

Create these items at the root of the repository:

- `flake.nix`
- `flake.lock`
- `templates/`
- `steps/`
- `projects/`
- `srcFiles/`
- optionally `presets/`, if you prefer to keep preset definitions in separate files

Directory ownership is split like this:

- `templates/`, `presets/`, and `srcFiles/` are admin-authored
- `steps/` and `projects/` are backend-managed

The `srcFiles/` directory must exist even if it is empty.

## Minimal `flake.nix`

At the repository root, create a `flake.nix` like this:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    pointy-stdlib = {
      url = "github:421anon/pointy-stdlib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ pointy-stdlib, ... }:
    pointy-stdlib.lib.mkFlake { inherit inputs; } {
      pointy = {
        stepDefs = pointy-stdlib.lib.loadDir ./steps;
        templates = pointy-stdlib.lib.loadDir ./templates;
        projects = pointy-stdlib.lib.loadDir ./projects;
        presets = { }; # or pointy-stdlib.lib.loadDir ./presets
        srcFiles = ./srcFiles;
      };
    };
}
```

This is enough to expose the flake outputs that Pointy needs, including:

- `.#pointy.stepConfig`
- `.#pointy.presets`
- `.#pointy.stepDefs`
- `.#pointy.projects`
- `.#pointy.srcFiles`
- `.#pointy.dependencies`
- per-system `.#pointy.steps.<id>`, `.#pointy.projectOutPaths`, and `.#pointy.autocomplete`

See the [CLI Reference](cli-reference.md) for concrete commands against those outputs.

## Project presets

Presets are named bundles of templates. They let admins offer a short menu such as "RNA-seq", "Variant calling", or "Custom QC" instead of asking every user to pick individual template ids for every project.

A preset definition has this shape:

```nix
{
  displayName = "RNA-seq";
  description = "Standard read-processing templates."; # optional
  sortKey = 10; # optional
  templates = [ "dataSource" "fastqc" "script" ];
}
```

Then expose the presets through the `pointy.presets` flake input, for example with `pointy-stdlib.lib.loadDir ./presets`. Pointy validates preset definitions when evaluating `.#pointy.presets`; a preset that names an unknown template fails evaluation so the admin can fix it before users see it.

Each project must define exactly one of:

- `preset = "<preset-name>";` to follow a preset bundle
- `templates = [ "templateA" "templateB" ];` for a custom template list

Changing a project's active preset or custom template list does not delete assigned steps. Steps whose template is no longer active are surfaced in the web UI as orphaned steps until a user re-enables the template or unassigns the step.

## Making extra packages available to templates

Templates receive `pkgs`. The supported way to add custom packages is to extend `pkgs` in `perSystem`.

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    pointy-stdlib = {
      url = "github:421anon/pointy-stdlib";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    myTool.url = "github:example/my-tool";
  };

  outputs =
    inputs@{ pointy-stdlib, ... }:
    pointy-stdlib.lib.mkFlake { inherit inputs; } {
      pointy = {
        stepDefs = pointy-stdlib.lib.loadDir ./steps;
        templates = pointy-stdlib.lib.loadDir ./templates;
        projects = pointy-stdlib.lib.loadDir ./projects;
        srcFiles = ./srcFiles;
      };

      perSystem =
        { system, inputs', ... }:
        {
          _module.args.pkgs =
            inputs.nixpkgs.legacyPackages.${system}.extend
              (_: prev: {
                myOrg.myTool = inputs'.myTool.packages.default;
                myOrg.helper = prev.callPackage ./packages/helper { };
              });
        };
    };
}
```

Templates can then reference `pkgs.myOrg.myTool` and `pkgs.myOrg.helper`.

A `packages/` directory is just a repository convention; Pointy does not load it automatically.

## Writing step templates

Every template file has two parts:

1. a top-level `pointy.type...` declaration that tells Pointy what kind of step it is
2. a `module = ...` definition that declares options and build behaviour

There are two top-level step kinds: file upload and derivation.

### Example: file upload template

This pattern matches the sample `dataSource` template:

```nix
{
  sortKey = 2;
  displayName = "Data Source";
  description = "Sequencing reads as FASTQ files.";

  pointy.type.fileUpload = {
    allowedExtensions = [ ".fastq.gz" ".fastq" ".fq.gz" ".fq" ];
  };

  module =
    { dream2nix, config, lib, ... }:
    let
      cfg = config.pointy.dataSource;
    in
    {
      imports = [ dream2nix.modules.dream2nix.mkDerivation ];

      config = {
        version = "1";
        name = "dataSource";
        mkDerivation = {
          dontUnpack = true;
          installPhase = "ln -s ${cfg.uploaded} $out";
          dontFixup = true;
          passthru.meta.pointy = {
            type = "dataSource";
            inherit (cfg) id;
          };
        };
      };

      options.pointy.dataSource = with config._pointy.lib.types; {
        id = lib.mkOption {
          type = pointy.string { description = ""; };
          visible = false;
        };

        uploaded = lib.mkOption { type = lib.types.package; };
      };
    };
}
```

Notes:

- `allowedExtensions` controls the frontend file picker.
- `uploaded` is filled in by Pointy automatically; template authors do not create a form field for it.
- `cfg.uploaded` is a store path pointing at the uploaded payload directory.

### Example: derivation template

This pattern matches the sample `fastqc` template:

```nix
{
  sortKey = 5;
  displayName = "FastQC";
  description = "Per-base quality report with FastQC.";

  pointy.type.derivation = { };

  module =
    { dream2nix, config, lib, pkgs, ... }:
    let
      cfg = config.pointy.fastqc;
    in
    {
      imports = [ dream2nix.modules.dream2nix.mkDerivation ];

      config = {
        version = "1";
        name = "fastqc";
        mkDerivation = {
          dontUnpack = true;
          buildInputs = [ pkgs.fastqc ];
          installPhase = ''
            mkdir $out
            ln -s ${cfg.dataSource}/* .
            fastqc \
              --threads 32 \
              --outdir $out \
              *.fastq* \
          '' + cfg.extraArgs;
          passthru.meta.pointy = {
            type = "fastqc";
            inherit (cfg) id;
          };
        };
      };

      options.pointy.fastqc = with config._pointy.lib.types; {
        id = lib.mkOption {
          type = pointy.string { description = ""; };
          visible = false;
        };

        dataSource = lib.mkOption {
          type = pointy.step {
            allowedTypes = [ "dataSource" "merge" ];
            displayName = "Reads";
            description = "FASTQ source to QC.";
          };
        };

        extraArgs = lib.mkOption {
          type = pointy.string {
            displayName = "Extra FastQC args";
            description = "Extra flags appended to the fastqc command.";
            display.command = "fastqc";
          };
          default = "";
        };
      };
    };
}
```

Notes:

- `pointy.step` options resolve to upstream step outputs at build time.
- `pointy.string` with `display.command` renders a command-style argument field in the UI.
- `displayName` becomes the visible form label; `description` becomes the hint shown under the field.
- `passthru.meta.pointy` should include both the template `type` and the step `id`.

For the available option types, see the [Type Reference](type-reference.md).

## Optional step notices

Templates can publish non-blocking, field-scoped notices through `passthru.meta.pointy.notices`:

```nix
passthru.meta.pointy.notices = [
  {
    field = "nixDeps";
    severity = "info";
    message = "This step uses a package with special license terms.";
  }
];
```

Pointy fetches these notices when a user opens the step parameters form. Notices are informational only; they do not prevent saving, evaluating, or building the step.

## The hidden `id` option

Every template should define an `id` option like this:

```nix
id = lib.mkOption {
  type = pointy.string { description = ""; };
  visible = false;
};
```

Pointy injects the numeric step identifier into this option. Templates then commonly forward it into `passthru.meta.pointy.id`.

This is useful both for step metadata and for templates that need the step id inside the build or in helper scripts.

## Injecting `srcFiles` into a build

If a derivation type should receive repository-backed source files at build time, enable `withSrcFiles`:

```nix
pointy.type.derivation = {
  withSrcFiles = true;
};
```

When this flag is enabled, Pointy symlinks every top-level entry from `srcFiles/<step-id>/` into the build working directory before the build runs.

In the web UI, such step types also show a **Source Files** section. See [Execution and Data Management](execution.md#source-files).

The sample `script` template uses this pattern.

## Injecting typed column metadata via `extras`

If a derivation type emits CSV or TSV output files, you can publish typed column schemas so that Pointy's [grid viewer](execution.md#csv-and-tsv-grid-preview) treats numeric columns as numbers for filtering and sorting instead of strings. Schemas are produced by a sibling derivation referenced as `passthru.meta.pointy.extras`.

```nix
passthru.meta.pointy = {
  type = "myStep";
  inherit (cfg) id;

  extras = pkgs.runCommand "myStep-extras" { } ''
    mkdir -p $out
    cat > $out/meta.json <<'JSON'
    {
      "results.csv": {
        "columns": [
          { "type": "int",    "nullable": false },
          { "type": "float",  "nullable": true  },
          { "type": "string", "nullable": false }
        ]
      }
    }
    JSON
  '';
};
```

The extras derivation must produce an output tree mirroring the step's output directory layout. For each directory that contains CSV/TSV files, write a `meta.json` at `$out/<dir>/meta.json` keyed by file name. Pointy fetches each `meta.json` lazily, only for directories the user actually opens.

Each per-directory `meta.json` has this shape:

```json
{
  "<file-name>": {
    "columns": [
      { "type": "int" | "float" | "string", "nullable": true | false }
    ]
  }
}
```

Notes:

- Columns are matched positionally against the file's header row. Extra trailing columns in the file (beyond the schema) default to nullable strings.
- Unrecognized values for `type` fall back to `string`.
- The extras derivation is built alongside the main step, and is also built lazily the first time the UI requests metadata for a step whose extras have not been realised. Failures are non-fatal: the file still renders, just without typed columns. Errors are visible in the backend log.
- Each per-directory `meta.json` is capped at 10 MiB.

### Optional: resource requirements for the extras build

By default the extras derivation builds with conservative slurm resources (`cpu = 1`, `ram = "1G"`, `ior = "0"`, `iow = "0"`). For larger metadata jobs, attach a `passthru.requirements` to the extras derivation:

```nix
extras = pkgs.runCommand "myStep-extras" {
  passthru.requirements = {
    cpu = 2;
    ram = "4G";
    ior = "0";
    iow = "0";
  };
} ''
  ...
'';
```

Pointy reads `meta.pointy.extras.requirements` when scheduling the extras build. The shape matches a regular step's `requirements`.

## After you commit template changes

Once template changes are committed and pushed to the user repository, the backend can evaluate them immediately. Frontend users will see the new or updated step types the next time the UI reloads its step configuration, such as on a page refresh.

For command-line inspection of the generated outputs, see the [CLI Reference](cli-reference.md).
