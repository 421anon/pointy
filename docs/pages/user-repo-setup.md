# Setting Up the User Repository

The user repository is a Git repository and Nix flake that Pointy treats as the source of truth for:

- step templates
- step instances
- project membership and ordering
- optional per-step source files

Pointy expects this repository to be wired through `trotter.lib.mkFlake`, which generates the flake outputs the backend and frontend read. See [Architecture & Configuration](admin.md) for the runtime overview.

## Minimal repository structure

Create these items at the root of the repository:

- `flake.nix`
- `flake.lock`
- `templates/`
- `steps/`
- `projects/`
- `srcFiles/`

Directory ownership is split like this:

- `templates/` and `srcFiles/` are admin-authored
- `steps/` and `projects/` are backend-managed

The `srcFiles/` directory must exist even if it is empty.

## Minimal `flake.nix`

At the repository root, create a `flake.nix` like this:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    trotter = {
      url = "gitlab:ggpeti/trotter-system";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ trotter, ... }:
    trotter.lib.mkFlake { inherit inputs; } {
      trotter = {
        stepDefs = trotter.lib.loadDir ./steps;
        templates = trotter.lib.loadDir ./templates;
        projects = trotter.lib.loadDir ./projects;
        srcFiles = ./srcFiles;
      };
    };
}
```

This is enough to expose the flake outputs that Pointy needs, including:

- `.#trotter.stepConfig`
- `.#trotter.stepDefs`
- `.#trotter.projects`
- `.#trotter.srcFiles`
- per-system `.#trotter.steps.<id>` and `.#trotter.projectOutPaths`

See the [CLI Reference](cli-reference.md) for concrete commands against those outputs.

## Making extra packages available to templates

Templates receive `pkgs`. The supported way to add custom packages is to extend `pkgs` in `perSystem`.

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    trotter = {
      url = "gitlab:ggpeti/trotter-system";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    myTool.url = "github:example/my-tool";
  };

  outputs =
    inputs@{ trotter, ... }:
    trotter.lib.mkFlake { inherit inputs; } {
      trotter = {
        stepDefs = trotter.lib.loadDir ./steps;
        templates = trotter.lib.loadDir ./templates;
        projects = trotter.lib.loadDir ./projects;
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

1. a top-level `trotter.type...` declaration that tells Pointy what kind of step it is
2. a `module = ...` definition that declares options and build behaviour

There are two top-level step kinds: file upload and derivation.

### Example: file upload template

This pattern matches the sample `dataSource` template:

```nix
{
  trotter.type.fileUpload = {
    allowedExtensions = [ ".fastq.gz" ".fastq" ".fq.gz" ".fq" ];
    description = "data source";
  };

  module =
    { dream2nix, config, lib, ... }:
    let
      cfg = config.trotter.dataSource;
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
          passthru.meta.trotter = {
            type = "dataSource";
            inherit (cfg) id;
          };
        };
      };

      options.trotter.dataSource = with config._trotter.lib.types; {
        id = lib.mkOption {
          type = trotter.string { description = ""; };
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
  trotter.type.derivation = { };

  module =
    { dream2nix, config, lib, pkgs, ... }:
    let
      cfg = config.trotter.fastqc;
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
          passthru.meta.trotter = {
            type = "fastqc";
            inherit (cfg) id;
          };
        };
      };

      options.trotter.fastqc = with config._trotter.lib.types; {
        id = lib.mkOption {
          type = trotter.string { description = ""; };
          visible = false;
        };

        dataSource = lib.mkOption {
          type = trotter.step {
            allowedTypes = [ "dataSource" "merge" ];
            description = "The data source for FastQC";
          };
        };

        extraArgs = lib.mkOption {
          type = trotter.string {
            description = "Extra arguments for the fastqc command";
            display.command = "fastqc";
          };
          default = "";
        };
      };
    };
}
```

Notes:

- `trotter.step` options resolve to upstream step outputs at build time.
- `trotter.string` with `display.command` renders a command-style argument field in the UI.
- `passthru.meta.trotter` should include both the template `type` and the step `id`.

For the available option types, see the [Type Reference](type-reference.md).

## The hidden `id` option

Every template should define an `id` option like this:

```nix
id = lib.mkOption {
  type = trotter.string { description = ""; };
  visible = false;
};
```

Pointy injects the numeric step identifier into this option. Templates then commonly forward it into `passthru.meta.trotter.id`.

This is useful both for step metadata and for templates that need the step id inside the build or in helper scripts.

## Injecting `srcFiles` into a build

If a derivation type should receive repository-backed source files at build time, enable `withSrcFiles`:

```nix
trotter.type.derivation = {
  withSrcFiles = true;
};
```

When this flag is enabled, Pointy symlinks every top-level entry from `srcFiles/<step-id>/` into the build working directory before the build runs.

In the web UI, such step types also show a **Source Files** section. See [Execution and Data Management](execution.md#source-files).

The sample `script` template uses this pattern.

## After you commit template changes

Once template changes are committed and pushed to the user repository, the backend can evaluate them immediately. Frontend users will see the new or updated step types the next time the UI reloads its step configuration, such as on a page refresh.

For command-line inspection of the generated outputs, see the [CLI Reference](cli-reference.md).
