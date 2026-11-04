---
description: "Write templates for analysis, file upload, and download steps."
---

# Templates

## Declare the arguments

Put this in `main.pointy`:

```pointy
program example {
  struct Greeting
    (message : String = "Hello, research!")
    : Directory < /message.txt >

  struct DataSource : Directory

  struct Download (url : String) : Directory < /data >
}
```

`Greeting` has one text argument with a default and describes a directory containing `message.txt`. The other two declarations are used by the optional upload and download examples below. The language declaration sets parameter names, types, order, defaults, and which values are required. The Nix template chooses how to display them.

Evaluating the form document does not build or check anything. When a step runs, Pointy builds the step's **certificate**, which checks the output against the output type its program declares, and the step's status reports whether that check passed. See [Reviewing results](reviews.md#reviewing-results).

## Example: derivation template

Put this in `templates/hello.nix`:

```nix
{
  displayName = "Greeting";
  description = "Write a message into a result file.";
  sortKey = 0;
  icon = "description";
  contract.interface = "Greeting";
  pointy.type.derivation = { };

  bindings.message = {
    displayName = "Message";
    description = "Text to put in message.txt.";
    widget.kind = "textarea";
  };

  compile = { lib, ... }: {
    build = { args, ... }: {
      mkDerivation = {
        installPhase = ''
          mkdir -p "$out"
          printf '%s\n' ${lib.escapeShellArg args.message} > "$out/message.txt"
        '';
        dontFixup = true;
      };
    };
  };
}
```

The filename gives the template its key, `hello`. `contract.interface` connects it to `Greeting`, and `bindings.message` changes the label and control without redefining the argument. The builder receives the resolved `args`. Shell quoting matters: a user's message is data here, not a shell fragment to execute.

`compile` receives `{ lib, pkgs, pointyLib }` and returns a `build` function. That function receives `{ args, public }`, where `public` is the step derivation. It returns `mkDerivation` attributes, optionally `name`, `version`, and `env`. The stdlib supplies the standard Nix build setup and Pointy metadata, including the step id, template type, arguments, and resource requirements. You do not need to repeat those in every template.

## Example: file upload template

Add `templates/dataSource.nix` using the `DataSource` declaration from `main.pointy`:

```nix
{
  displayName = "Data source";
  description = "Upload files for an analysis.";
  sortKey = 1;
  contract.interface = "DataSource";
  pointy.type.fileUpload.allowedExtensions = [ ".csv" ".tsv" ];

  compile = { ... }: {
    build = { args, ... }: {
      mkDerivation = {
        installPhase = "ln -s ${args.uploaded} \"$out\"";
        dontFixup = true;
      };
    };
  };
}
```

`allowedExtensions` is required; use `null` to accept any extension. It controls the browser picker, not a server-side security boundary. An upload runs as an ingest job; when it succeeds, the backend records the payload hash in `args.uploaded`, and the stdlib resolves that value to the stored object for the builder; a missing store object fails the build. `args.uploaded` is not a language parameter or a user-entered form field. Keep uploaded store objects in your backup plan: the upload reference cannot reconstruct missing data from Git alone.

## Example: download template

Add `templates/download.nix` using the `Download` declaration:

```nix
{
  displayName = "Download";
  description = "Fetch a remote file and retain its content hash.";
  sortKey = 2;
  contract.interface = "Download";
  pointy.type.download = { };
  bindings.url.displayName = "URL";

  compile = { ... }: {
    build = { args, ... }: {
      mkDerivation = {
        installPhase = ''
          mkdir -p "$out"
          ln -s ${args.downloaded} "$out/data"
        '';
        dontFixup = true;
      };
    };
  };
}
```

Unlike the generated `downloaded` value, **`url` is a parameter in the language declaration**. On save, the backend prefetches it and writes `args.downloaded = { url; hash; downloadedAt; }`. The stdlib resolves that value with `pkgs.fetchurl`; the form also gets a read-only download timestamp. An unchanged URL reuses the recorded hash, and the URL must use HTTP or HTTPS. See [Download steps](steps.md#download-steps).

## Making extra packages available to templates

Extend `pkgs` in `pointy.nix`:

```nix
pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux.extend (_: prev: {
  myLab.helper = prev.callPackage ./packages/helper { };
});
```

Templates can then use `pkgs.myLab.helper`. `packages/` is copied into the same store path as `pointy.nix` and the templates, but Pointy never loads its files as templates: import them explicitly. A package edit is visible to step evaluation. A flake-parts module that needs the same package set reads it from the `pointyRepo` module argument, as `pointyRepo.cfg.pkgs`.
