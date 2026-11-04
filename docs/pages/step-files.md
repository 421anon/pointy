---
description: "How steps and folders are stored in the repository."
---

# Step files

## Add a step and a folder

Put this in `steps/1.nix`:

```nix
{
  type = "hello";
  name = "First greeting";
  args.message = "Hello, research!";
}
```

`type` selects the template, `name` is the label shown in Pointy, and `args` holds the parameter values keyed by their language names.

An argument that names another step is written `{ step = N; }`. Pointy may add `path = [ ... ];` beside it to record the folder that held the producer; the build reads only `.step`, and Pointy keeps the path current when folders move. A path that no longer holds the referenced step is treated like a reference without a path, with an info notice on the field.

Put this in `projects/1.nix`:

```nix
{
  name = "Example analysis";
  templates = [ "hello" ];
  children = [
    { step = { id = 1; hidden = false; sortKey = 0; }; }
  ];
}
```

Put this in `projects/0.nix`:

```nix
{
  name = "Home";
  templates = [ ];
  children = [
    { project = { id = 1; hidden = false; sortKey = 0; }; }
  ];
}
```

Each `projects/<id>.nix` file is a plain attribute set:

- `name` is the folder name shown in Pointy.
- Exactly one of `preset` (a preset key) or `templates` (a list of template keys) is required, in `projects/0.nix` too; use `templates = [ ];` for a folder that starts with no template selection.
- `children` lists the folder's contents. Each entry is exactly one of `{ step = { ... }; }` or `{ project = { ... }; }`, with an integer `id` and the placement fields `hidden` and `sortKey`.
- Entries with a `sortKey` come first, ordered by `(sortKey, id)`; entries with `sortKey = null` follow, ordered by id. `hidden` and `sortKey` belong to the entry, not to the child: the same step or folder can sit in several folders with different placement.
- `projects/0.nix` is the root folder, Home. It cannot become a child or be deleted, and folders created in the UI are linked under it; keep the file in the repository.
- The children graph must stay acyclic; the backend refuses a change that would introduce a cycle. An id with no matching `steps/<id>.nix` or `projects/<id>.nix` is dropped and reported in `validationErrors`.

A step linked in no folder is **unfiled**. It still evaluates as `pointy.steps.<id>`, but the web UI loads and searches only steps linked from folders, so an unfiled step is neither listed nor offered by **Link existing**. Keep every step you want to use in at least one folder. Organize actions rewrite these files: Pointy re-renders the file from its data, so comments are dropped, keys are sorted, and the parent's sort keys are renumbered. See [Managing Projects](organize.md#moving-linking-and-reordering) for the UI actions.

Add the files to Git before the first flake command. Nix does not include untracked files automatically, and a Git-backed flake with an untracked `flake.nix` is rejected. From the repository root:

```sh
nix flake lock
nix eval --json .#pointy.stepConfig
nix eval --json .#pointy.projects
nix build .#pointy.steps.1
cat result/message.txt
nix build .#pointy.steps.1.certificate
```

The configuration should have `version: 4`, the projects output should show `Home` and `Example analysis`, the build should write `Hello, research!`, and the certificate build should succeed. Once the repository and its inputs are available to the backend, configure its URL and branch as described in the [Hosting](admin.md).

## The step id and metadata

The stdlib adds `args.id` from the numeric filename, as a string, and supplies `meta.pointy.id`, `type`, `requirements`, and `args` on the exported step. Do not declare a hidden `id` parameter or copy that metadata into every template. Put additional metadata, such as notices, under `mkDerivation.passthru.meta.pointy`.

A step definition may also carry `note`, `about`, `requirements`, `refine`, `reviewedRevision`, `reviewedBy`, and `reviewComments`. `about` is a list of absolute `http(s)` URLs for records the step's data describes. `requirements` overrides the template's resources and `refine` adds a type expression to the output the step must certify; neither reaches the build derivation. The review fields are managed through the normal save and review operations. A UI save rewrites the whole file from the form fields, so keep hand-added `requirements`, `refine`, and comments in Git. See [Reviewing results](reviews.md#reviewing-results).

## Optional step notices

Inside the builder's `mkDerivation` attributes, a template can add:

```nix
passthru.meta.pointy.notices = [
  {
    field = "message";
    severity = "info";
    message = "This text is written verbatim to the output file.";
  }
];
```

Notices are Markdown hints loaded with the parameter form. Use the exact field name and `severity = "info"`; other severities are not supported by the current frontend. A notice does not enforce a constraint or block a build.
