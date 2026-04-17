# Pointy Notebook

Pointy Notebook is a web interface for building and running step-based workflows backed by Nix. A workflow lives in a project, and each step is either a file upload or a derivation step that can depend on other steps.

## Start with the guide for your role

### If you use Pointy through the web UI

- [Managing Projects](projects.md) — create, open, hide, reorder, and delete projects.
- [Building Workflows (Steps)](steps.md) — add steps, connect them, reuse them across projects, and organize them.
- [Execution and Data Management](execution.md) — run steps, upload files, inspect outputs, work with source files, and create share links.

### If you administer a Pointy instance

- [Architecture & Configuration](admin.md) — runtime architecture, backend config, sync behaviour, status streaming, and GC roots.
- [Setting Up the User Repository](user-repo-setup.md) — create the Git-backed flake that defines templates, steps, projects, and source files.
- [Type Reference](type-reference.md) — reference for the template option types exposed by `pointy-stdlib`.
- [CLI Reference](cli-reference.md) — inspect and build flake outputs directly with `nix`.

## Core concepts

### Projects

A **project** is the top-level container for a workflow. Projects help you keep analyses separate and give you a place to organize related steps. See [Managing Projects](projects.md).

### Steps

A **step** is one unit of work in a project. Pointy supports two step families:

- **File upload steps** accept files through the UI.
- **Derivation steps** build results from parameters and upstream steps.

When one step refers to another, the downstream step uses the upstream step's output path during the build. This creates the workflow dependency graph described in [Building Workflows (Steps)](steps.md).

### Shareable steps and versioned views

Running steps and successful steps are **shareable**. In Pointy, this means the UI can expose inspect, clone, and share actions for that step state, and successful outputs can be shared through commit-pinned links. See [Execution and Data Management](execution.md).

### The user repository

Behind the UI, Pointy stores platform state in a Git-backed Nix flake usually called the **user repository**. Most users never need to touch it directly. Instance admins do: it is where templates, steps, projects, and optional source files live. See [Architecture & Configuration](admin.md) and [Setting Up the User Repository](user-repo-setup.md).
