---
description: "Tidy research computation: define, run, and share Nix-backed workflows."
---

# Pointy Notebook

Welcome to the Pointy Notebook home page.

Pointy helps research teams keep computational work tidy by storing inputs, software, notes, and results together so any run can be inspected in its wholeness. It achieves a very high level of computational reproducibility by utilizing the Nix sandbox, an isolated computational environment where the only way to enter data and software is by precise advance declaration. If you do the in vitro part of an experiment in a sterile environment, you should do the same with the in silico part, shouldn't you? Moreover: with Pointy, all workflows are ordinary code. You plug in a Git repository and Pointy writes versioned workflow definitions in it. Conveniently, you can edit the same repository using your own tooling: VS Code, LLM agents, etc. It's really that simple. And tidy. Marketing called and they told me to keep using the word "tidy".

The sections below describe three ways to work with Pointy: through the web UI, with an LLM working directly in the repository, or through a custom UI built against the backend API.

![Home, the root folder: the navigation drawer shows the folder tree, and the listing shows folders with step status counts next to a step.](screenshots/light/home-listing.png)

## Browse, share, and run simple analyses through the web UI

Pointy's web UI supports established analyses from setting up a folder through sharing results. Folders hold analysis steps (programs with their parameters, each made from a predefined analysis called a template), uploaded files, run states, and outputs. Folders nest inside other folders under Home, and the same step can appear in several folders. Results can be viewed in the browser as text, HTML, images, molecular structures, CSV, or TSV, compared with other outputs, reviewed, and shared using a read-only link to the exact state of the repository.

The built-in AI agent can set up or change an analysis when you ask it to. By default its changes are applied when a turn succeeds; turn **Auto-apply** off to review each changeset first.

→ [Managing Folders](projects.md) · [Working with Steps](steps.md) · [Running Steps and Viewing Results](execution.md) · [AI Agent](agent.md)

## Use your own LLM workflow

The Pointy data store is a Git repository, and step definitions are represented as plain Nix attribute sets, which are directly analogous to JSON objects. An LLM can work on the analysis directly: creating steps, connecting inputs and dependencies, adding scripts, and changing how results are combined.

For example, a bioinformatician processing a 96-well plate can define the per-sample analysis once, use an LLM to create and configure the steps for all 96 samples, and add an aggregate step that produces plate-level statistics and charts. This avoids creating and connecting dozens of runs by hand. The resulting workflow remains inspectable, versioned code in the repository rather than state held inside an LLM session.

→ [Setting Up the User Repository](user-repo-setup.md) · [CLI Reference](cli-reference.md)

## Build your own UI against the backend API

Some analyses require a dedicated workflow wizard that validates an experimental design, collects task-specific parameters, presents conditional questions, or generates a large set of connected analysis steps.

Pointy provides the platform behind such a UI. Templates defined in the user repository describe the available step types and their inputs. The backend API exposes the creation and configuration of those steps, runs them, reports their status, and exposes their outputs. Pointy catalogs every run and keeps its outputs connected to the inputs, parameters, and upstream steps that produced them, so the custom UI can focus on the task-specific workflow instead of run storage, output management, and data lineage.

→ [Using the Backend API](api-guide.md) · [Backend API Reference](api/) · [Type Reference](type-reference.md)

## Sharing results

A Pointy link can open a folder read-only at a specific Git commit, deep-link to an output file or directory, and highlight selected lines in a text result. The recipient sees the state that was shared, not whatever happens to be current later.

[Read the sharing documentation →](sharing.md#share-links)

![Output file preview with lines 6 through 9 selected and a Share lines action visible above the highlighted text.](screenshots/light/output-file-line-range-share.png)

## Deployment and storage

The frontend and backend are self-hosted. A NixOS module is provided for easy setup; check out Pointy's source code. Workflow state lives in the Git-backed user repository, and Nix defines and evaluates builds. Every run is submitted as a Slurm job; the supplied `single-node-slurm` NixOS module sets up a single-node Slurm cluster. The outputs of successful runs are kept reachable through registered garbage-collector roots.

→ [Instance Admin Guide](admin.md)

## Documentation

### Using Pointy Web

- [Folders](projects.md), [Organize](organize.md), and [Search](search.md): navigate Home and nested folders, move, link, hide, reorder, and find any step.
- [Steps](steps.md) and [Runs](execution.md): create, edit, and duplicate steps, upload data, start or stop builds, and follow cluster activity.
- [Outputs](outputs.md) and [Storage panel](storage-panel.md): browse, preview, and compare outputs and source files, and freeze, rewarm, or delete old revisions.
- [Reviews](reviews.md) and [Sharing](sharing.md): lock and compare reviewed results, and create commit-pinned read-only links.
- [Agent](agent.md), [Changesets](changesets.md), and [Chats](chats.md): ask the built-in agent to set up or investigate an analysis, review or auto-apply its edits, and manage chat history.

### Hosting and configuring workflows

- [Hosting](admin.md): runtime architecture, host setup, [Login](login.md), [Clusters](clusters.md), [Config](config.md), [Providers](providers.md), [NixOS](nixos.md), [Storage](storage.md), [Sync](sync.md), [Sandbox](sandbox.md), [Builds](builds.md), [Troubleshooting](troubleshooting.md), [Backups](backups.md), and [Upgrading](upgrading.md).
- [Repository](user-repo-setup.md): create the Git-backed flake with [Templates](templates.md), [Step files](step-files.md), [Presets](presets.md), [Resources](resources.md), [Extras](extras.md), [Linked data](linked-data.md), [Types](type-reference.md), and [Forms](forms.md).

### Developing and integrating

- [CLI](cli-reference.md): inspect and build user-repository flake outputs directly with `nix`.
- [API](api-guide.md), [Streams](streams.md), [Files](files.md), [Agent API](agent-api.md), and [API Reference](api/): read consistent revisions, mutate folders and steps, stream live status, and work with files from custom clients.
- [Contributing](contributing.md): build Pointy itself and run the development VM.

Pointy is distributed under the GNU AGPL v3.
