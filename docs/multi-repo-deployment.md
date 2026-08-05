# Multi-Repo Pointy: Single-Repo Variant

This documents how the multi-repo design (see `multi-repo-design.md`) is
realized in the single-repo-variant deployment. The backend itself unchanged:
**each running instance serves exactly one user repo** via the existing
`[user-repo]` config, and reads/writes stay scoped to that repo.

## Why single-repo per instance

The single-repo variant keeps the backend simple and unchanged while still
delivering the design's key property — LLM isolation for confidential data —
through **physical tenant separation** instead of in-server multi-repo
machinery:

- The **host instance** (`seqdb.ggpeti.com`) serves the regular user repo
  (`pointy-welker`). Its LLM agent's sandbox is a worktree of that one repo.
- The **confidential instance** (`pointy-c-instance`, a trotter tenant) serves
  the confidential user repo (`pointy-welker-c`) running inside its own Incus
  container. The host's agent has no mount, no network path, and no worktree
  into that container, so it cannot see the confidential data.

Confidential repo marking is deployment metadata kept in the operator config
(`repos.<name>.confidential` in trotter), which routes each repo to the right
instance. No code-level access control is required.

## Operator config lives in the deployment repo, not the backend

The instance's `config.toml` is generated from an **unencrypted** operator
config that lives in the trotter repo:

- `[user-repo]` — the repo binding (url, keyfile, branch); unencrypted.
- `[agent]` — agent runner settings (runner args, model, bootstrap prompt,
  timeouts). These migrated out of backend code defaults into the deployment
  config; the backend falls back to defaults only when the table is absent.
- `[nix-evaluator]` — optional tuning.

Runtime secrets stay encrypted/provisioned separately: the repo SSH deploy key
(`pointy-backend-deploy-key.age`) and the agent API key
(`pointy-agent-env.age`).

## Importing one user repo as a dependency of another

The pointy stdlib provides the cross-repo import machinery without any backend
support. A repo declares dependencies in its flake:

```nix
{
  inputs = {
    a.url = "git@example.com:org/edit-tooling.git";   # flake wiring
    pointy-stdlib.url = "github:421anon/pointy-stdlib";
  };
  outputs = { pointy-stdlib, ... }@inputs:
    pointy-stdlib.lib.mkFlake { inherit inputs; } {
      pointy = {
        # ...
        deps = {
          edit-tooling = { input = "a"; repo = "edit-tooling"; }; # -> pointy.namespaces.edit-tooling
        };
      };
    };
}
```

- `#pointy.deps` exposes `{ namespace = { input, repo? }; }` — the registry
  contract for the (future) multi-repo backend.
- `#pointy.namespaces.<ns>` (and per-system
  `packages.<system>.pointy.namespaces.<ns>`) mounts the dependency's full
  `pointy` output — stepDefs, stepConfig, templates, projects, presets,
  srcFiles, and (per-system) built `steps` — so a dependent repo can reach the
  dependency's domain objects under a namespace.

In the single-repo variant the dependency is resolved at flake-evaluation time
from the dependent's committed flake.lock (normal Nix input resolution), so the
backend evaluates the dependent exactly as today.
