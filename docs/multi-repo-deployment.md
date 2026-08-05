# Multi-Repo Pointy: Single-Repo Variant

How the multi-repo design (see `multi-repo-design.md`) is realized in the
single-repo-variant deployment. **The backend is unchanged**: each running
instance serves exactly **one** user repo via the existing `[user-repo]`
config; repo-scoped reads/writes and the agent sandbox stay within that repo.
There is no repo registry — no `repos` map, no default-repo pointer, no
in-server multi-repo machinery.

Multi-repo capability is expressed at the *deployment* level: one instance (and
one NixOS config) per user repo, plus a stdlib-side `deps`/namespace import
contract for repo-to-repo dependencies.

## Deployed topology

All work is on `multi-repo` branches in the five existing repos (pushed to
their remotes); `pointy-welker-c` and `pointy-c-instance` are new repos on
`main` (local; push to GitLab pending project creation).

| Instance | Repo served | Tracked branch | Config source (unencrypted) | Agent |
|---|---|---|---|---|
| **trotter host** (`seqdb.ggpeti.com`) | `pointy-welker` | `prod-backend` | trotter `config/pointy.nix` → `/home/backend/config.toml` | enabled (API key in `pointy-agent-env.age`) |
| **pointy-c-instance** (tenant container, `pointy-c.pointy.cloud`) | `pointy-welker-c` (confidential) | `main` | `pointy-c-instance/config/pointy-config.toml` → `/home/backend/config.toml` | **disabled** (no API key provisioned) |

Confidentiality is **LLM isolation by instance separation**, enforced two ways:

1. The host instance's agent sandbox is a worktree of `pointy-welker` only; it
   never mounts the c-instance container (no bind, no network path, no
   worktree) — `pointy-welker-c` is not reachable by the host's LLM agent.
2. The c-instance hosts `pointy-welker-c` but has **no agent credentials**: its
   `services.pointy-backend.agentEnvFile` is null and no agent API key is
   provisioned, so the configured LLM agent cannot run there either.
   Interactive evaluation and the web UI remain fully functional.

The pointy-c-instance is a trotter tenant registered in trotter's
`modules/host.nix`: hostnames `pointy-c.pointy.cloud`, deploy SSH port `2795`,
host web-proxy port `28084` (DNS A record + host `authorized_keys.d/deploy`
provisioning documented in the tenant repo's README).

## Operator config lives in the deployment repos, not the backend

Each instance's `/home/backend/config.toml` is generated (via `pkgs.writeText`
+ an `ExecStartPre` `install` in the backend systemd unit, running as
`backend`) from an **unencrypted** config file checked into the deployment
repo — not from an encrypted secret and not from backend code defaults:

- **trotter** `config/pointy.nix` (host instance) — mirrors the TOML:
  `user-repo` (url `git@gitlab.com:ggpeti/pointy-welker`, keyfile
  `/home/backend/.ssh/id_ed25519_deploy`, branch `prod-backend`), `agent`, and
  `nix-evaluator` (2048 MiB). Rendered by `modules/pointy-backend.nix`; the
  old `secrets/pointy-backend-config.age` was deleted.
- **pointy-c-instance** `config/pointy-config.toml` — same shape for
  `pointy-welker-c` (branch `main`).

`[agent]` settings (sbox/runner commands, `--model`, bootstrap prompt,
timeouts) **migrated from backend code defaults** (`Config.hs`/`example-config.toml`)
into these deployment configs; the backend falls back to defaults only when the
table is absent (e.g. local dev).

## What stays encrypted

Only runtime secrets, in `secrets/` alongside the unencrypted config:

- Repo SSH **deploy key** — trotter `pointy-backend-deploy-key.age` →
  `/home/backend/.ssh/id_ed25519_deploy` (host, for `pointy-welker`);
  pointy-c-instance `pointy-backend-deploy-key.age` (a fresh
  `pointy-c-instance@trotter` keypair, to be added as a GitLab deploy key with
  write access on `pointy-welker-c`).
- Agent **API key** — trotter `pointy-agent-env.age` →
  `/home/backend/agent-env`, consumed as a systemd `EnvironmentFile`
  (`DEEPSEEK_API_KEY`). Not present on the confidential instance.

Both are age-encrypted for recipients `ggpeti` + `trotter` (see each repo's
`secrets/secrets.nix`); decryptable with `/root/.ssh/id_ed25519`.

## Importing one user repo as a dependency of another (stdlib)

The pointy stdlib provides the cross-repo import machinery with no backend
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

- `#pointy.deps` exposes `{ namespace = { input, repo? }; }` — the backend
  contract for the future multi-repo backend; pure metadata of the repo's own
  committed state (evaluating it does not force dependency content).
- `#pointy.namespaces.<ns>` (and per-system
  `packages.<system>.pointy.namespaces.<ns>`) mounts the dependency's full
  `pointy` output — stepDefs, stepConfig, templates, projects, presets,
  srcFiles, and (per-system) built `steps` — so a dependent repo can reach the
  dependency's domain objects under a namespace.
- Unknown input / non-pointy input namespaces fail with a descriptive error
  when forced.

In the single-repo variant the dependency resolves at flake-evaluation time
from the dependent's committed `flake.lock` (normal Nix input resolution), so
the backend evaluates the dependent exactly as today. `pointy-welker` and
`pointy-welker-c` both declare `deps = { }`.

## Verification performed

- stdlib: `nix flake check`; two-flake harness (`#pointy.deps`,
  `#pointy.namespaces`, per-system steps; error case).
- pointy: diff vs `main` is docs-only (no backend source changes).
- pointy-welker: `#pointy.deps`, `#pointy.namespaces`, `#pointy.stepConfig`
  (16 template types), `#pointy.projects` all evaluate under the new stdlib.
- pointy-welker-c: `#pointy.*` evaluate; sample `report` step builds.
- trotter: `nix flake check` (full `nixosConfigurations.trotter`); rendered
  `/home/backend/config.toml` verified; only `pointy-agent-env` +
  `pointy-backend-deploy-key` remain as age secrets.
- pointy-c-instance: `nix flake check` (full NixOS config); config installed to
  `/home/backend/config.toml` in preStart; deploy node `seqdb.ggpeti.com:2795`.
- trotter-tenant: `nix flake check`.
