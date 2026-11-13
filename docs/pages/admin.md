---
description: "How a Pointy instance fits together, and how to set up its host, paths, and ports."
---

# Hosting

As the instance administrator, you make the analysis tools available and keep Pointy running. You provide the Git repository that stores the work, the machines that run it, access to the AI agent, and backups. Researchers can then create, inspect, and edit steps without managing those services themselves.

The Host pages cover hosting and operations. For everyday research work, start with [Agent](agent.md), [Steps](steps.md), and [Runs](execution.md). To add analysis tools, see [Repository](user-repo-setup.md).

## Runtime architecture

A Pointy deployment has five moving parts:

1. **Frontend**: the browser UI where users manage folders, steps, runs, outputs, and share links.
2. **Backend**: a service that reads and writes the user repository, serves API endpoints, streams live step statuses, accepts uploads, runs ingest jobs, scratch wraps, and two-sandbox agent sessions, and starts or stops builds.
3. **Nix evaluation**: two long-lived `nix repl` sessions evaluate the user repository: a pure one for pinned `git+file` revisions and an impure one for write worktrees and raw expressions. Short-lived `nix eval` processes handle bulk certificate work, and a write-once certificate store under `/home/backend/.local/state/pointy/certificates/` caches each step key's certificate and output paths.
4. **User repository**: a Git-backed Nix flake that stores templates, optional presets, step definitions, folder definitions, and source files. Folders nest: `projects/<id>.nix` holds tagged `children`, and the root folder **Home** is `projects/0.nix`.
5. **Nix / Slurm runtime**: each build target is a derivation the backend evaluated. Slurm batch jobs run a configured realise command against that derivation, by default `pointy-realise`, which runs `nix build` on it and its pinned inputs.

Optional pieces sit around that core: `pointy-hub` and `pointy-node` distribute realisation across a multi-node Slurm cluster, an external login page can issue session tokens, and a cold-storage store can hold large step outputs outside the backend's local store.

When a user saves a folder or step, the backend rewrites its `.nix` file, commits the change, and pushes it to the configured remote. Organize actions are queued by the UI and sent as one `POST /projects/batch` five seconds after the last action, producing a single commit that may touch several files. Each run uses a specific Git commit, so later edits do not change what that run means. Status updates appear in every project that links the step directly; ancestors see step counts and status totals from `GET /project-rollup`.

## Setting up a host

Start with an `x86_64-linux` NixOS host, a hostname pointing at it, and a [compatible user repository](user-repo-setup.md). Give the backend a deploy key that can both read and push the configured branch. Read-only Git access is not enough: saving a step is a repository write.

Add Pointy as an input to your NixOS flake and pass it to your host modules, for example through `specialArgs = { inherit pointy; };`. A host module can then contain:

```nix
{ pointy, pkgs, ... }:
{
  imports = [
    pointy.nixosModules.pointy-host
    pointy.nixosModules.single-node-slurm
  ];

  services.pointy-host = {
    hostname = "pointy.example.org";
    frontendPackage = pointy.packages.${pkgs.stdenv.hostPlatform.system}.frontend;
    basicAuthFile = "/run/secrets/pointy-htpasswd";
  };
  services.pointy-backend.agentEnvFile = "/run/secrets/pointy-agent-env";

  security.acme = {
    acceptTerms = true;
    defaults.email = "admin@example.org";
  };
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
```

Replace the hostname, email, and secret paths. The module creates the `backend` account and service, but does **not** provision the repository config or deploy key for you. Install `config.toml` at `/home/backend/config.toml`, give the account access to its configured key, and keep secrets out of Nix source files and the world-readable Nix store. Runtime secret files should be readable only by the accounts that need them. You can instead set `systemd.services.backend.environment.POINTY_CONFIG_PATH` to another runtime config path.

The example enables nginx basic authentication. With `basicAuthFile = null`, the virtual host has no basic-auth gate. Use HTTPS and protect the backend port too: the backend itself does not enforce per-user authorization. A share link pins a revision; it is not an access token.

`nixosModules.single-node-slurm` is a single-node starting point: it runs munge, `slurmctld` and `slurmd` on the host and wires the job-completion hook. On a non-VM host its fallback advertises only one CPU and 1024 MiB of memory. `services.slurm.nodeName` is a list option, and the module sets it as a plain value rather than with `mkDefault`, so a plain override is merged as a second `NodeName` line; use `lib.mkForce` to replace the fallback, as the development VM does. Do not advertise more RAM than `slurmd` reports. Its partition is fixed to `pointy` with `Default=YES MaxTime=INFINITE State=UP`. The backend submits builds through Slurm; there is no configuration switch to a local non-Slurm runner.

To use a cluster you manage yourself, leave out `single-node-slurm`. Point `services.pointy-backend.slurm.package` and `services.pointy-backend.slurm.confFile` at its client, if it is not the host's `services.slurm`, add `JobCompType=jobcomp/script` and `JobCompLoc=` followed by `services.pointy-backend.slurm.jobCompletionHook` to its `slurm.conf`, and order `backend` after the Slurm daemons the host runs. Set `services.pointy-backend.slurm.realiseCommand` to an executable installed on every compute node; it receives one derivation path and must make all of that derivation's outputs valid in the backend's store, with the build log readable there.

The fallback job reconciler polls every 30 seconds. Set `POINTY_SLURM_RECONCILE_SECONDS` to a positive integer to change that interval; the job-completion hook remains the immediate wake-up path.

After provisioning the config and secrets, restart `backend` and check its journal, `sinfo`, and a small build before inviting users. A successful HTTP response alone does not establish that Git pushes, evaluation, and scheduling work.

### Paths and ports

| Address | Purpose |
| --- | --- |
| HTTPS `/` | Static frontend, with application routes falling back to `index.html`. |
| HTTPS `/backend/` | Proxy to the backend on port **8081**, with the prefix removed. |
| HTTPS `/artifact/` | Linked-data route. A request accepting `text/turtle` is proxied to `/backend/rdf/artifact/`; `linkedDataBase` fixes the `@base` origin in the Turtle output. Other requests fall through to the frontend. It is behind any enabled login gate. |
| HTTPS `/docs/` | Optional `docsPackage`; this location explicitly disables both the basic-auth and token-auth gates. |
| HTTPS `/token-login` and `/token-logout` | Login form and logout endpoints, present only with `tokenAuth`; nginx's internal `/token-auth` check is not reachable from outside. |
| Port **8081** | Backend listener, bound to `127.0.0.1` by default (`services.pointy-backend.listenHost`, `listenPort`). The supplied nginx vhost and job-completion hook always connect to `127.0.0.1:<listenPort>`, so changing `listenHost` only moves the listener, not the proxy target; binding it away from loopback makes the supplied vhost fail unless you also change or bypass that proxy. Keep the port behind the proxy/firewall rather than exposing it as an unauthenticated alternative. |

`nixosModules.pointy-host` installs the backend, its sandbox, common nginx settings, and the application virtual host; `nixosModules.single-node-slurm` adds a local Slurm cluster; `nixosModules.dev-vm` composes both into the development VM, with a plain-HTTP `localhost` virtual host; `nixosModules.bootstrap` supplies their packages and internal configuration. `nixosModules.pointy-hub` and `nixosModules.pointy-node` add the multi-node realisation pair. Use the public module options rather than overriding `services.pointy.internal`; those options are read-only.

The module does not raise nginx's `client_max_body_size`, so the default **10 MiB** request-body limit applies to uploads through the supplied vhost. Raise `services.nginx.clientMaxBodySize` if users upload larger files. The `/backend/job-ended` route is reachable through the proxy; it only wakes build waiters after Slurm confirms the job has ended.
