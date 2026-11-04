---
description: "Options of the Pointy NixOS modules."
---

# NixOS

## NixOS module options

Import `pointy.nixosModules.pointy-host` to install the backend service, nginx virtual host, and agent runtime, and `pointy.nixosModules.single-node-slurm` for a local Slurm runtime. The modules provide these options:

| Option | Default | Purpose |
| --- | --- | --- |
| `services.pointy-host.hostname` | _(required)_ | nginx virtual host name, and the ACME certificate name when `tls` is on. |
| `services.pointy-host.tls` | `true` | Terminate HTTPS in the virtual host with an ACME certificate and redirect HTTP. Turn it off behind a proxy that terminates TLS, or for plain-HTTP local use. |
| `services.pointy-host.frontendPackage` | `null` | Built frontend package served at `/`. When `null`, the virtual host serves no frontend, as in the development VM. |
| `services.pointy-host.basicAuthFile` | `null` | Optional htpasswd file protecting the application virtual host. |
| `services.pointy-host.tokenAuth` | `null` | Optional session-token login gate. Its sub-options are `loginUrl` (required), `audience` (required), and `publicKeyFile` (required, an Ed25519 public key in PEM). See [Login gate](login.md#login-gate). |
| `services.pointy-host.linkedDataBase` | `null` | Origin written as the `@base` of artifact Turtle. `null` leaves IRIs relative to the requesting URL. |
| `services.pointy-host.docsPackage` | `null` | Optional documentation package served at `/docs/`; this location disables both the basic-auth and token-auth gates. |
| `services.pointy-backend.user`, `.group`, `.home` | `backend`, `backend`, `/home/backend` _(read-only)_ | The account the backend, agents and builds run as, and its home. Refer to these when granting the backend host resources or placing secrets such as `config.toml`. |
| `services.pointy-backend.listenHost` | `127.0.0.1` | Address the backend HTTP listener binds. The supplied vhost and hook still use `127.0.0.1:<listenPort>`. |
| `services.pointy-backend.listenPort` | `8081` | Backend HTTP listener port, also used by the supplied vhost and job-completion hook. |
| `services.pointy-backend.agentEnvFile` | `null` | Runtime `KEY=VALUE` secret file sourced by systemd, suitable for a sops-nix or agenix path. The runner receives only the chat provider's key. |
| `services.pointy-backend.piConfigDir` | Pointy's `backend/pi` | Pi configuration copied to `/home/backend/.pi` before the backend starts. Models and settings inside it are regenerated from `config.toml`. |
| `services.pointy-backend.ingestPackage` | built-in `pointy-ingest` (`nix store add` + jq) | Package providing `bin/pointy-ingest DIR NAME`, which turns a directory into a content-addressed store path. See [Data ingest and scratch space](storage.md#data-ingest-and-scratch-space). |
| `services.pointy-backend.storeUrl` | `null` | Nix store for the backend, its evaluators, the agent sandboxes and Slurm builds, exported as `NIX_REMOTE`. `null` uses the host store. |
| `services.pointy-backend.coldStorage.package` | `null` | Optional package providing the cold-storage command, whose `status` and `prefetch` subcommands the backend runs for cold storage. `null` disables cold status and prefetch. |
| `services.pointy-backend.coldStorage.ingestSocket` | `null` | Ingest socket of the cold-storage store, exported as `POINTY_COLD_STORE_SOCKET`. `null` disables cold status and prefetch. |
| `services.pointy-backend.scratchDirectory` | `null` | Directory users browse to wrap a whole subdirectory into a file-upload step. `null` hides the feature. |
| `services.pointy-backend.uploadDirectory` | `null` | Where uploads are staged before ingest. `null` uses the system temporary directory. |
| `services.pointy-backend.agentMemoryMax` | `24G` | Memory limit shared by every agent sandbox, including sandbox `/tmp`. `null` leaves them unlimited. See [Sandbox and credential boundaries](sandbox.md#sandbox-and-credential-boundaries). |
| `services.pointy-backend.slurm.package` | `config.services.slurm.package` | Slurm package whose client commands the backend runs. |
| `services.pointy-backend.slurm.confFile` | the host's `services.slurm` `slurm.conf` | Cluster configuration the backend submits to, exported as `SLURM_CONF`. |
| `services.pointy-backend.slurm.realiseCommand` | generated `pointy-realise` | Executable on every compute node, run as `<command> <derivation>`. The default runs `nix build --no-link`, passing `--store` when `storeUrl` is set. See [Build execution and GC roots](builds.md#build-execution-and-gc-roots). |
| `services.pointy-backend.slurm.jobCompletionHook` | _(read-only)_ | Script the cluster runs as `JobCompLoc` with `JobCompType=jobcomp/script`; `single-node-slurm` wires it. |

For a plain multi-node cluster:

| Option | Default | Purpose |
| --- | --- | --- |
| `services.pointy-hub.enable` | `false` | Enable the hub Nix store and build-log receiver. |
| `services.pointy-hub.nodeKeys` | `[]` | Compute nodes' SSH public keys, used for both the store and the log receiver. |
| `services.pointy-hub.nodePublicKeys` | `[]` | Compute nodes' Nix signing public keys; the untrusted store user may only add paths signed by one of these. |
| `services.pointy-hub.logUser` | `pointy-log` | System user that receives build logs. |
| `services.pointy-hub.realiseCommand` | `/run/current-system/sw/bin/pointy-realise-hub` _(read-only)_ | Command installed by `pointy-node`; set the backend's `slurm.realiseCommand` to it when the backend runs on the hub. |
| `services.pointy-node.enable` | `false` | Enable the compute-node integration. |
| `services.pointy-node.signingKeyFile` | _(required)_ | Secret Nix signing key for this node's outputs; the Slurm build user must be able to read it. |
| `services.pointy-node.nodePublicKeys` | `[]` | Nix public signing keys of all compute nodes, used to verify hub-substituted paths. |
| `services.pointy-node.hub.host` | _(required)_ | Hub hostname or address. |
| `services.pointy-node.hub.user` | `nix-ssh` | SSH user of the hub's Nix store. |
| `services.pointy-node.hub.logUser` | `pointy-log` | SSH user of the hub's build-log receiver. |
| `services.pointy-node.hub.port` | `null` | Hub SSH port; `null` uses the OpenSSH default. |
| `services.pointy-node.hub.sshKeyFile` | _(required)_ | Private SSH key this node presents to the hub; the Slurm build user must be able to read it. |
| `services.pointy-node.hub.hostPublicKey` | `null` | Base64 SSH host public key used to pin the hub. `null` uses the node's known hosts. |

The nginx configuration keeps `/backend/` SSE responses unbuffered and allows hour-long reads. If you replace the supplied reverse proxy, preserve those settings for the status, ingest, cluster, and agent-turn streams.

## NixOS integration

Use `services.pointy-backend.agentEnvFile` for provider API keys and `services.pointy-backend.piConfigDir` for extensions and static Pi configuration; both options are described under [NixOS module options](#nixos-module-options).

The service copies `piConfigDir` to `/home/backend/.pi` before each start, makes it readable only by the `backend` account, and adds the bundled question and glovebox extensions. Entries under `agent/extensions/` reach chat sessions; add a custom extension there. The backend regenerates `.pi/agent/models.json` and `.pi/agent/settings.json` from `[[providers]]` and `[models.chat]`, so edits to those two files in `piConfigDir` do not survive. Make durable configuration changes in the source directory, not in the generated copy. A missing optional environment file does not prevent service startup, so a running service does not prove the chat provider key was loaded.
