---
description: "Work on Pointy itself and run its development VM."
---

# Contributing

## Working on Pointy itself

The [CLI](cli-reference.md) commands belong in a **user repository**. These commands instead run from a checkout of the [Pointy application](https://github.com/421anon/pointy):

```sh
nix build .#backend
nix build .#frontend
nix run .#generate-openapi -- /tmp/pointy-openapi.json
nix develop .#backend
nix develop .#frontend
nix run .#dev-vm
```

`nix build .#backend` produces the `backend`, `generate-openapi`, `pointy-fixture-server`, and `pointy-migrate-project-tree` executables. The migration tool converts a user repository to the folder format; see [Migrating to the folder format](user-repo-setup.md#migrating-to-the-folder-format).

`generate-openapi` writes the specification used by the generated [API Reference](api/); the [API guide](api-guide.md) covers the streaming and raw-response contracts around it.

`nix develop .#backend` gives a GHC 9.10 and cabal environment for the Haskell backend. `nix develop .#frontend` resolves to the frontend package's development shell and provides Node.js, npm, Elm 0.19.2, and webpack, with the Nix-built `node_modules` on `NODE_PATH`.

For frontend dependencies, the flake provides `install-elm-pkg`, `uninstall-elm-pkg`, and `update-elm` apps. Run the package commands from `frontend/`, and `update-elm` from the application root to regenerate its Nix dependency descriptions after an Elm dependency change.

The application exports `nixosModules.bootstrap`, `nixosModules.pointy-host`, `nixosModules.pointy-node`, `nixosModules.pointy-hub`, `nixosModules.single-node-slurm`, and `nixosModules.dev-vm`, plus `nixosConfigurations.dev-vm` and the `dev-vm` app. `pointy-node` and `pointy-hub` provide the compute-node and shared-store integrations for a multi-node Slurm cluster. It also exports `lib.<system>.mkFixtureServer`, the wrapper the documentation tooling uses to run `pointy-fixture-server`, and `lib.<system>.tokenAuthVhost`, the nginx vhost fragment for token-based login. The `checks.x86_64-linux.hub-realise` check is a NixOS test of the hub and compute-node store path. The [admin guide](#development-vm) covers the `backend/dev-config.toml` prerequisite for the `dev-vm` app and the VM's forwarded ports.

This documentation lives in the application repository under `docs/pages`, next to the code it describes, so a change and its documentation land in one commit. `lib.<system>.mkDocs { screenshots = …; }` builds the site with Sourcey (`packages.sourcey`), copying the given screenshot directory to `screenshots/` and the specification from `packages.openapi-json` to `openapi.json`. The screenshot directory holds `light/` and `dark/` captures named as the pages reference them; the public site's repository produces them by driving `pointy-fixture-server` and passes them in.

## Development VM

From the Pointy application checkout, copy `backend/example-config.toml` to `backend/dev-config.toml` and fill in the repository URL, branch, and the deploy-key path **inside the guest**. Run the launcher from the repository root; it requires that file and otherwise fails with `error: backend/dev-config.toml is missing. Copy backend/example-config.toml to backend/dev-config.toml and fill in your settings.` It shares the backend directory at `/shared/dev-config`; the guest installs the TOML as `/home/backend/config.toml` with mode 0600 and copies an optional `backend/agent-env` in for provider credentials. Keep both files out of commits.

```sh
nix run .#dev-vm
```

The launcher runs `nixos-shell --flake .#dev-vm`. The VM has 4 CPUs, 8 GiB RAM, and a 20 GiB virtual disk. It forwards host `localhost:8080` to guest nginx and `localhost:2222` to guest SSH. The guest nginx serves `/backend/` and the Turtle `/artifact/` route; it has no UI, so port 8080 is not the app. It is a backend development environment, not the production static-frontend host.

For the browser UI, run the frontend dev server from `frontend/`:

```sh
nix develop .#frontend -c npm run dev-vm
```

`nix develop .#frontend` resolves to the frontend package's development shell and provides npm, Node, Elm, and webpack, with `NODE_PATH` pointing at the Nix-built dependencies, so no separate `npm ci` is needed. Open `http://localhost:3000`. Webpack proxies `/backend` through the VM's nginx. The backend shell is `nix develop .#backend`.

Press `Ctrl+a`, then `x`, to stop the VM console. Restart the VM to pick up backend changes. The VM disk retains state between launches; deleting `nixos.qcow2` is a destructive reset, not a normal restart. If a host Nix daemon is exposed on port 5000, the guest's daemon proxy uses it; otherwise it uses the guest daemon. Do not expose that daemon port on an untrusted network. The guest shares the host's `/nix/var/log` over virtiofs; the backend keeps its Nix cache on the VM disk, since an unprivileged virtiofsd can only create files as the host user.
