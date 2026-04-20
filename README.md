# Pointy Notebook

A notebook for keeping, organizing and sharing research computation.

![Project view](docs/pages/screenshots/light/project-view.png)

**For researchers**: Pointy is a web app where you upload experiment design / outcome data, and run analyses on them using any program. The results are pinned to the exact program versions that produced them for maximum traceability even after years. The server side needs to be set up on a Linux computer. Ask your admin.

**For admins**: Pointy is a web app for templating Nix derivations and presenting an auto-generated web UI for users. You write Nix derivation templates, the researchers parametrize and run them, browse and share the outputs, and organize them in projects. The data plane is a pluggable git repository where templates and data co-evolve in a shared history.

## Documentation

User and admin guides live under [docs/](docs/). Build them with `nix build .#docs` and open `./result/index.html`. But it is possible to view them using a markdown reader as well, such as the GitHub web UI.

- [Managing Projects](docs/pages/projects.md), [Building Workflows (Steps)](docs/pages/steps.md), [Execution and Data Management](docs/pages/execution.md) — web UI workflow
- [Architecture & Configuration](docs/pages/admin.md), [Setting Up the User Repository](docs/pages/user-repo-setup.md) — instance administration
- [Type Reference](docs/pages/type-reference.md), [CLI Reference](docs/pages/cli-reference.md) — template options and flake outputs

## Development

A NixOS VM runs a server environment:

```bash
nix run .#dev-vm
```

This starts the VM with the backend and nginx, and forwards:

- `localhost:8080` → VM nginx (backend + docs proxy)
- `localhost:2222` → VM SSH

Useful commands inside the VM:

- `systemctl status` - check services
- `journalctl -u backend -f` - follow backend logs
- `journalctl _SYSTEMD_SLICE=pointy-builds.slice -f` - follow step build logs

Press `C-a x` to shut the VM down. Delete `nixos.qcow2` to reset its persistent state. Restart the VM to pick up backend changes.

To run the frontend dev server against the VM backend, from `frontend/`:

```bash
nix develop .#frontend -c npm run dev-vm
```

## Building artifacts

```bash
nix build .#backend        # Haskell backend binary
nix build .#frontend       # compiled static assets
nix build .#docs           # mkdocs site
nix run .#take-screenshots # take screenshots for the mkdocs site
```

## License

Pointy Notebook is distributed under the GNU Affero General Public License, version 3 or later. See [LICENSE](LICENSE) for the full text.
