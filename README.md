# Pointy Notebook

A notebook for writing, running, organizing and sharing research computation.

[Home page](https://pointy.cloud/)

![Project view](https://pointy.cloud/screenshots/projects-home.png)

**For researchers**: Pointy is a web app where you upload experiment design / outcome data, and run analyses on them using any program. The results are pinned to the exact program versions that produced them for maximum traceability even after years. The server side needs to be set up on a Linux computer. Ask your admin.

**For admins**: Pointy is a web app for templating Nix derivations and presenting an auto-generated web UI for users. You write Nix derivation templates, the researchers parametrize and run them, browse and share the outputs, and organize them in projects. The data plane is a pluggable git repository where templates and data co-evolve in a shared history.

## Documentation

User and admin guides are available at [pointy.cloud](https://pointy.cloud/).

## Development

A NixOS VM runs a server environment:

```bash
nix run .#dev-vm
```

This starts the VM with the backend and nginx, and forwards:

- `localhost:8080` → VM nginx (backend)
- `localhost:2222` → VM SSH

Useful commands inside the VM:

- `systemctl status` - check services
- `journalctl -u backend -f` - follow backend logs
- `squeue` - list Slurm step build jobs
- `scontrol show job <job-id>` - inspect a Slurm build job
- `journalctl -u slurmctld -u slurmd -f` - follow Slurm controller/worker logs

Press `C-a x` to shut the VM down. Delete `nixos.qcow2` to reset its persistent state. Restart the VM to pick up backend changes.

To run the frontend dev server against the VM backend, from `frontend/`:

```bash
nix develop .#frontend -c npm run dev-vm
```

## Building artifacts

```bash
nix build .#backend        # Haskell backend binary
nix build .#frontend       # compiled static assets
nix run .#generate-openapi # generate OpenAPI specification
```

## License

Pointy Notebook is distributed under the GNU Affero General Public License, version 3 or later. See [LICENSE](LICENSE) for the full text.
