# Pointy Notebook

A notebook for writing, running, organizing and sharing research computation.

[Home page](https://pointy.cloud/)

**For researchers**: Pointy is a web app where you upload experiment design / outcome data, and run analyses on them using any program. The results are pinned to the exact program versions that produced them for maximum traceability even after years. The server side needs to be set up on a Linux computer. Ask your admin.

**For admins**: Pointy is a web app for templating Nix derivations and presenting an auto-generated web UI for users. You write Nix derivation templates, the researchers parametrize and run them, browse and share the outputs, and organize them in projects. The data plane is a pluggable git repository where templates and data co-evolve in a shared history.

## Documentation

User and admin guides are available at [pointy.cloud](https://pointy.cloud/).

## Project organization

- Steps not linked from any folder are loaded from raw project links and stay reachable through search and "Link existing". Unreadable raw links fail the load rather than exposing potentially linked steps as unfiled.
- Organization batches reject newly introduced project cycles, including cycles assembled within a single batch, without committing partial changes.
- Manual drag reorder follows the displayed folders-first order. Drops that leave the visible order unchanged do not queue a write.
- Historical views are read-only, including toast Undo, and retain the browser's native context menu.
- Listing header controls wrap on narrow screens. The selection action bar overlays the listing header, so selecting never shifts rows; it collapses to icons on narrow listings.
- The navigation toggle appears only once the folder tree has loaded. The open navigation drawer starts just wide enough for the folder tree, up to a quarter of the window, and resizes from its bottom-right corner like the agent panel; its header and its side edges cast shadows while the tree is scrolled vertically or horizontally. On narrow screens the drawer overlays the page, spanning the full width on phones and omitting folder icons to fit longer names.
- A plain row click opens a folder or a built step's outputs and never changes the selection. Selection uses the row checkbox, Ctrl/Cmd-click, or Shift-click for ranges.
- While the clipboard holds cut or copied items, Paste and Clear clipboard appear in the listing header, and Clear clipboard also joins the selection action bar and row menus while rows are selected; Ctrl/Cmd+V also pastes into the current folder. Cut items fade until the cut is pasted or cleared, and pasting a cut back into its source folder just ends it.

## Activity tray

- The bottom tray counts running, queued and starting builds and uploads. Cluster health has its own label; hover it for the reported detail.
- A build is starting from the moment the backend accepts it, while it resolves dependencies and submits Slurm jobs, until Slurm lists a queued or running job for it.
- Opening the tray groups steps by state. Each group collapses and shows the leading words its step names share, which are dropped from its rows.
- Running rows show elapsed time, queued rows show how long they have waited and Slurm's pending reason, starting rows show how long they have been preparing, and uploads show progress. A build of a commit other than the current one carries that commit's short hash.
- Running, queued and starting rows have a Stop button that cancels the build at the commit it is building.

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
