# Pointy Notebook

A notebook for writing, running, organizing and sharing research computation.

[Home page](https://pointy.cloud/)

**For researchers**: Pointy is a web app where you upload experiment design / outcome data, and run analyses on them using any program. The results are pinned to the exact program versions that produced them for maximum traceability even after years. The server side needs to be set up on a Linux computer. Ask your admin.

**For admins**: Pointy is a web app for templating Nix derivations and presenting an auto-generated web UI for users. You write Nix derivation templates, the researchers parametrize and run them, browse and share the outputs, and organize them in projects. The data plane is a pluggable git repository where templates and data co-evolve in a shared history.

## Documentation

User and admin guides are available at [pointy.cloud](https://pointy.cloud/).

## Project organization

- Organization batches reject newly introduced project cycles, including cycles assembled within a single batch, without committing partial changes.
- Manual drag reorder follows the displayed folders-first order and, when grouped by type, works within each group; a drag holding rows from several groups can only drop into folders. Drop lines appear only where a drop changes the order, so a dragged row or contiguous block gets none beside or inside it, while a scattered selection can gather at any line. A drop lands directly above the row below the line, or directly below the last row, so the result never depends on which half of the line the pointer is over. A drag holding folders can't drop into those folders or anything inside them, matching the Move to and Link to dialogs. Every drop clears the selection.
- Organize drags start only from a row's header. Links, images, selected text and form fields inside an open row keep the browser's native drag.
- Historical views are read-only, including toast Undo, and retain the browser's native context menu.
- Listing header controls wrap on narrow screens. The selection action bar overlays the listing header, so selecting never shifts rows; it collapses to icons on narrow listings.
- The navigation toggle appears only once the folder tree has loaded. The open navigation drawer starts just wide enough for the folder tree, up to a quarter of the window, and resizes from its bottom-right corner like the agent panel; its header and its side edges cast shadows while the tree is scrolled vertically or horizontally. On narrow screens the drawer overlays the page, spanning the full width on phones and omitting folder icons to fit longer names.
- A plain row click opens a folder or a built step's outputs and never changes the selection. Selection uses the row checkbox, Ctrl/Cmd-click, or Shift-click for ranges. Rows leave the selection when they disappear from the listing, including when Show hidden is turned off or a reload brings someone else's changes.
- The listing header counts only the rows shown. While Show hidden is off and the folder has hidden items, a muted "N hidden" note follows the count and turns Show hidden on when clicked. With Show hidden on, hidden rows have dimmed names and an eye-off marker. Folder status summaries still count the steps in hidden rows.
- Folder status summaries show a placeholder until they first load, and fade in and out while they reload. They reload when the folder is opened, after organizing changes are saved, and when a step anywhere below a listed folder changes status.
- Open outputs and file viewers keep their contents and scroll positions when hidden rows or listing groups appear or disappear, and through new commits and review changes while the step's build is unchanged.
- While the clipboard holds cut or copied items, Paste and Clear clipboard appear in the listing header, and Clear clipboard also joins the selection action bar and row menus while rows are selected; Ctrl/Cmd+V also pastes into the current folder. Cut items fade until the cut is pasted or cleared, and pasting a cut back into its source folder just ends it.
- The step search box matches words in any order, ranked by relevance and tolerant of typos and abbreviations. It searches the steps linked from folders by name, id, type, folder, note and argument values, shows where each result matched, and opens a step in the folder that matched. Steps whose every folder link is hidden rank lower. Suggestions appear only once something is typed, and the box says when nothing matches.
- When the backend has embedding and judge models configured, the same search box also lists semantically related steps under a Related heading, scored by the judge. Related entries skip the steps the word match already lists and open the step in one of its folders. While they load the menu says so, and if the backend cannot provide them the menu notes that and keeps the word matches.

## Activity tray

- The bottom tray counts running, queued and starting builds and uploads, and says Idle when there are none. Cluster health has its own label; hover it for the reported detail. Until the first cluster status arrives, the health label shows a spinner beside Connecting and the tray does not say Idle.
- A build is starting from the moment the backend accepts it, while it resolves dependencies and submits Slurm jobs, until Slurm lists a queued or running job for it.
- Opening the tray groups steps by state, and each group collapses.
- Each row shows the step's folder path under its name, starting below Home.
- Running rows show elapsed time, queued rows show how long they have waited and Slurm's pending reason, starting rows show how long they have been preparing, and uploads show progress. A build of a commit other than the current one carries that commit's short hash.
- Running, queued and starting rows have a Stop button that cancels the build at the commit it is building.

## Instance settings

- The user menu in the status bar opens User repository and Instance configuration. Without tokenAuth the menu is labelled Settings and has no Log out.
- User repository shows the public half of the backend's deploy key (`~/.ssh/id_ed25519_deploy` in the backend's home, generated on startup when missing; place a key there to use your own) with a copy button, and the SSH clone URL and branch. Add the key as a deploy key with write access on GitHub (Settings → Deploy keys) or GitLab (Settings → Repository → Deploy keys). Saving first checks the key can read the branch, then rewrites only the `[user-repo]` table in the configuration, keeping the rest of the file and its comments; when `[user-repo]` is written as an inline table or dotted keys instead, change it under Instance configuration.
- Instance configuration shows `config.toml` as a form generated from the configuration's definition in `backend/src/Config.hs`: every setting appears with its description, prefilled with its current value or default; choices appear as dropdowns, provider fields list the `[[providers]]` entered in the same form, and each top-level section can be collapsed, which only hides it. Optional sections have a checkbox: `[search]` has an Enabled checkbox inside it and its heading is marked "(off)" when unchecked, and nested ones such as `[search.judge]` have it in their heading. Saving the form rewrites `config.toml` without its comments. Edit as TOML switches to the raw file, which keeps comments; the dialog opens there when the file cannot be shown as a form. Either way, saving rejects values the backend cannot load, naming the offending key, and leaves the old file in place.
- Saving from the UI writes `config.toml`, reloads it, re-clones the user repository when its URL changed and fetches the configured branch, then reloads the page; if applying fails the dialog shows why. Saves run one at a time. The backend also watches `config.toml` with inotify, so edits made on disk apply the same way without a restart.
- Agent turns always run Pi inside sbox, both from the NixOS module; neither is set in `config.toml`.
- The backend starts even when the user repository is unreachable, so a new instance can be pointed at its repository from the UI.

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
