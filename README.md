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

## Linked data

- An artifact permalink, `/artifact/<project path>/<step id>/<commit>`, names the step's output at that commit, and `?path=<file>` names one file in it, the link the file browser opens. Requested with `Accept: text/turtle`, either returns that artifact and every artifact upstream of it at that commit as Turtle. The commit must be the full 40-digit lowercase hash; abbreviated hashes are refused because a prefix that is unique today can become ambiguous. The response never changes, so it is served as immutable.
- The Turtle is generated by Nix in `pointy-stdlib`, not by the backend: `pointy.root` mirrors the folder tree, `pointy.root.<folder>….steps.<step>.turtle` holds each output's document and `fileTurtle` a file's. The backend evaluates one document per request and the long-lived `nix repl` sessions keep the per-commit evaluation, so repeated permalinks do not recompute the repository. A commit whose `pointy-stdlib` predates linked data, so that `pointy.root` does not exist, is answered with 404, and so is a file request at a commit whose `pointy-stdlib` predates `fileTurtle`.
- IRIs are relative to the requested URL unless the instance names its origin with `services.pointy-host.linkedDataBase`, for example `"https://pointy.example.com"`; then the document starts with `@base <https://pointy.example.com/> .` and every permalink IRI resolves to that origin, whichever host served it.
- Each output is a `prov:Collection` and a `pointy:Step`, with its step's name as `rdfs:label`, its template, `prov:wasDerivedFrom` edges to the outputs its step consumes and `schema:about` links. There are no activities, because a pure build has no time, and no store paths, because a store path only means something on the host that built it. A file is a `prov:Entity` labelled with its name, and its output lists it with `prov:hadMember`. Files are named only when a link asks for one: the document does not read the store, so it is the same whether or not the output is built. An upstream output is named by the permalink of the folder its dependency reference records, or, without a usable recorded folder, of the first folder holding it. A step directly in the root folder, or in no folder, is named by `/artifact/<step id>/<commit>` with no folder segment.
- A dependency reference inside a step's `args` is `{ step = N; }` or, when the UI knows the folder holding step N, `{ path = [ P Q ]; step = N; }`. The path lists folder ids from a child of the root folder down to the folder that holds N. Builds ignore `path`, so outputs and certificates do not depend on folder organization.
- If step N is not in the recorded folder at the evaluated commit, the reference is stale and falls back to `{ step = N; }`: the build follows N wherever it lives, and the dependent step gets an informational notice.
- Moving or deleting folders and steps rewrites the affected references in the same commit as the organizational change, so recorded paths stay current without a rebuild. A reference follows the step's new folder when it has exactly one; otherwise it drops its `path`. A rewritten step file is regenerated the way UI saves write it, so hand-written formatting in that file is normalized.
- When an agent proposes a commit that would introduce stale references, applying it is refused and the agent is asked to point the references at the folder the step is in now, or to drop their paths when the step was removed. Reviewed steps cannot be edited by the agent, so the backend rewrites their references itself when a folder they point into moves.
- A step file may list the records its data is about beside `note`, for example a sample in an electronic lab notebook: `about = [ "https://notebook.example.org/graph/samples/…/" ];`. Editing or cloning the step in the UI keeps the list. Only absolute http(s) addresses are published.

## AI agent

- Sending a prompt asks for browser notification permission the first time. Afterwards, the tab that sent a turn shows a desktop notification when the turn finishes, fails or is stopped, and when the agent asks a question during the turn. No notification appears while the page has focus and that thread is open in the agent panel. Clicking a notification focuses the tab and opens the thread in the agent panel.
- The tab keeps its sent turns in `sessionStorage`, so after a reload it resumes watching the ones still running and notifies about turns that ended during the reload. A question already notified before the reload is not notified again. Other tabs and new browser sessions don't notify about turns they didn't send.
- Notifications are desktop-only: mobile browsers only allow notifications through a service worker, and Pointy unregisters service workers.

## Artifact viewer

- An artifact permalink opened in the browser shows the file full-screen, with Open agent and View in Pointy buttons. The file browser links outputs the browser can show natively, and CSV or TSV outputs up to the in-app 5 MB limit.
- CSV and TSV files open in the same grid as the file browser and Compare, with sorting, filters, summaries and the plain-text view; the plain-text view, like the file browser's, draws one text block and only the line numbers near the visible part. A download button sits in the viewer's toolbar in both modes.
- The Open agent button opens the agent panel, closed by default. Every turn sent from the page tells the agent which step, commit and output file is open, and the output's store path when the backend can resolve it. The chat shows only what the user typed.
- Ask agent, shown when the file is rendered in a frame, lets the user drag a rectangle over it. On release, the selection is attached to the agent's message box as one area capture, without sending; the panel opens and, if no chat is open, a new one starts. Escape cancels. The composer and the chat show it as a single image tile of the selected area with the selector's dashed border; clicking it opens the selected area and the overview in the image viewer. An area capture holds the selected area rendered at the screen's pixel ratio, the whole embedded document scaled down to at most 1024 pixels on its longest side with the selection outlined in red, the document's size, and the selection's position and size in the document's CSS pixels. The agent receives both images and a note saying which two images they are and what the numbers mean. Pages without `<!doctype html>` may come out shifted, because the renderer lays out its copy in standards mode.

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
