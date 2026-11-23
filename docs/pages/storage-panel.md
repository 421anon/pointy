---
description: "See how much disk and cold storage step outputs use, and freeze, rewarm, or delete old revisions."
---

# Storage panel

Open **Storage** from the drive icon in the bottom status bar to see how much space built step outputs take on this server and, when configured, in cold storage.

![The Storage panel open above the status bar, showing warm quota and cold storage bars, steps grouped by size, and an expanded step with its revision actions.](screenshots/light/storage-panel.png)

## Warm and cold capacity

At the top of the panel, capacity bars show how much space is in use:

- **Warm storage** covers step outputs stored on this server. On a filesystem, the bar compares step output bytes against those bytes plus the filesystem's free space; on an account quota, it compares step output bytes against the quota total, so freezing or deleting outputs frees quota immediately.
- **Cold storage** appears only when your administrator configured a cold storage provider. When the provider reports available space across its storage boxes, the panel shows a capacity bar; otherwise it shows your cold bytes without a bar.

Space is measured from the NAR sizes Nix records for each output path. **Updated** shows when Pointy last finished checking storage state.

## Steps and revisions

Below the bars, Pointy lists every step that still has built output revisions in the store, ordered by the larger of each step's warm and cold bytes so offloaded steps stay easy to find.

Each step row shows its total local bytes, cold bytes (when cold storage is enabled), and revision count. Click a step's id to open its output page, or expand the row to inspect individual revisions:

- the short Git commit hash and how long ago it was committed
- the step's historical name (when it differs from the current name)
- the output kind: **derived**, **download**, or **upload** (including scratch wraps)
- its local and cold sizes

When Pointy opens a large repository for the first time, it scans Git history in the background and shows scan progress at the top of the panel.

## Freezing, rewarming, and deleting outputs

Hover over a step or revision row (or focus it with the keyboard; always visible on touch screens) to reveal its actions:

- **Freeze** (when cold storage is enabled) moves local output files to cold storage. On a step row, it freezes every local or failed revision one after another; on a revision row, it freezes that revision. Progress appears on the row while files move.
- **Rewarm** (when cold storage is enabled) copies a frozen or failed revision's files back to this server. On a step row, it rewarms all cold revisions of that step.
- **Delete** permanently removes a revision's output from this server — and, when it has files in cold storage, removes its cold copy too — after asking for confirmation. The confirmation notes whether the output is derived (can be rebuilt) or a download/upload (must be fetched or uploaded again).

Freezing, rewarming, and deleting are disabled while the revision's own step, or any step that directly reads its output, is running or queued; the row names and links the active step.

For opening individual offloaded files or copying back cold inputs before a run, see [Outputs](outputs.md#browsing-output-files). For cold storage configuration and provider setup, see [Storage](storage.md).
