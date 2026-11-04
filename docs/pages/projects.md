---
description: "Find your way around Home, the folder tree, and the listing, and create folders with their templates."
---

# Folders

A folder keeps related steps together. Folders hold steps and other folders, so you can nest an analysis as deep as you like. The listing and the organize controls call these containers folders; a few older forms and messages still say "project", such as **Edit project** and the **Project name** field.

![The Example analysis folder page: breadcrumbs, the step search box, the listing header, and rows for its steps and its nested Quality control folder.](screenshots/light/folder-view.png)

You can ask the [Agent](agent.md) to set up or change the analysis. **Auto-apply** is on by default, so a successful turn's changes are applied automatically; turn it off in the agent panel to review each changeset before applying it. For smaller changes, you can also [edit steps yourself](steps.md).

## Home and the folder tree

Home is the root folder, at `/`. A subfolder's URL lists the folder ids from Home down, for example `/project/1/2`. If a saved URL no longer matches the current tree, Pointy opens the last folder in it. If that folder no longer exists, the page shows **Project not found** with a **Go home** link.

A folder page starts with breadcrumbs that name the whole path, with the current folder last. When the folder is also linked from somewhere else, an **Also in** button next to its name lists those other parents. Next to the breadcrumbs is **Edit project**, and the step search box sits on the right. Below them, the **Contents** listing holds one row per child — subfolders and steps — split by type when **Group by type** is on:

- A plain click opens a folder; on a step row it opens or closes the step's output files when its output has been built. Ctrl/Cmd-click or Shift-click selects the row instead; see [Selecting items](organize.md#selecting-items-and-the-action-bar).
- Folder rows show their name in bold, the folder icon, and a rollup after the name: one chip per status found anywhere below the folder, then the total number of steps. Every step in the whole subtree counts once, even when it sits in several subfolders, and hidden items count too.
- Every row shows when it was last modified; hover over the time for the exact date and time. For a folder this is the last change to the folder itself, not to the steps inside it.

![Home, the root folder, with folder rollup chips on its rows and the navigation drawer open beside the listing.](screenshots/light/home-listing.png)

### The navigation drawer

The drawer on the left holds the same folder tree, starting at Home. On a wide window Pointy opens it by default; use **Hide navigation** in its header to collapse it to a narrow rail, and **Show navigation** to bring it back. Folders that contain subfolders have an expander next to them; Pointy expands the folders along the path you open, and leaves open the ones you expanded yourself. The current folder is bold. Hidden folders appear only while **Show hidden** is on, and look like any other folder.

The drawer stays open while you move between folders; it closes only when you hide it. Drag its edge to resize it, and scroll it when a folder name is too long to fit. On a window 1000 px wide or narrower the open drawer floats over the page instead of sitting beside it, and on a phone-sized window it covers the full width.

## The listing header

The header above the listing shows **Contents** with the number of rows it is showing, and the controls for how the listing is shown:

- **N hidden** appears next to the count while hidden children are out of view; click it to turn on **Show hidden**.
- **Sort** chooses the order: **Manual** (the order stored in the folder), **Name**, **Type**, **Created**, **Modified**, or **Status**. Choosing the active option again reverses the order; an arrow next to it shows which direction is in use. When the listing is grouped, each group is sorted this way.
- **Folders first** keeps folders above steps when the listing is not grouped; it is on by default. **Group by type** splits the listing under a **Folders** heading and one heading per step type that occurs; it is also on by default, so listings open grouped. **Show hidden** reveals hidden items; see [Hidden items](organize.md#hidden-items).
- **New** opens the add menu: **New folder** first, then a **New …** entry for each of the folder's selected templates in order, and **Other types** with every remaining template. You can add any template's step in any folder; see [Folder templates](#folder-templates).
- **Link existing** links steps or folders that already exist into this folder; see [Creating, renaming, and editing folders](#creating-renaming-and-editing-folders).
- **Unhide all** appears when this folder has hidden children, and unhides them all.
- **Paste** and **Clear clipboard** appear while the clipboard holds items; see [Cut, copy, paste, and duplicate](organize.md#cut-copy-paste-and-duplicate).

The sort and the three toggles apply to every folder you open, and reset when you reload the page.

![The listing header on Home: Contents with its count, the sort control, the Folders first, Show hidden, and Group by type toggles, New, Link existing, and Unhide all.](screenshots/light/listing-header-controls.png)

![The New menu on a folder with Read QC and Script selected: New folder, a New entry per selected template, and Other types for the rest.](screenshots/light/listing-new-menu.png)

## Creating, renaming, and editing folders

To create a folder, open the folder that should contain it and choose **New folder** from the **New** menu. The form is titled **Create new Project**:

- **Name** is the folder name; the field's placeholder is **Project name**.
- **Preset** picks one of your administrator's template bundles, such as a standard analysis. **Custom (no preset)** lets you choose templates yourself; adding or removing a template switches the folder to its own list.
- **Templates** lists the step kinds the folder puts first in its **New** menu.

**Save** creates the folder inside the current one and commits it right away. **Cancel** closes the form without saving. The header's **Edit project** button, or **Edit** in a folder row's action menu, opens the same form as **Edit Project** with the folder's current settings. The folder page form appears above the listing; the row form appears under the row, and its row shows a **Saving...** spinner while the change goes through.

![The folder form, titled Create new Project, with its Create new and Link existing modes and Name, Preset, and Templates fields.](screenshots/light/folder-create-form.png)

For a quick rename, hover a folder's name in the listing and click the pencil that appears. Type the new name and press Enter or click outside the field to save; Escape cancels. Saving starts at once.

When you right-click an empty spot in the listing and choose **New folder**, the dialog skips the full form: it asks for a **Folder name**, prefilled **New folder**, and **Create** makes the folder here with this folder's templates.

To add items that already exist instead of creating them, use **Link existing** in the listing header. Pick steps and folders from the list; each entry shows its type, name, and a folder path, and hovering over an entry lists the folders it sits in. **Save** links every picked item into this folder; it is the same step or folder, so later changes stay in sync everywhere.

![The Link existing form above the listing, with the picker open, an item chosen, and candidates such as Trim reads and the Archived pilot folder listed below.](screenshots/light/link-existing-form.png)

## Folder templates

A folder's preset or template list does not filter the folder's contents: every step and subfolder stays visible whatever is selected. The selection decides three things:

- the order of the **New** menu, as described under [The listing header](#the-listing-header);
- what a new folder created inside this one starts from, whether through **New folder** or **Group into new folder**;
- what a duplicated folder copies.

A new folder starts with the current folder's preset or template list; a duplicated folder keeps its own.

It does not decide which kinds of step you can add, and it does not control the **Create …** shortcuts some step rows offer; [Steps](steps.md) covers those. Pointy can also warn about a folder itself: if the folder's file no longer evaluates, the reasons appear in a box at the top of the folder, and the folder's row in its parent shows an error icon whose tooltip lists them.

The folder form's **Preset** field shows **Custom (no preset)** once you edit the template list yourself. Choosing **Custom (no preset)** keeps the preset's templates as an editable starting list.

Continue with [Steps](steps.md) or [Runs](execution.md).
