---
description: "Select, copy, move, link, reorder, hide, and delete folders and steps, and see when changes reach Git."
---

# Organize

## Selecting items and the action bar

Select items with the checkbox on the left of a row, Ctrl/Cmd-click a row, or Shift-click to select a run of rows. Ctrl/Cmd+A selects every visible row. Escape clears the selection. The selection belongs to one folder and clears when you switch folders, so the things you act on are always the things on screen.

With at least one item selected, an action bar covers the listing header, showing how many you selected and what you can do:

- **Move to...**, **Link to...**, and **Group into new folder** put the selection somewhere else; see [Moving, linking, and reordering](#moving-linking-and-reordering).
- **Cut** and **Copy** put the selection on the clipboard; see [Cut, copy, paste, and duplicate](#cut-copy-paste-and-duplicate).
- **Clear clipboard** discards the clipboard while it holds items.
- **Hide** or **Unhide** changes visibility; see [Hidden items](#hidden-items).
- **Remove from here** and **Delete permanently** let go of the selection; see [Removing and deleting](#removing-and-deleting).
- **Duplicate** makes copies of the selected items without using the clipboard; see [Cut, copy, paste, and duplicate](#cut-copy-paste-and-duplicate).
- **Clear** deselects.

When the listing is narrower than 1100 px the buttons show only their icons; the name appears on hover. Right-clicking a row opens the same actions as a context menu, and right-clicking an empty spot in the list offers **New folder**, plus **Paste** and **Clear clipboard** while the clipboard holds items.

![The action bar over the listing header with two rows selected, showing Move to, Link to, Group into new folder, Cut, Copy, Hide, Remove from here, Duplicate, Delete permanently, and Clear.](screenshots/light/listing-selection-bar.png)

![The row context menu with the same actions as the action bar.](screenshots/light/listing-context-menu.png)

### Cut, copy, paste, and duplicate

**Cut** and **Copy** remember the selection; the clipboard survives moving to another folder. **Cut** items fade where they sit, in the listing and the navigation drawer, until you paste, cut or copy something else, or clear the clipboard. In the folder where you paste:

- items you copied are **linked** there — the same items, not copies;
- items you cut are **moved** there, and the clipboard then empties.

Pasting a cut back into its own folder simply ends the cut. **Clear clipboard** in the header, the action bar, or a context menu discards the clipboard without changing any folder.

**Duplicate** — in the action bar, a row's action menu, or the row context menu — makes independent copies without using the clipboard. A duplicated step gets a new name from its original: **Greeting script** becomes **Greeting script (Clone)**, or **Greeting script (Clone 2)** if that name is taken. A duplicated folder is named like **Quality control (Copy)** and links the same children, so both folders show the same steps and subfolders.

## Moving, linking, and reordering

**Move to...** opens a dialog that lists every folder by path; type in **Search folders** to filter the list. Choose a target and confirm with **Move**: the items are taken out of this folder and appended to the target. **Link to...** is the same dialog with the opposite result — confirm with **Link** and the items appear in the target while staying here. Both dialogs leave out the selected folders and their subfolders, and **Move to...** also leaves out the current folder, so a folder can never end up inside itself.

**Group into new folder** asks for a **Folder name**, prefilled **New folder**, creates that folder here with this folder's templates, and moves the selection into it; **Create** confirms.

You can also drag rows: grip a row by its header — there is no separate handle — and drop it on a folder row, a breadcrumb, or a folder in the navigation drawer. Over a folder, a plain drop moves the items; hold Ctrl, Cmd, or Alt while dropping to link them instead. A dashed outline shows the target folder, and a line between rows shows a reorder position. Dragging a selected row takes the whole selection along. A folder cannot be dropped into itself or into a folder below it, and such a target does not light up. The selection clears after a drop.

Drop on the top or bottom edge of a row to reorder. Lines appear only in the gaps where the drop would change the order, and the drop stays within the row's folder and group. Reordering works while **Sort** is **Manual**, including inside a type group when **Group by type** is on. The order you set is kept per folder, so the same step can sit in different positions in different folders.

Any folder except Home, or any step, can be linked into several folders at once. It is the same item everywhere: editing it in one folder changes it in all of them — it is one item, not a copy. The ways to link something are **Link existing**, **Move to...** or **Link to...**, **Copy** then **Paste**, and Ctrl/Cmd/Alt-drag.

![The Move to dialog with two rows selected, the Search folders field, folder paths as targets, and the Move and Cancel buttons.](screenshots/light/organize-move-dialog.png)

![The Also in popover on the Quality control folder, listing Example analysis as its other parent.](screenshots/light/also-in-popover.png)

## Keyboard shortcuts and undo

With a listing on screen: Ctrl/Cmd+A selects all, Ctrl/Cmd+X, Ctrl/Cmd+C, and Ctrl/Cmd+V cut, copy, and paste, Ctrl/Cmd+Z undoes the most recent change, and Delete or Backspace removes the selection from here. Escape closes a dialog or menu first, and otherwise clears the selection. Shortcuts are ignored while you type in a field.

Most changes arrive with a toast whose **Undo** button reverts exactly that change. Ctrl/Cmd+Z does the same for the most recent change still in the history, which lasts until you reload the tab. **Delete permanently** is the exception: it has no undo.

## Hidden items

**Hide** takes an item out of the default view without deleting it; **Unhide** brings it back. Hidden rows are left out of the listing and hidden folders out of the navigation drawer, unless **Show hidden** is on. Then hidden rows carry a crossed-eye icon and a faded name, while hidden folders in the drawer look like any other folder. Hiding is recorded per folder: the same step can be hidden in one folder and visible in another. **Unhide all** in the listing header restores every hidden child of the current folder.

## Removing and deleting

Two actions take items out of a folder, and they do different things:

- **Remove from here** — in a row's action menu, the action bar, or Delete/Backspace — takes the items out of this folder only. The steps and folders themselves stay in Pointy and in their other folders. There is no confirmation, and the toast offers **Undo**. A step that ends up in no folder drops out of the listing, and a page load no longer shows it in search or offers it in **Link existing**; its file stays in the repository. A folder with no parent left is out of the tree until you link it somewhere again.
- **Delete permanently** — in the action bar and the row context menu, and in red — asks for confirmation, and cannot be undone. For a step it deletes the step and its source files from the repository and removes it from every folder. For a folder it deletes only the folder: the steps and subfolders inside are not deleted, they stay in their other folders, and a step left in no folder drops out of Pointy. Home cannot be deleted.

Pointy refuses some deletes: a step under review must have its review removed first, and a step that another remaining step depends on cannot be deleted. **Delete permanently** is not offered while a reviewed step is selected, but **Remove from here** stays available in the action bar and the row's action menu. If the server refuses anyway, the toast shows the reason and the change is discarded.

![The Delete permanently dialog with the warning that this cannot be undone, and the red Delete and Cancel buttons.](screenshots/light/organize-delete-dialog.png)

## When changes reach Git

Organize changes — moving, linking, removing from here, hiding, unhiding, reordering, pasting, and deleting — appear in the page immediately and are sent to Git together in one commit about five seconds after your last change. If you close or leave the page before that, the pending changes are sent as the page goes away. Creating a folder or a step, renaming, and saving the folder form are committed immediately instead.

If the server refuses a change, the toast shows its reason, the change is discarded, and the folder reloads from Git.
