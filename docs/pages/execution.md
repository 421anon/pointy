# Execution and Data Management

This page covers running steps, uploading files, browsing results, using source files, and creating share links.

## Running derivation steps

Derivation steps show a **Run** button. Clicking it asks the backend to build that step and whatever upstream dependencies it needs.

Pointy runs builds from a Git-pinned version of the user repository, so a build corresponds to a specific committed workflow state.

![A derivation step row showing the Run button alongside the step status indicator and other step actions.](screenshots/light/step-derivation-header.png)

<style>
@keyframes doc-status-spin { to { transform: rotate(360deg); } }
.doc-status-spinner {
  display: inline-block;
  width: 9px; height: 9px;
  border-radius: 50%;
  border: 1.5px solid rgba(100,100,100,0.25);
  border-top-color: #666;
  animation: doc-status-spin 0.8s linear infinite;
  box-sizing: border-box;
  vertical-align: middle;
}
.doc-status-dot-wrap {
  display: inline-block;
  position: relative;
  width: 9px; height: 9px;
  vertical-align: middle;
}
.doc-status-dot-wrap .doc-status-spinner {
  position: absolute;
  top: 0; left: 0;
  border-color: rgba(255,255,255,0.35);
  border-top-color: white;
}
</style>

## Step statuses

Each step row shows a small status dot. Pointy reports six statuses:

- <span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(255,255,255,0.4),#808080 60%);box-shadow:inset 1px 1px 2px rgba(255,255,255,0.6),0 2px 4px rgba(0,0,0,0.1);vertical-align:middle"></span> **Not Started** — the step exists but has not produced a successful output yet
- <span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(255,255,255,0.4),#e2b714 60%);box-shadow:inset 1px 1px 2px rgba(255,255,255,0.6),0 2px 4px rgba(0,0,0,0.1);vertical-align:middle"></span> **Running** — the backend is currently building the step
- <span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(255,255,255,0.4),#2ea043 60%);box-shadow:inset 1px 1px 2px rgba(255,255,255,0.6),0 2px 4px rgba(0,0,0,0.1);vertical-align:middle"></span> **Success** — the build finished successfully and output files are available
- <span style="display:inline-block;width:9px;height:9px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(255,255,255,0.4),#e05252 60%);box-shadow:inset 1px 1px 2px rgba(255,255,255,0.6),0 2px 4px rgba(0,0,0,0.1);vertical-align:middle"></span> **Failure** — the build failed; hover the dot to see the last meaningful line from the build log when available
- <span class="doc-status-spinner"></span> **Loading** (unknown) — the status is not yet known; a spinner is shown with no dot behind it
- <span class="doc-status-dot-wrap"><span style="display:block;width:9px;height:9px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(255,255,255,0.4),#e2b714 60%);box-shadow:inset 1px 1px 2px rgba(255,255,255,0.6),0 2px 4px rgba(0,0,0,0.1);"></span><span class="doc-status-spinner"></span></span> **Loading Running** — the step was just started and Pointy is waiting for the backend to confirm it is running; the yellow Running dot is shown behind the spinner

Status changes are pushed to the browser automatically, so you do not need to refresh the page while waiting for a build.

![A project view with step status indicators visible in the left column; these update automatically as backend status events arrive.](screenshots/light/project-view-status-tooltip.png)

## Shareable states

- **Running** steps are shareable.
- **Successful** steps are shareable.
- **Failed** steps are not marked shareable.

In Pointy, shareable means the UI can expose inspect, clone, and share actions for that step state. Successful steps also expose share actions on output entries. See [Building Workflows (Steps)](steps.md) for step-editing workflows.

## Stopping a running step

While a step is running, a **Stop** button appears in the same control area as **Run**. Stopping the step cancels the active build so you can adjust the step and run it again.

![A derivation step row while the step is running, with the Stop button hovered in the execution control area.](screenshots/light/step-derivation-stop-header.png)

## Uploading files

File upload steps use **Upload files** instead of **Run**.

Templates decide which file extensions are accepted. You can upload one or more files, watch transfer progress in the UI, and cancel an in-progress upload if needed.

After the upload finishes, Pointy immediately builds the file-upload step, so it then moves through the same Running / Success / Failure lifecycle as other steps.

![A file-upload step row showing the upload-oriented controls used for input steps.](screenshots/light/step-file-upload-header.png)

## Browsing output files

Successful steps can open an **Output Files** browser.

From there you can:

- expand folders
- preview supported files inline
- download files
- share the whole output, a folder, or a specific file

![The output files browser open on a script step, showing an expanded output folder with a previewed hello file displaying its Hello World content.](screenshots/light/output-files-browser.png)

HTML outputs support both:

- inline preview
- **Open in new tab** for a normal browser view

Inline HTML preview also exposes **Zoom In** and **Zoom Out** controls.

![The output files browser open on a fastqc step, showing chim_R1_fastqc.html previewed inline with its FastQC Report content visible alongside the other output files.](screenshots/light/output-files-html-preview.png)

## Source files

Some derivation step types enable **Source Files**. For those step types, Pointy shows a Source Files section in the step view even when the directory is still empty.

That section lets users:

- browse and download any existing source files
- see which `srcFiles/<step-id>/` directory belongs to the step
- see which user-repository URL and branch those files come from

Source files are configured by instance admins. See [Setting Up the User Repository](user-repo-setup.md#injecting-srcfiles-into-a-build) if you need to enable this for a template.

![A step showing the Source Files section with an expanded directory view of the srcFiles directory from the user repository.](screenshots/light/step-source-files-section.png)

## Share links and read-only views

Shareable steps and shareable output entries expose a **Share** action.

A share link:

- opens the project in a **read-only** view
- pins the page to a specific Git commit
- can deep-link to the step itself or to a specific output file or folder inside it

This is what makes share links stable: the recipient sees the version of the workflow state that the link was created for, not whatever happens to be current later on.

The read-only banner includes a **View current version** link when you want to leave the pinned view and go back to the live project.

![The share button hovered next to an output file, showing the file-level action used to create a deeplink.](screenshots/light/output-file-share-button.png)
