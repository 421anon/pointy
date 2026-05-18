# Managing Projects

Projects are the top-level containers in Pointy. The home page lists them and lets you open, create, reorder, hide, and delete them. Each project also chooses which step templates are active, either through an admin-defined preset or through a custom template list.

![The project list shows all projects with row-level actions such as edit, hide, and delete.](screenshots/light/projects-home.png)

If you want to work inside a project after opening it, continue with [Building Workflows (Steps)](steps.md).

## Opening, creating, and renaming projects

Click a project row to open it.

To create a project, use the **+** button in the Projects table header.

![The Projects table header with the add-project control hovered alongside Show Hidden and Unhide All.](screenshots/light/projects-header-controls.png)

![The new project form lets you name your project before saving.](screenshots/light/project-create-form.png)

You can rename an existing project from the edit form, or by using the inline pencil icon next to the project name. Project and step rows also show a relative **last modified** timestamp when Pointy can read the corresponding Git history from the user repository. Hover it to see the exact timestamp.

## Choosing project templates

The project form includes two related controls:

- **Preset** — an admin-defined bundle of templates, such as a standard workflow type. New projects default to the first preset by `sortKey` when presets are available.
- **Templates** — a custom list of active step templates for the project. Choose **Custom (no preset)** when the project should not follow a preset bundle.

Only active templates get their own step sections inside the project. Existing assigned steps whose template is no longer active are not deleted; Pointy moves them into a collapsible warning at the top of the project so you can decide whether to re-enable the template, unassign the step, or leave it hidden from the main workflow tables.

If a project references an unknown preset, an unknown template, or a step id that no longer exists in the user repository, Pointy surfaces that validation error in the project row and again inside the project view instead of silently dropping the information.

## Hiding and showing projects

Hiding a project removes it from the default home-page list without deleting it. This is useful when you want to keep old work around but reduce visual clutter.

Use:

- **Hide** on a project row to hide a single project
- **Show Hidden** in the table header to include hidden projects in the list
- **Unhide All** in the table header to make every hidden project visible again

![The row-level visibility button toggles whether a project appears in the default list.](screenshots/light/steps-visibility-button.png)

## Reordering projects

You can reorder projects with the drag handle shown on each row. The order is saved, so the home page keeps the same project ordering on reload.

![A close-up of the drag handle used to reorder rows.](screenshots/light/steps-reorder-handle.png)

## Deleting projects

Deleting a project removes the project itself from the UI and deletes its `projects/<id>.nix` definition from the user repository.

![The delete action for a project, shown as the row-level Remove icon button.](screenshots/light/project-delete-button.png)

## Jumping directly to a step

The header search box is a **global step search**. It does not search project names. Instead, it lets you jump straight to a step from anywhere in the UI.

Selecting a result opens the corresponding project and highlights the chosen step.

![The header search box is used to jump directly to a step.](screenshots/light/steps-search-box.png)

For step-level organization inside a project, see [Building Workflows (Steps)](steps.md). For running steps and browsing outputs, see [Execution and Data Management](execution.md).
