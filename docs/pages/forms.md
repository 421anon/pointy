---
description: "Choose how a template's inputs appear in the step form."
---

# Forms

## Bindings

A binding changes presentation, not the declaration. Its name must match a language parameter. A record override must name a real record field. Unknown binding names, record-field overrides, and unsupported widget choices fail configuration evaluation instead of silently adding fields.

For the `Analysis` interface in [Contracts and argument values](type-reference.md#contracts-and-argument-values):

```nix
bindings = {
  body = {
    displayName = "Script";
    description = "Shell code to run for this analysis.";
    widget = { kind = "code"; language = "shell"; };
  };
  threads.displayName = "Threads";
  reads = {
    displayName = "Input reads";
    quickCreate = true;
  };
  dependencies.list.displayName = "Extra reads";
  samples.list.record.fields = {
    name.displayName = "Sample name";
    layout.enumDisplayNames = {
      single = "Single-end";
      paired = "Paired-end";
    };
    packages.widget = { kind = "tokens"; hook = "packages"; };
  };
};
```

Here `packages` is an autocomplete hook you provide. An outer list's labels belong on the outer binding. Element overrides go under `list`; record-field overrides go under `record.fields.<name>`. Nest those once for each layer of the declared shape.

| Shape | Binding keys |
| --- | --- |
| Text, integer, or boolean | `displayName`, `description`, `widget`, `autocomplete`, `readOnly`, `path` |
| Choice | `displayName`, `description`, `widget`, `enumDisplayNames` |
| Step input (`subject`) | `displayName`, `description`, `widget`, `quickCreate` |
| List (`array`) | `displayName`, `description`, `widget`, `list` |
| Record | `displayName`, `description`, `record` |

`enumDisplayNames` changes labels, not stored values.

A **step-input** picker is driven by the language, not by a binding key. It offers the steps linked directly in the folder you are editing whose template's output meets the parameter's declared type (the field's `accepts`); steps in other folders, and steps outside every folder, are not offered. The type system's outright proofs (`proven`) decide which rows get the quick-create shortcut. There is no `allowedTypes` key. A saved reference whose step is not loaded shows as `#<id> (missing)`.

`quickCreate = true` on a step-input binding adds a **Create …** row action to steps whose template the field proves meets the parameter's type; the label names the target template. The action opens the target step's form with this input already selected. It appears only for target templates that steps in the same folder already use. For a list of step inputs, put it on the element: `bindings.dependencies.list.quickCreate = true;`.

## Widgets

A widget is a form control. Pointy chooses a default to match the argument's type; declare `widget` only to change the control or add an autocomplete hook.

| Argument shape | Default | Other supported presentation |
| --- | --- | --- |
| Text | `text` | `textarea`, `code`, `command`, `datetime` |
| Integer | `number` | Use `number` with this frontend. |
| Boolean | `checkbox` | — |
| Choice | `select` | Labels through `enumDisplayNames`. |
| Step input | `step` | Filtering and quick-create on the input binding. |
| List of text | `tokens` | Optional `hook` on the list's widget. |
| List of step inputs | `steps` | Input settings under `list`. |
| Other list | `list` | Element bindings under `list`. |
| Record | `record` | Fields under `record.fields`. |

The complete widget vocabulary is `text`, `textarea`, `code`, `command`, `number`, `checkbox`, `select`, `tokens`, `list`, `step`, `steps`, `record`, and `datetime`. It is not an arbitrary browser-component name. `code` requires `language`, and `command` requires `prefix`:

```nix
bindings.body.widget = { kind = "code"; language = "python"; };
```

For a text parameter called `extraArgs`, a command-style control would be:

```nix
bindings.extraArgs.widget = { kind = "command"; prefix = "fastqc"; };
```

The prefix is a display hint, not a command automatically prepended to the build. The template still decides how to use the value. Widgets must match the field's shape in both the stdlib and frontend; in particular, use `number`, not `text`, for integer arguments. Only the `text` and `tokens` widgets accept a `hook`.

### Read-only fields and paths

A text, integer, or boolean binding may set `readOnly = true`. A `path` list tells the frontend where to read the value inside `args`, as with the generated download timestamp. This only changes the form: it does not add an argument or enforce an edit restriction on the backend.

There is no `visible = false` option system in this API. Values supplied by Pointy, such as `args.id` and upload/download store references, are handled by the stdlib rather than declared as hidden form options.

## Autocomplete

Hooks return JSON-serializable lists of strings. This template-level hook works with `packages.widget` above:

```nix
autocomplete = { pkgs, lib }: {
  packages = { query ? "", limit ? 25, ... }:
    lib.take limit
      (lib.filter (lib.hasPrefix query) (builtins.attrNames pkgs));
};
```

A token list names the hook on its own widget: `{ kind = "tokens"; hook = "packages"; }`. If the list widget has no hook, the frontend falls back to the hook of its text element. Scalar text bindings also accept `autocomplete = "hookName"`, which supplies the hook when the default widget is selected. An explicit `widget` replaces that default, so include its `hook` explicitly when needed.

The frontend sends `query`, `limit`, and simple text/choice values from the surrounding record as context. Accept those values in the function's argument set, or use `...` if you do not need them. `limit` defaults to 25 and is clamped to 1–100, and the query and context values may only contain letters, digits, and `_.+-`. Suggestions do not restrict the final value: users may enter their own strings.

## The generated form document

`nix eval --json .#pointy.stepConfig` produces a document with this structure:

```json
{
  "version": 4,
  "templates": {
    "hello": {
      "displayName": "Greeting",
      "description": "Write a message into a result file.",
      "sortKey": 0,
      "icon": "description",
      "kind": "derivation",
      "withSrcFiles": false,
      "fields": [
        {
          "name": "message",
          "label": "Message",
          "help": "Text to put in message.txt.",
          "readOnly": false,
          "required": false,
          "default": "Hello, research!",
          "widget": { "kind": "textarea" },
          "shape": { "kind": "text" }
        }
      ]
    }
  }
}
```

Each field has one widget and one value shape. In the JSON, shape kinds are `text`, `int`, `bool`, `choice`, `artifact`, `list`, and `record`. A choice includes `options` with `value` and optional `label`; an artifact (step input) includes `accepts`, `proven`, and `create`; a list includes an `element` with its own widget and shape; a record includes a `fields` array whose entries carry `required: true` and `default: null`. Upload entries use `kind = "upload"` and `accepts`; download entries use `kind = "download"` and add the read-only downloaded timestamp field.

The language argument-schema version is **4**, and the form-document version is also **4**; they are different documents. Custom clients should consume the form document rather than guessing controls from template names. See [API](api-guide.md).
