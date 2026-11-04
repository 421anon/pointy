---
description: "Offer named bundles of templates for folders."
---

# Presets

## Presets

A preset bundles templates under a useful name, such as "RNA-seq", so people do not have to reconstruct the same selection for each folder. Presets are read from `presets/`, one file per preset; the filename is the preset key. Add this to `presets/example.nix`:

```nix
{
  displayName = "Example analysis";
  description = "A small starting point.";
  sortKey = 0;
  templates = [ "hello" ];
}
```

`templates` is required and every entry must be a loaded template; `description` and `sortKey` default to `null`, and `displayName` must be a string. An unknown template fails evaluation with ``Preset `<name>` references unknown templates: <list>.``.

A folder must choose exactly one of `preset = "example";` or `templates = [ "hello" ];`. The selection does not remove the folder's steps or filter its contents; it decides which templates appear directly in the folder's **New** menu, and the remaining templates stay available under **Other types**. When someone creates a folder, it starts with the same template selection as the folder where the action was used: the same preset, or the same custom template list.
