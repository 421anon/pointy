---
description: "Declare the resources a step's build requests."
---

# Resources

## Resource requirements

A template may declare an attribute set or a function of resolved arguments:

```nix
requirements = args: {
  cpu = args.threads or 2;
  ram = "4G";
};
```

Template requirements are merged over the defaults `{ cpu = 1; ram = "1G"; ior = "0"; iow = "0"; }`.

To override resources for a single step in `steps/<id>.nix`, set a top-level `requirements` attribute set containing all four fields (a step-level override replaces the template's requirements completely rather than merging):

```nix
{
  type = "align";
  name = "Large cohort alignment";
  requirements = {
    cpu = 8;
    ram = "32G";
    ior = "0";
    iow = "0";
  };
  args = { ... };
}
```

Inspect the resolved requirements for any step with `nix eval --json .#pointy.steps.<id>.meta.pointy.requirements`. Whether CPU and memory requests are enforced as `--cpus-per-task` and `--mem` or only exported as `POINTY_REQ_*` job environment variables depends on `[slurm].enforcement`; see [Backend configuration](config.md#backend-configuration).
