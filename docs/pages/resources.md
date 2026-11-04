---
description: "Declare the resources a step's build requests."
---

# Resources

## Resource requirements

A template may declare an attribute set or a function of resolved arguments:

```nix
requirements = args: {
  cpu = 2;
  ram = "4G";
};
```

Template requirements are merged over `{ cpu = 1; ram = "1G"; ior = "0"; iow = "0"; }`. A step's own non-null `requirements` takes precedence as a complete override and must contain all four fields. The Slurm policy decides whether CPU and memory requests are enforced; see [Backend configuration](config.md#backend-configuration).
