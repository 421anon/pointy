---
description: "Serve step outputs as Turtle."
---

# Linked data

## Linked data

An artifact permalink can also answer as linked data. A request for `/artifact/<folder ids>/<step id>/<full 40-character commit>` whose `Accept` header includes `text/turtle` is answered at that commit and describes the step and the artifacts upstream of it; add `?path=<file>` to describe one file in the output.

```sh
curl -H 'Accept: text/turtle' \
  'https://pointy.example.com/artifact/1/42/<40-char-commit>?path=message.txt'
```

Relative identifiers resolve against the requesting URL. Set `services.pointy-host.linkedDataBase = "https://pointy.example.com"` to emit `@base <https://pointy.example.com/> .` so IRIs stay independent of the host that served the request.

The repository must publish `pointy.root` with its Turtle documents; an older revision returns `404`. A step's top-level `about` list of absolute `http(s)` URLs appears as links in the document. See [Artifacts as Turtle](files.md#artifacts-as-turtle) for route and caching details.
