---
description: "Serve step outputs as Turtle."
---

# Linked data

## Linked data

An artifact permalink can also answer as linked data. A request for `/artifact/<folder ids>/<step id>/<full 40-character commit>` that accepts `text/turtle` is answered at that commit and describes the step and the artifacts upstream of it; add `?path=<file>` to describe one file in the output. Relative identifiers resolve against the requesting URL. Set `services.pointy-host.linkedDataBase = "https://pointy.example.com"` to make them absolute and independent of the host that served the request. The repository must publish `pointy.root` with its Turtle documents; an older revision returns 404. A step's `about` URLs appear as links in the document.
