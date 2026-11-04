---
description: "Find any step by name, id, type, folder, note, or argument."
---

# Search

## Searching for a step

The search box at the top right of every folder page finds steps, not folders, across all folders. Matches appear as you type and are ranked: every word in the query must match some part of the step — its name, id, type, folder path, note, or argument values — and the strongest matches come first. Up to 50 direct matches are listed.

Each direct match shows the step's name with the matched letters highlighted. Its second line gives where the match came from: a note excerpt, an argument, or the step's type and folder path. Hover a result to see its id. If your administrator configured semantic search, the box also shows a **Related** section with steps that mean something similar, marked **Relevance N%** or **Similarity N%**; a notice shows while they load, and if they cannot be fetched the box says so.

Selecting a result opens a folder that contains the step and highlights its row, opening the step's output when it has been built; when the query matched a folder name, that folder is the one that opens. Steps that sit in no folder are not shown anywhere in Pointy, so they never appear in the results or in **Link existing**.

![The step search box with direct matches for "greet" and the start of the Related section below them.](screenshots/light/steps-search-box.png)

## Searching chats

The chat history has its own search box that works the same way over chat titles and conversations; see [Searching chats](chats.md#searching-chats).
