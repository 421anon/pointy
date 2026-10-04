Commenting code is NEVER allowed. No comments at all. Communicate through code.

## Pull requests

- When the change has a visible surface, add screenshots that capture as much of it as possible: at most 6, and preferably 3 or fewer. Upload them as GitHub attachments, as the PR web editor does when you drop an image, and embed the resulting `github.com/user-attachments` URLs. Never commit screenshots to a branch.
- Describe the user-facing features as a bulleted list.
- Describe the technical changes as a bulleted list.
- End with a chronological timeline of the agent prompts behind the change, covering every session on the branch. Quote short verbatim excerpts of the prompts that shaped the design, and say what each one led to: the initial design first, then how it evolved. Leave out agent-management chatter. Earlier sessions' prompts are in the transcripts under `~/.omp/agent/sessions/`.
