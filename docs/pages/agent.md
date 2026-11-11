---
description: "Ask the agent for help with an analysis, steer or stop it, and show it images or part of an artifact."
---

# Agent

You can ask Pointy's agent to set up an analysis, connect existing steps, or write a script, just as you would ask a colleague. It can also help you understand what is already there. Its edits go to a private copy of the saved folders. By default, Pointy applies the edits of a successful turn to the shared branch when the turn ends; switch **Auto-apply** off in the panel header if you want to review them first.

For example:

- “Explain the inputs and parameters of step 42. Don't change anything yet.”
- “Set up a quality-control step for each sample, keeping their results separate.”
- “Make a copy of step 42 with a stricter threshold. Keep the original for comparison.”

Tell the agent what you want to learn or produce, which steps to use, and what you want to leave unchanged. Step numbers help when several steps have similar names. You do not need to describe repository files or know how Pointy stores them.

When you send a message, Pointy tells the agent which folder page you have open, so it can assume that is the folder you mean. On an artifact page, it also tells the agent which step, commit, and output file you are viewing. These notes do not appear in the chat. If a turn fails or you stop it, Pointy sends the folder note again with your next message.

## Opening the agent panel

Use **Agent** in the bottom status bar. The panel is available on every workspace page. In the artifact viewer, choose **Open agent** to show the panel beside the output. On a narrow screen, the artifact viewer hides its agent buttons; use **View in Pointy** and the status bar instead.

**Focus agent panel** gives the chat more space; use **Exit focus mode (Esc)** to return to the smaller panel.

**Close agent panel** hides the panel without stopping the agent. Opening it again brings back the chat you were using.

![The agent panel after a turn with Auto-apply on: the request, the agent's reply, and an Applied changeset for steps/6.nix.](screenshots/light/agent-panel.png)

## Asking for help

Type your request and click **Send**, or press **Ctrl+Enter** (**Cmd+Enter** on macOS). The reply appears in the chat as the agent works. Until the first reply arrives, the chat shows **Waiting for a reply**. You can keep inspecting the folder while you wait. Each reply is labelled **Coder** or **Answerer** after the model that wrote it; without a second model, every reply is a **Coder** reply.

The agent can also show you an image, such as a chart it drew to answer your question. The image appears inside its reply, at the point where the agent showed it.

Pointy shows a tool call only when it has been running for 20 seconds. The chat then shows a line such as **Running bash · 0:24** with the command the agent is running. Calls that finish sooner never appear, and the line goes away when the call finishes. The agent's own question appears as a question card, not as a running tool.

### Steering a running turn

You do not have to wait for the agent to finish before correcting it. While it works, **Send** becomes **Steer**. For example: “Keep the original step; make a copy for this comparison instead.” The same keyboard shortcut sends the correction. Your steer appears at once as a **You** message marked **Steering** until the agent picks it up.

### Stopping

**Stop** interrupts the agent. You may briefly see **Stopping**, and the turn is then marked **Stopped**. If the agent had not replied yet, the message reads **Stopped before a reply.** Stopping does not undo edits the agent already made: they stay on a **Pending** changeset card. With auto-apply on, the next successful turn applies them; otherwise you can inspect and apply or discard them.

Stopping the agent does not stop an analysis that is already running; use the step's own **Stop** button for that.

### Answering questions

The agent may pause to ask you a question. For a single-choice question, click an option and your answer is sent straight away. For a multiple-choice question, select the options you want and click **Send answer**. You can also type your own answer in the message box.

While the chat waits for your answer, its entry in the chat history shows **Needs input**, and the agent's time budget is paused: time spent waiting for your answer does not count against it.

### Attaching images

Click **Attach images** inside the prompt field, or paste or drop image files into the chat. Pointy accepts PNG, JPEG, GIF, and WebP images. You can send images without any text.

Attached images appear as thumbnails above the prompt. Click a thumbnail to view it larger, or use its close button to remove it. Sent images appear under your message. Pointy refuses a set of images that would make the message too large; attach fewer or smaller images. While the agent is working, you can steer it with text, but images can be sent only once the turn finishes.

### Asking about part of an artifact

In the artifact viewer, choose **Ask agent**, then drag a rectangle over the page. A question field opens beside the selection. Type your question and press Enter to ask it in a new chat; Shift+Enter adds a line, and Escape cancels. Pointy switches the panel to the new chat, with the area already attached and your question in it.

![An area dragged over a FastQC report in the artifact viewer, with a question typed into the field below the selection.](screenshots/light/output-file-area-question.png)

The capture contains two images: the whole page with your area outlined, and the area itself at native resolution. The agent also receives the artifact's step, commit, and file path. In the chat, the capture appears as an image tile. Click it to view the selected area and the whole page, or use its link button to open the artifact in a new tab with this chat at that message. If the area is too large to send, Pointy asks you to select a smaller one.

Area capture is offered for outputs shown as a page, not for table previews.

### Desktop notifications

Pointy can notify you when a turn you started in this tab finishes or asks for input. It asks your browser for notification permission when you send a message. The notification is titled with the chat's name and says **Finished**, **Stopped**, **Failed: …**, or **Needs your input**. Pointy does not show one while you have that chat open and the window focused. Click the notification to open the chat. Your browser's notification settings decide whether these appear.

### Following references

A step reference in a reply links to the step's output in the folder you have open, or in the first folder that lists it. A folder name links to that folder's page. A step reference also gets a run button when the step has not run successfully. These links point to the same steps you can open in a folder, not copies stored in the chat.

When a run fails, or a step fails certification, open the step's log and choose **Investigate with agent**. Pointy starts a new chat and sends the step's build log to the agent right away, asking it to investigate the failure.

### Long conversations and errors

In a long chat, Pointy shows a strip under the chat title when the conversation has used about half of the context the model can consider at once. It tells you the percentage and token counts in use and offers **Compact**. While the agent is not working, **Compact** asks it to summarise the earlier conversation to free space. The strip says **Compacting the conversation to free context.** while that runs. If compaction fails, the strip shows the error and a **Try again** button. Restate important constraints after compacting if you need to, or start a new chat for a separate task.

Pointy stops a turn that runs too long: by default a turn is limited to 30 minutes; time spent waiting for your answer does not count. Your administrator can change the limit in [the backend configuration](config.md#backend-configuration).

If the agent reports an error, read it before sending the same request again. It may already have made some edits. Check those before deciding whether to continue or discard them.
