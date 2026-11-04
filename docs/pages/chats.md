---
description: "Start, name, find, link, archive, and delete agent chats."
---

# Chats

## Managing chats

### New chat

Choose **New chat** in the panel header. Use separate chats for separate tasks. Several chats can work at once; switching between them does not stop their work. Sending a message in a chat that is already working steers that work rather than starting another task in the same chat.

A chat that never receives a message is removed automatically, so **New chat** is unavailable while the current chat is still empty.

### Naming a chat

Pointy gives the chat a short title based on your request. Until that is ready, it shows the start of your first message. To rename it, click **Rename chat** beside the title, enter up to 80 characters, and press Enter or click **Save**. Escape or **Cancel** cancels. Automatic naming will not replace a name you choose.

### Chat history and links

Use **Open chat history** to switch between chats. Chats are listed newest first. **Show archived (N)** reveals the chats you have put away.

The **not applied** label means the chat has edits waiting to be applied or discarded. **Working**, **Needs input**, **Reconnecting**, **Applying**, **Conflict**, and **Needs fix** tell you what is happening in that chat.

![Chat history with the Search chats box above the chat titled Raise alignment threads, labelled not applied, with its Copy link to chat and Archive buttons.](screenshots/light/agent-chat-history.png)

Use **Copy link to chat** to return to a conversation or send it to a colleague. The **You** label on a message that started a request links to that point in the conversation. Your colleague needs access to the same Pointy instance; a link does not make the chat public.

### Searching chats

The **Search chats** box at the top of the chat history finds chats by title, ranked as you type with the matched letters highlighted, the same way the [step search box](search.md) does; archived chats are included but ranked lower. If your administrator configured semantic search, a **Related** section lists chats whose conversation (your messages and the agent's replies) is about what you typed, marked **Relevance N%** or **Similarity N%**. Press Enter to open the top result and Escape to clear the search.

### Archiving and deleting

When you are done, open chat history and use **Archive** on the chat row. The button appears only when the agent is not working; finish or stop it first. Archived chats move under **Show archived (N)**, where you can still read them, but you cannot send more messages. Their conversation is kept. Apply or discard anything you still want before archiving.

To delete an archived chat permanently, click **Delete permanently** on the chat row and confirm with **Confirm**. This removes the chat metadata, logs, worktree, and agent branch, and it cannot be undone. It removes any unapplied edits as well; it does not undo changes already applied to the shared folders.
