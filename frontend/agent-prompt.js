export function agentPrompt(action) {
  const prompt = document.getElementById("agent-prompt");

  if (action === "read") {
    return prompt?.value ?? "";
  }

  if (action === "clear" && prompt) {
    prompt.value = "";
    prompt.dispatchEvent(new Event("input", { bubbles: true }));
  }

  return null;
}

export function restoreAgentPrompt(text) {
  const prompt = document.getElementById("agent-prompt");
  if (!prompt || prompt.value.trim() !== "") return null;
  prompt.value = text;
  prompt.dispatchEvent(new Event("input", { bubbles: true }));
  return null;
}
