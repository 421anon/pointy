const AUTO_APPLY_KEY = "agent:autoApply";
const CLIENT_ID_KEY = "agent:clientId";
const LAST_CHAT_KEY = "agent:lastChat";
const SUBMITTED_TURNS_KEY = "agent:submittedTurns";

export function storedAutoApply() {
  return localStorage.getItem(AUTO_APPLY_KEY) !== "false";
}

export function storeAutoApply(enabled) {
  localStorage.setItem(AUTO_APPLY_KEY, String(enabled));
}

export function agentClientId() {
  const stored = localStorage.getItem(CLIENT_ID_KEY);
  if (stored) return stored;
  const created = Array.from(crypto.getRandomValues(new Uint8Array(16)), (byte) =>
    byte.toString(16).padStart(2, "0")
  ).join("");
  try {
    localStorage.setItem(CLIENT_ID_KEY, created);
  } catch {}
  return created;
}

export function storedLastChat() {
  return localStorage.getItem(LAST_CHAT_KEY);
}

export function storeLastChat(sessionId) {
  localStorage.setItem(LAST_CHAT_KEY, sessionId);
}

export function storedSubmittedTurns() {
  try {
    return JSON.parse(sessionStorage.getItem(SUBMITTED_TURNS_KEY)) ?? {};
  } catch {
    return {};
  }
}

export function storeSubmittedTurns(turns) {
  sessionStorage.setItem(SUBMITTED_TURNS_KEY, JSON.stringify(turns));
}

export function syncAutoApplyAcrossTabs(app) {
  window.addEventListener("storage", (event) => {
    if ((event.key === null || event.key === AUTO_APPLY_KEY) && app.ports && app.ports.agentAutoApplyIn) {
      app.ports.agentAutoApplyIn.send(storedAutoApply());
    }
  });
}
