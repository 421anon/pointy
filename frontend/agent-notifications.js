const agentTurnNotifications = new Map();

export function requestNotificationPermission() {
  if ("Notification" in window && Notification.permission === "default") {
    Notification.requestPermission();
  }
}

export function notifyAgentTurn(app, { sessionId, turnId, title, body, threadOpen }) {
  if (!("Notification" in window) || Notification.permission !== "granted") return;
  if (threadOpen && document.hasFocus()) return;
  agentTurnNotifications.get(turnId)?.close();
  let notification;
  try {
    notification = new Notification(title, { body });
  } catch {
    return;
  }
  agentTurnNotifications.set(turnId, notification);
  notification.onclose = () => {
    if (agentTurnNotifications.get(turnId) === notification) agentTurnNotifications.delete(turnId);
  };
  notification.onclick = () => {
    window.focus();
    notification.close();
    app.ports?.agentNotificationClickIn?.send(sessionId);
  };
}
