import { agentPrompt, restoreAgentPrompt } from "./agent-prompt.js";
import { notifyAgentTurn, requestNotificationPermission } from "./agent-notifications.js";
import { storeAutoApply, storeLastChat, storeSubmittedTurns, syncAutoApplyAcrossTabs } from "./agent-storage.js";
import { closeAgentTurnStream, connectAgentTurnStreams } from "./agent-turn-streams.js";
import { captureArtifactArea } from "./area-capture.js";
import { connectBackendStreams } from "./backend-streams.js";
import { installDragDropListeners } from "./drag-drop.js";
import { installGutterDragListeners, zoomIframe } from "./file-viewer.js";
import { installUnsentTreeOpsFlush, setUnsentTreeOps } from "./unsent-tree-ops.js";

function openDialog(id) {
  const dialog = document.getElementById(id);
  if (!dialog || dialog.open) return;
  dialog.showModal();
}

function closeDialog(id) {
  const dialog = document.getElementById(id);
  if (!dialog || !dialog.open) return;
  dialog.close();
}

function hidePopover(id) {
  document.getElementById(id)?.hidePopover();
}

function copyToClipboard(text) {
  navigator.clipboard.writeText(text);
}

function toggleTheme() {
  const current = document.documentElement.getAttribute("data-theme");
  const next = current === "light" ? "dark" : "light";
  document.documentElement.setAttribute("data-theme", next);
  localStorage.setItem("theme", next);
}

function scrolledToBottom({ id, slack }) {
  const element = document.getElementById(id);
  return element !== null && element.scrollHeight - (element.scrollTop + element.clientHeight) < slack;
}

export function connectPorts(app) {
  const ffiFns = {
    openDialog,
    closeDialog,
    hidePopover,
    copyToClipboard,
    closeAgentTurnStream,
    zoomIframe,
    captureArtifactArea,
    toggleTheme,
    agentPrompt,
    restoreAgentPrompt,
    storeLastChat,
    storeAutoApply,
    setUnsentTreeOps,
    storeSubmittedTurns,
    storeToastHistory: (json) => sessionStorage.setItem("toast-history", json),
    requestNotificationPermission,
    notifyAgentTurn: (turn) => notifyAgentTurn(app, turn),
    scrolledToBottom,
  };

  installGutterDragListeners(app);
  installDragDropListeners(app);
  installUnsentTreeOpsFlush();
  connectBackendStreams(app);
  connectAgentTurnStreams(app);
  syncAutoApplyAcrossTabs(app);

  if (app.ports && app.ports.ffiOut) {
    app.ports.ffiOut.subscribe(async (req) => {
      let value = null;
      try {
        value = await ffiFns[req.fn]?.(req.value);
      } catch (error) {
        console.error(error);
      }
      app.ports.ffiIn.send({ key: req.key, value: value ?? null });
    });
  }
}
