import { toCanvas } from "html-to-image";
import { installDragDropListeners } from "./drag-drop.js";

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

let unsentTreeOps = null;

function setUnsentTreeOps(request) {
  unsentTreeOps = request || null;
}

function sendUnsentTreeOps() {
  if (unsentTreeOps) {
    const { url, body } = unsentTreeOps;
    fetch(url, {
      method: "POST",
      keepalive: true,
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
    });
    unsentTreeOps = null;
  }
}

function zoomIframe({ id, zoom }) {
  const iframe = document.getElementById(id);
  if (!iframe) return;
  iframe.dataset.zoom = zoom;
  const apply = () => {
    try {
      if (iframe.contentDocument && iframe.contentDocument.body) {
        iframe.contentDocument.body.style.zoom = zoom;
      }
    } catch (_) {}
  };
  apply();
  if (!iframe.dataset.zoomListenerAttached) {
    iframe.dataset.zoomListenerAttached = "true";
    iframe.addEventListener("load", () => {
      const z = iframe.dataset.zoom;
      if (z && iframe.contentDocument && iframe.contentDocument.body) {
        iframe.contentDocument.body.style.zoom = z;
      }
    });
  }
}

const OVERVIEW_MAX_EDGE = 1024;
const OVERVIEW_BORDER_COLOR = "red";
const OVERVIEW_BORDER_WIDTH = 2;

function renderPage(page, { pixelRatio, width, height, style }) {
  // html-to-image sizes the image from `width`/`height` and also writes them onto the root's style; `style` restores the page's own size so only the requested window is rendered without reflowing. https://github.com/bubkoo/html-to-image/blob/v1.11.13/src/apply-style.ts
  // Font embedding inserts fetched rules into live stylesheets, including the host page's. https://github.com/bubkoo/html-to-image/blob/v1.11.13/src/embed-webfonts.ts
  return toCanvas(page, { pixelRatio, width, height, backgroundColor: "white", skipFonts: true, style });
}

async function captureArtifactArea({ frameId, x, y, width, height }) {
  const frame = document.getElementById(frameId);
  const view = frame.contentWindow;
  const page = frame.contentDocument.documentElement;
  const pageStyle = view.getComputedStyle(page);
  const pixelRatio = window.devicePixelRatio;
  const documentSize = { width: page.scrollWidth, height: page.scrollHeight };
  const selectionRect = {
    x: Math.round(x + view.scrollX),
    y: Math.round(y + view.scrollY),
    width: Math.max(1, Math.round(width)),
    height: Math.max(1, Math.round(height)),
  };
  const viewport = await renderPage(page, {
    pixelRatio,
    width: frame.clientWidth,
    height: frame.clientHeight,
    style: {
      width: pageStyle.width,
      height: pageStyle.height,
      transform: `translate(${-view.scrollX}px, ${-view.scrollY}px)`,
    },
  });
  const selection = document.createElement("canvas");
  selection.width = Math.round(width * pixelRatio);
  selection.height = Math.round(height * pixelRatio);
  selection.getContext("2d").drawImage(viewport, x * pixelRatio, y * pixelRatio, selection.width, selection.height, 0, 0, selection.width, selection.height);
  const overviewScale = Math.min(1, OVERVIEW_MAX_EDGE / Math.max(documentSize.width, documentSize.height));
  const overview = await renderPage(page, {
    pixelRatio: overviewScale,
    width: documentSize.width,
    height: documentSize.height,
    style: {
      width: `${documentSize.width}px`,
      height: `${documentSize.height}px`,
    },
  });
  const overviewContext = overview.getContext("2d");
  overviewContext.strokeStyle = OVERVIEW_BORDER_COLOR;
  overviewContext.lineWidth = OVERVIEW_BORDER_WIDTH;
  overviewContext.strokeRect(
    selectionRect.x * overviewScale,
    selectionRect.y * overviewScale,
    selectionRect.width * overviewScale,
    selectionRect.height * overviewScale,
  );
  return {
    overview: overview.toDataURL("image/png"),
    selection: selection.toDataURL("image/png"),
    documentSize,
    selectionRect,
  };
}

let clusterStatusSource = null;

function openClusterStatusStream(app) {
  if (clusterStatusSource && clusterStatusSource.readyState !== EventSource.CLOSED) {
    return;
  }
  clusterStatusSource = new EventSource("/backend/cluster-status-stream");
  clusterStatusSource.addEventListener("cluster-status", (event) => {
    try {
      if (app.ports && app.ports.clusterStatusIn) {
        app.ports.clusterStatusIn.send(JSON.parse(event.data));
      }
    } catch (_) {}
  });
}

let ingestJobsSource = null;

function openIngestStream(app) {
  if (ingestJobsSource && ingestJobsSource.readyState !== EventSource.CLOSED) {
    return;
  }
  ingestJobsSource = new EventSource("/backend/ingest-stream");
  ingestJobsSource.addEventListener("ingest-jobs", (event) => {
    try {
      if (app.ports && app.ports.ingestJobsIn) {
        app.ports.ingestJobsIn.send(JSON.parse(event.data));
      }
    } catch (_) {}
  });
}

let elmApp = null;
let stepStatusSource = null;
const AGENT_TURN_INITIAL_RETRY_DELAY = 1000;
const AGENT_TURN_MAX_RETRY_DELAY = 30000;

const agentTurnStreams = new Map();

function closeStepStatusStream() {
  if (stepStatusSource) {
    stepStatusSource.close();
    stepStatusSource = null;
  }
}

function closeAgentTurnStream(turnId) {
  const stream = agentTurnStreams.get(turnId);
  if (!stream) {
    return;
  }
  clearTimeout(stream.timer);
  if (stream.source) {
    stream.source.close();
  }
  agentTurnStreams.delete(turnId);
}

function closeAllAgentTurnStreams() {
  for (const turnId of [...agentTurnStreams.keys()]) {
    closeAgentTurnStream(turnId);
  }
}

function emitAgentTurnEvent(type, data) {
  if (elmApp && elmApp.ports && elmApp.ports.agentTurnIn) {
    elmApp.ports.agentTurnIn.send({ type, data });
  }
}

function connectAgentTurnStream(turnId) {
  const stream = agentTurnStreams.get(turnId);
  if (!stream) {
    return;
  }
  const { sessionId } = stream;
  let replaySkip = stream.delivered;
  const source = new EventSource(
    `/backend/agent/turn/${encodeURIComponent(turnId)}/stream`,
  );
  stream.source = source;

  source.addEventListener("chunk", (event) => {
    try {
      const data = JSON.parse(event.data);
      const fresh = data.chunk.slice(replaySkip);
      replaySkip = 0;
      stream.delivered += fresh.length;
      stream.retryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
      if (fresh) {
        emitAgentTurnEvent("chunk", { sessionId, chunk: fresh });
      }
    } catch (err) {
      emitAgentTurnEvent("error", {
        sessionId,
        turnId,
        message: `Failed to parse agent log chunk: ${String(err)}`,
      });
    }
  });

  source.addEventListener("done", () => {
    emitAgentTurnEvent("done", { sessionId, turnId });
    closeAgentTurnStream(turnId);
  });

  source.addEventListener("heartbeat", () => {
    stream.retryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
    emitAgentTurnEvent("heartbeat", { sessionId, turnId });
  });

  source.onerror = () => {
    source.close();
    if (agentTurnStreams.get(turnId) !== stream || stream.source !== source) {
      return;
    }
    stream.source = null;
    const delay = stream.retryDelay;
    if (delay === AGENT_TURN_INITIAL_RETRY_DELAY) {
      emitAgentTurnEvent("error", {
        sessionId,
        turnId,
        message: "Agent turn stream connection issue",
      });
    }
    stream.retryDelay = Math.min(delay * 2, AGENT_TURN_MAX_RETRY_DELAY);
    stream.timer = setTimeout(() => {
      if (agentTurnStreams.get(turnId) === stream && !stream.source) {
        connectAgentTurnStream(turnId);
      }
    }, delay);
  };
}

function openAgentTurnStream({ sessionId, turnId }) {
  if (!turnId) {
    return;
  }
  const stream = agentTurnStreams.get(turnId);
  if (stream) {
    if (stream.source) {
      return;
    }
    clearTimeout(stream.timer);
    stream.retryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
    connectAgentTurnStream(turnId);
    return;
  }
  agentTurnStreams.set(turnId, {
    sessionId,
    source: null,
    delivered: 0,
    retryDelay: AGENT_TURN_INITIAL_RETRY_DELAY,
    timer: null,
  });
  connectAgentTurnStream(turnId);
}

function toggleTheme() {
  const current = document.documentElement.getAttribute("data-theme");
  const next = current === "light" ? "dark" : "light";
  document.documentElement.setAttribute("data-theme", next);
  localStorage.setItem("theme", next);
}

function agentPrompt(action) {
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

function restoreAgentPrompt(text) {
  const prompt = document.getElementById("agent-prompt");
  if (!prompt || prompt.value.trim() !== "") return null;
  prompt.value = text;
  prompt.dispatchEvent(new Event("input", { bubbles: true }));
  return null;
}

function installGutterDragListeners(app) {
  const emitEnd = () => {
    if (app.ports && app.ports.gutterDragEnd) {
      app.ports.gutterDragEnd.send(null);
    }
  };

  document.addEventListener("pointerdown", (event) => {
    if (event.target?.matches?.(".file-line-number.is-gutter")) {
      event.target.releasePointerCapture(event.pointerId);
    }
  });

  document.addEventListener("pointerup", emitEnd);
  document.addEventListener("pointercancel", emitEnd);
}

export function storedAutoApply() {
  return localStorage.getItem("agent:autoApply") !== "false";
}

export function agentClientId() {
  const stored = localStorage.getItem("agent:clientId");
  if (stored) return stored;
  const created = Array.from(crypto.getRandomValues(new Uint8Array(16)), (byte) =>
    byte.toString(16).padStart(2, "0")
  ).join("");
  try {
    localStorage.setItem("agent:clientId", created);
  } catch {}
  return created;
}

const SUBMITTED_TURNS_KEY = "agent:submittedTurns";

export function storedSubmittedTurns() {
  try {
    return JSON.parse(sessionStorage.getItem(SUBMITTED_TURNS_KEY)) ?? {};
  } catch {
    return {};
  }
}

function storeSubmittedTurns(turns) {
  sessionStorage.setItem(SUBMITTED_TURNS_KEY, JSON.stringify(turns));
}

function requestNotificationPermission() {
  if ("Notification" in window && Notification.permission === "default") {
    Notification.requestPermission();
  }
}

function scrolledToBottom({ id, slack }) {
  const element = document.getElementById(id);
  return element !== null && element.scrollHeight - (element.scrollTop + element.clientHeight) < slack;
}

const agentTurnNotifications = new Map();

function notifyAgentTurn({ sessionId, turnId, title, body, threadOpen }) {
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
    elmApp?.ports?.agentNotificationClickIn?.send(sessionId);
  };
}

export function connectPorts(app) {
  elmApp = app;
  function emitToElm(type, data) {
    if (app.ports && app.ports.stepStatusIn) {
      app.ports.stepStatusIn.send({ type, data });
    }
  }

  function openStepStatusStream() {
    if (stepStatusSource && stepStatusSource.readyState !== EventSource.CLOSED) {
      return;
    }
    stepStatusSource = new EventSource("/backend/step-status-stream");

    stepStatusSource.addEventListener("snapshot", (event) => {
      try {
        emitToElm("snapshot", JSON.parse(event.data));
      } catch (err) {
        emitToElm("error", `Failed to parse snapshot event: ${String(err)}`);
      }
    });

    stepStatusSource.addEventListener("heartbeat", (event) => {
      try {
        emitToElm("heartbeat", JSON.parse(event.data));
      } catch {}
    });

    stepStatusSource.onerror = () => {
      emitToElm("error", "Step status stream connection issue");
    };
  }

  function storeLastChat(sessionId) {
    localStorage.setItem("agent:lastChat", sessionId);
  }

  function storeAutoApply(enabled) {
    localStorage.setItem("agent:autoApply", String(enabled));
  }

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
    requestNotificationPermission,
    notifyAgentTurn,
    scrolledToBottom,
  };

  installGutterDragListeners(app);
  installDragDropListeners(app);

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

  if (app.ports && app.ports.openStepStatusStream) {
    app.ports.openStepStatusStream.subscribe(() => openStepStatusStream());
  }

  if (app.ports && app.ports.openAgentTurnStream) {
    app.ports.openAgentTurnStream.subscribe(openAgentTurnStream);
  }

  if (app.ports && app.ports.openClusterStatusStream) {
    app.ports.openClusterStatusStream.subscribe(() => openClusterStatusStream(app));
  }

  if (app.ports && app.ports.openIngestStream) {
    app.ports.openIngestStream.subscribe(() => openIngestStream(app));
  }

  window.addEventListener("storage", (event) => {
    if ((event.key === null || event.key === "agent:autoApply") && app.ports && app.ports.agentAutoApplyIn) {
      app.ports.agentAutoApplyIn.send(storedAutoApply());
    }
  });

  window.addEventListener("beforeunload", () => {
    closeStepStatusStream();
    closeAllAgentTurnStreams();
    if (clusterStatusSource) clusterStatusSource.close();
    if (ingestJobsSource) ingestJobsSource.close();
  });

  window.addEventListener("pagehide", sendUnsentTreeOps);
}
