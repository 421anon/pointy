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

let stepStatusSource = null;
let stepStatusTargetKey = null;
let stepStatusApplicationError = false;
let agentTurnSource = null;
let agentTurnTargetKey = null;
const AGENT_TURN_INITIAL_RETRY_DELAY = 1000;
const AGENT_TURN_MAX_RETRY_DELAY = 30000;
let agentTurnDeliveredLength = 0;
let agentTurnRetryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;

function closeStepStatusStream() {
  if (stepStatusSource) {
    stepStatusSource.close();
    stepStatusSource = null;
  }
  stepStatusTargetKey = null;
  stepStatusApplicationError = false;
}

function closeAgentTurnStream() {
  if (agentTurnSource) {
    agentTurnSource.close();
    agentTurnSource = null;
  }
  agentTurnTargetKey = null;
  agentTurnDeliveredLength = 0;
  agentTurnRetryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
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

export function connectPorts(app) {
  function emitToElm(type, data) {
    if (app.ports && app.ports.stepStatusIn) {
      app.ports.stepStatusIn.send({ type, data });
    }
  }

  function emitAgentTurn(type, data) {
    if (app.ports && app.ports.agentTurnIn) {
      app.ports.agentTurnIn.send({ type, data });
    }
  }

  function openStepStatusStream({ projectId, commit }) {
    const params = new URLSearchParams({ project_id: String(projectId) });
    if (commit) {
      params.set("commit", commit);
    }

    const url = `/backend/step-status-stream?${params.toString()}`;

    if (
      stepStatusSource &&
      stepStatusTargetKey === url &&
      stepStatusSource.readyState !== EventSource.CLOSED
    ) {
      return;
    }

    closeStepStatusStream();

    stepStatusSource = new EventSource(url);
    stepStatusTargetKey = url;

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

    stepStatusSource.addEventListener("status-error", (event) => {
      stepStatusApplicationError = true;
      try {
        emitToElm("error", JSON.parse(event.data));
      } catch (err) {
        emitToElm("error", `Failed to parse status error event: ${String(err)}`);
      }
    });

    stepStatusSource.onerror = () => {
      if (stepStatusApplicationError) {
        stepStatusApplicationError = false;
        return;
      }
      emitToElm("error", "Step status stream connection issue");
    };
  }

  function connectAgentTurnStream(turnId) {
    const source = new EventSource(
      `/backend/agent/turn/${encodeURIComponent(turnId)}/stream`,
    );
    let replayLength = agentTurnDeliveredLength;
    agentTurnSource = source;

    source.addEventListener("chunk", (event) => {
      try {
        const data = JSON.parse(event.data);
        data.chunk = data.chunk.slice(replayLength);
        replayLength = 0;
        agentTurnDeliveredLength += data.chunk.length;
        agentTurnRetryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
        if (data.chunk) emitAgentTurn("chunk", data);
      } catch (err) {
        emitAgentTurn("error", `Failed to parse agent log chunk: ${String(err)}`);
      }
    });

    source.addEventListener("done", (event) => {
      try {
        emitAgentTurn("done", JSON.parse(event.data));
      } catch {
        emitAgentTurn("done", { turnId });
      }
      closeAgentTurnStream();
    });

    source.addEventListener("heartbeat", (event) => {
      agentTurnRetryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
      try {
        emitAgentTurn("heartbeat", JSON.parse(event.data));
      } catch {}
    });

    source.onerror = () => {
      source.close();
      if (source !== agentTurnSource || agentTurnTargetKey !== turnId) return;

      agentTurnSource = null;
      const delay = agentTurnRetryDelay;
      if (delay === AGENT_TURN_INITIAL_RETRY_DELAY) {
        emitAgentTurn("error", "Agent turn stream connection issue");
      }
      agentTurnRetryDelay = Math.min(delay * 2, AGENT_TURN_MAX_RETRY_DELAY);
      setTimeout(() => {
        if (agentTurnTargetKey === turnId && !agentTurnSource) {
          connectAgentTurnStream(turnId);
        }
      }, delay);
    };
  }

  function openAgentTurnStream({ turnId }) {
    closeAgentTurnStream();
    agentTurnTargetKey = turnId;
    connectAgentTurnStream(turnId);
  }

  function storeLastChat(sessionId) {
    localStorage.setItem("agent:lastChat", sessionId);
  }

  const ffiFns = {
    openDialog,
    closeDialog,
    hidePopover,
    copyToClipboard,
    closeStepStatusStream,
    closeAgentTurnStream,
    zoomIframe,
    toggleTheme,
    agentPrompt,
    storeLastChat,
  };

  installGutterDragListeners(app);

  if (app.ports && app.ports.ffiOut) {
    app.ports.ffiOut.subscribe((req) => {
      const value = ffiFns[req.fn]?.(req.value);
      app.ports.ffiIn.send({ key: req.key, value: value ?? null });
    });
  }

  if (app.ports && app.ports.openStepStatusStream) {
    app.ports.openStepStatusStream.subscribe(openStepStatusStream);
  }

  if (app.ports && app.ports.openAgentTurnStream) {
    app.ports.openAgentTurnStream.subscribe(openAgentTurnStream);
  }

  if (app.ports && app.ports.openClusterStatusStream) {
    app.ports.openClusterStatusStream.subscribe(() => openClusterStatusStream(app));
  }

  window.addEventListener("beforeunload", () => {
    closeStepStatusStream();
    closeAgentTurnStream();
    if (clusterStatusSource) clusterStatusSource.close();
  });
}
