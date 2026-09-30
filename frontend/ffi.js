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

  const ffiFns = {
    openDialog,
    closeDialog,
    hidePopover,
    copyToClipboard,
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

  window.addEventListener("beforeunload", () => {
    closeStepStatusStream();
    closeAllAgentTurnStreams();
    if (clusterStatusSource) clusterStatusSource.close();
    if (ingestJobsSource) ingestJobsSource.close();
  });
}
