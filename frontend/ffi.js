function openDialog(id) {
  const dialog = document.getElementById(id);
  if (!dialog) return;
  dialog.showModal();
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

let stepStatusSource = null;
let stepStatusTargetKey = null;

function closeStepStatusStream() {
  if (stepStatusSource) {
    stepStatusSource.close();
    stepStatusSource = null;
  }
  stepStatusTargetKey = null;
}

function toggleTheme() {
  const current = document.documentElement.getAttribute("data-theme");
  const next = current === "light" ? "dark" : "light";
  document.documentElement.setAttribute("data-theme", next);
  localStorage.setItem("theme", next);
}

export function connectPorts(app) {
  function emitToElm(type, data) {
    if (app.ports && app.ports.stepStatusIn) {
      app.ports.stepStatusIn.send({ type, data });
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

    stepStatusSource.onerror = () => {
      emitToElm("error", "Step status stream connection issue");
    };
  }

  const ffiFns = {
    openDialog,
    hidePopover,
    copyToClipboard,
    closeStepStatusStream,
    zoomIframe,
    toggleTheme,
  };

  if (app.ports && app.ports.ffiOut) {
    app.ports.ffiOut.subscribe((req) => {
      ffiFns[req.fn]?.(req.value);
      app.ports.ffiIn.send({ key: req.key, value: null });
    });
  }

  if (app.ports && app.ports.openStepStatusStream) {
    app.ports.openStepStatusStream.subscribe(openStepStatusStream);
  }

  window.addEventListener("beforeunload", closeStepStatusStream);
}
