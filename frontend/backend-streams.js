let stepStatusSource = null;
let clusterStatusSource = null;
let ingestJobsSource = null;
let stepEventSource = null;
let watchedStepStream = null;

const REOPEN_DELAY_MS = 2000;

function isOpen(source) {
  return source && source.readyState !== EventSource.CLOSED;
}

function reopenWhenClosed(source, reopen) {
  source.addEventListener("error", () => {
    if (source.readyState === EventSource.CLOSED) {
      setTimeout(reopen, REOPEN_DELAY_MS);
    }
  });
}

function openStepStatusStream(app) {
  if (isOpen(stepStatusSource)) {
    return;
  }
  const emitToElm = (type, data) => {
    if (app.ports && app.ports.stepStatusIn) {
      app.ports.stepStatusIn.send({ type, data });
    }
  };
  stepStatusSource = new EventSource("/backend/step-status-stream");
  reopenWhenClosed(stepStatusSource, () => openStepStatusStream(app));

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

function openClusterStatusStream(app) {
  if (isOpen(clusterStatusSource)) {
    return;
  }
  clusterStatusSource = new EventSource("/backend/cluster-status-stream");
  reopenWhenClosed(clusterStatusSource, () => openClusterStatusStream(app));
  clusterStatusSource.addEventListener("cluster-status", (event) => {
    try {
      if (app.ports && app.ports.clusterStatusIn) {
        app.ports.clusterStatusIn.send(JSON.parse(event.data));
      }
    } catch (_) {}
  });
}

function openIngestStream(app) {
  if (isOpen(ingestJobsSource)) {
    return;
  }
  ingestJobsSource = new EventSource("/backend/ingest-stream");
  reopenWhenClosed(ingestJobsSource, () => openIngestStream(app));
  ingestJobsSource.addEventListener("ingest-jobs", (event) => {
    try {
      if (app.ports && app.ports.ingestJobsIn) {
        app.ports.ingestJobsIn.send(JSON.parse(event.data));
      }
    } catch (_) {}
  });
}

function watchStepEvents(app, { stepId, commit }) {
  const stepStream = `/backend/agent/step/${stepId}/stream?commit=${encodeURIComponent(commit)}`;
  if (stepStream === watchedStepStream && isOpen(stepEventSource)) {
    return;
  }
  stepEventSource?.close();
  watchedStepStream = stepStream;
  stepEventSource = new EventSource(stepStream);
  reopenWhenClosed(stepEventSource, () => {
    if (watchedStepStream === stepStream) watchStepEvents(app, { stepId, commit });
  });
  for (const type of ["newer-revision", "build-started", "build-finished"]) {
    stepEventSource.addEventListener(type, (event) => {
      try {
        app.ports?.stepEventIn?.send({ type, data: JSON.parse(event.data) });
      } catch (_) {}
    });
  }
}

function closeBackendStreams() {
  stepStatusSource?.close();
  stepStatusSource = null;
  clusterStatusSource?.close();
  ingestJobsSource?.close();
  stepEventSource?.close();
}

export function connectBackendStreams(app) {
  if (app.ports && app.ports.openStepStatusStream) {
    app.ports.openStepStatusStream.subscribe(() => openStepStatusStream(app));
  }
  if (app.ports && app.ports.openClusterStatusStream) {
    app.ports.openClusterStatusStream.subscribe(() => openClusterStatusStream(app));
  }
  if (app.ports && app.ports.openIngestStream) {
    app.ports.openIngestStream.subscribe(() => openIngestStream(app));
  }
  app.ports?.watchStepEvents?.subscribe((artifact) => watchStepEvents(app, artifact));
  window.addEventListener("beforeunload", closeBackendStreams);
}
