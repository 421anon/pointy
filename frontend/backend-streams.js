let stepStatusSource = null;
let clusterStatusSource = null;
let ingestJobsSource = null;

function isOpen(source) {
  return source && source.readyState !== EventSource.CLOSED;
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
  ingestJobsSource.addEventListener("ingest-jobs", (event) => {
    try {
      if (app.ports && app.ports.ingestJobsIn) {
        app.ports.ingestJobsIn.send(JSON.parse(event.data));
      }
    } catch (_) {}
  });
}

function closeBackendStreams() {
  stepStatusSource?.close();
  stepStatusSource = null;
  clusterStatusSource?.close();
  ingestJobsSource?.close();
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
  window.addEventListener("beforeunload", closeBackendStreams);
}
