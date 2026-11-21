import { keepEventSourceOpen } from "./event-source.js";

const BACKEND_EVENT_NAMES = ["step-status", "cluster-status", "ingest-jobs", "storage"];
const BACKEND_HEARTBEAT_SECONDS = 30;

let backendEvents = null;

function openBackendEvents(app) {
  if (backendEvents) {
    return;
  }
  const forward = (name) => (event) => app.ports.backendEventIn?.send({ event: name, data: event.data });
  backendEvents = keepEventSourceOpen({
    url: () => "/backend/events",
    heartbeatSeconds: BACKEND_HEARTBEAT_SECONDS,
    events: Object.fromEntries(BACKEND_EVENT_NAMES.map((name) => [name, forward(name)])),
    onConnectionChange: (connected) => app.ports.backendConnectionIn?.send(connected),
  });
}

export function connectBackendEvents(app) {
  app.ports.openBackendEvents?.subscribe(() => openBackendEvents(app));
}
