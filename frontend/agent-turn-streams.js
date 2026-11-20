import { keepEventSourceOpen } from "./event-source.js";

const AGENT_TURN_HEARTBEAT_SECONDS = 5;

const agentTurnStreams = new Map();

export function closeAgentTurnStream(turnId) {
  agentTurnStreams.get(turnId)?.close();
  agentTurnStreams.delete(turnId);
}

function openAgentTurnStream(app, { sessionId, turnId }) {
  if (!turnId || agentTurnStreams.has(turnId)) {
    return;
  }
  const emit = (type, data) => app.ports.agentTurnIn?.send({ type, data });
  const streamUrl = `/backend/agent/turn/${encodeURIComponent(turnId)}/stream`;
  let deliveredOffset = null;
  agentTurnStreams.set(
    turnId,
    keepEventSourceOpen({
      url: () => (deliveredOffset === null ? streamUrl : `${streamUrl}?offset=${deliveredOffset}`),
      heartbeatSeconds: AGENT_TURN_HEARTBEAT_SECONDS,
      events: {
        chunk: (event) => {
          deliveredOffset = event.lastEventId;
          emit("chunk", { sessionId, chunk: JSON.parse(event.data).chunk });
        },
        done: () => {
          emit("done", { sessionId, turnId });
          closeAgentTurnStream(turnId);
        },
      },
      onConnectionChange: (connected) => {
        if (!connected) {
          emit("error", { sessionId, turnId, message: "Agent turn stream connection issue" });
        }
      },
    }),
  );
}

export function connectAgentTurnStreams(app) {
  app.ports.openAgentTurnStream?.subscribe((request) => openAgentTurnStream(app, request));
}
