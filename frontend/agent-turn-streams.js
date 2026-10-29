const AGENT_TURN_INITIAL_RETRY_DELAY = 1000;
const AGENT_TURN_MAX_RETRY_DELAY = 30000;

const agentTurnStreams = new Map();

export function closeAgentTurnStream(turnId) {
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

function emitAgentTurnEvent(app, type, data) {
  if (app.ports && app.ports.agentTurnIn) {
    app.ports.agentTurnIn.send({ type, data });
  }
}

function connectAgentTurnStream(app, turnId) {
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
        emitAgentTurnEvent(app, "chunk", { sessionId, chunk: fresh });
      }
    } catch (err) {
      emitAgentTurnEvent(app, "error", {
        sessionId,
        turnId,
        message: `Failed to parse agent log chunk: ${String(err)}`,
      });
    }
  });

  source.addEventListener("done", () => {
    emitAgentTurnEvent(app, "done", { sessionId, turnId });
    closeAgentTurnStream(turnId);
  });

  source.addEventListener("heartbeat", () => {
    stream.retryDelay = AGENT_TURN_INITIAL_RETRY_DELAY;
    emitAgentTurnEvent(app, "heartbeat", { sessionId, turnId });
  });

  source.onerror = () => {
    source.close();
    if (agentTurnStreams.get(turnId) !== stream || stream.source !== source) {
      return;
    }
    stream.source = null;
    const delay = stream.retryDelay;
    if (delay === AGENT_TURN_INITIAL_RETRY_DELAY) {
      emitAgentTurnEvent(app, "error", {
        sessionId,
        turnId,
        message: "Agent turn stream connection issue",
      });
    }
    stream.retryDelay = Math.min(delay * 2, AGENT_TURN_MAX_RETRY_DELAY);
    stream.timer = setTimeout(() => {
      if (agentTurnStreams.get(turnId) === stream && !stream.source) {
        connectAgentTurnStream(app, turnId);
      }
    }, delay);
  };
}

function openAgentTurnStream(app, { sessionId, turnId }) {
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
    connectAgentTurnStream(app, turnId);
    return;
  }
  agentTurnStreams.set(turnId, {
    sessionId,
    source: null,
    delivered: 0,
    retryDelay: AGENT_TURN_INITIAL_RETRY_DELAY,
    timer: null,
  });
  connectAgentTurnStream(app, turnId);
}

export function connectAgentTurnStreams(app) {
  if (app.ports && app.ports.openAgentTurnStream) {
    app.ports.openAgentTurnStream.subscribe((request) => openAgentTurnStream(app, request));
  }
  window.addEventListener("beforeunload", closeAllAgentTurnStreams);
}
