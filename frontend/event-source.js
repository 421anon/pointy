const REOPEN_DELAY_MS = 2000;
const SILENCE_LIMIT_IN_HEARTBEATS = 2.5;

export function keepEventSourceOpen({ url, heartbeatSeconds, events, onConnectionChange }) {
  const listeners = { heartbeat: () => {}, ...events };
  const silenceLimitMs = heartbeatSeconds * 1000 * SILENCE_LIMIT_IN_HEARTBEATS;
  let source = null;
  let connected = false;
  let silenceTimer = null;
  let reopenTimer = null;

  const reportConnection = (nowConnected) => {
    if (connected !== nowConnected) {
      connected = nowConnected;
      onConnectionChange(nowConnected);
    }
  };

  const expectNextMessage = () => {
    clearTimeout(silenceTimer);
    silenceTimer = setTimeout(reopenLater, silenceLimitMs);
  };

  const reopenLater = () => {
    clearTimeout(silenceTimer);
    clearTimeout(reopenTimer);
    source.close();
    reportConnection(false);
    reopenTimer = setTimeout(open, REOPEN_DELAY_MS);
  };

  const open = () => {
    const opened = new EventSource(url());
    source = opened;
    opened.addEventListener("open", () => {
      reportConnection(true);
      expectNextMessage();
    });
    opened.addEventListener("error", () => {
      if (opened.readyState === EventSource.CLOSED) {
        reopenLater();
      } else {
        reportConnection(false);
      }
    });
    for (const [name, handle] of Object.entries(listeners)) {
      opened.addEventListener(name, (event) => {
        expectNextMessage();
        handle(event);
      });
    }
  };

  open();

  return {
    close() {
      clearTimeout(silenceTimer);
      clearTimeout(reopenTimer);
      source.close();
    },
  };
}
