let unsentTreeOps = null;

export function setUnsentTreeOps(request) {
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

export function installUnsentTreeOpsFlush() {
  window.addEventListener("pagehide", sendUnsentTreeOps);
}
