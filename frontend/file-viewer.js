export function zoomIframe({ id, zoom }) {
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

export function installGutterDragListeners(app) {
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
