import { canDrawFrame, captureFrameArea } from "./frame-drawing.js";
import { createPageSnapshot } from "./page-snapshot.js";

const snapshots = new WeakMap();

function snapshotFor(frame) {
  const document = frame.contentDocument;
  if (!document) throw new Error("The artifact frame has no document.");
  let snapshot = snapshots.get(document);
  if (!snapshot) {
    snapshot = createPageSnapshot(document);
    snapshots.set(document, snapshot);
  }
  return snapshot;
}

function prepareSoon(frame) {
  setTimeout(() => {
    if (frame.isConnected && frame.contentDocument) snapshotFor(frame).prepare().catch(() => {});
  }, 0);
}

// The browser shows the overlay's crosshair only after the pointermove that reaches the overlay has been handled, so the snapshot starts in a later task. A frame the browser can draw needs no snapshot.
export function prepareArtifactArea({ frameId }) {
  const frame = document.getElementById(frameId);
  if (!frame?.contentDocument || canDrawFrame(frame)) return;
  const armOnFirstMove = () => {
    const overlay = frame.parentElement?.querySelector(".artifact-viewer-capture");
    if (!overlay) return false;
    if (overlay.dataset.prewarming) return true;
    overlay.dataset.prewarming = "true";
    overlay.addEventListener(
      "pointermove",
      () => {
        delete overlay.dataset.prewarming;
        prepareSoon(frame);
      },
      { once: true },
    );
    return true;
  };
  if (armOnFirstMove()) return;
  requestAnimationFrame(() => {
    if (!armOnFirstMove()) prepareSoon(frame);
  });
}

export async function captureArtifactArea({ frameId, x, y, width, height }) {
  const frame = document.getElementById(frameId);
  const rect = { x, y, width, height };
  const { selection, overview, documentSize, selectionRect } = (await captureFrameArea(frame, rect)) ?? (await snapshotFor(frame).captureArea(rect));
  return {
    overview: overview.toDataURL("image/png"),
    selection: selection.toDataURL("image/png"),
    documentSize,
    selectionRect,
  };
}
