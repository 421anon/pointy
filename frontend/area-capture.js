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

// The browser shows the overlay's crosshair only after the pointermove that reaches the overlay has been handled, so the snapshot starts in a later task.
export function prepareArtifactArea({ frameId }) {
  const frame = document.getElementById(frameId);
  if (!frame?.contentDocument) return;
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
  const { selection, overview, documentSize, selectionRect } = await snapshotFor(frame).captureArea({ x, y, width, height });
  return {
    overview: overview.toDataURL("image/png"),
    selection: selection.toDataURL("image/png"),
    documentSize,
    selectionRect,
  };
}
