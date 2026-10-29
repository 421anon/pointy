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
        snapshotFor(frame)
          .prepare()
          .catch(() => {});
      },
      { once: true },
    );
    return true;
  };
  if (armOnFirstMove()) return;
  requestAnimationFrame(() => {
    if (!armOnFirstMove()) snapshotFor(frame).prepare().catch(() => {});
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
