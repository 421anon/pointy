let indicator = null;

function parseRef(token) {
  if (!token) return null;
  const [kind, rawId] = token.split(":");
  const id = Number(rawId);
  if ((kind !== "step" && kind !== "project") || !Number.isInteger(id)) return null;
  return { kind, id };
}

function selectedRefTokens(row) {
  const container = row.closest("[data-selected-refs]");
  if (!container) return [];
  return (container.dataset.selectedRefs || "").split(/\s+/).filter(Boolean);
}

function isEditable(target) {
  if (!(target instanceof Element)) return false;
  const tag = target.tagName;
  return (
    tag === "INPUT" ||
    tag === "TEXTAREA" ||
    tag === "SELECT" ||
    target.isContentEditable ||
    !!target.closest(".cm-editor")
  );
}

function chooseToken(node, linkRequested) {
  const tokens = (node.dataset.dropAllowed || "").split(/\s+/).filter(Boolean);
  const wanted = linkRequested ? "link" : "move";
  return tokens.includes(wanted) ? wanted : null;
}

function inEdgeBand(row, event) {
  const rect = row.getBoundingClientRect();
  if (rect.height === 0) return false;
  const offset = event.clientY - rect.top;
  return offset < rect.height * 0.3 || rect.height - offset < rect.height * 0.3;
}

function isTopHalf(row, event) {
  const rect = row.getBoundingClientRect();
  return event.clientY < rect.top + rect.height / 2;
}

function allowsEdge(row, before) {
  return (row.dataset.dropEdges || "").split(/\s+/).includes(before ? "before" : "after");
}

function hitTest(event) {
  const target = event.target instanceof Element ? event.target : null;
  if (!target) return null;
  const row = target.closest("[data-drag-ref]");
  const folderNode = target.closest("[data-drop-folder]");
  const linkRequested = event.ctrlKey || event.metaKey || event.altKey;
  const before = !!row && isTopHalf(row, event);
  const edge = !linkRequested && !!row && allowsEdge(row, before);
  if (edge && inEdgeBand(row, event)) return edgeHit(row, before);
  const token = folderNode ? chooseToken(folderNode, linkRequested) : null;
  if (token) return { kind: "folder", node: folderNode, token };
  if (edge) return edgeHit(row, before);
  return null;
}

function edgeHit(row, before) {
  const next = before ? null : row.nextElementSibling;
  return next && next.matches("[data-drag-ref]")
    ? { kind: "edge", row: next, before: true, token: "move" }
    : { kind: "edge", row, before, token: "move" };
}

function clearIndicator() {
  if (indicator) {
    indicator.classList.remove("drop-into", "drop-before", "drop-after");
    indicator = null;
  }
}

function indicatorTarget(hit) {
  if (hit.kind !== "edge") return { node: hit.node, className: "drop-into" };
  return { node: hit.row, className: hit.before ? "drop-before" : "drop-after" };
}

function showIndicator(hit) {
  clearIndicator();
  const { node, className } = indicatorTarget(hit);
  node.classList.add(className);
  indicator = node;
}

function setDragImage(event, count) {
  const ghost = document.createElement("div");
  ghost.className = "drag-ghost";
  ghost.textContent = count > 1 ? String(count) : "";
  document.body.appendChild(ghost);
  event.dataTransfer.setDragImage(ghost, 14, 14);
  setTimeout(() => ghost.remove(), 0);
}

function onDragStart(app, event) {
  const row =
    event.target instanceof Element && event.target.matches("[data-drag-handle]")
      ? event.target.closest("[data-drag-ref]")
      : null;
  if (!row || !event.dataTransfer) return;
  const rowToken = row.dataset.dragRef;
  if (!parseRef(rowToken)) return;
  const folder = row.dataset.dragFolder || "";
  const tokens = selectedRefTokens(row);
  const payload = tokens.includes(rowToken) ? tokens : [rowToken];
  const refs = payload.map(parseRef).filter(Boolean);
  if (!refs.length) return;
  setDragImage(event, refs.length);
  if (app.ports && app.ports.organizeDragIn) {
    app.ports.organizeDragIn.send({
      type: "start",
      sourceFolderId: folder ? Number(folder) : null,
      refs,
    });
  }
}

function onDragOver(event) {
  const hit = hitTest(event);
  if (!hit) {
    if (event.dataTransfer) event.dataTransfer.dropEffect = "none";
    clearIndicator();
    return;
  }
  event.preventDefault();
  if (event.dataTransfer) {
    event.dataTransfer.dropEffect = hit.token === "link" ? "link" : "move";
  }
  showIndicator(hit);
}

function onDragLeave(event) {
  if (!event.relatedTarget) clearIndicator();
}

function onDrop(app, event) {
  const hit = hitTest(event);
  clearIndicator();
  if (!hit) return;
  event.preventDefault();
  const linkModifier = !!(event.ctrlKey || event.metaKey || event.altKey);
  if (!(app.ports && app.ports.organizeDragIn)) return;
  if (hit.kind === "folder") {
    const folderId = Number(hit.node.dataset.dropFolder);
    if (!Number.isInteger(folderId)) return;
    app.ports.organizeDragIn.send({
      type: "drop",
      target: { kind: "folder", folderId },
      linkModifier,
    });
    return;
  }
  const ref = parseRef(hit.row.dataset.dragRef);
  if (!ref) return;
  const folder = hit.row.dataset.dragFolder || "";
  app.ports.organizeDragIn.send({
    type: "drop",
    target: {
      kind: "edge",
      parentId: folder ? Number(folder) : null,
      ref,
      before: hit.before,
    },
    linkModifier,
  });
}

function onDragEnd(app) {
  clearIndicator();
  if (app.ports && app.ports.organizeDragIn) {
    app.ports.organizeDragIn.send({ type: "end" });
  }
}

const SHORTCUT_KEYS = ["a", "c", "v", "x", "z"];

function hasTextSelection() {
  const selection = window.getSelection();
  return !!selection && !selection.isCollapsed;
}

function onKeydownGuard(event) {
  if (!(event.ctrlKey || event.metaKey) || event.altKey || event.shiftKey) return;
  if (isEditable(event.target)) return;
  if (!SHORTCUT_KEYS.includes(event.key.toLowerCase())) return;
  if (hasTextSelection()) return;
  if (!document.querySelector("[data-selected-refs]")) return;
  event.preventDefault();
}

let installed = false;

export function installDragDropListeners(app) {
  if (installed) return;
  installed = true;
  document.addEventListener("dragstart", (event) => onDragStart(app, event));
  document.addEventListener("dragenter", onDragOver);
  document.addEventListener("dragover", onDragOver);
  document.addEventListener("dragleave", onDragLeave);
  document.addEventListener("drop", (event) => onDrop(app, event));
  document.addEventListener("dragend", () => onDragEnd(app));
  document.addEventListener("keydown", onKeydownGuard);
}
