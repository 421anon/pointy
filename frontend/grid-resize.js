// Column resize for delimited grid viewer.
// Standalone DOM listeners — not in ffi.js because this is not called from Elm.
(function () {
  let active = null;
  let startX = 0;
  let startWidth = 0;

  function clamp(v, lo, hi) {
    return v < lo ? lo : v > hi ? hi : v;
  }

  function getColElement(colIndex, handle) {
    const viewer = handle.closest(".delimited-grid-viewer");
    if (!viewer) return null;
    const table = viewer.querySelector(".delimited-grid-table");
    if (!table) return null;
    const cols = table.querySelectorAll("colgroup col");
    return cols[colIndex] || null;
  }

  // Suppress click events on resize handles to avoid triggering sort.
  document.addEventListener(
    "click",
    (event) => {
      if (event.target.closest(".delimited-grid-resize-handle")) {
        event.stopPropagation();
        event.stopImmediatePropagation();
      }
    },
    true
  );

  document.addEventListener("pointerdown", (event) => {
    const handle = event.target.closest(".delimited-grid-resize-handle");
    if (!handle) return;
    if (event.button !== 0) return;

    const colIndex = parseInt(handle.dataset.colIndex, 10);
    const col = getColElement(colIndex, handle);
    if (!col) return;

    startX = event.clientX;
    startWidth = col.offsetWidth || parseInt(col.style.width, 10) || 0;
    active = { col };

    handle.setPointerCapture(event.pointerId);
    event.preventDefault();
  });

  document.addEventListener("pointermove", (event) => {
    if (!active) return;
    const delta = event.clientX - startX;
    const newWidth = clamp(startWidth + delta, 40, 800);
    active.col.style.width = newWidth + "px";
  });

  document.addEventListener("pointerup", () => {
    active = null;
  });

  document.addEventListener("pointercancel", () => {
    active = null;
  });
})();
