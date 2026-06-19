// Column resize for delimited grid viewer.
// Standalone DOM listeners — not in ffi.js because this is not called from Elm.
// Virtual rows are created/destroyed as you scroll, so column widths live on
// `.delimited-grid` CSS variables instead of rendered row cells.
(function () {
  const MIN_COLUMN_WIDTH = 40;
  const MAX_COLUMN_WIDTH = 800;

  let active = null;

  function clamp(v, lo, hi) {
    return v < lo ? lo : v > hi ? hi : v;
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

    const colIndex = Number.parseInt(handle.dataset.colIndex, 10);
    if (Number.isNaN(colIndex)) return;

    const grid = handle.closest(".delimited-grid");
    const cell = handle.closest(".delimited-grid-th");
    if (!grid || !cell) return;

    // Prefer the inline width so repeated resizes accumulate from the last drag.
    const startWidth = cell.getBoundingClientRect().width;
    const startGridWidth =
      Number.parseFloat(grid.style.width) || grid.getBoundingClientRect().width;

    active = {
      grid,
      colIndex,
      startX: event.clientX,
      startWidth,
      startGridWidth,
    };

    handle.setPointerCapture(event.pointerId);
    event.preventDefault();
  });

  document.addEventListener(
    "pointermove",
    (event) => {
      if (!active) return;

      event.preventDefault();

      const newWidth = clamp(
        active.startWidth + event.clientX - active.startX,
        MIN_COLUMN_WIDTH,
        MAX_COLUMN_WIDTH
      );
      const widthDelta = newWidth - active.startWidth;

      active.grid.style.setProperty("--dg-col-" + active.colIndex, newWidth + "px");
      active.grid.style.width = active.startGridWidth + widthDelta + "px";
    },
    { passive: false }
  );

  const releasePointer = () => {
    active = null;
  };
  document.addEventListener("pointerup", releasePointer);
  document.addEventListener("pointercancel", releasePointer);
})();
