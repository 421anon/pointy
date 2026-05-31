// Column resize for delimited grid viewer.
// Standalone DOM listeners — not in ffi.js because this is not called from Elm.
(function () {
  const MIN_COLUMN_WIDTH = 40;
  const MAX_COLUMN_WIDTH = 800;

  let active = null;

  function clamp(v, lo, hi) {
    return v < lo ? lo : v > hi ? hi : v;
  }

  function getGridElements(colIndex, handle) {
    const viewer = handle.closest(".delimited-grid-viewer");
    if (!viewer) return null;
    const table = viewer.querySelector(".delimited-grid-table");
    if (!table) return null;

    const cols = Array.from(table.querySelectorAll("colgroup col"));
    const col = cols[colIndex] || null;
    if (!col) return null;

    return { table, cols, col };
  }

  function freezeRenderedLayout(table, cols) {
    const colWidths = cols.map((col) => col.getBoundingClientRect().width);
    const tableWidth = table.getBoundingClientRect().width;

    // The table may be stretched by min-width: 100%; freeze rendered pixels
    // before resizing one <col> so the browser does not redistribute columns.
    table.style.width = tableWidth + "px";
    table.style.minWidth = tableWidth + "px";
    cols.forEach((col, index) => {
      col.style.width = colWidths[index] + "px";
    });

    return { colWidths, tableWidth };
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
    const grid = getGridElements(colIndex, handle);
    if (!grid) return;

    const { colWidths, tableWidth } = freezeRenderedLayout(grid.table, grid.cols);

    active = {
      col: grid.col,
      table: grid.table,
      startX: event.clientX,
      startWidth: colWidths[colIndex],
      startTableWidth: tableWidth,
    };

    handle.setPointerCapture(event.pointerId);
    event.preventDefault();
  });

  document.addEventListener("pointermove", (event) => {
    if (!active) return;

    const newWidth = clamp(
      active.startWidth + event.clientX - active.startX,
      MIN_COLUMN_WIDTH,
      MAX_COLUMN_WIDTH
    );
    const widthDelta = newWidth - active.startWidth;
    const newTableWidth = active.startTableWidth + widthDelta;

    active.col.style.width = newWidth + "px";
    active.table.style.width = newTableWidth + "px";
    active.table.style.minWidth = newTableWidth + "px";
  });

  document.addEventListener("pointerup", () => {
    active = null;
  });

  document.addEventListener("pointercancel", () => {
    active = null;
  });
})();
