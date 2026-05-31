import "./styles/main.scss";
import { Elm } from "./src/Main.elm";
import { connectPorts } from "./ffi.js";
import "./code-editor.js";

// Initialize the Elm app
const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: { origin: window.location.origin },
});

connectPorts(app);

// OS theme change listener (when no manual preference)
window
  .matchMedia("(prefers-color-scheme: light)")
  .addEventListener("change", (e) => {
    if (!localStorage.getItem("theme")) {
      document.documentElement.setAttribute(
        "data-theme",
        e.matches ? "light" : "dark"
      );
    }
  });

// Auto-resize textarea functionality
function setupTextarea(textarea) {
  textarea.style.height = "auto";
  textarea.style.height = textarea.scrollHeight + "px";
  textarea.oninput = () => {
    textarea.style.height = "auto";
    textarea.style.height = textarea.scrollHeight + "px";
  };
}

// Initial setup and observe for new textareas
setTimeout(
  () =>
    document
      .querySelectorAll("textarea[data-auto-resize]")
      .forEach(setupTextarea),
  100,
);
new MutationObserver(() => {
  document
    .querySelectorAll("textarea[data-auto-resize]:not([data-setup])")
    .forEach((textarea) => {
      textarea.setAttribute("data-setup", "true");
      setupTextarea(textarea);
    });
}).observe(document.body, { childList: true, subtree: true });


// Column resize for delimited grid viewer.
(function () {
  let active = null;
  let startX = 0;
  let startWidth = 0;

  function clamp(v, lo, hi) {
    return v < lo ? lo : v > hi ? hi : v;
  }

  function getColElement(colIndex, handle) {
    const viewer = handle.closest('.delimited-grid-viewer');
    if (!viewer) return null;
    const table = viewer.querySelector('.delimited-grid-table');
    if (!table) return null;
    const cols = table.querySelectorAll('colgroup col');
    return cols[colIndex] || null;
  }

  document.addEventListener('click', (event) => {
    if (event.target.closest('.delimited-grid-resize-handle')) {
      event.stopPropagation();
      event.stopImmediatePropagation();
    }
  }, true);

  document.addEventListener('pointerdown', (event) => {
    const handle = event.target.closest('.delimited-grid-resize-handle');
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

  document.addEventListener('pointermove', (event) => {
    if (!active) return;
    const delta = event.clientX - startX;
    const newWidth = clamp(startWidth + delta, 40, 800);
    active.col.style.width = newWidth + 'px';
  });

  document.addEventListener('pointerup', () => {
    active = null;
  });

  document.addEventListener('pointercancel', () => {
    active = null;
  });
})();
