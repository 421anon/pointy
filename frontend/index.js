import "./styles/main.scss";
import { Elm } from "./src/Main.elm";
import { connectPorts } from "./ffi.js";

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
