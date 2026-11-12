export function readInput(inputId) {
  return document.getElementById(inputId)?.value ?? null;
}

export function setInput({ id, value }) {
  const input = document.getElementById(id);
  if (!input) return;
  input.value = value;
  input.dispatchEvent(new Event("input", { bubbles: true }));
}

export async function focusInput(inputId) {
  await new Promise((resolve) => requestAnimationFrame(resolve));
  const input = document.getElementById(inputId);
  if (!input) return;
  input.focus();
  input.setSelectionRange(input.value.length, input.value.length);
}

export function installChangeMarkers() {
  document.addEventListener("input", ({ target }) => {
    if (!target.hasAttribute?.("data-track-changes")) return;
    target.toggleAttribute("data-changed", target.value !== target.defaultValue);
  });
}
