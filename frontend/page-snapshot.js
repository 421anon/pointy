import { clonePage } from "./page-clone.js";

const XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
const SVG_NAMESPACE = "http://www.w3.org/2000/svg";
const TRANSPARENT = "rgba(0, 0, 0, 0)";
const DEFAULT_CANVAS_COLOR = "white";
const DEFAULT_OVERVIEW_MAX_EDGE = 1024;
const DEFAULT_MARKER_COLOR = "red";
const DEFAULT_MARKER_WIDTH = 2;
const CLONE_INVALIDATING_EVENTS = ["pointerdown", "pointerup", "pointercancel", "keydown", "keyup", "input", "change", "toggle", "focusin", "focusout", "load", "error", "animationstart", "transitionrun"];

const yieldToBrowser = () => globalThis.scheduler?.yield?.() ?? new Promise((resolve) => setTimeout(resolve, 0));

// A reusable snapshot of a document. prepare() clones the page and renders the current viewport and overview ahead of a capture; captureArea(rect) then only crops and marks the selection. Both are rebuilt on the next call after the page changes. Coordinates passed in are viewport CSS pixels; the returned selectionRect is in page CSS pixels.
export function createPageSnapshot(document, options = {}) {
  const overviewMaxEdge = options.overviewMaxEdge ?? DEFAULT_OVERVIEW_MAX_EDGE;
  const markerColor = options.markerColor ?? DEFAULT_MARKER_COLOR;
  const markerWidth = options.markerWidth ?? DEFAULT_MARKER_WIDTH;
  const view = document.defaultView;
  let page = null;
  let pendingClone = null;
  let pendingRenders = null;
  let pendingRendersKey = null;
  let renders = null;
  let watcher = null;
  let generation = 0;
  let disposed = false;

  function invalidateEntry(entry) {
    forgetPageClone(entry);
    if (watcher === entry) {
      watcher = null;
      page = null;
      renders = null;
    }
  }

  function forgetWatcher() {
    if (watcher) invalidateEntry(watcher);
  }

  function watch() {
    const entry = { stale: false, cleanups: [], observer: new MutationObserver(() => invalidateEntry(entry)) };
    const forget = () => invalidateEntry(entry);
    const listen = (target, name, listener, listenerOptions) => {
      target.addEventListener(name, listener, listenerOptions);
      entry.cleanups.push(() => target.removeEventListener(name, listener, listenerOptions));
    };
    entry.observer.observe(document.documentElement, { subtree: true, childList: true, characterData: true, attributes: true });
    for (const name of CLONE_INVALIDATING_EVENTS) listen(document, name, forget, { capture: true, passive: true });
    listen(
      document,
      "scroll",
      (event) => {
        if (event.target !== document && event.target !== document.scrollingElement) forget();
      },
      { capture: true, passive: true },
    );
    listen(view, "resize", forget, { passive: true });
    listen(view, "hashchange", forget, { passive: true });
    listen(view.matchMedia("(prefers-color-scheme: dark)"), "change", forget);
    if (document.fonts) listen(document.fonts, "loadingdone", forget);
    return entry;
  }

  async function clonePageInSlices() {
    const startedAt = generation;
    const watching = isStaticDocument(document) ? watch() : null;
    try {
      const cloned = await clonePage(document, { yieldBetweenBatches: yieldToBrowser });
      if (watching && !watching.stale && startedAt === generation) {
        watcher = watching;
        page = cloned;
      } else if (watching) {
        forgetPageClone(watching);
      }
      return { page: cloned, consistent: !watching || !watching.stale };
    } catch (error) {
      if (watching) forgetPageClone(watching);
      throw error;
    }
  }

  async function preparePage() {
    let result = await clonePageInSlices();
    if (!result.consistent) result = await clonePageInSlices();
    return result.page;
  }

  function ensurePage() {
    if (page) return Promise.resolve(page);
    if (!pendingClone) pendingClone = preparePage().finally(() => (pendingClone = null));
    return pendingClone;
  }

  function measure() {
    const root = document.documentElement;
    return {
      scroll: { x: view.scrollX, y: view.scrollY },
      rootStyle: view.getComputedStyle(root),
      backgroundColor: options.backgroundColor ?? canvasColor(document),
      documentSize: { width: root.scrollWidth, height: root.scrollHeight },
      pixelRatio: options.pixelRatio ?? view.devicePixelRatio,
      viewportSize: { width: view.innerWidth, height: view.innerHeight },
    };
  }

  function renderKey(measurement) {
    const { scroll, rootStyle, backgroundColor, documentSize, pixelRatio, viewportSize } = measurement;
    return [generation, scroll.x, scroll.y, rootStyle.width, rootStyle.height, backgroundColor, documentSize.width, documentSize.height, pixelRatio, viewportSize.width, viewportSize.height].join("|");
  }

  async function ensureRenders(measurement) {
    const key = renderKey(measurement);
    if (watcher && renders?.key === key) return renders;
    if (pendingRenders && pendingRendersKey === key) return pendingRenders;
    pendingRendersKey = key;
    pendingRenders = (async () => {
      const cloned = await ensurePage();
      const { scroll, rootStyle, backgroundColor, documentSize, pixelRatio, viewportSize } = measurement;
      const overviewScale = Math.min(1, overviewMaxEdge / Math.max(documentSize.width, documentSize.height));
      const [viewport, overview] = await Promise.all([
        renderPage(document, cloned, {
          size: viewportSize,
          scale: pixelRatio,
          root: { width: rootStyle.width, height: rootStyle.height, backgroundColor, scroll },
        }),
        renderPage(document, cloned, {
          size: documentSize,
          scale: overviewScale,
          root: { width: `${documentSize.width}px`, height: `${documentSize.height}px`, backgroundColor, scroll: { x: 0, y: 0 } },
        }),
      ]);
      const result = { key, viewport, overview, overviewScale };
      if (watcher) renders = result;
      return result;
    })().finally(() => {
      pendingRenders = null;
      pendingRendersKey = null;
    });
    return pendingRenders;
  }

  async function prepare() {
    if (disposed) throw new Error("This page snapshot has been disposed.");
    await ensurePage();
    if (watcher) await ensureRenders(measure());
  }

  async function captureArea(rect) {
    if (disposed) throw new Error("This page snapshot has been disposed.");
    const measurement = measure();
    const { viewport, overview, overviewScale } = await ensureRenders(measurement);
    const selectionRect = {
      x: Math.round(rect.x + measurement.scroll.x),
      y: Math.round(rect.y + measurement.scroll.y),
      width: Math.max(1, Math.round(rect.width)),
      height: Math.max(1, Math.round(rect.height)),
    };
    const selection = canvasOf(document, Math.max(1, Math.round(rect.width * measurement.pixelRatio)), Math.max(1, Math.round(rect.height * measurement.pixelRatio)));
    selection.getContext("2d").drawImage(viewport, rect.x * measurement.pixelRatio, rect.y * measurement.pixelRatio, selection.width, selection.height, 0, 0, selection.width, selection.height);
    const markedOverview = canvasOf(document, overview.width, overview.height);
    markedOverview.getContext("2d").drawImage(overview, 0, 0);
    drawMarker(markedOverview, selectionRect, overviewScale, markerColor, markerWidth);
    return { selection, overview: markedOverview, documentSize: measurement.documentSize, selectionRect };
  }

  function invalidate() {
    generation += 1;
    forgetWatcher();
  }

  function dispose() {
    invalidate();
    disposed = true;
  }

  return { prepare, captureArea, invalidate, dispose };
}

function forgetPageClone(entry) {
  entry.stale = true;
  entry.observer.disconnect();
  entry.cleanups.splice(0).forEach((cleanup) => cleanup());
}

// Canvas and video frames, nested documents and shadow roots change without a mutation the observer can see.
function isStaticDocument(document) {
  if (document.querySelector("canvas, video, iframe, object, embed")) return false;
  for (const element of document.querySelectorAll("*")) if (element.shadowRoot) return false;
  if (document.fonts && document.fonts.status !== "loaded") return false;
  return !document.getAnimations || document.getAnimations().length === 0;
}

function htmlElement(document, name) {
  return document.createElementNS(XHTML_NAMESPACE, name);
}

function canvasOf(document, width, height) {
  const canvas = htmlElement(document, "canvas");
  canvas.width = width;
  canvas.height = height;
  return canvas;
}

// The root's background, or else the body's, paints the whole canvas, beyond the root's box. https://www.w3.org/TR/css-backgrounds-3/#special-backgrounds
function canvasColor(document) {
  return (
    [document.documentElement, document.body]
      .filter(Boolean)
      .map((element) => document.defaultView.getComputedStyle(element).backgroundColor)
      .find((color) => color !== TRANSPARENT) ?? DEFAULT_CANVAS_COLOR
  );
}

function escapeText(text) {
  return text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
}

// A canvas that draws an SVG from a blob URL is tainted when the SVG has a foreignObject, so the SVG goes in a data URL.
function svgDataUrl(page, { size, root }) {
  const svg =
    `<svg xmlns="${SVG_NAMESPACE}" width="${size.width}" height="${size.height}" viewBox="0 0 ${size.width} ${size.height}">` +
    `<style>${escapeText(page.styleSheet)}</style>` +
    `<foreignObject x="0" y="0" width="100%" height="100%">${page.markup(root)}</foreignObject></svg>`;
  return `data:image/svg+xml;charset=utf-8,${encodeURIComponent(svg)}`;
}

// In the SVG the root is an ordinary box, so the canvas outside it, such as a collapsed body margin or the space below a short page, is painted separately.
async function renderPage(document, page, { size, scale, root }) {
  const image = htmlElement(document, "img");
  image.src = svgDataUrl(page, { size, root });
  await image.decode();
  const canvas = canvasOf(document, Math.round(size.width * scale), Math.round(size.height * scale));
  const context = canvas.getContext("2d");
  context.fillStyle = root.backgroundColor;
  context.fillRect(0, 0, canvas.width, canvas.height);
  context.drawImage(image, 0, 0, canvas.width, canvas.height);
  return canvas;
}

function drawMarker(canvas, rect, scale, color, width) {
  const context = canvas.getContext("2d");
  context.strokeStyle = color;
  context.lineWidth = width;
  context.strokeRect(rect.x * scale, rect.y * scale, rect.width * scale, rect.height * scale);
}
