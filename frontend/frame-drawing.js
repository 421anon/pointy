import { DEFAULT_MARKER_COLOR, DEFAULT_MARKER_WIDTH, DEFAULT_OVERVIEW_MAX_EDGE, canvasColor, canvasOf, cropAndMark, overviewScaleFor } from "./area-images.js";

// Chrome 157 draws same-origin documents with drawElementImage, but leaves out or misplaces the contents of nested canvases and media, and leaves out cross-origin fonts and CSS images even when CORS allows them. https://github.com/WICG/html-in-canvas#read-back-allowed-rendering
const UNDRAWN_CONTENT = "canvas, video, object, embed";
const NON_PAINTING_INITIATORS = ["script", "fetch", "xmlhttprequest", "beacon"];
// Entries past the default buffer size are dropped, so a full buffer may hide a cross-origin resource. https://w3c.github.io/resource-timing/#sec-extensions-performance-interface
const RESOURCE_TIMING_BUFFER_SIZE = 250;
const SKIPPING_CONTENT_VISIBILITIES = ["auto", "hidden"];
const LAYOUT_SAMPLE_SIZE = 2000;
const LAYOUT_TOLERANCE = 0.5;

function supportsElementImages(window) {
  return "drawElementImage" in window.CanvasRenderingContext2D.prototype && "requestPaint" in window.HTMLCanvasElement.prototype;
}

function frameDocument(frame) {
  try {
    return frame.contentDocument;
  } catch {
    return null;
  }
}

function documentTree(document) {
  const documents = [document];
  for (const frame of document.querySelectorAll("iframe, frame")) {
    const nested = frameDocument(frame);
    const nestedTree = nested && documentTree(nested);
    if (!nestedTree) return null;
    documents.push(...nestedTree);
  }
  return documents;
}

function loadsOnlySameOriginPaint(document) {
  const view = document.defaultView;
  const entries = view.performance.getEntriesByType("resource");
  if (entries.length >= RESOURCE_TIMING_BUFFER_SIZE) return false;
  return entries.every((entry) => NON_PAINTING_INITIATORS.includes(entry.initiatorType) || new URL(entry.name).origin === view.location.origin);
}

function rulesSkipContent(rules) {
  for (const rule of rules) {
    if (rule.styleSheet && rulesSkipContent(rule.styleSheet.cssRules)) return true;
    if (rule.style && SKIPPING_CONTENT_VISIBILITIES.includes(rule.style.getPropertyValue("content-visibility"))) return true;
    if (rule.cssRules && rulesSkipContent(rule.cssRules)) return true;
  }
  return false;
}

// Content the browser skips because it is off screen stays unpainted in a drawing of the whole page.
function mayLeaveContentUnpainted(document) {
  if (document.querySelector('[style*="content-visibility" i]')) return true;
  try {
    return [...document.styleSheets, ...document.adoptedStyleSheets].some((sheet) => rulesSkipContent(sheet.cssRules));
  } catch {
    return true;
  }
}

export function canDrawFrame(frame) {
  const document = frameDocument(frame);
  if (!document || !supportsElementImages(frame.ownerDocument.defaultView)) return false;
  const documents = documentTree(document);
  return documents !== null && documents.every((nested) => !nested.querySelector(UNDRAWN_CONTENT) && loadsOnlySameOriginPaint(nested) && !mayLeaveContentUnpainted(nested));
}

function followsViewport(element, positions) {
  const view = element.ownerDocument.defaultView;
  for (let node = element; node; node = node.parentElement) {
    if (!positions.has(node)) positions.set(node, view.getComputedStyle(node).position);
    const position = positions.get(node);
    if (position === "fixed" || position === "sticky") return true;
  }
  return false;
}

function pageBox(element) {
  const view = element.ownerDocument.defaultView;
  const box = element.getBoundingClientRect();
  return [box.left + view.scrollX, box.top + view.scrollY, box.width, box.height];
}

// Fixed and sticky elements follow the viewport, so they are left out of the comparison.
function layoutSample(document) {
  const elements = document.getElementsByTagName("*");
  const step = Math.max(1, Math.floor(elements.length / LAYOUT_SAMPLE_SIZE));
  const positions = new Map();
  const sample = [];
  for (let index = 0; index < elements.length; index += step) {
    const element = elements[index];
    if (element.getClientRects().length > 0 && !followsViewport(element, positions)) sample.push({ element, box: pageBox(element) });
  }
  return sample;
}

function keepsLayout(sample) {
  return sample.every(({ element, box }) => pageBox(element).every((value, index) => Math.abs(value - box[index]) <= LAYOUT_TOLERANCE));
}

function drawOnNextPaint(host, draw) {
  return new Promise((resolve, reject) => {
    host.addEventListener(
      "paint",
      () => {
        try {
          resolve(draw(host.getContext("2d")));
        } catch (error) {
          reject(error);
        }
      },
      { once: true },
    );
    host.requestPaint();
  });
}

function copyOf(canvas) {
  const copy = canvasOf(canvas.ownerDocument, canvas.width, canvas.height);
  copy.getContext("2d").drawImage(canvas, 0, 0);
  return copy;
}

function afterNextRendering(window) {
  return new Promise((resolve) => window.requestAnimationFrame(() => window.setTimeout(resolve, 0)));
}

// The frame is moved into a drawable canvas with moveBefore, which keeps its document loaded, and shown through the canvas until it is moved back. The whole page is drawn by making the frame as tall as the page, which is only a faithful overview when that leaves the layout of the page as it was. https://dom.spec.whatwg.org/#dom-parentnode-movebefore
async function drawFrameArea(frame, rect) {
  const document = frame.contentDocument;
  const view = frame.contentWindow;
  const root = document.documentElement;
  const hostDocument = frame.ownerDocument;
  const scroll = { x: view.scrollX, y: view.scrollY };
  const documentSize = { width: root.scrollWidth, height: root.scrollHeight };
  const layoutWidth = root.clientWidth;
  const pixelRatio = view.devicePixelRatio;
  const backgroundColor = canvasColor(document);
  const frameBox = frame.getBoundingClientRect();
  const sample = layoutSample(document);
  const frameStyle = frame.getAttribute("style");
  const host = canvasOf(hostDocument, Math.round(frameBox.width * pixelRatio), Math.round(frameBox.height * pixelRatio));
  host.setAttribute("content", "drawable");
  host.style.cssText = `display:block;width:${frameBox.width}px;height:${frameBox.height}px`;
  frame.parentNode.insertBefore(host, frame);
  host.moveBefore(frame, null);
  frame.setAttribute("drawable", "");
  frame.style.width = `${frameBox.width}px`;
  frame.style.height = `${frameBox.height}px`;
  try {
    const viewport = await drawOnNextPaint(host, (context) => {
      context.reset();
      context.fillStyle = backgroundColor;
      context.fillRect(0, 0, host.width, host.height);
      context.drawElementImage(frame, 0, 0, host.width, host.height, { preserveElementGeometry: true });
      return copyOf(host);
    });
    frame.style.height = `${documentSize.height}px`;
    frame.style.width = `${frameBox.width + Math.max(documentSize.width, layoutWidth) - root.clientWidth}px`;
    if (root.scrollHeight !== documentSize.height || !keepsLayout(sample)) return null;
    const overviewScale = overviewScaleFor(documentSize, DEFAULT_OVERVIEW_MAX_EDGE);
    const overview = await drawOnNextPaint(host, (context) => {
      host.width = Math.max(1, Math.round(documentSize.width * overviewScale));
      host.height = Math.max(1, Math.round(documentSize.height * overviewScale));
      context.fillStyle = backgroundColor;
      context.fillRect(0, 0, host.width, host.height);
      context.drawElementImage(frame, 0, 0, documentSize.width, documentSize.height, 0, 0, host.width, host.height, { preserveElementGeometry: true });
      const drawn = copyOf(host);
      host.width = viewport.width;
      host.height = viewport.height;
      context.drawImage(viewport, 0, 0);
      return drawn;
    });
    return cropAndMark(document, rect, { scroll, pixelRatio, documentSize, viewport, overview, overviewScale, markerColor: DEFAULT_MARKER_COLOR, markerWidth: DEFAULT_MARKER_WIDTH });
  } finally {
    frame.removeAttribute("drawable");
    if (frameStyle === null) frame.removeAttribute("style");
    else frame.setAttribute("style", frameStyle);
    host.parentNode.moveBefore(frame, host);
    host.remove();
    view.scrollTo(scroll.x, scroll.y);
    await afterNextRendering(hostDocument.defaultView);
  }
}

// Returns null when the frame cannot be drawn faithfully, and the caller then clones the page instead.
export async function captureFrameArea(frame, rect) {
  if (!canDrawFrame(frame)) return null;
  try {
    return await drawFrameArea(frame, rect);
  } catch (error) {
    console.warn("Drawing the artifact frame failed, so it is cloned instead.", error);
    return null;
  }
}
