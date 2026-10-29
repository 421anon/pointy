import { clonePage } from "./page-clone.js";

const OVERVIEW_MAX_EDGE = 1024;
const OVERVIEW_BORDER_COLOR = "red";
const OVERVIEW_BORDER_WIDTH = 2;
const SVG_NAMESPACE = "http://www.w3.org/2000/svg";
const TRANSPARENT = "rgba(0, 0, 0, 0)";
const DEFAULT_CANVAS_COLOR = "white";

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
async function renderPage(page, { size, scale, root }) {
  const image = new Image();
  image.src = svgDataUrl(page, { size, root });
  await image.decode();
  const canvas = document.createElement("canvas");
  canvas.width = Math.round(size.width * scale);
  canvas.height = Math.round(size.height * scale);
  const context = canvas.getContext("2d");
  context.fillStyle = root.backgroundColor;
  context.fillRect(0, 0, canvas.width, canvas.height);
  context.drawImage(image, 0, 0, canvas.width, canvas.height);
  return canvas;
}

export async function captureArtifactArea({ frameId, x, y, width, height }) {
  const frame = document.getElementById(frameId);
  const view = frame.contentWindow;
  const pageStyle = view.getComputedStyle(frame.contentDocument.documentElement);
  const backgroundColor = canvasColor(frame.contentDocument);
  const documentSize = { width: frame.contentDocument.documentElement.scrollWidth, height: frame.contentDocument.documentElement.scrollHeight };
  const scroll = { x: view.scrollX, y: view.scrollY };
  const pixelRatio = window.devicePixelRatio;
  const selectionRect = {
    x: Math.round(x + scroll.x),
    y: Math.round(y + scroll.y),
    width: Math.max(1, Math.round(width)),
    height: Math.max(1, Math.round(height)),
  };
  const overviewScale = Math.min(1, OVERVIEW_MAX_EDGE / Math.max(documentSize.width, documentSize.height));
  const page = await clonePage(frame.contentDocument);
  const [viewport, overview] = await Promise.all([
    renderPage(page, {
      size: { width: frame.clientWidth, height: frame.clientHeight },
      scale: pixelRatio,
      root: { width: pageStyle.width, height: pageStyle.height, backgroundColor, scroll },
    }),
    renderPage(page, {
      size: documentSize,
      scale: overviewScale,
      root: { width: `${documentSize.width}px`, height: `${documentSize.height}px`, backgroundColor, scroll: { x: 0, y: 0 } },
    }),
  ]);
  const selection = document.createElement("canvas");
  selection.width = Math.round(width * pixelRatio);
  selection.height = Math.round(height * pixelRatio);
  selection.getContext("2d").drawImage(viewport, x * pixelRatio, y * pixelRatio, selection.width, selection.height, 0, 0, selection.width, selection.height);
  const overviewContext = overview.getContext("2d");
  overviewContext.strokeStyle = OVERVIEW_BORDER_COLOR;
  overviewContext.lineWidth = OVERVIEW_BORDER_WIDTH;
  overviewContext.strokeRect(
    selectionRect.x * overviewScale,
    selectionRect.y * overviewScale,
    selectionRect.width * overviewScale,
    selectionRect.height * overviewScale,
  );
  return {
    overview: overview.toDataURL("image/png"),
    selection: selection.toDataURL("image/png"),
    documentSize,
    selectionRect,
  };
}
