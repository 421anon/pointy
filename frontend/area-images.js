const XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
const TRANSPARENT = "rgba(0, 0, 0, 0)";
const DEFAULT_CANVAS_COLOR = "white";
export const DEFAULT_OVERVIEW_MAX_EDGE = 1024;
export const DEFAULT_MARKER_COLOR = "red";
export const DEFAULT_MARKER_WIDTH = 2;

export function canvasOf(document, width, height) {
  const canvas = document.createElementNS(XHTML_NAMESPACE, "canvas");
  canvas.width = width;
  canvas.height = height;
  return canvas;
}

// The root's background, or else the body's, paints the whole canvas, beyond the root's box. https://www.w3.org/TR/css-backgrounds-3/#special-backgrounds
export function canvasColor(document) {
  return (
    [document.documentElement, document.body]
      .filter(Boolean)
      .map((element) => document.defaultView.getComputedStyle(element).backgroundColor)
      .find((color) => color !== TRANSPARENT) ?? DEFAULT_CANVAS_COLOR
  );
}

export function overviewScaleFor(documentSize, overviewMaxEdge) {
  return Math.min(1, overviewMaxEdge / Math.max(documentSize.width, documentSize.height));
}

// `rect` is in viewport CSS pixels of a `viewport` image drawn at `pixelRatio`; the returned selectionRect is in page CSS pixels and is outlined on the `overview` image of the whole page.
export function cropAndMark(document, rect, { scroll, pixelRatio, documentSize, viewport, overview, overviewScale, markerColor, markerWidth }) {
  const selectionRect = {
    x: Math.round(rect.x + scroll.x),
    y: Math.round(rect.y + scroll.y),
    width: Math.max(1, Math.round(rect.width)),
    height: Math.max(1, Math.round(rect.height)),
  };
  const selection = canvasOf(document, Math.max(1, Math.round(rect.width * pixelRatio)), Math.max(1, Math.round(rect.height * pixelRatio)));
  selection.getContext("2d").drawImage(viewport, rect.x * pixelRatio, rect.y * pixelRatio, selection.width, selection.height, 0, 0, selection.width, selection.height);
  const markedOverview = canvasOf(document, overview.width, overview.height);
  const context = markedOverview.getContext("2d");
  context.drawImage(overview, 0, 0);
  context.strokeStyle = markerColor;
  context.lineWidth = markerWidth;
  context.strokeRect(selectionRect.x * overviewScale, selectionRect.y * overviewScale, selectionRect.width * overviewScale, selectionRect.height * overviewScale);
  return { selection, overview: markedOverview, documentSize, selectionRect };
}
