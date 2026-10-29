import { toSvg } from "html-to-image";

const OVERVIEW_MAX_EDGE = 1024;
const OVERVIEW_BORDER_COLOR = "red";
const OVERVIEW_BORDER_WIDTH = 2;
const CAPTURE_SCROLL_BOX_ATTRIBUTE = "data-pointy-capture-scroll-box";
const CAPTURE_FONT_SIZE_ATTRIBUTE = "data-pointy-capture-font-size";
const XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
const SCROLLING_OVERFLOWS = ["auto", "scroll"];
const TRANSPARENT = "rgba(0, 0, 0, 0)";
const DEFAULT_CANVAS_COLOR = "white";

function scrollBoxState(element) {
  const style = element.ownerDocument.defaultView.getComputedStyle(element);
  if (!SCROLLING_OVERFLOWS.includes(style.overflowX) && !SCROLLING_OVERFLOWS.includes(style.overflowY)) return null;
  const scrollbarWidth = element.offsetWidth - element.clientWidth - parseFloat(style.borderLeftWidth) - parseFloat(style.borderRightWidth);
  const scrollbarHeight = element.offsetHeight - element.clientHeight - parseFloat(style.borderTopWidth) - parseFloat(style.borderBottomWidth);
  const hasScrollbars = scrollbarWidth >= 1 || scrollbarHeight >= 1;
  if (!hasScrollbars && element.scrollLeft === 0 && element.scrollTop === 0) return null;
  return {
    scrollX: element.scrollLeft,
    scrollY: element.scrollTop,
    size: hasScrollbars ? { width: element.offsetWidth, height: element.offsetHeight } : null,
  };
}

function markScrollBoxes(document) {
  const marked = [...document.querySelectorAll("*")]
    .filter(
      (element) =>
        element !== document.documentElement &&
        element !== document.scrollingElement &&
        (element.scrollLeft !== 0 || element.scrollTop !== 0 || element.scrollWidth > element.clientWidth || element.scrollHeight > element.clientHeight),
    )
    .flatMap((element) => {
      const state = scrollBoxState(element);
      if (!state) return [];
      element.setAttribute(CAPTURE_SCROLL_BOX_ATTRIBUTE, JSON.stringify(state));
      return [element];
    });
  return () => marked.forEach((element) => element.removeAttribute(CAPTURE_SCROLL_BOX_ATTRIBUTE));
}

function markFontSizes(document) {
  const styledElements = [...document.querySelectorAll("*")].filter((element) => element.style);
  const fontSizes = styledElements.map((element) => document.defaultView.getComputedStyle(element).fontSize);
  styledElements.forEach((element, index) => element.setAttribute(CAPTURE_FONT_SIZE_ATTRIBUTE, fontSizes[index]));
  return () => styledElements.forEach((element) => element.removeAttribute(CAPTURE_FONT_SIZE_ATTRIBUTE));
}

// The copied style lists each logical property after its physical counterparts, so it would override what is set here. https://drafts.csswg.org/css-logical/#box
function removeLogicalProperties(style, properties) {
  properties.forEach((property) => style.removeProperty(property));
}

function pinBoxSize(style, { width, height }) {
  removeLogicalProperties(style, ["inline-size", "block-size", "min-inline-size", "min-block-size", "max-inline-size", "max-block-size"]);
  style.boxSizing = "border-box";
  style.width = `${width}px`;
  style.height = `${height}px`;
  style.minWidth = "0px";
  style.minHeight = "0px";
  style.maxWidth = "none";
  style.maxHeight = "none";
}

function offsetBox(style, scrollX, scrollY) {
  const left = parseFloat(style.left) || 0;
  const top = parseFloat(style.top) || 0;
  removeLogicalProperties(style, ["inset-inline-start", "inset-inline-end", "inset-block-start", "inset-block-end"]);
  style.left = `${left - scrollX}px`;
  style.top = `${top - scrollY}px`;
}

function shiftScrolledContent(box, scrollX, scrollY) {
  const isContainingBlock = box.style.position !== "" && box.style.position !== "static";
  for (const child of [...box.childNodes]) {
    if (child.nodeType === Node.TEXT_NODE && child.textContent.trim() !== "") {
      const span = box.ownerDocument.createElementNS(XHTML_NAMESPACE, "span");
      child.replaceWith(span);
      span.append(child);
      span.style.position = "relative";
      offsetBox(span.style, scrollX, scrollY);
    } else if (child.nodeType === Node.ELEMENT_NODE && child.style) {
      const position = child.style.position || "static";
      if (position === "static") {
        child.style.position = "relative";
        child.style.left = "0px";
        child.style.top = "0px";
        offsetBox(child.style, scrollX, scrollY);
      } else if (position === "relative" || position === "sticky" || (position === "absolute" && isContainingBlock)) {
        offsetBox(child.style, scrollX, scrollY);
      }
    }
  }
}

function restoreFontSize(element) {
  element.style.fontSize = element.getAttribute(CAPTURE_FONT_SIZE_ATTRIBUTE);
  element.removeAttribute(CAPTURE_FONT_SIZE_ATTRIBUTE);
}

function restoreScrollBox(box) {
  const { scrollX, scrollY, size } = JSON.parse(box.getAttribute(CAPTURE_SCROLL_BOX_ATTRIBUTE));
  box.removeAttribute(CAPTURE_SCROLL_BOX_ATTRIBUTE);
  if (size) pinBoxSize(box.style, size);
  if (scrollX !== 0 || scrollY !== 0) shiftScrolledContent(box, scrollX, scrollY);
}

// A transformed root would become the containing block of the page's fixed elements and carry them off with the scroll, while an offset root leaves them on the viewport and lets sticky elements stick. https://drafts.csswg.org/css-transforms-1/#containing-block-for-all-descendants
function scrollRoot(svg, { x, y }) {
  const root = svg.querySelector("foreignObject").firstElementChild;
  root.style.position = "relative";
  offsetBox(root.style, x, y);
}

function restoreRenderedPage(svgUrl, scroll) {
  const dataStart = svgUrl.indexOf(",") + 1;
  const svg = new DOMParser().parseFromString(decodeURIComponent(svgUrl.slice(dataStart)), "image/svg+xml");
  svg.querySelectorAll(`[${CAPTURE_FONT_SIZE_ATTRIBUTE}]`).forEach(restoreFontSize);
  svg.querySelectorAll(`[${CAPTURE_SCROLL_BOX_ATTRIBUTE}]`).forEach(restoreScrollBox);
  scrollRoot(svg, scroll);
  return svgUrl.slice(0, dataStart) + encodeURIComponent(new XMLSerializer().serializeToString(svg));
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

async function renderPage(page, { pixelRatio, width, height, style, scroll = { x: 0, y: 0 } }) {
  // html-to-image sizes the image from `width`/`height` and also writes them onto the root's style; `style` restores the page's own size so only the requested window is rendered without reflowing. https://github.com/bubkoo/html-to-image/blob/v1.11.13/src/apply-style.ts
  // Font embedding inserts fetched rules into live stylesheets, including the host page's. https://github.com/bubkoo/html-to-image/blob/v1.11.13/src/embed-webfonts.ts
  // Its copy of the page starts every scroll box at the top left and fixes each box's height without room for its scrollbars, so the state marked by markScrollBoxes is reapplied to the SVG before drawing. https://github.com/bubkoo/html-to-image/blob/v1.11.13/src/clone-node.ts
  // It also floors every font size and subtracts 0.1px, which narrows text and moves its line breaks, so the sizes marked by markFontSizes are reapplied too. https://github.com/bubkoo/html-to-image/blob/v1.11.13/src/clone-node.ts
  // In the SVG the root is an ordinary box, so the canvas outside it, such as a collapsed body margin or the space below a short page, is painted separately.
  const backgroundColor = canvasColor(page.ownerDocument);
  const svgUrl = await toSvg(page, { width, height, backgroundColor, skipFonts: true, style });
  const image = new Image();
  image.src = restoreRenderedPage(svgUrl, scroll);
  await image.decode();
  const canvas = document.createElement("canvas");
  canvas.width = Math.round(width * pixelRatio);
  canvas.height = Math.round(height * pixelRatio);
  const context = canvas.getContext("2d");
  context.fillStyle = backgroundColor;
  context.fillRect(0, 0, canvas.width, canvas.height);
  context.drawImage(image, 0, 0, canvas.width, canvas.height);
  return canvas;
}

export async function captureArtifactArea({ frameId, x, y, width, height }) {
  const frame = document.getElementById(frameId);
  const view = frame.contentWindow;
  const page = frame.contentDocument.documentElement;
  const pageStyle = view.getComputedStyle(page);
  const pixelRatio = window.devicePixelRatio;
  const documentSize = { width: page.scrollWidth, height: page.scrollHeight };
  const selectionRect = {
    x: Math.round(x + view.scrollX),
    y: Math.round(y + view.scrollY),
    width: Math.max(1, Math.round(width)),
    height: Math.max(1, Math.round(height)),
  };
  const unmarkScrollBoxes = markScrollBoxes(frame.contentDocument);
  const unmarkFontSizes = markFontSizes(frame.contentDocument);
  try {
    const viewport = await renderPage(page, {
      pixelRatio,
      width: frame.clientWidth,
      height: frame.clientHeight,
      style: { width: pageStyle.width, height: pageStyle.height },
      scroll: { x: view.scrollX, y: view.scrollY },
    });
    const selection = document.createElement("canvas");
    selection.width = Math.round(width * pixelRatio);
    selection.height = Math.round(height * pixelRatio);
    selection.getContext("2d").drawImage(viewport, x * pixelRatio, y * pixelRatio, selection.width, selection.height, 0, 0, selection.width, selection.height);
    const overviewScale = Math.min(1, OVERVIEW_MAX_EDGE / Math.max(documentSize.width, documentSize.height));
    const overview = await renderPage(page, {
      pixelRatio: overviewScale,
      width: documentSize.width,
      height: documentSize.height,
      style: {
        width: `${documentSize.width}px`,
        height: `${documentSize.height}px`,
      },
    });
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
  } finally {
    unmarkScrollBoxes();
    unmarkFontSizes();
  }
}

