const XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
const SVG_NAMESPACE = "http://www.w3.org/2000/svg";
const ROOT_STYLE_PLACEHOLDER = "pointy-capture-root-style";
const STYLE_CLASS_PREFIX = "pointy-";
const STYLE_READ_BATCH = 512;
const PAGE_STYLE_LAYER = "pointy-capture-page";
const SCROLLING_OVERFLOWS = ["auto", "scroll"];
const CHECKABLE_INPUT_TYPES = ["checkbox", "radio"];
const EMBEDDED_URL_PROPERTIES = ["background-image", "mask-image", "-webkit-mask-image"];
const CSS_URL = /url\("((?:[^"\\]|\\.)*)"\)/g;
const CHILD_DEPENDENCY_PROBES = [
  "color-scheme: dark",
  "font: italic 700 37px / 3 serif",
  "font-family: monospace",
  "zoom: 2",
  "writing-mode: vertical-rl",
  "direction: rtl",
  "display: block",
  "display: flex",
  "display: grid",
  "display: list-item",
  "display: table",
  "display: table-cell",
  "display: inline-block",
  "display: block; width: 123px; height: 45px",
  "position: absolute",
  "position: relative",
  "position: sticky",
  "position: fixed",
  "float: left",
  "overflow: scroll",
  "border-style: solid",
  "outline-style: solid",
  "column-count: 2; column-rule-style: solid",
];
const PARENT_DEPENDENCY_PROBES = [
  "display: grid; justify-items: legacy center; color: rgb(1, 2, 3); color-scheme: dark; font: 37px / 3 monospace; writing-mode: vertical-rl; direction: rtl; visibility: hidden",
  "display: flex",
];
const SHIFTED_TEXT_ATTRIBUTE = "data-pointy-capture-shifted-text";

// `all` resets every property except direction and unicode-bidi, so each XHTML element of the copy starts from initial values and its declarations list only what differs. Text wrapped to follow a scroll offset keeps inheriting instead. The page's own style sheets move to a layer below, so they keep styling the SVG content, which is copied without declarations, but cannot override the reset. https://drafts.csswg.org/css-cascade-5/#all-shorthand https://drafts.csswg.org/css-cascade-5/#layering
const RESET_STYLE_SHEET = `@namespace x url(${XHTML_NAMESPACE});x|*:not(x|style, foreignObject svg *, [${SHIFTED_TEXT_ATTRIBUTE}]), x|*::before, x|*::after{all:initial}`;
const NOT_RESET_BY_ALL = ["direction", "unicode-bidi"];

// Logical properties only mirror their physical counterparts, the copy has no interaction, scrolling or transitions, and the computed values of animated properties already hold the frame on screen, which a copied animation would replay from its start. https://drafts.csswg.org/css-logical/#logical-property-group
const LOGICAL_PROPERTY = /(^|-)(block|inline)(-|$)|^border-(start|end)-(start|end)-radius$/;
const UNRENDERED_PROPERTY =
  /^(cursor|pointer-events|user-select|touch-action|-webkit-tap-highlight-color|interactivity|scroll-behavior|scroll-initial-target|timeline-scope)$|^(caret|transition|animation|scroll-margin|scroll-padding|scroll-snap|scroll-timeline|view-timeline|view-transition|overscroll-behavior)-/;
const PROBE_COLOR = "rgb(1, 2, 3)";
const GENERIC_FONT_FAMILIES = new Set(["serif", "sans-serif", "monospace", "cursive", "fantasy", "system-ui", "ui-serif", "ui-sans-serif", "ui-monospace", "ui-rounded", "emoji", "math", "fangsong", "-apple-system", "blinkmacsystemfont"]);

// Chromium shades inset, outset, groove and ridge borders differently when their color is currentcolor than when it is the same color given explicitly, so border colors are always copied.
const BORDER_COLOR = /^border-(top|right|bottom|left)-color$/;
const supportedValues = new Map();
const embeddedResources = new Map();
let copiedProperties = null;

function sandboxProbe(sandboxDocument, names, { parent = "", child = "", lang = "" }) {
  const parentElement = sandboxDocument.createElement("div");
  parentElement.style.cssText = `all: initial; ${parent}`;
  const childElement = sandboxDocument.createElement("span");
  childElement.style.cssText = child;
  const probedProperties = new Set(childElement.style);
  childElement.style.cssText = `all: initial; ${child}`;
  if (lang) childElement.lang = lang;
  parentElement.append(childElement);
  sandboxDocument.body.append(parentElement);
  const style = sandboxDocument.defaultView.getComputedStyle(childElement);
  const values = names.map((name) => (probedProperties.has(name) ? null : style.getPropertyValue(name)));
  parentElement.remove();
  const drivers = names.flatMap((name, index) => (probedProperties.has(name) ? [index] : []));
  return { values, drivers: drivers.length > 0 ? drivers : null };
}

// An initial value can depend on other properties, like currentcolor on color or a border width on the border style. A probe sets one such property, or the parent's style, and each property whose value moves is copied unless the element's probed properties are still at their initial values.
function measureCopiedProperties() {
  const names = [...getComputedStyle(document.documentElement)].filter(
    (name) => !name.startsWith("--") && !LOGICAL_PROPERTY.test(name) && !UNRENDERED_PROPERTY.test(name),
  );
  const sandbox = document.createElement("iframe");
  sandbox.style.cssText = "position: fixed; left: -10000px; top: 0; width: 400px; height: 400px; border: 0";
  document.body.append(sandbox);
  try {
    const sandboxDocument = sandbox.contentDocument;
    sandboxDocument.open();
    sandboxDocument.write("<!doctype html><html><body></body></html>");
    sandboxDocument.close();
    const { values: initial } = sandboxProbe(sandboxDocument, names, {});
    const colorProbe = sandboxProbe(sandboxDocument, names, { child: `color: ${PROBE_COLOR}` });
    const probes = [
      colorProbe,
      ...CHILD_DEPENDENCY_PROBES.map((child) => sandboxProbe(sandboxDocument, names, { child })),
      ...PARENT_DEPENDENCY_PROBES.map((parent) => sandboxProbe(sandboxDocument, names, { parent })),
      sandboxProbe(sandboxDocument, names, { lang: "ja" }),
    ];
    const drivers = names.map((name, index) => {
      if (NOT_RESET_BY_ALL.includes(name) || BORDER_COLOR.test(name)) return null;
      const moving = probes.filter(({ values }) => values[index] !== null && values[index] !== initial[index]);
      return moving.some((probe) => probe.drivers === null) ? null : [...new Set(moving.flatMap((probe) => probe.drivers))];
    });
    return {
      names,
      initial,
      drivers,
      followsColor: colorProbe.values.map((value, index) => value === PROBE_COLOR && !BORDER_COLOR.test(names[index])),
      colorIndex: names.indexOf("color"),
    };
  } finally {
    sandbox.remove();
  }
}

function settableValue(name, value) {
  const key = `${name}:${value}`;
  let supported = supportedValues.get(key);
  if (supported === undefined) {
    supported = CSS.supports(name, value);
    supportedValues.set(key, supported);
  }
  return supported ? value : "revert";
}

function isImpliedByReset(index, values) {
  const { initial, drivers, followsColor, colorIndex } = copiedProperties;
  if (followsColor[index] && values[index] === values[colorIndex]) return true;
  if (values[index] !== initial[index] || drivers[index] === null) return false;
  return drivers[index].every((driver) => values[driver] === initial[driver]);
}

// Values that only the user agent may set, like text-align: -internal-center on th, fall back to the user agent's style. A table's height is distributed over its rows, which already carry their own heights, so declaring the table's height again would redistribute them.
function declarations(values, resetsToInitial, isTable) {
  const { names } = copiedProperties;
  let text = "";
  for (let index = 0; index < names.length; index++) {
    if (isTable && names[index] === "height") continue;
    if (!resetsToInitial) text += `${names[index]}:${values[index]};`;
    else if (!isImpliedByReset(index, values)) text += `${names[index]}:${settableValue(names[index], values[index])};`;
  }
  return text;
}

function computedValues(computed) {
  return copiedProperties.names.map((name) => computed.getPropertyValue(name));
}

function noteFontFamilies(value, context) {
  if (context.seenFontFamilies.has(value)) return;
  context.seenFontFamilies.add(value);
  for (const family of value.split(",")) {
    const name = family.trim().replace(/^['"]|['"]$/g, "");
    if (name && !GENERIC_FONT_FAMILIES.has(name.toLowerCase())) context.fontFamilies.add(name.toLowerCase());
  }
}

// A style sheet's font files cannot load into an SVG drawn as an image, so the faces the page uses carry their data inline. https://www.w3.org/TR/SVG2/conform.html#secure-static-mode
async function embedFontFaces(document, context) {
  if (context.fontFamilies.size === 0) return "";
  const faces = [];
  for (const sheet of document.styleSheets) {
    let rules;
    try {
      rules = sheet.cssRules;
    } catch {
      continue;
    }
    for (const rule of rules) {
      if (rule.type !== CSSRule.FONT_FACE_RULE) continue;
      const family = rule.style.getPropertyValue("font-family").trim().replace(/^['"]|['"]$/g, "").toLowerCase();
      if (!context.fontFamilies.has(family)) continue;
      const sources = [...rule.style.getPropertyValue("src").matchAll(CSS_URL)].map((match) => match[1]).filter((source) => !isDataUrl(source));
      faces.push(
        (async () => {
          for (const source of sources) {
            let url;
            try {
              url = new URL(source, document.baseURI).href;
            } catch {
              continue;
            }
            const dataUrl = await embeddedResource(url);
            if (dataUrl) return rule.cssText.replace(/src\s*:[^;}]*;?/i, `src: url("${dataUrl}");`);
          }
          return "";
        })(),
      );
    }
  }
  return (await Promise.all(faces)).join("");
}

// Reading one property across a run of elements is faster than reading every property of one element in turn.
function styleElements(styledElements, context) {
  const { names } = copiedProperties;
  const fontFamily = names.indexOf("font-family");
  for (let start = 0; start < styledElements.length; start += STYLE_READ_BATCH) {
    const batch = styledElements.slice(start, start + STYLE_READ_BATCH);
    const values = batch.map(() => new Array(names.length));
    names.forEach((name, index) => batch.forEach(({ computed }, row) => (values[row][index] = computed.getPropertyValue(name))));
    if (fontFamily >= 0) values.forEach((row) => noteFontFamilies(row[fontFamily], context));
    batch.forEach(({ clone, computed, resetsToInitial, extraDeclarations }, row) =>
      clone.classList.add(styleClass(context, "", declarations(values[row], resetsToInitial, computed.display === "table") + extraDeclarations)),
    );
  }
}

async function fetchDataUrl(url) {
  try {
    const response = await fetch(url);
    if (!response.ok) return null;
    const blob = await response.blob();
    return await new Promise((resolve, reject) => {
      const reader = new FileReader();
      reader.onload = () => resolve(reader.result);
      reader.onerror = () => reject(reader.error);
      reader.readAsDataURL(blob);
    });
  } catch {
    return null;
  }
}

function embeddedResource(url) {
  if (!embeddedResources.has(url)) {
    embeddedResources.set(
      url,
      fetchDataUrl(url).then((dataUrl) => {
        if (!dataUrl) embeddedResources.delete(url);
        return dataUrl;
      }),
    );
  }
  return embeddedResources.get(url);
}

function isDataUrl(url) {
  return url.startsWith("data:");
}

// An SVG drawn as an image loads nothing, so every resource it shows is inlined. https://www.w3.org/TR/SVG2/conform.html#secure-static-mode
function embedAttribute(context, element, attribute, url) {
  if (!url || isDataUrl(url)) return;
  context.embeddings.push(
    embeddedResource(url).then((dataUrl) => {
      if (dataUrl) element.setAttribute(attribute, dataUrl);
    }),
  );
}

function embedStyleUrls(context, clone, computed) {
  for (const name of EMBEDDED_URL_PROPERTIES) {
    const value = computed.getPropertyValue(name);
    const urls = [...value.matchAll(CSS_URL)].map((match) => match[1]).filter((url) => !isDataUrl(url));
    if (urls.length === 0) continue;
    context.embeddings.push(
      Promise.all(urls.map(embeddedResource)).then((dataUrls) => {
        const embedded = value.replace(CSS_URL, (match, url) => {
          const dataUrl = dataUrls[urls.indexOf(url)];
          return dataUrl ? `url("${dataUrl}")` : match;
        });
        clone.style.setProperty(name, embedded);
      }),
    );
  }
}

function layerPageStyle(style) {
  style.textContent = `@layer ${PAGE_STYLE_LAYER}{${style.textContent}}`;
  return style;
}

function svgClone(svg, context) {
  const clone = svg.cloneNode(true);
  clone.querySelectorAll("style").forEach(layerPageStyle);
  clone.querySelectorAll("image").forEach((image) => {
    const href = image.getAttribute("href") ?? image.getAttributeNS("http://www.w3.org/1999/xlink", "href");
    if (href) embedAttribute(context, image, "href", new URL(href, image.baseURI).href);
  });
  return clone;
}

function imageOf(element, src) {
  const image = element.ownerDocument.createElementNS(XHTML_NAMESPACE, "img");
  if (src) image.setAttribute("src", src);
  return image;
}

function canvasDataUrl(canvas) {
  try {
    const dataUrl = canvas.toDataURL();
    return dataUrl === "data:," ? null : dataUrl;
  } catch {
    return null;
  }
}

function videoClone(video, context) {
  if (video.currentSrc) {
    const canvas = document.createElement("canvas");
    canvas.width = video.clientWidth;
    canvas.height = video.clientHeight;
    canvas.getContext("2d").drawImage(video, 0, 0, canvas.width, canvas.height);
    return imageOf(video, canvasDataUrl(canvas));
  }
  const image = imageOf(video, null);
  embedAttribute(context, image, "src", video.poster);
  return image;
}

function shallowClone(element, context) {
  if (element.namespaceURI === SVG_NAMESPACE && element.localName === "svg") return svgClone(element, context);
  if (element.namespaceURI !== XHTML_NAMESPACE) return element.cloneNode(false);
  switch (element.localName) {
    case "canvas": {
      const dataUrl = canvasDataUrl(element);
      return dataUrl ? imageOf(element, dataUrl) : element.cloneNode(false);
    }
    case "video":
      return videoClone(element, context);
    case "img": {
      const clone = element.cloneNode(false);
      clone.removeAttribute("srcset");
      clone.removeAttribute("loading");
      embedAttribute(context, clone, "src", element.currentSrc || element.src);
      return clone;
    }
    case "style":
      return layerPageStyle(element.cloneNode(true));
    default:
      return element.cloneNode(false);
  }
}

function hasChildrenToCopy(element) {
  if (element.namespaceURI === SVG_NAMESPACE) return element.localName !== "svg";
  return !(element.namespaceURI === XHTML_NAMESPACE && ["canvas", "video", "style"].includes(element.localName));
}

function sourceChildren(element) {
  if (element.localName === "slot" && element.assignedNodes) return element.assignedNodes({ flatten: true });
  return (element.shadowRoot ?? element).childNodes;
}

// Hidden content draws nothing, but its style sheets still apply and its SVG symbols can be used elsewhere.
function hiddenResources(element, context) {
  return [...element.querySelectorAll("style, svg")]
    .filter((resource) => !resource.parentElement.closest("svg"))
    .map((resource) => (resource.localName === "svg" ? svgClone(resource, context) : layerPageStyle(resource.cloneNode(true))));
}

function scrollBoxState(element, computed) {
  if (!SCROLLING_OVERFLOWS.includes(computed.overflowX) && !SCROLLING_OVERFLOWS.includes(computed.overflowY)) return null;
  const scrollbarWidth = element.offsetWidth - element.clientWidth - parseFloat(computed.borderLeftWidth) - parseFloat(computed.borderRightWidth);
  const scrollbarHeight = element.offsetHeight - element.clientHeight - parseFloat(computed.borderTopWidth) - parseFloat(computed.borderBottomWidth);
  const hasScrollbars = scrollbarWidth >= 1 || scrollbarHeight >= 1;
  if (!hasScrollbars && element.scrollLeft === 0 && element.scrollTop === 0) return null;
  return {
    scrollX: element.scrollLeft,
    scrollY: element.scrollTop,
    size: hasScrollbars ? { width: element.offsetWidth, height: element.offsetHeight } : null,
  };
}

function pinBoxSize(style, { width, height }) {
  style.boxSizing = "border-box";
  style.width = `${width}px`;
  style.height = `${height}px`;
  style.minWidth = "0px";
  style.minHeight = "0px";
  style.maxWidth = "none";
  style.maxHeight = "none";
}

function offsetBox(style, offset, scrollX, scrollY) {
  style.left = `${offset.left - scrollX}px`;
  style.top = `${offset.top - scrollY}px`;
}

function positionOffset(computed) {
  return { left: parseFloat(computed.left) || 0, top: parseFloat(computed.top) || 0 };
}

// A copy has no scroll position, so a scrolled box's content is shifted by its scroll offset instead.
function shiftScrolledContent(box, boxComputed, scrollX, scrollY, context) {
  const isContainingBlock = boxComputed.position !== "static";
  for (const child of [...box.childNodes]) {
    const childComputed = context.computedStyles.get(child);
    if (child.nodeType === Node.TEXT_NODE && child.textContent.trim() !== "") {
      const span = box.ownerDocument.createElementNS(XHTML_NAMESPACE, "span");
      child.replaceWith(span);
      span.append(child);
      span.setAttribute(SHIFTED_TEXT_ATTRIBUTE, "");
      span.style.position = "relative";
      offsetBox(span.style, { left: 0, top: 0 }, scrollX, scrollY);
    } else if (childComputed) {
      const position = childComputed.position;
      if (position === "static") {
        child.style.position = "relative";
        offsetBox(child.style, { left: 0, top: 0 }, scrollX, scrollY);
      } else if (position === "relative" || position === "sticky" || (position === "absolute" && isContainingBlock)) {
        offsetBox(child.style, positionOffset(childComputed), scrollX, scrollY);
      }
    }
  }
}

// Elements with the same declarations share a class, which keeps the markup small and lets the renderer reuse the matched style.
function styleClass(context, pseudo, declarationText) {
  const key = `${pseudo}${declarationText}`;
  let className = context.styleClasses.get(key);
  if (!className) {
    className = `${STYLE_CLASS_PREFIX}${context.styleClasses.size.toString(36)}`;
    context.styleClasses.set(key, className);
    context.styleRules.push(`.${className}${pseudo}{${declarationText}}`);
  }
  return className;
}

function copyPseudoElements(element, clone, context) {
  const view = element.ownerDocument.defaultView;
  const resetsToInitial = clone.namespaceURI === XHTML_NAMESPACE;
  for (const pseudo of ["::before", "::after"]) {
    const computed = view.getComputedStyle(element, pseudo);
    const content = computed.getPropertyValue("content");
    if (content === "" || content === "none") continue;
    clone.classList.add(styleClass(context, pseudo, declarations(computedValues(computed), resetsToInitial)));
  }
}

// The page's inline styles are replaced by the computed ones, except custom properties, which SVG content copied without inline styles may still read.
function keepCustomProperties(element, clone) {
  if (!clone.hasAttribute("style")) return;
  const customProperties = [...element.style].filter((name) => name.startsWith("--"));
  if (customProperties.length === 0) clone.removeAttribute("style");
  else clone.setAttribute("style", customProperties.map((name) => `${name}:${element.style.getPropertyValue(name)};`).join(""));
}

function copyFormState(element, clone) {
  if (element.namespaceURI !== XHTML_NAMESPACE) return;
  if (element.localName === "input") {
    clone.setAttribute("value", element.value);
    if (CHECKABLE_INPUT_TYPES.includes(element.type)) clone.toggleAttribute("checked", element.checked);
  } else if (element.localName === "textarea") {
    clone.textContent = element.value;
  } else if (element.localName === "select") {
    clone.querySelectorAll("option")[element.selectedIndex]?.setAttribute("selected", "");
  }
}

function frameBody(frame) {
  try {
    return frame.contentDocument?.body ?? null;
  } catch {
    return null;
  }
}

function cloneNode(node, context) {
  if (node.nodeType === Node.TEXT_NODE) return node.cloneNode(false);
  if (node.nodeType === Node.ELEMENT_NODE) return cloneElement(node, context);
  return null;
}

function cloneElement(element, context, frameComputed = null) {
  const view = element.ownerDocument.defaultView;
  const computed = view.getComputedStyle(element);
  const styled = frameComputed ?? computed;
  const isFrame = element.namespaceURI === XHTML_NAMESPACE && element.localName === "iframe";
  const body = isFrame ? frameBody(element) : null;
  if (body) return cloneElement(body, context, computed);
  const clone = shallowClone(element, context);
  const isHidden = styled.display === "none";
  if (hasChildrenToCopy(element)) {
    const children = isHidden ? hiddenResources(element, context) : Array.from(sourceChildren(element), (child) => cloneNode(child, context));
    for (const child of children) if (child) clone.append(child);
  }
  if (!clone.style) return clone;
  if (isHidden) {
    clone.classList.add(styleClass(context, "", "display:none"));
    return clone;
  }
  keepCustomProperties(element, clone);
  context.styledElements.push({
    clone,
    computed: styled,
    resetsToInitial: clone.namespaceURI === XHTML_NAMESPACE,
    extraDeclarations: (isFrame || frameComputed) && styled.display === "inline" ? "display:block;" : "",
  });
  context.computedStyles.set(clone, styled);
  embedStyleUrls(context, clone, styled);
  copyPseudoElements(element, clone, context);
  copyFormState(element, clone);
  const ownerDocument = element.ownerDocument;
  if (element !== ownerDocument.documentElement && element !== ownerDocument.scrollingElement) {
    const scrollBox = scrollBoxState(element, computed);
    if (scrollBox?.size) pinBoxSize(clone.style, scrollBox.size);
    if (scrollBox && (scrollBox.scrollX !== 0 || scrollBox.scrollY !== 0)) shiftScrolledContent(clone, styled, scrollBox.scrollX, scrollBox.scrollY, context);
  }
  return clone;
}

function escapeAttribute(text) {
  return text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
}

// Clones the page as XHTML that renders like the live page inside an SVG foreignObject: computed styles in a style sheet, resources embedded and scroll offsets applied. The root's position and size are left to `markup`, so one clone can be rendered at different sizes and scroll positions.
export async function clonePage(document) {
  copiedProperties ??= measureCopiedProperties();
  const context = { styleClasses: new Map(), styleRules: [], styledElements: [], embeddings: [], computedStyles: new WeakMap(), fontFamilies: new Set(), seenFontFamilies: new Set() };
  const root = document.documentElement;
  const rootOffset = positionOffset(document.defaultView.getComputedStyle(root));
  const rootClone = cloneElement(root, context);
  styleElements(context.styledElements, context);
  const [fontFaces] = await Promise.all([embedFontFaces(document, context), Promise.all(context.embeddings)]);
  const rootInlineStyle = rootClone.getAttribute("style") ?? "";
  rootClone.setAttribute("style", ROOT_STYLE_PLACEHOLDER);
  const serialized = new XMLSerializer().serializeToString(rootClone);
  const placeholderStart = serialized.indexOf(ROOT_STYLE_PLACEHOLDER);
  const beforeRootStyle = serialized.slice(0, placeholderStart);
  const afterRootStyle = serialized.slice(placeholderStart + ROOT_STYLE_PLACEHOLDER.length);
  return {
    styleSheet: RESET_STYLE_SHEET + fontFaces + context.styleRules.join(""),
    // A transformed root would become the containing block of the page's fixed elements and carry them off with the scroll, while an offset root leaves them on the viewport and lets sticky elements stick. https://drafts.csswg.org/css-transforms-1/#containing-block-for-all-descendants
    markup: ({ width, height, backgroundColor, scroll }) =>
      beforeRootStyle +
      escapeAttribute(
        `${rootInlineStyle}background-color:${backgroundColor};width:${width};height:${height};position:relative;left:${rootOffset.left - scroll.x}px;top:${rootOffset.top - scroll.y}px;`,
      ) +
      afterRootStyle,
  };
}
