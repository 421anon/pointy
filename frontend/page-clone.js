const XHTML_NAMESPACE = "http://www.w3.org/1999/xhtml";
const SVG_NAMESPACE = "http://www.w3.org/2000/svg";
const ROOT_STYLE_PLACEHOLDER = "pointy-capture-root-style";
const STYLE_CLASS_PREFIX = "pointy-";
const SLICE_ELEMENTS = 128;
const PAGE_STYLE_LAYER = "pointy-capture-page";
const SCROLLING_OVERFLOWS = ["auto", "scroll"];
const CHECKABLE_INPUT_TYPES = ["checkbox", "radio"];
const EMBEDDED_URL_PROPERTIES = ["background-image", "mask-image", "-webkit-mask-image"];
const CSS_URL = /url\("((?:[^"\\]|\\.)*)"\)/g;
const BEFORE_OR_AFTER = /::?(?:before|after)\b/i;
const PSEUDO_ELEMENT_GENERATING_PROPERTIES = ["content", "animation-name"];
// Quotation marks in all browsers, and Firefox's alt text for images. https://searchfox.org/mozilla-central/source/layout/style/res/html.css
const USER_AGENT_PSEUDO_ELEMENT_HOSTS = ["q", "img", "input[type=image i]"];
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

// The values of these attributes style nothing unless a selector or attr() reads them, though their presence can, as a link's href does.
const VALUE_FREE_ATTRIBUTE = /^(data-|aria-)|^(href|src|srcset|alt|title|target|rel|download|hreflang|ping|referrerpolicy)$/;
const ATTRIBUTE_SELECTOR_NAME = /\[\s*(?:[^\s|\]]*\|)?([^\s~|^$*=\]]+)/g;
// Elements with the same parent style, tag, attributes and matches of the page's selectors that look beyond those have the same computed style, so its values are read once per style. getComputedStyle still answers these with each element's laid-out size and position. https://drafts.csswg.org/cssom/#resolved-values
const LAYOUT_DEPENDENT_PROPERTIES = new Set([
  "width",
  "height",
  "margin-top",
  "margin-right",
  "margin-bottom",
  "margin-left",
  "padding-top",
  "padding-right",
  "padding-bottom",
  "padding-left",
  "top",
  "right",
  "bottom",
  "left",
  "transform",
  "transform-origin",
  "perspective-origin",
  "grid-template-columns",
  "grid-template-rows",
]);
// Within one style, these follow each element's layout only under the style's own conditions: insets of positioned boxes and the tracks of grid containers resolve to used values, and a transform to a matrix unless it is none. The origins take effect only with a transform or perspective, so without one the style's value draws the same on every element. https://drafts.csswg.org/cssom/#resolved-values
const isPositioned = (value) => value("position") !== "static";
const isGridContainer = (value) => value("display")?.includes("grid") ?? true;
const LAYOUT_CONDITIONS = {
  top: isPositioned,
  right: isPositioned,
  bottom: isPositioned,
  left: isPositioned,
  "grid-template-columns": isGridContainer,
  "grid-template-rows": isGridContainer,
  transform: (value) => value("transform") !== "none",
  "transform-origin": (value) => ["transform", "rotate", "scale", "translate", "offset-path"].some((name) => value(name) !== "none"),
  "perspective-origin": (value) => value("perspective") !== "none",
};
// The user agent's style sheets style these by state, content or position among siblings, which the page's selectors do not show.
const UNSHARED_ELEMENTS =
  "input, select, textarea, button, option, optgroup, datalist, progress, meter, output, fieldset, legend, details, summary, dialog, img, picture, video, audio, canvas, iframe, frame, object, embed, bdi, [dir=auto i], [popover]";
const PSEUDO_ELEMENT_SELECTOR = /::|:(before|after|first-line|first-letter)\b/i;
const SELECTOR_ESCAPES_ATTRIBUTES_AND_STRINGS = /\\.|\[[^\]]*\]|"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/g;
const NON_LOCAL_SELECTOR = /[:+~]/;
const CONTAINER_DEPENDENT_STYLE = /container|\dcq(?:[whib]|min|max)\b/i;
const SELECTOR_COMBINATOR = /[\s>+~]/;
const PARENTHESIZED = /\([^()]*\)/;
const SELECTOR_TAG = /^[a-zA-Z][\w-]*/;
const SELECTOR_ID_OR_CLASS = /([.#])(-?[_a-zA-Z\u0080-\uffff][\w\u0080-\uffff-]*)/g;
// Matching each selector costs about one test per element, which can outweigh the reads that sharing saves.
const MAX_SELECTOR_TESTS = 30_000_000;
const MAX_SELECTOR_MATCHES = 2_000_000;

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
      layoutIndices: names.flatMap((name, index) => (LAYOUT_DEPENDENT_PROPERTIES.has(name) ? [index] : [])),
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

function propertyValues(entries, indices) {
  const { names } = copiedProperties;
  const values = entries.map(() => new Array(names.length));
  for (const index of indices) {
    const name = names[index];
    entries.forEach(({ computed }, row) => (values[row][index] = computed.getPropertyValue(name)));
  }
  return values;
}

// The indices of the properties each element of a style reads for itself, the same array for styles that read the same ones.
function ownLayoutIndices(values, indicesBySignature) {
  const { names, layoutIndices } = copiedProperties;
  const value = (name) => {
    const index = names.indexOf(name);
    return index >= 0 ? values[index] : undefined;
  };
  const indices = layoutIndices.filter((index) => LAYOUT_CONDITIONS[names[index]]?.(value) ?? true);
  const signature = indices.join(",");
  if (!indicesBySignature.has(signature)) indicesBySignature.set(signature, indices);
  return indicesBySignature.get(signature);
}

// Reading one property across a run of elements is faster than reading every property of one element in turn. An element sharing its style with an earlier one reads only the properties that follow its layout.
async function styleElements(styledElements, context, yieldBetweenSlices) {
  const { names, layoutIndices } = copiedProperties;
  const allIndices = names.map((_, index) => index);
  const fontFamily = names.indexOf("font-family");
  const display = names.indexOf("display");
  const valuesByStyle = new Map();
  const ownIndicesByStyle = new Map();
  const indicesBySignature = new Map();
  const classesByStyleAndLayout = new Map();
  for (let start = 0; start < styledElements.length; start += SLICE_ELEMENTS) {
    const batch = styledElements.slice(start, start + SLICE_ELEMENTS);
    const stylesInBatch = new Set();
    const read = [];
    const shared = [];
    for (const entry of batch) {
      const { styleKey } = entry;
      if (styleKey !== null && (valuesByStyle.has(styleKey) || stylesInBatch.has(styleKey))) {
        shared.push(entry);
      } else {
        read.push(entry);
        if (styleKey !== null) stylesInBatch.add(styleKey);
      }
    }
    const valuesByEntry = new Map();
    propertyValues(read, allIndices).forEach((values, row) => {
      valuesByEntry.set(read[row], values);
      if (read[row].styleKey !== null) {
        valuesByStyle.set(read[row].styleKey, values);
        ownIndicesByStyle.set(read[row].styleKey, ownLayoutIndices(values, indicesBySignature));
      }
      if (fontFamily >= 0) noteFontFamilies(values[fontFamily], context);
    });
    const sharedByIndices = new Map();
    for (const entry of shared) {
      const indices = ownIndicesByStyle.get(entry.styleKey);
      if (!sharedByIndices.has(indices)) sharedByIndices.set(indices, []);
      sharedByIndices.get(indices).push(entry);
    }
    for (const [indices, entries] of sharedByIndices) {
      propertyValues(entries, indices).forEach((ownValues, row) => {
        const values = [...valuesByStyle.get(entries[row].styleKey)];
        for (const index of indices) values[index] = ownValues[index];
        valuesByEntry.set(entries[row], values);
      });
    }
    for (const entry of batch) {
      const values = valuesByEntry.get(entry);
      const styleAndLayout = entry.styleKey === null ? null : `${entry.styleKey}\u0000${layoutIndices.map((index) => values[index]).join("\u0000")}`;
      let className = styleAndLayout === null ? undefined : classesByStyleAndLayout.get(styleAndLayout);
      if (className === undefined) {
        className = styleClass(context, "", declarations(values, entry.resetsToInitial, (display >= 0 ? values[display] : entry.computed.display) === "table") + entry.extraDeclarations);
        if (styleAndLayout !== null) classesByStyleAndLayout.set(styleAndLayout, className);
      }
      entry.clone.classList.add(className);
    }
    await yieldBetweenSlices();
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

function unrenderedResources(element, context) {
  if (element.localName === "svg") return [svgClone(element, context)];
  if (element.localName === "style") return [layerPageStyle(element.cloneNode(true))];
  return hiddenResources(element, context);
}

// A closed details element renders only its summary, unless the page makes the rest visible. Returns the children it leaves unrendered, or an empty list.
function unrenderedDetailsChildren(element) {
  if (element.namespaceURI !== XHTML_NAMESPACE || element.localName !== "details" || element.open) return [];
  const summary = [...element.children].find((child) => child.namespaceURI === XHTML_NAMESPACE && child.localName === "summary");
  const content = [...element.children].filter((child) => child !== summary);
  return content.length > 0 && !content[0].checkVisibility() ? content : [];
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

function splitSelectorList(selectorText) {
  const selectors = [];
  let depth = 0;
  let quote = null;
  let start = 0;
  for (let index = 0; index < selectorText.length; index++) {
    const character = selectorText[index];
    if (quote) {
      if (character === "\\") index += 1;
      else if (character === quote) quote = null;
    } else if (character === '"' || character === "'") quote = character;
    else if (character === "(" || character === "[") depth += 1;
    else if (character === ")" || character === "]") depth -= 1;
    else if (character === "," && depth === 0) {
      selectors.push(selectorText.slice(start, index));
      start = index + 1;
    }
  }
  selectors.push(selectorText.slice(start));
  return selectors;
}

function originatingElementSelector(selector) {
  const match = BEFORE_OR_AFTER.exec(selector);
  if (!match) return null;
  const compound = selector.slice(0, match.index).trim();
  if (compound === "") return "*";
  return /[>+~]$/.test(compound) ? `${compound} *` : compound;
}

// A ::before or ::after box exists only where a rule gives it content, possibly through an animation, so only elements matching such a rule are asked for their pseudo-element styles, which browsers otherwise resolve one element at a time. Returns null when the rules cannot all be read or matched, and then every element is asked.
function pseudoElementHosts(document) {
  const view = document.defaultView;
  const selectors = [...USER_AGENT_PSEUDO_ELEMENT_HOSTS];
  const collect = (rules, nested) => {
    for (const rule of rules) {
      if (rule instanceof view.CSSImportRule) {
        if (!rule.styleSheet || !collect(rule.styleSheet.cssRules, nested)) return false;
      } else if (view.CSSScopeRule && rule instanceof view.CSSScopeRule) {
        if ([...rule.cssRules].some((inner) => BEFORE_OR_AFTER.test(inner.cssText))) return false;
      } else if (rule instanceof view.CSSStyleRule) {
        const generates = PSEUDO_ELEMENT_GENERATING_PROPERTIES.some((name) => rule.style.getPropertyValue(name) !== "");
        if (generates && BEFORE_OR_AFTER.test(rule.selectorText)) {
          if (nested) return false;
          selectors.push(...splitSelectorList(rule.selectorText).map(originatingElementSelector).filter((selector) => selector !== null));
        }
        if (rule.cssRules && !collect(rule.cssRules, true)) return false;
      } else if (rule.cssRules && !collect(rule.cssRules, nested)) {
        return false;
      }
    }
    return true;
  };
  try {
    for (const sheet of [...document.styleSheets, ...(document.adoptedStyleSheets ?? [])]) {
      if (!collect(sheet.cssRules, false)) return null;
    }
    return new Set(document.querySelectorAll(selectors.join(",")));
  } catch {
    return null;
  }
}

function mayHavePseudoElements(element, context) {
  const root = element.getRootNode();
  if (root.nodeType !== Node.DOCUMENT_NODE) return true;
  if (!context.pseudoElementHosts.has(root)) context.pseudoElementHosts.set(root, pseudoElementHosts(root));
  const hosts = context.pseudoElementHosts.get(root);
  return hosts === null || hosts.has(element);
}

function matchesSelector(element, selector) {
  try {
    return element.matches(selector);
  } catch {
    return false;
  }
}

function selectorCompounds(selector) {
  const compounds = [];
  let depth = 0;
  let quote = null;
  let start = 0;
  for (let index = 0; index < selector.length; index++) {
    const character = selector[index];
    if (quote) {
      if (character === "\\") index += 1;
      else if (character === quote) quote = null;
    } else if (character === "\\") index += 1;
    else if (character === '"' || character === "'") quote = character;
    else if (character === "(" || character === "[") depth += 1;
    else if (character === ")" || character === "]") depth -= 1;
    else if (depth === 0 && SELECTOR_COMBINATOR.test(character)) {
      compounds.push(selector.slice(start, index));
      start = index + 1;
    }
  }
  compounds.push(selector.slice(start));
  return compounds.filter((compound) => compound !== "");
}

// The tag, ids and classes an element needs to match the compound, outside any functional pseudo-class. Returns null when escapes or namespaces make them uncertain.
function requiredNames(compound) {
  if (compound.includes("\\") || compound.includes("|")) return null;
  let bare = compound.replace(SELECTOR_ESCAPES_ATTRIBUTES_AND_STRINGS, "");
  while (PARENTHESIZED.test(bare)) bare = bare.replace(PARENTHESIZED, "");
  const tag = SELECTOR_TAG.exec(bare)?.[0].toLowerCase() ?? null;
  const names = [...bare.matchAll(SELECTOR_ID_OR_CLASS)].map(([, kind, name]) => ({ kind, name }));
  return { tag, names };
}

function namesInDocument(document) {
  const tags = new Set();
  const ids = new Set();
  const classes = new Set();
  for (const element of document.getElementsByTagName("*")) {
    tags.add(element.localName);
    if (element.id) ids.add(element.id);
    for (const name of element.classList) classes.add(name);
  }
  return { tags, ids, classes };
}

// A selector matches nothing when one of its compounds needs a tag, id or class that no element has.
function mayMatchIn(selector, present) {
  return selectorCompounds(selector.trim()).every((compound) => {
    const required = requiredNames(compound);
    if (required === null) return true;
    if (required.tag !== null && !present.tags.has(required.tag)) return false;
    return required.names.every(({ kind, name }) => (kind === "#" ? present.ids : present.classes).has(name));
  });
}

// A selector that names only tags, ids, classes and attributes of an element and its ancestors matches alike wherever those are alike; any other selector is matched against the elements it may apply to. Returns null when a style may depend on what the style key leaves out: quirks, shadow trees, nested or scoped rules, or container sizes.
function styleSharing(document) {
  const view = document.defaultView;
  if (document.compatMode === "BackCompat" || document.querySelector('[style*="container" i], [style*="cq" i]')) return null;
  const nonLocalSelectors = [];
  const selectedAttributes = new Set();
  let readsAttributeValues = document.querySelector('[style*="attr(" i]') !== null;
  const collect = (rules) => {
    for (const rule of rules) {
      if (rule instanceof view.CSSImportRule) {
        if (!rule.styleSheet || !collect(rule.styleSheet.cssRules)) return false;
      } else if (rule instanceof view.CSSStyleRule) {
        if (rule.cssRules?.length > 0 || CONTAINER_DEPENDENT_STYLE.test(rule.style.cssText)) return false;
        let stylesElements = false;
        for (const selector of splitSelectorList(rule.selectorText)) {
          const bare = selector.replace(SELECTOR_ESCAPES_ATTRIBUTES_AND_STRINGS, "");
          const selectsPseudoElement = PSEUDO_ELEMENT_SELECTOR.test(bare);
          stylesElements ||= !selectsPseudoElement;
          if (!selectsPseudoElement && NON_LOCAL_SELECTOR.test(bare)) nonLocalSelectors.push(selector.trim());
        }
        for (const [, name] of rule.selectorText.matchAll(ATTRIBUTE_SELECTOR_NAME)) {
          if (name.includes("\\")) readsAttributeValues = true;
          selectedAttributes.add(name.toLowerCase());
        }
        if (stylesElements && rule.style.cssText.includes("attr(")) readsAttributeValues = true;
      } else if ((view.CSSContainerRule && rule instanceof view.CSSContainerRule) || (view.CSSScopeRule && rule instanceof view.CSSScopeRule)) {
        return false;
      } else if (rule.cssRules && !collect(rule.cssRules)) {
        return false;
      }
    }
    return true;
  };
  try {
    for (const sheet of [...document.styleSheets, ...document.adoptedStyleSheets]) {
      if (!collect(sheet.cssRules)) return null;
    }
  } catch {
    return null;
  }
  const elements = document.getElementsByTagName("*");
  for (const element of elements) if (element.shadowRoot) return null;
  const present = namesInDocument(document);
  const selectors = nonLocalSelectors.filter((selector) => mayMatchIn(selector, present));
  if (selectors.length * elements.length > MAX_SELECTOR_TESTS) return null;
  const unshared = new Set(document.querySelectorAll(UNSHARED_ELEMENTS));
  if (document.activeElement) unshared.add(document.activeElement);
  for (const animation of document.getAnimations()) if (animation.effect?.target) unshared.add(animation.effect.target);
  const selectorMatches = new Map();
  if (selectors.length > 0) {
    const affected = document.querySelectorAll(`:is(${selectors.join(",")})`);
    if (affected.length * selectors.length > MAX_SELECTOR_MATCHES) {
      for (const element of affected) unshared.add(element);
    } else {
      for (const element of affected) selectorMatches.set(element, selectors.map((selector) => (matchesSelector(element, selector) ? "1" : "0")).join(""));
    }
  }
  const ignoresValue = (name) => !readsAttributeValues && VALUE_FREE_ATTRIBUTE.test(name) && !selectedAttributes.has(name);
  return { document, unshared, selectorMatches, ignoresValue, keys: new Map(), ids: new Map(), uniqueKeys: 0 };
}

// Returns the element's style key, or null when its style is read on its own. Children of such an element can still share a style with each other.
function assignStyleKey(element, context, shareable) {
  const sharing = context.styleSharing;
  if (!sharing) return null;
  const parentKey = element === sharing.document.documentElement ? "root" : sharing.keys.get(element.parentElement);
  let key;
  if (!shareable || parentKey === undefined || element.ownerDocument !== sharing.document || element.namespaceURI !== XHTML_NAMESPACE || element.localName.includes("-") || sharing.unshared.has(element)) {
    sharing.uniqueKeys -= 1;
    key = sharing.uniqueKeys;
  } else {
    let text = `${parentKey}\u0000${element.localName}\u0000${sharing.selectorMatches.get(element) ?? ""}`;
    for (const { name, value } of element.attributes) text += `\u0000${name.length}:${name}` + (sharing.ignoresValue(name) ? "" : `${value.length}:${value}`);
    key = sharing.ids.get(text);
    if (key === undefined) {
      key = sharing.ids.size;
      sharing.ids.set(text, key);
    }
  }
  sharing.keys.set(element, key);
  return key >= 0 ? key : null;
}

function copyPseudoElements(element, clone, context) {
  if (!mayHavePseudoElements(element, context)) return;
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

// Elements are prepared on the way down and their children are appended from an explicit stack, so a large page can be cloned over several turns of the event loop.
function prepareElement(element, context, frameComputed = null) {
  const view = element.ownerDocument.defaultView;
  const computed = view.getComputedStyle(element);
  const styled = frameComputed ?? computed;
  const isFrame = element.namespaceURI === XHTML_NAMESPACE && element.localName === "iframe";
  const body = isFrame ? frameBody(element) : null;
  if (body) return prepareElement(body, context, computed);
  const clone = shallowClone(element, context);
  const isHidden = styled.display === "none";
  const children = [];
  if (hasChildrenToCopy(element)) {
    if (isHidden) for (const resource of hiddenResources(element, context)) clone.append(resource);
    else {
      const unrendered = new Set(unrenderedDetailsChildren(element));
      for (const child of unrendered) clone.append(...unrenderedResources(child, context));
      children.push(...[...sourceChildren(element)].filter((child) => !unrendered.has(child)));
    }
  }
  if (!clone.style) return { clone, children };
  if (isHidden) {
    clone.classList.add(styleClass(context, "", "display:none"));
    return { clone, children: [] };
  }
  keepCustomProperties(element, clone);
  context.styledElements.push({
    clone,
    computed: styled,
    resetsToInitial: clone.namespaceURI === XHTML_NAMESPACE,
    extraDeclarations: (isFrame || frameComputed) && styled.display === "inline" ? "display:block;" : "",
    styleKey: assignStyleKey(element, context, !isFrame && !frameComputed),
  });
  context.computedStyles.set(clone, styled);
  embedStyleUrls(context, clone, styled);
  copyPseudoElements(element, clone, context);
  context.formStates.push({ element, clone });
  const ownerDocument = element.ownerDocument;
  if (element !== ownerDocument.documentElement && element !== ownerDocument.scrollingElement) {
    const scrollBox = scrollBoxState(element, computed);
    if (scrollBox?.size) pinBoxSize(clone.style, scrollBox.size);
    if (scrollBox && (scrollBox.scrollX !== 0 || scrollBox.scrollY !== 0)) context.scrollBoxes.push({ clone, computed: styled, scrollX: scrollBox.scrollX, scrollY: scrollBox.scrollY });
  }
  return { clone, children };
}

async function cloneElementTree(root, context, yieldBetweenSlices) {
  const prepared = prepareElement(root, context);
  const stack = prepared.children.length > 0 ? [{ clone: prepared.clone, children: prepared.children, next: 0 }] : [];
  let slice = 0;
  while (stack.length > 0) {
    const frame = stack[stack.length - 1];
    if (frame.next >= frame.children.length) {
      stack.pop();
      continue;
    }
    const node = frame.children[frame.next];
    frame.next += 1;
    if (node.nodeType === Node.TEXT_NODE) {
      frame.clone.append(node.cloneNode(false));
      continue;
    }
    if (node.nodeType !== Node.ELEMENT_NODE) continue;
    const entry = prepareElement(node, context);
    frame.clone.append(entry.clone);
    if (entry.children.length > 0) stack.push({ clone: entry.clone, children: entry.children, next: 0 });
    slice += 1;
    if (slice >= SLICE_ELEMENTS) {
      slice = 0;
      await yieldBetweenSlices();
    }
  }
  for (const box of context.scrollBoxes) shiftScrolledContent(box.clone, box.computed, box.scrollX, box.scrollY, context);
  for (const state of context.formStates) copyFormState(state.element, state.clone);
  return prepared.clone;
}

function escapeAttribute(text) {
  return text.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
}

// Clones the page as XHTML that renders like the live page inside an SVG foreignObject: computed styles in a style sheet, resources embedded and scroll offsets applied. The root's position and size are left to `markup`, so one clone can be rendered at different sizes and scroll positions. `yieldBetweenSlices` is awaited between slices of the work so that a large page stays responsive.
export async function clonePage(document, { yieldBetweenSlices }) {
  if (!copiedProperties) {
    copiedProperties = measureCopiedProperties();
    await yieldBetweenSlices();
  }
  const context = { styleClasses: new Map(), styleRules: [], styledElements: [], embeddings: [], computedStyles: new WeakMap(), fontFamilies: new Set(), seenFontFamilies: new Set(), scrollBoxes: [], formStates: [], pseudoElementHosts: new Map(), styleSharing: styleSharing(document) };
  const root = document.documentElement;
  const rootOffset = positionOffset(document.defaultView.getComputedStyle(root));
  const rootClone = await cloneElementTree(root, context, yieldBetweenSlices);
  await styleElements(context.styledElements, context, yieldBetweenSlices);
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
