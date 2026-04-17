(() => {
  const SCREENSHOT_PATH_SEGMENT = "/screenshots/";

  function getPaletteScheme() {
    const schemeAttr =
      document.body?.getAttribute("data-md-color-scheme") ||
      document.documentElement?.getAttribute("data-md-color-scheme");

    if (schemeAttr) {
      return schemeAttr;
    }

    try {
      const palette = JSON.parse(localStorage.getItem("__palette") || "null");
      if (palette && palette.color && typeof palette.color.scheme === "string") {
        return palette.color.scheme;
      }
    } catch (_err) {
      // Ignore palette parsing issues and fall back below.
    }

    return window.matchMedia("(prefers-color-scheme: dark)").matches
      ? "slate"
      : "default";
  }

  function getScreenshotTheme() {
    return getPaletteScheme() === "slate" ? "dark" : "light";
  }

  function normalizeScreenshotBaseSrc(rawSrc) {
    if (!rawSrc) {
      return null;
    }

    const url = new URL(rawSrc, document.baseURI);
    if (!url.pathname.includes(SCREENSHOT_PATH_SEGMENT)) {
      return null;
    }

    url.pathname = url.pathname.replace(
      /\/screenshots\/(?:light|dark)\//,
      "/screenshots/",
    );

    return url.toString();
  }

  function buildThemedScreenshotSrc(baseSrc, theme) {
    const url = new URL(baseSrc, document.baseURI);
    url.pathname = url.pathname.replace(
      SCREENSHOT_PATH_SEGMENT,
      `${SCREENSHOT_PATH_SEGMENT}${theme}/`,
    );
    return url.toString();
  }

  function screenshotImagesWithin(root) {
    if (!root) {
      return [];
    }

    if (root.tagName === "IMG") {
      return [root];
    }

    return Array.from(root.querySelectorAll("img"));
  }

  function applyThemeToScreenshot(img, theme) {
    const fallbackSrc =
      img.dataset.themeScreenshotFallbackSrc || img.getAttribute("src") || "";
    const baseSrc =
      img.dataset.themeScreenshotBase ||
      normalizeScreenshotBaseSrc(img.getAttribute("src"));

    if (!baseSrc) {
      return;
    }

    img.dataset.themeScreenshot = "true";
    img.dataset.themeScreenshotBase = baseSrc;
    img.dataset.themeScreenshotFallbackSrc = fallbackSrc;

    const themedSrc = buildThemedScreenshotSrc(baseSrc, theme);
    const currentSrc = img.currentSrc || img.getAttribute("src") || "";

    if (currentSrc === themedSrc && img.dataset.themeScreenshotTheme === theme) {
      img.dataset.themeScreenshotReady = "true";
      return;
    }

    img.dataset.themeScreenshotTheme = theme;
    img.dataset.themeScreenshotReady = "false";

    const cleanup = () => {
      img.removeEventListener("load", onLoad);
      img.removeEventListener("error", onError);
    };

    const onLoad = () => {
      img.dataset.themeScreenshotReady = "true";
      cleanup();
    };

    const onError = () => {
      cleanup();

      if (img.getAttribute("src") !== fallbackSrc) {
        img.dataset.themeScreenshotReady = "false";
        img.addEventListener(
          "load",
          () => {
            img.dataset.themeScreenshotReady = "true";
          },
          { once: true },
        );
        img.setAttribute("src", fallbackSrc);
      } else {
        img.dataset.themeScreenshotReady = "true";
      }
    };

    img.addEventListener("load", onLoad);
    img.addEventListener("error", onError);
    img.setAttribute("src", themedSrc);
  }

  function syncThemeScreenshots(root = document) {
    const theme = getScreenshotTheme();

    for (const img of screenshotImagesWithin(root)) {
      applyThemeToScreenshot(img, theme);
    }
  }

  function observeThemeChanges() {
    const observer = new MutationObserver(() => {
      syncThemeScreenshots(document);
    });

    if (document.body) {
      observer.observe(document.body, {
        attributes: true,
        attributeFilter: ["data-md-color-scheme"],
      });
    }

    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["data-md-color-scheme"],
    });
  }

  function observeContentChanges() {
    if (!document.body) {
      return;
    }

    const observer = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        for (const node of mutation.addedNodes) {
          if (node.nodeType === Node.ELEMENT_NODE) {
            syncThemeScreenshots(node);
          }
        }
      }
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true,
    });
  }

  function init() {
    syncThemeScreenshots(document);
    observeThemeChanges();
    observeContentChanges();

    if (typeof document$ !== "undefined" && document$.subscribe) {
      document$.subscribe(() => {
        syncThemeScreenshots(document);
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init, { once: true });
  } else {
    init();
  }
})();
