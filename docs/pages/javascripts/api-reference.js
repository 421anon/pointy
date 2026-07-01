(() => {
  const API_PATH_SUFFIX = "/api/";

  function normalizedPathname(url) {
    return url.pathname.endsWith("/") ? url.pathname : `${url.pathname}/`;
  }

  function isApiReferenceUrl(rawHref) {
    if (!rawHref) {
      return false;
    }

    try {
      const url = new URL(rawHref, document.baseURI);
      return (
        url.origin === window.location.origin &&
        normalizedPathname(url).endsWith(API_PATH_SUFFIX) &&
        !url.hash
      );
    } catch (_err) {
      return false;
    }
  }

  function markApiReferenceLinks(root = document) {
    const links = root.matches && root.matches("a[href]")
      ? [root]
      : Array.from(root.querySelectorAll?.("a[href]") || []);

    for (const link of links) {
      if (isApiReferenceUrl(link.getAttribute("href"))) {
        link.setAttribute("data-no-instant", "");
      }
    }
  }

  function getMkDocsScheme() {
    return (
      document.body?.getAttribute("data-md-color-scheme") ||
      document.documentElement?.getAttribute("data-md-color-scheme") ||
      ""
    );
  }

  function syncSourceyTheme() {
    if (!document.getElementById("sourcey")) {
      return;
    }

    document.documentElement.classList.toggle("dark", getMkDocsScheme() === "slate");
  }

  function apply(root = document) {
    markApiReferenceLinks(root);
    syncSourceyTheme();
  }

  function observeChanges() {
    if (!document.body) {
      return;
    }

    const observer = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        if (mutation.type === "attributes") {
          syncSourceyTheme();
          continue;
        }

        for (const node of mutation.addedNodes) {
          if (node.nodeType === Node.ELEMENT_NODE) {
            apply(node);
          }
        }
      }
    });

    observer.observe(document.body, {
      attributes: true,
      attributeFilter: ["data-md-color-scheme"],
      childList: true,
      subtree: true,
    });

    observer.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["data-md-color-scheme"],
    });
  }

  function init() {
    apply(document);
    observeChanges();

    if (typeof document$ !== "undefined" && document$.subscribe) {
      document$.subscribe(() => {
        apply(document);
      });
    }
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", init, { once: true });
  } else {
    init();
  }
})();
