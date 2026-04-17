"use strict";

const fs = require("fs");
const path = require("path");

/**
 * Parse command-line arguments for screenshot generation.
 */
function parseArgs(argv) {
  const args = {};
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--output" && argv[i + 1]) args.output = argv[++i];
    else if (argv[i] === "--url" && argv[i + 1]) args.url = argv[++i];
    else if (argv[i] === "--theme" && argv[i + 1]) args.theme = argv[++i];
  }
  return args;
}

function normalizeTheme(theme) {
  return theme === "light" || theme === "dark" ? theme : null;
}

/**
 * Take a screenshot of the entire page viewport.
 */
async function screenshot(page, outputDir, name) {
  const filePath = path.join(outputDir, name);
  await page.screenshot({ path: filePath, fullPage: false });
  console.log(`Saved ${filePath}`);
}

/**
 * Take a screenshot of a specific locator element.
 */
async function screenshotLocator(outputDir, name, locator) {
  const filePath = path.join(outputDir, name);
  await locator.screenshot({ path: filePath });
  console.log(`Saved ${filePath}`);
}

/**
 * Click an element if it exists.
 */
async function clickIfExists(page, selector) {
  const element = await page.$(selector);
  if (!element) return false;
  await element.click();
  await waitForMaterialIcons(page);
  return true;
}

/**
 * Find the first visible element matching a locator.
 */
async function firstVisibleLocator(locator) {
  const count = await locator.count();
  for (let i = 0; i < count; i++) {
    const candidate = locator.nth(i);
    if (await candidate.isVisible()) {
      return candidate;
    }
  }
  return null;
}

/**
 * Click the first visible element matching a locator.
 */
async function clickFirstVisible(locator) {
  const candidate = await firstVisibleLocator(locator);
  if (!candidate) return false;
  await candidate.click();
  await waitForMaterialIcons(locator.page());
  return true;
}

/**
 * Expand all collapsed folders within a directory section.
 */
async function expandAllVisibleFolders(sectionLocator) {
  let expandedAny = false;

  while (true) {
    const collapsedHeaders = sectionLocator
      .locator(".directory-folder .folder-header")
      .filter({
        has: sectionLocator.locator(
          '.folder-expand-icon .material-symbols-outlined:text-is("expand_more")',
        ),
      });

    const header = await firstVisibleLocator(collapsedHeaders);
    if (!header) break;

    let label = "<unknown folder>";
    try {
      label = (await header.innerText()).replace(/\s+/g, " ").trim();
    } catch (_err) {
      // ignore logging failure
    }

    console.log(`[debug] Expanding folder: ${label}`);
    await header.click();
    await waitForNoLoading(sectionLocator);
    await waitForMaterialIcons(sectionLocator.page());
    expandedAny = true;
  }

  return expandedAny;
}

/**
 * Wait for a locator to be visible and have no loading overlay.
 */
async function waitForNoLoading(locator) {
  await locator.waitFor({ state: "visible", timeout: 30000 });
  await locator
    .page()
    .waitForFunction(
      (el) => !!el && !el.querySelector(".loading-overlay"),
      await locator.elementHandle(),
      { timeout: 30000 },
    );
}

/**
 * Wait for directory contents to finish loading.
 */
async function waitForDirectoryContents(locator) {
  await waitForNoLoading(locator);
  await locator.page().waitForFunction(
    (el) => {
      if (!el) return false;
      if (el.querySelector(".loading-overlay")) return false;

      const directoryRoot = el.querySelector(".directory-view") || el;

      return Boolean(
        directoryRoot.querySelector(".directory-file-container") ||
          directoryRoot.querySelector(".directory-folder") ||
          Array.from(directoryRoot.querySelectorAll("div, span")).some((node) => {
          const text = (node.textContent || "").trim();
          return (
            text === "Directory is empty" ||
            text === "Failed to load" ||
            text === "Loading directory contents..."
          );
        }),
      );
    },
    await locator.elementHandle(),
    { timeout: 30000 },
  );
}

/**
 * Show a native title attribute as a visible DOM tooltip for screenshots.
 * Returns a cleanup function to remove the tooltip.
 */
async function showTitleAsTooltip(page, locator) {
  const visibleLocator = await firstVisibleLocator(locator);
  if (!visibleLocator) return () => {};

  const tooltipId = await visibleLocator.evaluate((el) => {
    const title = el.getAttribute("title");
    if (!title) return null;

    const rect = el.getBoundingClientRect();
    const tooltip = document.createElement("div");
    tooltip.id = "screenshot-tooltip-" + Date.now();
    tooltip.textContent = title;
    tooltip.style.cssText = `
      position: fixed;
      left: ${rect.left}px;
      top: ${rect.bottom + 4}px;
      background: #1a1a1a;
      color: #fff;
      padding: 4px 8px;
      border-radius: 4px;
      font-size: 12px;
      max-width: 400px;
      z-index: 99999;
      pointer-events: none;
    `;
    document.body.appendChild(tooltip);
    return tooltip.id;
  });

  return async () => {
    if (tooltipId) {
      await page.evaluate((id) => {
        const el = document.getElementById(id);
        if (el) el.remove();
      }, tooltipId);
    }
  };
}

/**
 * Execute an action with a locator in a hovered state.
 */
async function withHoveredLocator(page, locator, action, label = "hover target") {
  const visibleLocator = await firstVisibleLocator(locator);
  if (!visibleLocator) {
    console.warn(`No visible ${label} found.`);
    return false;
  }

  await visibleLocator.waitFor({ state: "visible", timeout: 30000 });
  await visibleLocator.hover();

  // Wait for Material Icons to load after hover state changes
  await waitForMaterialIcons(page);

  try {
    await action(visibleLocator);
    return true;
  } finally {
    const viewport = page.viewportSize() || { width: 1280, height: 800 };
    await page.mouse.move(viewport.width - 1, viewport.height - 1);
  }
}

/**
 * Wait for a step status event to be tracked by the injected script.
 */
async function waitForStepStatusEvent(page) {
  await page.waitForFunction(
    () => (window.__pointyStepStatusEventCount || 0) > 0,
    { timeout: 30000 },
  );
}

/**
 * Wait for Material Icons to be fully loaded and rendered.
 * Call this after DOM changes that might add new icons.
 */
async function waitForMaterialIcons(page) {
  await page.evaluate(() => document.fonts.ready);
  await page.waitForFunction(
    async () => {
      if (!document.fonts || !document.fonts.check) return false;

      try {
        await document.fonts.load('18px "Material Symbols Outlined"');
      } catch (_err) {
        return false;
      }

      return document.fonts.check('18px "Material Symbols Outlined"');
    },
    { timeout: 10000 },
  );
  // Small buffer to ensure icons are painted
  await page.waitForTimeout(200);
}

/**
 * Wait for the app to fully load, including fonts and SSE connections.
 */
async function waitForApp(page) {
  // Use 'load' instead of 'networkidle': the app keeps an open SSE connection
  // to /backend/ which would prevent networkidle from ever being reached.
  await page.waitForLoadState("load");
  // Wait for all fonts (including Material Symbols from Google Fonts CDN).
  await page.evaluate(() => document.fonts.ready);
  // Google Fonts stylesheets can finish loading after the page load event, so wait
  // explicitly for the icon font to become available before taking screenshots.
  await page.waitForFunction(
    async () => {
      if (!document.fonts || !document.fonts.check) return false;

      try {
        await document.fonts.load('18px "Material Symbols Outlined"');
      } catch (_err) {
        return false;
      }

      return document.fonts.check('18px "Material Symbols Outlined"');
    },
    { timeout: 30000 },
  );
  // Buffer for Elm re-renders triggered by API responses arriving after load.
  await page.waitForTimeout(2000);
}

/**
 * Poll /backend/projects until the backend is up and serving data.
 */
async function waitForBackend(page, baseUrl) {
  const readyMarker = process.env.SCREENSHOT_BACKEND_READY_FILE;

  if (readyMarker && fs.existsSync(readyMarker)) {
    console.log("Backend readiness marker found; skipping startup wait.");
    return;
  }

  const maxAttempts = 40;
  const retryDelay = 5000;

  // Navigate first so fetch runs from the same origin (avoids null-origin CORS issues).
  await page.goto(`${baseUrl}/`, { waitUntil: "load" });

  for (let i = 1; i <= maxAttempts; i++) {
    const result = await page.evaluate(async (url) => {
      try {
        const r = await fetch(url);
        const body = await r.text();
        return { ok: r.ok, status: r.status, body: body.slice(0, 300) };
      } catch (e) {
        return { ok: false, error: String(e) };
      }
    }, `${baseUrl}/backend/projects`);

    if (result.ok) {
      console.log(`Backend ready after ${i} attempt(s).`);

      if (readyMarker) {
        try {
          fs.writeFileSync(readyMarker, `${Date.now()}\n`);
        } catch (err) {
          console.warn(`Failed to write backend readiness marker ${readyMarker}: ${String(err)}`);
        }
      }

      return;
    }

    console.log(
      `Backend not ready yet (attempt ${i}/${maxAttempts}): ${JSON.stringify(result)}, retrying in ${retryDelay / 1000}s…`,
    );
    await page.waitForTimeout(retryDelay);
  }

  throw new Error(
    "Backend never became ready — /backend/projects never returned 200.",
  );
}

/**
 * Create a browser context with the step status event tracking script injected.
 */
async function createContextWithStepTracking(browser) {
  const forcedTheme =
    normalizeTheme(process.env.SCREENSHOT_THEME) ||
    normalizeTheme(parseArgs(process.argv.slice(2)).theme);

  const context = await browser.newContext({
    deviceScaleFactor: 2,
    viewport: { width: 1280, height: 800 },
    ...(forcedTheme ? { colorScheme: forcedTheme } : {}),
  });

  await context.addInitScript((theme) => {
    if (theme === "light" || theme === "dark") {
      try {
        localStorage.setItem("theme", theme);
      } catch (_err) {
        // Ignore storage failures in screenshot automation.
      }

      if (document.documentElement) {
        document.documentElement.setAttribute("data-theme", theme);
      }
    }

    window.__pointyStepStatusEventCount = 0;

    const NativeEventSource = window.EventSource;
    if (!NativeEventSource) return;

    window.EventSource = class extends NativeEventSource {
      constructor(url, config) {
        super(url, config);

        const track = (type) => {
          if (
            typeof url === "string" &&
            url.includes("/backend/step-status-stream")
          ) {
            window.__pointyStepStatusEventCount =
              (window.__pointyStepStatusEventCount || 0) + 1;
            window.__pointyLastStepStatusEventType = type;
          }
        };

        this.addEventListener("snapshot", () => track("snapshot"));
        this.addEventListener("heartbeat", () => track("heartbeat"));
        this.addEventListener("error", () => track("error"));
      }
    };
  }, forcedTheme);

  return context;
}

module.exports = {
  parseArgs,
  screenshot,
  screenshotLocator,
  clickIfExists,
  firstVisibleLocator,
  clickFirstVisible,
  expandAllVisibleFolders,
  waitForNoLoading,
  waitForDirectoryContents,
  withHoveredLocator,
  showTitleAsTooltip,
  waitForStepStatusEvent,
  waitForMaterialIcons,
  waitForApp,
  waitForBackend,
  createContextWithStepTracking,
};
