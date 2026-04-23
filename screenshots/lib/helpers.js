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

function escapeRegExp(value) {
  return value.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}

async function getVisibleStepRows(page) {
  const stepRows = page.locator('.table[id^="table-"]:not(#table-projects) .table-record[id]');
  const count = await stepRows.count();
  const visibleRows = [];

  for (let i = 0; i < count; i++) {
    const row = stepRows.nth(i);
    if (await row.isVisible().catch(() => false)) {
      visibleRows.push(row);
    }
  }

  return visibleRows;
}

async function getStepRowDisplayName(stepRow) {
  return stepRow.locator(".table-record-name").first().evaluate((el) => {
    const clone = el.cloneNode(true);
    clone
      .querySelectorAll(".table-record-id, .pending-record-indicator, .shareable-icon")
      .forEach((node) => node.remove());

    return (clone.textContent || "").replace(/\s+/g, " ").trim();
  });
}

async function getDirectoryFileName(fileRow) {
  return fileRow.locator(".file-name").first().evaluate((el) => {
    return (el.textContent || "").replace(/\s+/g, " ").trim();
  });
}

function matchesPreferredName(name, preferredNames) {
  return preferredNames.some((preferredName) => {
    if (preferredName instanceof RegExp) {
      return preferredName.test(name);
    }

    return preferredName === name;
  });
}

/**
 * Find a visible previewable file row in a directory section.
 */
async function findPreviewableFileInSection(
  sectionLocator,
  { preferredNames = [], preferNonHtml = false } = {},
) {
  const fileRows = sectionLocator.locator(".directory-file-container");
  const count = await fileRows.count();
  const candidates = [];

  for (let i = 0; i < count; i++) {
    const fileRow = fileRows.nth(i);
    if (!(await fileRow.isVisible().catch(() => false))) continue;

    const previewButton = await firstVisibleLocator(
      fileRow.locator("button.dir-item-icon-btn").filter({
        has: sectionLocator.page().locator(".material-symbols-outlined", {
          hasText: /^visibility(_off)?$/ ,
        }),
      }),
    );

    if (!previewButton) continue;

    let fileName = "";
    try {
      fileName = await getDirectoryFileName(fileRow);
    } catch (_err) {
      continue;
    }

    if (!fileName) continue;

    candidates.push({ fileRow, fileName, previewButton });
  }

  const preferredCandidate = candidates.find(({ fileName }) =>
    matchesPreferredName(fileName, preferredNames),
  );
  if (preferredCandidate) return preferredCandidate;

  if (preferNonHtml) {
    const nonHtmlCandidate = candidates.find(
      ({ fileName }) => !/\.html$/i.test(fileName),
    );
    if (nonHtmlCandidate) return nonHtmlCandidate;
  }

  return candidates[0] || null;
}

/**
 * Find the first visible step row exposing a specific row action button.
 */
async function findVisibleStepRowWithButton(page, buttonTitle) {
  const visibleRows = await getVisibleStepRows(page);

  for (const stepRow of visibleRows) {
    const button = await firstVisibleLocator(
      stepRow.locator(`button.icon-btn[title="${buttonTitle}"]`),
    );

    if (button) {
      return { stepRow, button };
    }
  }

  return { stepRow: null, button: null };
}

/**
 * Find a visible step row with Clone plus its visible cloned counterpart.
 */
function getCloneBaseName(stepName) {
  const cloneSuffixIndex = stepName.indexOf(" (Clone");
  return cloneSuffixIndex === -1
    ? stepName
    : stepName.slice(0, cloneSuffixIndex);
}

async function findVisibleClonePair(page) {
  const visibleRows = await getVisibleStepRows(page);
  const stepInfos = [];

  for (const stepRow of visibleRows) {
    const cloneButton = await firstVisibleLocator(
      stepRow.locator('button.icon-btn[title="Clone"]'),
    );

    if (!cloneButton) continue;

    let stepName = "";
    try {
      stepName = await getStepRowDisplayName(stepRow);
    } catch (_err) {
      continue;
    }

    if (!stepName) continue;

    stepInfos.push({
      stepRow,
      cloneButton,
      stepName,
      cloneBaseName: getCloneBaseName(stepName),
    });
  }

  for (let i = 0; i < stepInfos.length; i++) {
    const source = stepInfos[i];
    const cloneNamePattern = new RegExp(
      `^${escapeRegExp(source.cloneBaseName)} \\(Clone(?: \\d+)?\\)$`,
    );

    for (const candidate of visibleRows.slice(i + 1)) {
      let candidateName = "";
      try {
        candidateName = await getStepRowDisplayName(candidate);
      } catch (_err) {
        continue;
      }

      if (cloneNamePattern.test(candidateName) && candidateName !== source.stepName) {
        return {
          sourceStepRow: source.stepRow,
          cloneStepRow: candidate,
          cloneButton: source.cloneButton,
          sourceStepName: source.stepName,
          cloneStepName: candidateName,
        };
      }
    }
  }

  return {
    sourceStepRow: null,
    cloneStepRow: null,
    cloneButton: null,
    sourceStepName: null,
    cloneStepName: null,
  };
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
 * Wait for the projects table to finish loading and render a visible row.
 */
async function waitForProjectRows(page) {
  const projectsTable = page.locator("#table-projects").first();
  const loadingPlaceholder = projectsTable.locator(".table-records-loading").first();
  const firstProjectRow = projectsTable.locator(".table-record").first();

  await projectsTable.waitFor({ state: "visible", timeout: 30000 });

  try {
    await loadingPlaceholder.waitFor({ state: "hidden", timeout: 30000 });
    await firstProjectRow.waitFor({ state: "visible", timeout: 30000 });
  } catch (err) {
    let tableText = "<unavailable>";

    try {
      tableText = (await projectsTable.innerText()).replace(/\s+/g, " ").trim();
      if (!tableText) tableText = "<empty>";
    } catch (_err) {
      // Ignore diagnostic failures while building the error message.
    }

    const reason = err instanceof Error ? err.message : String(err);
    throw new Error(
      `Projects table never rendered a visible project row. Current table text: ${tableText}. Cause: ${reason}`
    );
  }

  return firstProjectRow;
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
  findPreviewableFileInSection,
  findVisibleStepRowWithButton,
  findVisibleClonePair,
  expandAllVisibleFolders,
  waitForNoLoading,
  waitForDirectoryContents,
  withHoveredLocator,
  showTitleAsTooltip,
  waitForStepStatusEvent,
  waitForMaterialIcons,
  waitForApp,
  waitForProjectRows,
  waitForBackend,
  createContextWithStepTracking,
};
