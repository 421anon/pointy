#!/usr/bin/env node
"use strict";

const fs = require("fs");
const path = require("path");
const { chromium } = require("playwright-core");

// List of all screenshot generator scripts
const screenshotScripts = [
  // Projects
  "projects-home.js",
  "projects-header-controls.js",
  "project-row-actions.js",
  "project-delete-button.js",
  "project-create-form.js",
  "project-template-settings-form.js",
  "project-view.js",
  "project-config-warning.js",

  // Steps overview
  "steps-search-box.js",
  "steps-visibility-controls.js",
  "steps-visibility-button.js",
  "steps-reorder-handle.js",
  "steps-add-from-other-project-form.js",
  "steps-assign-unassign.js",
  "step-last-modified-badge.js",

  // Step types
  "step-file-upload-header.js",
  "step-derivation-header.js",
  "step-derivation-stop-header.js",

  // Step actions
  "step-inspect-form.js",
  "step-clone-button.js",
  "step-edit-form.js",

  // Files
  "step-source-files-section.js",
  "output-file-share-button.js",
  "output-files-browser.js",
  "output-files-html-preview.js",
  "output-file-line-range-share.js",
];

function resolveScriptsDir() {
  const candidates = [
    process.env.SCREENSHOTS_SCRIPTS_DIR,
    path.join(process.cwd(), "screenshots"),
    __dirname,
  ].filter(Boolean);

  for (const candidate of candidates) {
    if (fs.existsSync(path.join(candidate, screenshotScripts[0]))) {
      return candidate;
    }
  }

  throw new Error(
    `Could not locate screenshot scripts directory. Tried: ${candidates.join(", ")}`,
  );
}

function parseRunnerArgs(args) {
  let output = path.join(__dirname, "../docs/pages/screenshots");
  let url = "http://localhost";
  let themes = [];

  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--output" && args[i + 1]) {
      output = args[++i];
    } else if (args[i] === "--url" && args[i + 1]) {
      url = args[++i];
    } else if (args[i] === "--theme" && args[i + 1]) {
      themes = args[++i]
        .split(",")
        .map((theme) => theme.trim().toLowerCase())
        .filter(Boolean);
    }
  }

  if (themes.length === 0 && process.env.SCREENSHOT_THEMES) {
    themes = process.env.SCREENSHOT_THEMES
      .split(/[\s,]+/)
      .map((theme) => theme.trim().toLowerCase())
      .filter(Boolean);
  }

  themes = [...new Set(themes)].filter(
    (theme) => theme === "light" || theme === "dark",
  );

  if (themes.length === 0) {
    themes = ["light", "dark"];
  }

  return { output, url, themes };
}


function timestamp() {
  return new Date().toISOString();
}

function formatDuration(ms) {
  if (ms < 1000) return `${ms}ms`;
  const seconds = ms / 1000;
  if (seconds < 60) return `${seconds.toFixed(1)}s`;
  const minutes = Math.floor(seconds / 60);
  return `${minutes}m ${Math.round(seconds % 60)}s`;
}

function log(message) {
  console.log(`[${timestamp()}] ${message}`);
}

function logError(message) {
  console.error(`[${timestamp()}] ${message}`);
}

function logErrorLines(label, output) {
  for (const line of output.split(/\r?\n/).filter(Boolean)) {
    logError(`${label} ${line}`);
  }
}

async function runCapture(scriptPath, session, label) {
  const startedAt = Date.now();
  log(`start ${label}`);

  const captureSession = {
    ...session,
    warn: (message) => logError(`${label} warn ${message}`),
  };

  try {
    const { capture } = require(scriptPath);
    if (typeof capture !== "function") {
      throw new Error(`${path.basename(scriptPath)} does not export capture(session)`);
    }

    await capture(captureSession);
    session.location = captureSession.location;
    log(`ok ${label} ${formatDuration(Date.now() - startedAt)}`);
  } catch (err) {
    logError(`fail ${label} ${formatDuration(Date.now() - startedAt)}`);
    logErrorLines(label, err.stack || err.message);
    throw err;
  }
}

async function getCurrentTheme(page) {
  return page.evaluate(() => document.documentElement.getAttribute("data-theme"));
}

async function switchTheme(page, targetTheme) {
  const currentTheme = await getCurrentTheme(page);
  if (currentTheme === targetTheme) return;

  const switcher = page.locator(".theme-toggle-btn").first();
  await switcher.waitFor({ state: "visible", timeout: 10000 });
  await switcher.click();
  await page.waitForFunction(
    (theme) => document.documentElement.getAttribute("data-theme") === theme,
    targetTheme,
    { timeout: 10000 },
  );
  await page.waitForTimeout(150);
}

async function main() {
  const runStartedAt = Date.now();
  const args = process.argv.slice(2);
  const { output, url: baseUrl, themes } = parseRunnerArgs(args);
  const backendReadyMarker = path.join(output, ".backend-ready");

  const scriptsDir = resolveScriptsDir();
  const {
    launchBrowser,
    closeBrowser,
    createContextWithStepTracking,
    waitForBackend,
  } = require(path.join(scriptsDir, "lib/helpers"));
  log(`run start scripts=${screenshotScripts.length} themes=${themes.join(",")} output=${output}`);

  fs.rmSync(backendReadyMarker, { force: true });
  process.env.SCREENSHOT_BACKEND_READY_FILE = backendReadyMarker;

  let completedCount = 0;
  const totalRuns = screenshotScripts.length * themes.length;
  const browser = await launchBrowser(chromium);
  log("browser shared-started");

  try {
    const hasLightAndDark = themes.includes("light") && themes.includes("dark");

    if (hasLightAndDark) {
      const lightOutput = path.join(output, "light");
      const darkOutput = path.join(output, "dark");
      fs.rmSync(lightOutput, { recursive: true, force: true });
      fs.rmSync(darkOutput, { recursive: true, force: true });
      fs.mkdirSync(lightOutput, { recursive: true });
      fs.mkdirSync(darkOutput, { recursive: true });

      process.env.SCREENSHOT_THEME = "light";
      const context = await createContextWithStepTracking(browser);
      const page = await context.newPage();
      const session = {
        browser,
        context,
        page,
        output: lightOutput,
        baseUrl,
        location: "unknown",
      };

      log(`theme start light output=${lightOutput}`);
      log(`theme start dark output=${darkOutput}`);

      try {
        await waitForBackend(page, baseUrl);
        await switchTheme(page, "light");

        for (const scriptName of screenshotScripts) {
          const scriptPath = path.join(scriptsDir, scriptName);

          session.output = lightOutput;
          await switchTheme(page, "light");
          await runCapture(scriptPath, session, `light/${scriptName}`);
          completedCount++;

          session.output = darkOutput;
          await switchTheme(page, "dark");
          try {
            await runCapture(scriptPath, session, `dark/${scriptName}`);
            completedCount++;
          } finally {
            await switchTheme(page, "light");
          }
        }
      } finally {
        await context.close();
      }
    } else {
      for (const theme of themes) {
        const themeOutput = path.join(output, theme);
        fs.rmSync(themeOutput, { recursive: true, force: true });
        fs.mkdirSync(themeOutput, { recursive: true });

        process.env.SCREENSHOT_THEME = theme;
        const context = await createContextWithStepTracking(browser);
        const page = await context.newPage();
        const session = {
          browser,
          context,
          page,
          output: themeOutput,
          baseUrl,
          location: "unknown",
        };

        log(`theme start ${theme} output=${themeOutput}`);

        try {
          await waitForBackend(page, baseUrl);
          await switchTheme(page, theme);

          for (const scriptName of screenshotScripts) {
            const scriptPath = path.join(scriptsDir, scriptName);
            await runCapture(scriptPath, session, `${theme}/${scriptName}`);
            completedCount++;
          }
        } finally {
          await context.close();
        }
      }
    }
  } finally {
    await closeBrowser(browser);
  }

  log(`run done success=${completedCount}/${totalRuns} duration=${formatDuration(Date.now() - runStartedAt)}`);
}

main().catch((err) => {
  logError(`fatal ${err.message}`);
  process.exit(1);
});
