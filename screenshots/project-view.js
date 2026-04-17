#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshot,
  showTitleAsTooltip,
  waitForBackend,
  waitForApp,
  waitForStepStatusEvent,
  createContextWithStepTracking,
} = require("./lib/helpers");

async function main() {
  const { output = path.join(__dirname, "../docs/pages/screenshots"), url: baseUrl = "http://localhost" } =
    parseArgs(process.argv.slice(2));

  fs.mkdirSync(output, { recursive: true });

  const browser = await chromium.launch({
    headless: true,
    args: [
      "--no-sandbox",
      "--disable-setuid-sandbox",
      "--disable-dev-shm-usage",
    ],
  });

  const context = await createContextWithStepTracking(browser);
  const page = await context.newPage();

  await waitForBackend(page, baseUrl);
  await page.goto(`${baseUrl}/`, { waitUntil: "load" });
  await waitForApp(page);

  const firstProjectRow = await page.$("#table-projects .table-record");
  if (firstProjectRow) {
    await page.evaluate(() => {
      window.__pointyStepStatusEventCount = 0;
      window.__pointyLastStepStatusEventType = null;
    });
    await firstProjectRow.click();
    await waitForApp(page);
    await waitForStepStatusEvent(page);

    // Scroll to bottom to ensure all steps are visible (including script 1)
    const lastStepRow = page
      .locator('.table-record')
      .filter({ hasText: "script 1" })
      .first();

    const lastStepVisible = await lastStepRow
      .waitFor({ state: "visible", timeout: 10000 })
      .then(() => true)
      .catch(() => false);

    if (lastStepVisible) {
      await lastStepRow.scrollIntoViewIfNeeded();
      await page.waitForTimeout(300);
    }

    // Move mouse away from content to avoid accidental hover highlights
    const viewport = page.viewportSize() || { width: 1280, height: 800 };
    await page.mouse.move(viewport.width - 1, viewport.height - 1);
    await page.waitForTimeout(100);

    // Screenshot 1: Full table without tooltip (for Building Workflows section)
    // Use fullPage to capture all steps
    const projectViewPath = path.join(output, "project-view.png");
    await page.screenshot({ path: projectViewPath, fullPage: true });
    console.log(`Saved ${projectViewPath}`);

    // Screenshot 2: With tooltip on failed step (for Step statuses section)
    const failedStepRow = page
      .locator('.table-record')
      .filter({ hasText: "sleep-then-fail" })
      .first();

    const failedStepVisible = await failedStepRow
      .waitFor({ state: "visible", timeout: 10000 })
      .then(() => true)
      .catch(() => false);

    if (failedStepVisible) {
      const statusIndicator = failedStepRow
        .locator(".status-indicator-wrapper")
        .first();

      // Scroll so the failed step is near the top, leaving room for tooltip below
      await failedStepRow.evaluate((el) => {
        el.scrollIntoView({ block: "start", behavior: "instant" });
      });
      await page.waitForTimeout(300);

      // Move mouse away from content to avoid accidental hover highlights
      await page.mouse.move(viewport.width - 1, viewport.height - 1);
      await page.waitForTimeout(100);

      // Show the title attribute as a visible tooltip
      const cleanupTooltip = await showTitleAsTooltip(page, statusIndicator);
      await page.waitForTimeout(100);
      await screenshot(page, output, "project-view-status-tooltip.png");
      await cleanupTooltip();
    }
  } else {
    console.warn("No project rows found in #table-projects.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
