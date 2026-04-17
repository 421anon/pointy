#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  firstVisibleLocator,
  withHoveredLocator,
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

    const derivationHeader = await firstVisibleLocator(
      page.locator(
        '.table-record[id^="alignment-"] .table-record-header, .table-record:has(button.icon-btn[title="Run"]) .table-record-header',
      ),
    );
    if (derivationHeader) {
      const runButton = derivationHeader.locator(
        'button.icon-btn[title="Run"]',
      );
      await withHoveredLocator(
        page,
        runButton,
        async () =>
          screenshotLocator(output, "step-derivation-header.png", derivationHeader),
        "derivation run button",
      );
    } else {
      console.warn("No visible derivation step header found.");
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
