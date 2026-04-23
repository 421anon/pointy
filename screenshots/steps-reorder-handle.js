#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const { parseArgs,
screenshotLocator,
waitForBackend, waitForApp, waitForProjectRows, waitForStepStatusEvent,
createContextWithStepTracking, } = require("./lib/helpers")

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

  const firstProjectRow = await waitForProjectRows(page);
  await page.evaluate(() => {
    window.__pointyStepStatusEventCount = 0;
    window.__pointyLastStepStatusEventType = null;
  });
  await firstProjectRow.click();
  await waitForApp(page);
  await waitForStepStatusEvent(page);

  const firstStepRow = page
    .locator('.table-record[id]:has(button.icon-btn[title="Edit"])')
    .first();
  if (await firstStepRow.count()) {
    await screenshotLocator(
      output,
      "steps-reorder-handle.png",
      firstStepRow.locator(".table-record-drag-target").first(),
    );
  } else {
    console.warn("No step rows found with Edit button.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
