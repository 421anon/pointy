#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const { parseArgs,
screenshotLocator,
waitForBackend, waitForApp, waitForProjectRows, createContextWithStepTracking, } = require("./lib/helpers")

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

  const firstProjectRecord = await waitForProjectRows(page);

  await screenshotLocator(
    output,
    "project-delete-button.png",
    firstProjectRecord.locator('button.icon-btn[title="Remove"]').first(),
  );

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
