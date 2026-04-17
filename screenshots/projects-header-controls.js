#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  withHoveredLocator,
  waitForBackend,
  waitForApp,
  waitForMaterialIcons,
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

  await withHoveredLocator(
    page,
    page.locator("#table-projects .table-header-controls button.icon-btn").first(),
    async () =>
      screenshotLocator(
        output,
        "projects-header-controls.png",
        page.locator("#table-projects .table-header").first(),
      ),
    "add project button",
  );

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
