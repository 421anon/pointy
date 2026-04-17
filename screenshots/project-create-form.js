#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
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

  const addProjectBtn = await page.$(
    "#table-projects .table-header-controls button.icon-btn",
  );
  if (addProjectBtn) {
    await addProjectBtn.click();
    await page.waitForTimeout(500);
    await waitForMaterialIcons(page);
    await screenshotLocator(
      output,
      "project-create-form.png",
      page.locator(".table-form-wrapper .form").first(),
    );
  } else {
    console.warn("Add project button not found in #table-projects header.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
