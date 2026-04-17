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

  const projectsTable = page.locator("#table-projects").first();
  const firstProjectRecord = page.locator("#table-projects .table-record").first();

  if (await firstProjectRecord.count()) {
    const firstActionBtn = firstProjectRecord
      .locator(".table-record-actions .icon-btn");

    await withHoveredLocator(
      page,
      firstActionBtn,
      async () => screenshotLocator(output, "projects-home.png", projectsTable),
      "project row action button",
    );
  } else {
    await screenshotLocator(output, "projects-home.png", projectsTable);
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
