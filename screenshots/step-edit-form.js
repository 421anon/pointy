#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshot,
  firstVisibleLocator,
  waitForBackend,
  waitForApp,
  waitForStepStatusEvent,
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

  const firstProjectRow = await page.$("#table-projects .table-record");
  if (firstProjectRow) {
    await page.evaluate(() => {
      window.__pointyStepStatusEventCount = 0;
      window.__pointyLastStepStatusEventType = null;
    });
    await firstProjectRow.click();
    await waitForApp(page);
    await waitForStepStatusEvent(page);

    let editStepBtn = await firstVisibleLocator(
      page.locator('.table-record[id="97"] button.icon-btn[title="Edit"]'),
    );
    if (!editStepBtn) {
      editStepBtn = await firstVisibleLocator(
        page.locator('.table-record[id] button.icon-btn[title="Edit"]'),
      );
    }

    if (editStepBtn) {
      await editStepBtn.click();
      await page.waitForTimeout(500);
      await waitForMaterialIcons(page);
      await screenshot(page, output, "step-edit-form.png");
    } else {
      console.warn("Edit button not found on any visible step row.");
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
