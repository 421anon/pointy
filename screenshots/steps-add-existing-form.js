#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  clickIfExists,
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

    const addStepBtn = await page.$(
      '.table[id^="table-"]:not(#table-projects) .table-header-controls button.icon-btn',
    );
    if (addStepBtn) {
      await addStepBtn.click();
      await page.waitForTimeout(500);
      await waitForMaterialIcons(page);

      if (
        await clickIfExists(
          page,
          'input[type="radio"][name^="addMode"]:nth-of-type(2)',
        )
      ) {
        await page.waitForTimeout(300);
        await waitForMaterialIcons(page);
      } else {
        await clickIfExists(
          page,
          '.form-mode-selector label:nth-of-type(2) input[type="radio"]',
        );
        await page.waitForTimeout(300);
        await waitForMaterialIcons(page);
      }

      const addExistingForm = page.locator(".table-form-wrapper .form").first();
      if (await addExistingForm.count()) {
        await screenshotLocator(
          output,
          "steps-add-existing-form.png",
          addExistingForm,
        );
      } else {
        console.warn("Add existing form not found.");
      }
    } else {
      console.warn("Add step button not found.");
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
