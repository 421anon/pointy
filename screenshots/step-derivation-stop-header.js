#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  firstVisibleLocator,
  clickFirstVisible,
  withHoveredLocator,
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

    const derivationHeader = await firstVisibleLocator(
      page.locator(
        '.table-record[id^="alignment-"] .table-record-header, .table-record:has(button.icon-btn[title="Run"]) .table-record-header',
      ),
    );
    if (derivationHeader) {
      const runButton = derivationHeader.locator(
        'button.icon-btn[title="Run"]',
      );

      const clickedRun = await clickFirstVisible(runButton);
      if (clickedRun) {
        await waitForMaterialIcons(page);
        const stopButton = derivationHeader.locator(
          'button.icon-btn[title="Stop"]',
        );
        const stopVisible = await stopButton
          .first()
          .waitFor({ state: "visible", timeout: 10000 })
          .then(() => true)
          .catch(() => false);

        if (stopVisible) {
          await withHoveredLocator(
            page,
            stopButton,
            async () =>
              screenshotLocator(
                output,
                "step-derivation-stop-header.png",
                derivationHeader,
              ),
            "derivation stop button",
          );
        } else {
          console.warn("Stop button did not appear after clicking Run.");
        }
      } else {
        console.warn("No visible Run button found in derivation header.");
      }
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
