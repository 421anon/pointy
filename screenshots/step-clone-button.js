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

    const alignmentStepRow = page.locator('.table-record[id="97"]').first();
    const alignmentStepRowVisible = await alignmentStepRow
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (alignmentStepRowVisible) {
      const cloneBtn = alignmentStepRow
        .locator('button.icon-btn[title="Clone"]')
        .first();
      const cloneBtnVisible = await cloneBtn
        .waitFor({ state: "visible", timeout: 30000 })
        .then(() => true)
        .catch(() => false);

      if (cloneBtnVisible) {
        const clonedStepRow = page.locator('.table-record[id="106"]').first();
        const clonedStepRowVisible = await clonedStepRow
          .waitFor({ state: "visible", timeout: 10000 })
          .then(() => true)
          .catch(() => false);

        if (clonedStepRowVisible) {
          await withHoveredLocator(
            page,
            cloneBtn,
            async () => {
              const box97 = await alignmentStepRow.boundingBox();
              const box106 = await clonedStepRow.boundingBox();
              if (box97 && box106) {
                const x = Math.min(box97.x, box106.x);
                const y = box97.y;
                const width =
                  Math.max(
                    box97.x + box97.width,
                    box106.x + box106.width,
                  ) - x;
                const height = box106.y + box106.height - y;
                const filePath = path.join(output, "step-clone-button.png");
                await page.screenshot({
                  path: filePath,
                  clip: { x, y, width, height },
                });
                console.log(`Saved ${filePath}`);
              } else {
                await screenshotLocator(
                  output,
                  "step-clone-button.png",
                  alignmentStepRow,
                );
              }
            },
            "clone button for alignment step 97",
          );
        } else {
          console.warn("Cloned step row with id=106 was not visible.");
        }
      } else {
        console.warn("Clone button not visible on step row with id=97.");
      }
    } else {
      console.warn("Step row with id=97 was not visible.");
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
