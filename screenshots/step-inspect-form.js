#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshot,
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
      const inspectBtn = alignmentStepRow
        .locator('button.icon-btn[title="Inspect Parameters"]')
        .first();
      const inspectBtnVisible = await inspectBtn
        .waitFor({ state: "visible", timeout: 30000 })
        .then(() => true)
        .catch(() => false);

      if (inspectBtnVisible) {
        await alignmentStepRow.scrollIntoViewIfNeeded();
        await page.waitForTimeout(300);

        await withHoveredLocator(
          page,
          inspectBtn,
          async (hoveredInspectBtn) => {
            await hoveredInspectBtn.click();
            await page.waitForTimeout(500);

            const inspectForm = page.locator(".table-form-wrapper").first();
            await inspectForm
              .waitFor({ state: "visible", timeout: 10000 })
              .catch(() => {});

            await hoveredInspectBtn.hover();
            await page.waitForTimeout(100);

            const headerBox = await alignmentStepRow
              .locator(".table-record-header")
              .first()
              .boundingBox();
            const formBox = await inspectForm.boundingBox();

            if (headerBox && formBox) {
              const x = Math.min(headerBox.x, formBox.x);
              const y = headerBox.y;
              const width =
                Math.max(
                  headerBox.x + headerBox.width,
                  formBox.x + formBox.width,
                ) - x;
              const height = formBox.y + formBox.height - y;
              const filePath = path.join(output, "step-inspect-form.png");
              await page.screenshot({
                path: filePath,
                clip: { x, y, width, height },
              });
              console.log(`Saved ${filePath}`);
            } else {
              await screenshot(page, output, "step-inspect-form.png");
            }
          },
          "inspect button for step 97",
        );
      } else {
        console.warn("Inspect Parameters button not visible on step row with id=97.");
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
