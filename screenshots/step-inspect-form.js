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
  waitForProjectRows,
  waitForStepStatusEvent,
  createContextWithStepTracking,
  findVisibleStepRowWithButton,
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

  const firstProjectRow = await waitForProjectRows(page);
  await page.evaluate(() => {
    window.__pointyStepStatusEventCount = 0;
    window.__pointyLastStepStatusEventType = null;
  });
  await firstProjectRow.click();
  await waitForApp(page);
  await waitForStepStatusEvent(page);

  const { stepRow, button: inspectBtn } = await findVisibleStepRowWithButton(
    page,
    "Inspect Parameters",
  );

  if (stepRow && inspectBtn) {
    await stepRow.scrollIntoViewIfNeeded();
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

        const headerBox = await stepRow
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
      "inspect button for a visible shareable step",
    );
  } else {
    console.warn("No visible step row exposed an Inspect Parameters button.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
