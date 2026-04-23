#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const { parseArgs,
  screenshotLocator,
  clickFirstVisible,
  withHoveredLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  findVisibleStepRowWithButton,
  waitForBackend, waitForApp, waitForProjectRows, waitForStepStatusEvent,
  createContextWithStepTracking, } = require("./lib/helpers")

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

  const { stepRow: outputStepRow, button: browseBtn } =
    await findVisibleStepRowWithButton(page, "Browse output files");

  if (outputStepRow) {
    const outputSection = outputStepRow.locator(".output-files-section").first();

    if (!(await outputSection.isVisible().catch(() => false))) {
      await clickFirstVisible(browseBtn);
    }

    await waitForNoLoading(outputStepRow);
    await outputSection.waitFor({ state: "visible", timeout: 30000 });
    await waitForDirectoryContents(outputSection);

    const chimR1Row = outputSection
      .locator(".directory-file-container")
      .filter({
        has: page.locator(".file-name", { hasText: /^chim_R1_fastqc\.html$/ }),
      })
      .first();

    await chimR1Row.waitFor({ state: "visible", timeout: 10000 });

    const previewBtn = chimR1Row
      .locator("button.dir-item-icon-btn")
      .filter({
        has: page.locator(".material-symbols-outlined", {
          hasText: /^visibility(_off)?$/ ,
        }),
      })
      .first();

    await previewBtn.waitFor({ state: "visible", timeout: 10000 });

    const fileViewer = chimR1Row.locator(".file-content-viewer").first();
    if (!(await fileViewer.isVisible().catch(() => false))) {
      await previewBtn.click();
      await waitForNoLoading(chimR1Row);
      await fileViewer.waitFor({ state: "visible", timeout: 10000 });
      await waitForNoLoading(fileViewer);

      const zoomOutBtn = chimR1Row.locator("button.iframe-zoom-btn.zoom-out").first();
      const zoomOutVisible = await zoomOutBtn
        .waitFor({ state: "visible", timeout: 5000 })
        .then(() => true)
        .catch(() => false);

      if (zoomOutVisible) {
        await zoomOutBtn.click();
        await page.waitForTimeout(200);
        await zoomOutBtn.click();
        await page.waitForTimeout(200);
        await zoomOutBtn.click();
        await page.waitForTimeout(300);
      }
    }

    await outputStepRow.scrollIntoViewIfNeeded();
    await page.waitForTimeout(300);

    await withHoveredLocator(
      page,
      previewBtn,
      async (hoveredBtn) => {
        await hoveredBtn.hover();
        await page.waitForTimeout(100);
        await screenshotLocator(output, "output-files-html-preview.png", outputStepRow);
      },
      "preview button",
    );
  } else {
    console.warn("No visible step row exposed a Browse output files button.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
