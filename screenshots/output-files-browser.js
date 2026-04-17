#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  clickFirstVisible,
  withHoveredLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
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

    const outputStepRow = page.locator('.table-record[id="91"]').first();
    const outputStepRowVisible = await outputStepRow
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (outputStepRowVisible) {
      const outputSection = outputStepRow.locator(".output-files-section").first();
      const browseBtn = outputStepRow
        .locator('button.icon-btn[title="Browse output files"]')
        .first();

      const browseBtnVisible = await browseBtn
        .waitFor({ state: "visible", timeout: 30000 })
        .then(() => true)
        .catch(() => false);

      if (!browseBtnVisible) {
        console.warn("Browse output files button not visible on step row with id=91.");
      } else {
        if (!(await outputSection.isVisible().catch(() => false))) {
          await clickFirstVisible(browseBtn);
        }

        await waitForNoLoading(outputStepRow);
        await outputSection.waitFor({ state: "visible", timeout: 30000 });

        await waitForDirectoryContents(outputSection);
        await expandAllVisibleFolders(outputSection);
        await waitForDirectoryContents(outputSection);

        const helloRow = outputSection
          .locator(".directory-file-container")
          .filter({
            has: page.locator(".file-name", { hasText: /^hello$/ }),
          })
          .first();

        await helloRow.waitFor({ state: "visible", timeout: 10000 });

        const previewBtn = helloRow
          .locator("button.dir-item-icon-btn")
          .filter({
            has: page.locator(".material-symbols-outlined", {
              hasText: /^visibility(_off)?$/,
            }),
          })
          .first();

        const fileViewer = helloRow.locator(".file-content-viewer").first();
        if (!(await fileViewer.isVisible().catch(() => false))) {
          await previewBtn.click();
          await waitForNoLoading(helloRow);
          await fileViewer.waitFor({ state: "visible", timeout: 10000 });
          await waitForNoLoading(fileViewer);
        }

        await outputStepRow.scrollIntoViewIfNeeded();
        await page.waitForTimeout(300);

        await withHoveredLocator(
          page,
          browseBtn,
          async (hoveredBtn) => {
            await hoveredBtn.hover();
            await page.waitForTimeout(100);
            await screenshotLocator(output, "output-files-browser.png", outputStepRow);
          },
          "browse output files button",
        );
      }
    } else {
      console.warn("Step row with id=91 was not visible.");
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
