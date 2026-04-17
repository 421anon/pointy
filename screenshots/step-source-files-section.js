#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
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

    const srcFilesStepRow = page.locator('.table-record[id="91"]').first();
    const srcFilesStepRowVisible = await srcFilesStepRow
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (srcFilesStepRowVisible) {
      const srcSection = srcFilesStepRow.locator(".src-files-section").first();
      const browseSrcBtn = srcFilesStepRow
        .locator('button.icon-btn[title="Browse source files"]')
        .first();

      const browseSrcBtnVisible = await browseSrcBtn
        .waitFor({ state: "visible", timeout: 30000 })
        .then(() => true)
        .catch(() => false);

      if (!browseSrcBtnVisible) {
        console.warn("Browse source files button not visible on step row with id=91.");
      } else {
        await browseSrcBtn.click();
        await waitForNoLoading(srcFilesStepRow);
        await srcSection.waitFor({ state: "visible", timeout: 30000 });

        await waitForDirectoryContents(srcSection);
        await expandAllVisibleFolders(srcSection);

        const mainPyRow = srcSection
          .locator(".directory-file-container")
          .filter({
            has: page.locator(".file-name", { hasText: /^main\.py$/ }),
          })
          .first();

        await mainPyRow.waitFor({ state: "visible", timeout: 10000 });

        const previewBtn = mainPyRow
          .locator("button.dir-item-icon-btn")
          .filter({
            has: page.locator(".material-symbols-outlined", {
              hasText: /^visibility(_off)?$/,
            }),
          })
          .first();

        const fileViewer = mainPyRow.locator(".file-content-viewer").first();
        if (!(await fileViewer.isVisible().catch(() => false))) {
          await previewBtn.click();
          await waitForNoLoading(srcFilesStepRow);
          await fileViewer.waitFor({ state: "visible", timeout: 10000 });
          await page.waitForTimeout(1000);
          await waitForNoLoading(mainPyRow);
        }

        await srcFilesStepRow.scrollIntoViewIfNeeded();
        await page.waitForTimeout(300);

        await withHoveredLocator(
          page,
          browseSrcBtn,
          async (hoveredBtn) => {
            await hoveredBtn.hover();
            await page.waitForTimeout(100);
            await screenshotLocator(output, "step-source-files-section.png", srcFilesStepRow);
          },
          "browse source files button",
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
