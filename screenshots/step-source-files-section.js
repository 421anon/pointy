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
  findPreviewableFileInSection,
  findVisibleStepRowWithButton,
  waitForBackend,
  waitForApp,
  waitForProjectRows,
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

  const firstProjectRow = await waitForProjectRows(page);
  await page.evaluate(() => {
    window.__pointyStepStatusEventCount = 0;
    window.__pointyLastStepStatusEventType = null;
  });
  await firstProjectRow.click();
  await waitForApp(page);
  await waitForStepStatusEvent(page);

  const { stepRow: srcFilesStepRow, button: browseSrcBtn } =
    await findVisibleStepRowWithButton(page, "Browse source files");

  if (srcFilesStepRow && browseSrcBtn) {
    const srcSection = srcFilesStepRow.locator(".src-files-section").first();
    const browseSrcBtnVisible = await browseSrcBtn
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (!browseSrcBtnVisible) {
      console.warn("Browse source files button was not visible on any visible step row.");
    } else {
      if (!(await srcSection.isVisible().catch(() => false))) {
        await browseSrcBtn.click();
      }

      await waitForNoLoading(srcFilesStepRow);
      await srcSection.waitFor({ state: "visible", timeout: 30000 });

      await waitForDirectoryContents(srcSection);
      await expandAllVisibleFolders(srcSection);
      await waitForDirectoryContents(srcSection);

      const previewableSourceFile = await findPreviewableFileInSection(srcSection, {
        preferredNames: [/^main\.py$/],
      });

      if (!previewableSourceFile) {
        console.warn(
          "No previewable source file row was visible in the source files section.",
        );
      } else {
        const { fileRow, previewButton } = previewableSourceFile;
        const fileViewer = fileRow.locator(".file-content-viewer").first();

        if (!(await fileViewer.isVisible().catch(() => false))) {
          await previewButton.click();
          await waitForNoLoading(srcFilesStepRow);
          await fileViewer.waitFor({ state: "visible", timeout: 10000 });
          await page.waitForTimeout(1000);
          await waitForNoLoading(fileRow);
        }

        await srcFilesStepRow.scrollIntoViewIfNeeded();
        await page.waitForTimeout(300);

        await withHoveredLocator(
          page,
          browseSrcBtn,
          async (hoveredBtn) => {
            await hoveredBtn.hover();
            await page.waitForTimeout(100);
            await screenshotLocator(
              output,
              "step-source-files-section.png",
              srcFilesStepRow,
            );
          },
          "browse source files button",
        );
      }
    }
  } else {
    console.warn("No visible step row exposed a Browse source files button.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
