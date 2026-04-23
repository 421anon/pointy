#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  withHoveredLocator,
  clickFirstVisible,
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

  const { stepRow: outputStepRow, button: browseBtn } =
    await findVisibleStepRowWithButton(page, "Browse output files");

  if (outputStepRow && browseBtn) {
    const outputSection = outputStepRow.locator(".output-files-section").first();
    const browseBtnVisible = await browseBtn
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (!browseBtnVisible) {
      console.warn("Browse output files button was not visible on any visible step row.");
    } else {
      if (!(await outputSection.isVisible().catch(() => false))) {
        await clickFirstVisible(browseBtn);
      }

      await waitForNoLoading(outputStepRow);
      await outputSection.waitFor({ state: "visible", timeout: 30000 });

      await waitForDirectoryContents(outputSection);
      await expandAllVisibleFolders(outputSection);
      await waitForDirectoryContents(outputSection);

      const previewableOutputFile = await findPreviewableFileInSection(outputSection, {
        preferredNames: [/^hello$/],
        preferNonHtml: true,
      });

      if (!previewableOutputFile) {
        console.warn(
          "No previewable output file row was visible in the output files section.",
        );
      } else {
        const { fileRow, previewButton } = previewableOutputFile;
        const fileViewer = fileRow.locator(".file-content-viewer").first();

        if (!(await fileViewer.isVisible().catch(() => false))) {
          await previewButton.click();
          await waitForNoLoading(fileRow);
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
            await screenshotLocator(
              output,
              "output-files-browser.png",
              outputStepRow,
            );
          },
          "browse output files button",
        );
      }
    }
  } else {
    console.warn("No visible step row exposed a Browse output files button.");
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
