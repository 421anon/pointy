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

    const outputStepRow = page.locator('.table-record[id="2"]').first();
    const outputStepRowVisible = await outputStepRow
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (outputStepRowVisible) {
      const outputSection = outputStepRow.locator(".output-files-section").first();
      const browseBtn = outputStepRow
        .locator('button.icon-btn[title="Browse output files"]')
        .first();

      if (!(await outputSection.isVisible().catch(() => false))) {
        const openedStep = await clickFirstVisible(browseBtn);
        if (!openedStep) {
          console.warn(
            "Could not open output files for step row with id=2.",
          );
        }
      }

      await waitForNoLoading(outputStepRow);
      const outputSectionVisible = await outputSection
        .waitFor({ state: "visible", timeout: 30000 })
        .then(() => true)
        .catch(() => false);

      if (!outputSectionVisible) {
        console.warn(
          "Output files section did not become visible for step row with id=2.",
        );
      } else {
        await waitForDirectoryContents(outputSection);

        const expandedFolders = await expandAllVisibleFolders(outputSection);
        void expandedFolders;

        const logFinalRow = outputSection
          .locator(".directory-file-container")
          .filter({
            has: page.locator(".file-name", { hasText: /^Log\.final\.out$/ }),
          })
          .first();

        const logFinalRowVisible = await logFinalRow
          .waitFor({ state: "visible", timeout: 10000 })
          .then(() => true)
          .catch(() => false);

        let shareBtn = null;

        if (logFinalRowVisible) {
          const fileViewer = logFinalRow.locator(".file-content-viewer").first();
          let fileViewerVisible = await fileViewer
            .isVisible()
            .catch(() => false);

          if (!fileViewerVisible) {
            const previewBtn = logFinalRow
              .locator("button.dir-item-icon-btn")
              .filter({
                has: page.locator(".material-symbols-outlined", {
                  hasText: /^visibility$/,
                }),
              })
              .first();

            const previewBtnVisible = await previewBtn
              .waitFor({ state: "visible", timeout: 10000 })
              .then(() => true)
              .catch(() => false);

            if (previewBtnVisible) {
              await previewBtn.click();
              await waitForNoLoading(logFinalRow);
              fileViewerVisible = await fileViewer
                .waitFor({ state: "visible", timeout: 10000 })
                .then(() => true)
                .catch(() => false);
              if (!fileViewerVisible) {
                console.warn(
                  "Log.final.out preview did not become visible before share capture.",
                );
              }
            } else {
              console.warn(
                "Preview button was not visible for Log.final.out before share capture.",
              );
            }
          }

          const logFinalShareBtn = logFinalRow
            .locator("button.dir-item-icon-btn")
            .filter({
              has: page.locator(".material-symbols-outlined", {
                hasText: /^share$/,
              }),
            })
            .first();

          const logFinalShareVisible = await logFinalShareBtn
            .waitFor({ state: "visible", timeout: 10000 })
            .then(() => true)
            .catch(() => false);

          if (logFinalShareVisible) {
            shareBtn = logFinalShareBtn;
          }
        }

        if (!shareBtn) {
          const anyShareBtn = outputSection
            .locator("button.dir-item-icon-btn")
            .filter({
              has: page.locator(".material-symbols-outlined", {
                hasText: /^share$/,
              }),
            })
            .first();

          const anyShareVisible = await anyShareBtn
            .waitFor({ state: "visible", timeout: 10000 })
            .then(() => true)
            .catch(() => false);

          if (anyShareVisible) {
            shareBtn = anyShareBtn;
          }
        }

        if (shareBtn) {
          await shareBtn.waitFor({ state: "visible", timeout: 30000 });
          await withHoveredLocator(
            page,
            shareBtn,
            async () =>
              screenshotLocator(
                output,
                "output-file-share-button.png",
                outputStepRow,
              ),
            "output file share button",
          );
        } else {
          console.warn(
            "No visible file-level share button found in the output browser for step 2.",
          );
        }
      }
    } else {
      console.warn("Step row with id=2 was not visible.");
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
