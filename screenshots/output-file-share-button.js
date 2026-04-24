#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  clickFirstVisible,
  withHoveredLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  findVisibleStepRowWithButton,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const { stepRow: outputStepRow, button: browseBtn } =
    await findVisibleStepRowWithButton(page, "Browse output files");

  if (outputStepRow) {
    const outputSection = outputStepRow.locator(".output-files-section").first();

    if (!(await outputSection.isVisible().catch(() => false))) {
      const openedStep = await clickFirstVisible(browseBtn);
      if (!openedStep) {
        session.warn("Could not open output files from any visible step row exposing Browse output files.");
      }
    }

    await waitForNoLoading(outputStepRow);
    const outputSectionVisible = await outputSection
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (!outputSectionVisible) {
      session.warn("Output files section did not become visible for the selected visible step row.");
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
                hasText: /^visibility$/ ,
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
              session.warn("Log.final.out preview did not become visible before share capture.");
            }
          } else {
            session.warn("Preview button was not visible for Log.final.out before share capture.");
          }
        }

        const logFinalShareBtn = logFinalRow
          .locator("button.dir-item-icon-btn")
          .filter({
            has: page.locator(".material-symbols-outlined", {
              hasText: /^share$/ ,
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
              hasText: /^share$/ ,
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
          session.warn,
        );
      } else {
        session.warn("No visible file-level share button found in the selected output browser.");
      }
    }
  } else {
    session.warn("No visible step row exposed a Browse output files button.");
  }

}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
