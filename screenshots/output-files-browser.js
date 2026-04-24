#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  withHoveredLocator,
  clickFirstVisible,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  findPreviewableFileInSection,
  findVisibleStepRowWithButton,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const { stepRow: outputStepRow, button: browseBtn } =
    await findVisibleStepRowWithButton(page, "Browse output files");

  if (outputStepRow && browseBtn) {
    const outputSection = outputStepRow.locator(".output-files-section").first();
    const browseBtnVisible = await browseBtn
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (!browseBtnVisible) {
      session.warn("Browse output files button was not visible on any visible step row.");
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
        session.warn("No previewable output file row was visible in the output files section.");
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
          session.warn,
        );
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
