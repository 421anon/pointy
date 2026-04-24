#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  withHoveredLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  findPreviewableFileInSection,
  findVisibleStepRowWithButton,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;

  await prepareProjectPage(session);

  const { stepRow: srcFilesStepRow, button: browseSrcBtn } =
    await findVisibleStepRowWithButton(page, "Browse source files");

  if (srcFilesStepRow && browseSrcBtn) {
    const srcSection = srcFilesStepRow.locator(".src-files-section").first();
    const browseSrcBtnVisible = await browseSrcBtn
      .waitFor({ state: "visible", timeout: 30000 })
      .then(() => true)
      .catch(() => false);

    if (!browseSrcBtnVisible) {
      session.warn("Browse source files button was not visible on any visible step row.");
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
        session.warn("No previewable source file row was visible in the source files section.");
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
          session.warn,
        );
      }
    }
  } else {
    session.warn("No visible step row exposed a Browse source files button.");
  }

}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
