#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  findPreviewableFileInSection,
} = require("./lib/helpers");

const TARGET_STEP_ID = "206";
const TARGET_FILE = /^samples\.csv$/i;

async function capture(session) {
  const { page, output, warn } = session;
  await prepareProjectPage(session);

  const stepRow = page.locator(`.table-record[id="${TARGET_STEP_ID}"]`).first();

  try {
    await stepRow.scrollIntoViewIfNeeded({ timeout: 5000 });
  } catch (_err) {
    // The row may not yet be in the DOM; the waitFor below handles that.
  }
  await stepRow.waitFor({ state: "visible", timeout: 30000 });

  const browseBtn = stepRow
    .locator('button.icon-btn[title="Browse output files"]')
    .first();
  const browseVisible = await browseBtn
    .waitFor({ state: "visible", timeout: 30000 })
    .then(() => true)
    .catch(() => false);

  if (!browseVisible) {
    warn(`Step ${TARGET_STEP_ID} does not expose a Browse output files button.`);
    return;
  }

  const outputSection = stepRow.locator(".output-files-section").first();
  if (!(await outputSection.isVisible().catch(() => false))) {
    await browseBtn.click();
  }

  await waitForNoLoading(stepRow);
  await outputSection.waitFor({ state: "visible", timeout: 30000 });
  await waitForDirectoryContents(outputSection);
  await expandAllVisibleFolders(outputSection);
  await waitForDirectoryContents(outputSection);

  const previewable = await findPreviewableFileInSection(outputSection, {
    preferredNames: [TARGET_FILE],
  });

  if (!previewable) {
    warn(`No previewable file matching ${TARGET_FILE} in step ${TARGET_STEP_ID}.`);
    return;
  }

  const { fileRow, previewButton } = previewable;
  const gridViewer = fileRow.locator(".delimited-grid-viewer").first();

  if (!(await gridViewer.isVisible().catch(() => false))) {
    await previewButton.click();
    await waitForNoLoading(fileRow);
    await gridViewer.waitFor({ state: "visible", timeout: 30000 });
  }

  await stepRow.scrollIntoViewIfNeeded();
  // Brief settle so layout + font rendering is stable before capture.
  await page.waitForTimeout(300);

  await screenshotLocator(output, "output-files-csv-grid.png", stepRow);
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
