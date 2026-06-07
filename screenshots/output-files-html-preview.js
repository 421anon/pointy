#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  firstVisibleLocator,
  prepareProjectPage,
  runStandalone,
  screenshotLocator,
  waitForDirectoryContents,
  waitForNoLoading,
  expandAllVisibleFolders,
} = require("./lib/helpers");

const STEP_ROW_SELECTOR = '.table[id^="table-"]:not(#table-projects) .table-record[id]';
const HTML_FILE_NAME = /\.html?$/i;

async function directoryFileName(fileRow) {
  return fileRow.locator(".file-name").first().evaluate((el) => {
    return (el.textContent || "").replace(/\s+/g, " ").trim();
  });
}

async function previewButtonFor(page, fileRow) {
  return firstVisibleLocator(
    fileRow.locator("button.dir-item-icon-btn").filter({
      has: page.locator(".material-symbols-outlined", {
        hasText: /^visibility(_off)?$/,
      }),
    }),
  );
}

async function openOutputSection(stepRow, browseBtn) {
  const outputSection = stepRow.locator(".output-files-section").first();

  if (!(await outputSection.isVisible().catch(() => false))) {
    await browseBtn.click();
  }

  await waitForNoLoading(stepRow);
  await outputSection.waitFor({ state: "visible", timeout: 30000 });
  await waitForDirectoryContents(outputSection);
  await expandAllVisibleFolders(outputSection);
  await waitForDirectoryContents(outputSection);

  return outputSection;
}

async function findHtmlOutput(page) {
  const stepRows = page.locator(STEP_ROW_SELECTOR);
  const count = await stepRows.count();

  for (let i = 0; i < count; i++) {
    const stepRow = stepRows.nth(i);
    if (!(await stepRow.isVisible().catch(() => false))) continue;

    const browseBtn = await firstVisibleLocator(
      stepRow.locator('button.icon-btn[title="Browse output files"]'),
    );
    if (!browseBtn) continue;

    const outputSection = await openOutputSection(stepRow, browseBtn);
    const fileRows = outputSection.locator(".directory-file-container");
    const fileCount = await fileRows.count();

    for (let j = 0; j < fileCount; j++) {
      const fileRow = fileRows.nth(j);
      if (!(await fileRow.isVisible().catch(() => false))) continue;

      let fileName = "";
      try {
        fileName = await directoryFileName(fileRow);
      } catch (_err) {
        continue;
      }

      if (!HTML_FILE_NAME.test(fileName)) continue;

      const previewButton = await previewButtonFor(page, fileRow);
      if (!previewButton) continue;

      return { stepRow, fileRow, previewButton, fileName };
    }
  }

  return null;
}

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const htmlOutput = await findHtmlOutput(page);
  if (!htmlOutput) {
    session.warn("No previewable HTML output file was visible in any output files section.");
    return;
  }

  const { stepRow, fileRow, previewButton } = htmlOutput;
  const fileViewer = fileRow.locator(".file-content-viewer").first();

  if (!(await fileViewer.isVisible().catch(() => false))) {
    await previewButton.click();
    await waitForNoLoading(fileRow);
    await fileViewer.waitFor({ state: "visible", timeout: 30000 });
  }

  const htmlFrame = fileViewer.locator("iframe.file-html-viewer").first();
  const htmlFrameVisible = await htmlFrame
    .waitFor({ state: "visible", timeout: 30000 })
    .then(() => true)
    .catch(() => false);

  if (!htmlFrameVisible) {
    session.warn("HTML file preview did not render an iframe before capture.");
    return;
  }

  const zoomOutBtn = fileRow.locator("button.iframe-zoom-btn.zoom-out").first();
  if (await zoomOutBtn.isVisible().catch(() => false)) {
    for (let i = 0; i < 3; i++) {
      await zoomOutBtn.click();
      await page.waitForTimeout(200);
    }
  }

  await stepRow.scrollIntoViewIfNeeded();
  await page.waitForTimeout(300);

  await previewButton.hover();
  await page.waitForTimeout(100);
  await screenshotLocator(output, "output-files-html-preview.png", stepRow);
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
