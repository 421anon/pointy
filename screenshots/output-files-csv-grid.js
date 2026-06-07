#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  firstVisibleLocator,
  prepareProjectPage,
  runStandalone,
  screenshotLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
} = require("./lib/helpers");

const STEP_ROW_SELECTOR = '.table[id^="table-"]:not(#table-projects) .table-record[id]';
const DELIMITED_FILE_NAME = /\.(csv|tsv)$/i;
const PREFERRED_FILE = /^samples\.csv$/i;

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

function fileRank(fileName) {
  return PREFERRED_FILE.test(fileName) ? 0 : 1;
}

async function findDelimitedOutput(page) {
  const stepRows = page.locator(STEP_ROW_SELECTOR);
  const rowCount = await stepRows.count();
  const candidates = [];

  for (let i = 0; i < rowCount; i++) {
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

      if (!DELIMITED_FILE_NAME.test(fileName)) continue;

      const previewButton = await previewButtonFor(page, fileRow);
      if (!previewButton) continue;

      candidates.push({ stepRow, fileRow, previewButton, fileName, rowIndex: i, fileIndex: j });
    }
  }

  candidates.sort((a, b) => {
    const rankDiff = fileRank(a.fileName) - fileRank(b.fileName);
    if (rankDiff !== 0) return rankDiff;
    if (a.rowIndex !== b.rowIndex) return a.rowIndex - b.rowIndex;
    return a.fileIndex - b.fileIndex;
  });

  return candidates[0] || null;
}

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const delimitedOutput = await findDelimitedOutput(page);
  if (!delimitedOutput) {
    session.warn("No previewable CSV or TSV output file was visible in any output files section.");
    return;
  }

  const { stepRow, fileRow, previewButton } = delimitedOutput;
  const gridViewer = fileRow.locator(".delimited-grid-viewer").first();

  if (!(await gridViewer.isVisible().catch(() => false))) {
    await previewButton.click();
    await waitForNoLoading(fileRow);
    await gridViewer.waitFor({ state: "visible", timeout: 30000 });
  }

  await stepRow.scrollIntoViewIfNeeded();
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
