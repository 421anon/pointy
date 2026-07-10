#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  clickFirstVisible,
  firstVisibleLocator,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  findVisibleStepRowWithButton,
  waitForMaterialIcons,
  withHoveredLocator,
} = require("./lib/helpers");

const PREFERRED_RANGE = { from: 6, to: 9 };
const PLAIN_TEXT_EXCLUSIONS = /\.(csv|tsv|html?|png|jpe?g|gif|webp|pdf)$/i;

function formatLineRange({ from, to }) {
  return from === to ? String(from) : `${from}-${to}`;
}

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

function textCandidateRank(fileName) {
  if (/^Log\.final\.out$/i.test(fileName)) return 0;
  if (!PLAIN_TEXT_EXCLUSIONS.test(fileName)) return 1;
  return 10;
}

async function findTextCandidates(page, outputSection) {
  const fileRows = outputSection.locator(".directory-file-container");
  const count = await fileRows.count();
  const candidates = [];

  for (let i = 0; i < count; i++) {
    const fileRow = fileRows.nth(i);
    if (!(await fileRow.isVisible().catch(() => false))) continue;

    let fileName = "";
    try {
      fileName = await directoryFileName(fileRow);
    } catch (_err) {
      continue;
    }

    if (textCandidateRank(fileName) >= 10) continue;

    const previewButton = await previewButtonFor(page, fileRow);
    if (!previewButton) continue;

    candidates.push({ fileRow, fileName, previewButton, originalIndex: i });
  }

  return candidates.sort((a, b) => {
    const rankDiff = textCandidateRank(a.fileName) - textCandidateRank(b.fileName);
    if (rankDiff !== 0) return rankDiff;
    return a.originalIndex - b.originalIndex;
  });
}

async function selectLineRange(fileViewer, preferredRange) {
  const fileContent = fileViewer.locator(".file-content").first();
  await fileContent.waitFor({ state: "visible", timeout: 3000 });

  const gutterCells = fileContent.locator(".file-gutter .file-line-number");
  const lineCount = await gutterCells.count();
  if (lineCount === 0) return null;

  const hasPreferredRange = lineCount >= preferredRange.to;
  const from = hasPreferredRange ? preferredRange.from : 1;
  const to = hasPreferredRange ? preferredRange.to : Math.min(lineCount, 3);
  if (from > to) return null;

  const gutterFor = (line) =>
    fileContent.locator(`.file-line-number[data-line="${line}"]`).first();
  const startGutter = gutterFor(from);
  const endGutter = gutterFor(to);
  await startGutter.scrollIntoViewIfNeeded();
  await endGutter.scrollIntoViewIfNeeded();

  const startBox = await startGutter.boundingBox();
  const endBox = await endGutter.boundingBox();
  if (!startBox || !endBox) return null;

  const page = fileViewer.page();
  await page.mouse.move(
    startBox.x + startBox.width / 2,
    startBox.y + startBox.height / 2,
  );
  await page.mouse.down();
  await page.mouse.move(
    endBox.x + endBox.width / 2,
    endBox.y + endBox.height / 2,
    { steps: Math.max(1, Math.abs(to - from)) },
  );
  await page.mouse.up();
  await page.waitForTimeout(200);

  const highlightedCount = await fileContent.locator(".file-line-number.highlighted").count();
  return highlightedCount > 0 ? { from, to } : null;
}

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const { stepRow: outputStepRow, button: browseBtn } =
    await findVisibleStepRowWithButton(page, "Browse output files");

  if (!outputStepRow || !browseBtn) {
    session.warn("No visible step row exposed a Browse output files button.");
    return;
  }

  const outputSection = outputStepRow.locator(".output-files-section").first();
  if (!(await outputSection.isVisible().catch(() => false))) {
    await clickFirstVisible(browseBtn);
  }

  await waitForNoLoading(outputStepRow);
  await outputSection.waitFor({ state: "visible", timeout: 30000 });
  await waitForDirectoryContents(outputSection);
  await expandAllVisibleFolders(outputSection);
  await waitForDirectoryContents(outputSection);

  const candidates = await findTextCandidates(page, outputSection);
  if (candidates.length === 0) {
    session.warn("No plain text output file row was visible for line-range sharing.");
    return;
  }

  for (const { fileRow, previewButton } of candidates) {
    const fileViewer = fileRow.locator(".file-content-viewer").first();

    if (!(await fileViewer.isVisible().catch(() => false))) {
      await previewButton.click();
      await waitForNoLoading(fileRow);
      await fileViewer.waitFor({ state: "visible", timeout: 10000 });
      await waitForNoLoading(fileViewer);
    }

    const selectedRange = await selectLineRange(fileViewer, PREFERRED_RANGE).catch(() => null);
    if (!selectedRange) continue;

    const rangeText = formatLineRange(selectedRange);
    const rangeShareButton = fileRow
      .locator("button.dir-item-icon-btn")
      .filter({
        has: page.locator(".material-symbols-outlined", {
          hasText: /^share$/,
        }),
      })
      .first();

    const rangeShareVisible = await rangeShareButton
      .waitFor({ state: "visible", timeout: 10000 })
      .then(() => true)
      .catch(() => false);

    if (!rangeShareVisible) continue;

    const rangeShareTitle = await rangeShareButton.getAttribute("title");
    if (rangeShareTitle !== `Share lines ${rangeText}`) continue;

    await waitForMaterialIcons(page);
    await outputStepRow.scrollIntoViewIfNeeded();
    await page.waitForTimeout(300);

    await withHoveredLocator(
      page,
      rangeShareButton,
      async () => {
        await screenshotLocator(
          output,
          "output-file-line-range-share.png",
          outputStepRow,
        );
      },
      "line-range share button",
      session.warn,
    );
    return;
  }

  session.warn("Could not select and share a line range in any visible plain text output file.");
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
