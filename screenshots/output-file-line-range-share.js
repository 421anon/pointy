#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  clickFirstVisible,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  findPreviewableFileInSection,
  findVisibleStepRowWithButton,
  waitForMaterialIcons,
  withHoveredLocator,
} = require("./lib/helpers");

const PREFERRED_RANGE = { from: 6, to: 9 };

function formatLineRange({ from, to }) {
  return from === to ? String(from) : `${from}-${to}`;
}

async function selectLineRange(fileViewer, preferredRange) {
  const fileContent = fileViewer.locator(".file-content").first();
  await fileContent.waitFor({ state: "visible", timeout: 10000 });

  const lineRows = fileContent.locator(".file-line");
  const lineCount = await lineRows.count();
  if (lineCount === 0) return null;

  const hasPreferredRange = lineCount >= preferredRange.to;
  const from = hasPreferredRange ? preferredRange.from : 1;
  const to = hasPreferredRange ? preferredRange.to : Math.min(lineCount, 3);
  if (from > to) return null;

  const startGutter = lineRows.nth(from - 1).locator(".file-line-number").first();
  const endGutter = lineRows.nth(to - 1).locator(".file-line-number").first();
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

  const highlightedCount = await fileContent.locator(".file-line.highlighted").count();
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

  const previewableOutputFile = await findPreviewableFileInSection(outputSection, {
    preferredNames: [/^Log\.final\.out$/],
    preferNonHtml: true,
  });

  if (!previewableOutputFile) {
    session.warn("No previewable output file row was visible for line-range sharing.");
    return;
  }

  const { fileRow, previewButton } = previewableOutputFile;
  const fileViewer = fileRow.locator(".file-content-viewer").first();

  if (!(await fileViewer.isVisible().catch(() => false))) {
    await previewButton.click();
    await waitForNoLoading(fileRow);
    await fileViewer.waitFor({ state: "visible", timeout: 10000 });
    await waitForNoLoading(fileViewer);
  }

  const selectedRange = await selectLineRange(fileViewer, PREFERRED_RANGE);
  if (!selectedRange) {
    session.warn("Could not select a line range in the previewed output file.");
    return;
  }

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

  if (!rangeShareVisible) {
    session.warn(`Line-range share button did not appear for lines ${rangeText}.`);
    return;
  }

  const rangeShareTitle = await rangeShareButton.getAttribute("title");
  if (rangeShareTitle !== `Share lines ${rangeText}`) {
    session.warn(`Line-range share button did not target lines ${rangeText}.`);
    return;
  }

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
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
