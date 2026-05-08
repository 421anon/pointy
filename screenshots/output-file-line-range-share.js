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
} = require("./lib/helpers");

const PREFERRED_RANGE = { from: 6, to: 9 };

function formatLineRange({ from, to }) {
  return from === to ? String(from) : `${from}-${to}`;
}

async function selectLineRange(fileViewer, preferredRange) {
  const fileContent = fileViewer.locator(".file-content").first();
  await fileContent.waitFor({ state: "visible", timeout: 10000 });

  return fileContent.evaluate((viewer, range) => {
    const lineNodes = Array.from(viewer.querySelectorAll(".file-line[data-line]"));
    if (lineNodes.length === 0) return null;

    const hasPreferredRange = lineNodes.length >= range.to;
    const from = hasPreferredRange ? range.from : 1;
    const to = hasPreferredRange ? range.to : Math.min(lineNodes.length, 3);
    if (from > to) return null;

    const startLine = viewer.querySelector(`[data-line="${from}"] .file-line-content`);
    const endLine = viewer.querySelector(`[data-line="${to}"] .file-line-content`);
    if (!startLine || !endLine) return null;

    const startNode = startLine.firstChild || startLine;
    const endNode = endLine.firstChild || endLine;
    const endOffset =
      endNode.nodeType === Node.TEXT_NODE
        ? endNode.textContent.length
        : endNode.childNodes.length;

    const domRange = document.createRange();
    domRange.setStart(startNode, 0);
    domRange.setEnd(endNode, endOffset);

    const selection = window.getSelection();
    if (!selection) return null;
    selection.removeAllRanges();
    selection.addRange(domRange);

    viewer.dispatchEvent(
      new MouseEvent("mouseup", { bubbles: true, cancelable: true }),
    );

    return { from, to };
  }, preferredRange);
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
  const rangeBanner = fileViewer
    .locator(".range-share-banner")
    .filter({ hasText: `Lines ${rangeText} selected` })
    .first();

  const bannerVisible = await rangeBanner
    .waitFor({ state: "visible", timeout: 10000 })
    .then(() => true)
    .catch(() => false);

  if (!bannerVisible) {
    session.warn(`Line-range share banner did not appear for lines ${rangeText}.`);
    return;
  }

  await rangeBanner.locator(".range-share-btn").first().hover();
  await waitForMaterialIcons(page);
  await outputStepRow.scrollIntoViewIfNeeded();
  await page.waitForTimeout(300);

  await screenshotLocator(
    output,
    "output-file-line-range-share.png",
    outputStepRow,
  );
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
