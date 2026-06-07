"use strict";

const {
  prepareProjectPage,
  waitForNoLoading,
  waitForDirectoryContents,
  expandAllVisibleFolders,
  firstVisibleLocator,
  findVisibleStepRowWithButton,
} = require("./helpers");

async function directoryFileName(fileRow) {
  return fileRow.locator(".file-name").first().evaluate((el) => {
    return (el.textContent || "").replace(/\s+/g, " ").trim();
  });
}

async function compareButtonFor(page, fileRow) {
  return firstVisibleLocator(
    fileRow.locator("button.dir-item-icon-btn").filter({
      has: page.locator(".material-symbols-outlined", {
        hasText: /^compare_arrows$/,
      }),
    }),
  );
}

function compareCandidateRank(candidate) {
  const name = candidate.fileName;
  const preferredNames = [/^hello$/i, /^Log\.final\.out$/i];
  const preferredIndex = preferredNames.findIndex((pattern) => pattern.test(name));

  if (preferredIndex !== -1) {
    return preferredIndex;
  }

  if (/\.(png|jpe?g|gif|webp|html?|pdf)$/i.test(name)) {
    return 100;
  }

  return 10;
}

async function findComparableFiles(page, sectionLocator) {
  const fileRows = sectionLocator.locator(".directory-file-container");
  const count = await fileRows.count();
  const candidates = [];

  for (let i = 0; i < count; i++) {
    const fileRow = fileRows.nth(i);
    if (!(await fileRow.isVisible().catch(() => false))) continue;

    const compareButton = await compareButtonFor(page, fileRow);
    if (!compareButton) continue;

    let fileName = "";
    try {
      fileName = await directoryFileName(fileRow);
    } catch (_err) {
      continue;
    }

    if (!fileName) continue;
    candidates.push({ fileRow, fileName, compareButton, originalIndex: i });
  }

  return candidates.sort((a, b) => {
    const rankDiff = compareCandidateRank(a) - compareCandidateRank(b);
    if (rankDiff !== 0) return rankDiff;
    return a.originalIndex - b.originalIndex;
  });
}

async function openOutputFilesForComparison(session, minCandidates = 1) {
  const { page } = session;

  await prepareProjectPage(session);

  const { stepRow, button: browseBtn } = await findVisibleStepRowWithButton(
    page,
    "Browse output files",
  );

  if (!stepRow || !browseBtn) {
    session.warn("No visible step row exposed a Browse output files button.");
    return null;
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

  const candidates = await findComparableFiles(page, outputSection);
  if (candidates.length < minCandidates) {
    session.warn(
      `Expected at least ${minCandidates} comparable output file(s), found ${candidates.length}.`,
    );
    return null;
  }

  return { stepRow, outputSection, candidates };
}

async function clearCompareUi(page) {
  const dialogClose = page
    .locator('#compare-dialog[open] button[title="Close comparison"]')
    .first();

  if (await dialogClose.isVisible().catch(() => false)) {
    await dialogClose.click();
    await page
      .locator("#compare-dialog")
      .waitFor({ state: "hidden", timeout: 10000 })
      .catch(() => {});
  }

  const bannerCancel = page
    .locator(".compare-banner button", { hasText: /^Cancel$/ })
    .first();

  if (await bannerCancel.isVisible().catch(() => false)) {
    await bannerCancel.click();
    await page
      .locator(".compare-banner")
      .waitFor({ state: "hidden", timeout: 10000 })
      .catch(() => {});
  }
}

module.exports = {
  clearCompareUi,
  openOutputFilesForComparison,
};
