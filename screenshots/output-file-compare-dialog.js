#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const { runStandalone, screenshotLocator } = require("./lib/helpers");
const {
  clearCompareUi,
  openOutputFilesForComparison,
} = require("./lib/file-compare");

async function capture(session) {
  const { page, output } = session;
  const setup = await openOutputFilesForComparison(session, 2);
  if (!setup) return;

  try {
    const [leftFile, rightFile] = setup.candidates;

    await leftFile.compareButton.click();
    await page.locator(".compare-banner").first().waitFor({
      state: "visible",
      timeout: 10000,
    });

    await rightFile.compareButton.click();

    const dialog = page.locator("#compare-dialog[open]").first();
    const dialogVisible = await dialog
      .waitFor({ state: "visible", timeout: 10000 })
      .then(() => true)
      .catch(() => false);

    if (!dialogVisible) {
      session.warn("Compare dialog did not become visible.");
      return;
    }

    const loaded = await page
      .waitForFunction(
        () => {
          const activeDialog = document.querySelector("#compare-dialog[open]");
          return (
            activeDialog &&
            !activeDialog.querySelector(".compare-loading") &&
            !activeDialog.querySelector(".loading-overlay")
          );
        },
        { timeout: 30000 },
      )
      .then(() => true)
      .catch(() => false);

    if (!loaded) {
      session.warn("Compare dialog still showed a loading state before capture.");
    }

    await page.waitForTimeout(200);
    await screenshotLocator(output, "output-file-compare-dialog.png", dialog);
  } finally {
    await clearCompareUi(page);
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
