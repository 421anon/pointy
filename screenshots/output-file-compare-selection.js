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
  const setup = await openOutputFilesForComparison(session, 1);
  if (!setup) return;

  try {
    const firstFile = setup.candidates[0];
    await firstFile.compareButton.click();

    const banner = page.locator(".compare-banner").first();
    const bannerVisible = await banner
      .waitFor({ state: "visible", timeout: 10000 })
      .then(() => true)
      .catch(() => false);

    if (!bannerVisible) {
      session.warn("Compare selection banner did not become visible.");
      return;
    }

    await screenshotLocator(output, "output-file-compare-selection.png", banner);
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
