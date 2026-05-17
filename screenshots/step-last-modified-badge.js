#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  firstVisibleLocator,
  showTitleAsTooltip,
  waitForMaterialIcons,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const badge = await firstVisibleLocator(page.locator(".table-record-mtime"));
  if (!badge) {
    session.warn("No last-modified badge is visible in the fixture project.");
    return;
  }

  const row = badge.locator("xpath=ancestor::*[contains(concat(' ', normalize-space(@class), ' '), ' table-record ')][1]");
  const cleanup = await showTitleAsTooltip(page, badge);
  await waitForMaterialIcons(page);

  try {
    await screenshotLocator(output, "step-last-modified-badge.png", row);
  } finally {
    await cleanup();
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
