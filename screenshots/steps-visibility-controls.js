#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const firstStepTable = page
    .locator('.table[id^="table-"]:not(#table-projects)')
    .first();
  if (await firstStepTable.count()) {
    await screenshotLocator(
      output,
      "steps-visibility-controls.png",
      firstStepTable.locator(".table-header").first(),
    );
  } else {
    session.warn("No step table found in project view.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}