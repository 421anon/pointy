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

  const firstStepRow = page
    .locator('.table-record[id]:has(button.icon-btn[title="Edit"])')
    .first();
  if (await firstStepRow.count()) {
    await screenshotLocator(
      output,
      "steps-visibility-button.png",
      firstStepRow
        .locator(
          'button.icon-btn[title="Hide"], button.icon-btn[title="Show"]',
        )
        .first(),
    );
  } else {
    session.warn("No step rows found with Edit button.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}