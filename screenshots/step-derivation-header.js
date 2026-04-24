#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  firstVisibleLocator,
  prepareProjectPage,
  runStandalone,
  screenshotLocator,
  withHoveredLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const derivationHeader = await firstVisibleLocator(
    page.locator(
      '.table-record[id^="alignment-"] .table-record-header, .table-record:has(button.icon-btn[title="Run"]) .table-record-header',
    ),
  );
  if (derivationHeader) {
    const runButton = derivationHeader.locator(
      'button.icon-btn[title="Run"]',
    );
    await withHoveredLocator(
      page,
      runButton,
      async () =>
        screenshotLocator(output, "step-derivation-header.png", derivationHeader),
      "derivation run button",
      session.warn,
    );
  } else {
    session.warn("No visible derivation step header found.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
