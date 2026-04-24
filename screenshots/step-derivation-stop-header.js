#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  clickFirstVisible,
  firstVisibleLocator,
  prepareProjectPage,
  runStandalone,
  screenshotLocator,
  waitForMaterialIcons,
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

    const clickedRun = await clickFirstVisible(runButton);
    if (clickedRun) {
      await waitForMaterialIcons(page);
      const stopButton = derivationHeader.locator(
        'button.icon-btn[title="Stop"]',
      );
      const stopVisible = await stopButton
        .first()
        .waitFor({ state: "visible", timeout: 10000 })
        .then(() => true)
        .catch(() => false);

      if (stopVisible) {
        await withHoveredLocator(
          page,
          stopButton,
          async () =>
            screenshotLocator(
              output,
              "step-derivation-stop-header.png",
              derivationHeader,
            ),
          "derivation stop button",
          session.warn,
        );
      } else {
        session.warn("Stop button did not appear after clicking Run.");
      }
    } else {
      session.warn("No visible Run button found in derivation header.");
    }
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
