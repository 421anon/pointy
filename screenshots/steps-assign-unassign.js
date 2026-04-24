#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  findVisibleStepRowWithButton,
  prepareProjectPage,
  runStandalone,
  screenshotLocator,
  withHoveredLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const { stepRow, button: removeBtn } = await findVisibleStepRowWithButton(
    page,
    "Remove",
  );

  if (stepRow && removeBtn) {
    const stepHeader = stepRow.locator(".table-record-header").first();
    await withHoveredLocator(
      page,
      removeBtn,
      async () =>
        screenshotLocator(output, "steps-assign-unassign.png", stepHeader),
      "remove button for a visible step row",
      session.warn,
    );
  } else {
    session.warn("No visible step row exposed a Remove button.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
