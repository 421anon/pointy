#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectsPage,
  screenshotLocator,
  waitForMaterialIcons,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;

  await prepareProjectsPage(session);

  const addProjectBtn = await page.$(
    "#table-projects .table-header-controls button.icon-btn",
  );
  if (addProjectBtn) {
    await addProjectBtn.click();
    await page.waitForTimeout(500);
    await waitForMaterialIcons(page);
    await screenshotLocator(
      output,
      "project-create-form.png",
      page.locator(".table-form-wrapper .form").first(),
    );
  } else {
    session.warn("Add project button not found in #table-projects header.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}