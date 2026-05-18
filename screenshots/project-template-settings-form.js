#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectsPage,
  screenshotLocator,
  firstVisibleLocator,
  waitForMaterialIcons,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  const firstProjectRecord = await prepareProjectsPage(session);

  const editButton = await firstVisibleLocator(
    firstProjectRecord.locator('button.icon-btn[title="Edit"]'),
  );

  if (!editButton) {
    session.warn("Project edit button not found.");
    return;
  }

  await editButton.click();
  await page.waitForTimeout(500);
  await waitForMaterialIcons(page);

  const form = page.locator(".table-form-wrapper .form").first();
  if (await form.isVisible().catch(() => false)) {
    await screenshotLocator(output, "project-template-settings-form.png", form);
  } else {
    session.warn("Project template settings form not found.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
