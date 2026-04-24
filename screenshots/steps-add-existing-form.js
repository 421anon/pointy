#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  clickIfExists,
  waitForMaterialIcons,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const addStepBtn = await page.$(
    '.table[id^="table-"]:not(#table-projects) .table-header-controls button.icon-btn',
  );
  if (addStepBtn) {
    await addStepBtn.click();
    await page.waitForTimeout(500);
    await waitForMaterialIcons(page);

    if (
      await clickIfExists(
        page,
        'input[type="radio"][name^="addMode"]:nth-of-type(2)',
      )
    ) {
      await page.waitForTimeout(300);
      await waitForMaterialIcons(page);
    } else {
      await clickIfExists(
        page,
        '.form-mode-selector label:nth-of-type(2) input[type="radio"]',
      );
      await page.waitForTimeout(300);
      await waitForMaterialIcons(page);
    }

    const addExistingForm = page.locator(".table-form-wrapper .form").first();
    if (await addExistingForm.count()) {
      await screenshotLocator(
        output,
        "steps-add-existing-form.png",
        addExistingForm,
      );
    } else {
      session.warn("Add existing form not found.");
    }
  } else {
    session.warn("Add step button not found.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}