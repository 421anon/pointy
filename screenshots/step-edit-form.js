#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshot,
  waitForMaterialIcons,
  findVisibleStepRowWithButton,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;

  await prepareProjectPage(session);

  const { button: editStepBtn } = await findVisibleStepRowWithButton(page, "Edit");

  if (editStepBtn) {
    await editStepBtn.click();
    await page.waitForTimeout(500);
    await waitForMaterialIcons(page);
    await screenshot(page, output, "step-edit-form.png");
  } else {
    session.warn("No visible step row exposed an Edit button.");
  }

}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
