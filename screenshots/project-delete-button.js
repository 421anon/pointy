#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectsPage,
  screenshotLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;

  const firstProjectRecord = await prepareProjectsPage(session);

  await screenshotLocator(
    output,
    "project-delete-button.png",
    firstProjectRecord.locator('button.icon-btn[title="Remove"]').first(),
  );
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}