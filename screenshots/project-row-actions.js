#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectsPage,
  screenshotLocator,
  withHoveredLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;

  const firstProjectRecord = await prepareProjectsPage(session);

  const firstActionBtn = firstProjectRecord
    .locator(".table-record-actions .icon-btn");

  await withHoveredLocator(
    page,
    firstActionBtn,
    async () =>
      screenshotLocator(output, "project-row-actions.png", firstProjectRecord),
    "project row action button",
    session.warn,
  );
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}