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

  await prepareProjectsPage(session);

  await withHoveredLocator(
    page,
    page.locator("#table-projects .table-header-controls button.icon-btn").first(),
    async () =>
      screenshotLocator(
        output,
        "projects-header-controls.png",
        page.locator("#table-projects .table-header").first(),
      ),
    "add project button",
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