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

  const projectsTable = page.locator("#table-projects").first();
  const firstProjectRecord = await prepareProjectsPage(session);

  const firstActionBtn = firstProjectRecord
    .locator(".table-record-actions .icon-btn");

  await withHoveredLocator(
    page,
    firstActionBtn,
    async () => screenshotLocator(output, "projects-home.png", projectsTable),
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