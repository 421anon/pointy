#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  firstVisibleLocator,
  prepareProjectPage,
  runStandalone,
  screenshotLocator,
  withHoveredLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const fileUploadHeader = await firstVisibleLocator(
    page.locator(
      '.table-record[id^="library-"] .table-record-header, .table-record:has(button.icon-btn[title="Upload files"]) .table-record-header',
    ),
  );
  if (fileUploadHeader) {
    await withHoveredLocator(
      page,
      fileUploadHeader.locator('button.icon-btn[title="Upload files"]'),
      async () =>
        screenshotLocator(output, "step-file-upload-header.png", fileUploadHeader),
      "file upload button",
      session.warn,
    );
  } else {
    session.warn("No visible file-upload step header found.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
