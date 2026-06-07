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

const RUN_BUTTON_SELECTOR =
  '.table[id^="table-"]:not(#table-projects) .table-record button.icon-btn[title="Run"]';
const STOP_BUTTON_SELECTOR =
  '.table[id^="table-"]:not(#table-projects) .table-record button.icon-btn[title="Stop"]';

async function waitForVisibleStopButton(page, timeoutMs) {
  const deadline = Date.now() + timeoutMs;

  while (Date.now() < deadline) {
    const stopButton = await firstVisibleLocator(page.locator(STOP_BUTTON_SELECTOR));
    if (stopButton) return stopButton;
    await page.waitForTimeout(100);
  }

  return firstVisibleLocator(page.locator(STOP_BUTTON_SELECTOR));
}

async function captureStopHeader(page, output, stopButton, warn) {
  const header = stopButton.locator(
    'xpath=ancestor::*[contains(concat(" ", normalize-space(@class), " "), " table-record-header ")][1]',
  );

  await withHoveredLocator(
    page,
    stopButton,
    async () => screenshotLocator(output, "step-derivation-stop-header.png", header),
    "derivation stop button",
    warn,
  );
}

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const alreadyRunning = await firstVisibleLocator(page.locator(STOP_BUTTON_SELECTOR));
  if (alreadyRunning) {
    await captureStopHeader(page, output, alreadyRunning, session.warn);
    return;
  }

  const runButtons = page.locator(RUN_BUTTON_SELECTOR);
  const runButtonCount = await runButtons.count();

  for (let i = 0; i < runButtonCount; i++) {
    const runButton = runButtons.nth(i);
    if (!(await runButton.isVisible().catch(() => false))) continue;

    await runButton.click();

    const stopButton = await waitForVisibleStopButton(page, 1500);
    if (stopButton) {
      await captureStopHeader(page, output, stopButton, session.warn);
      return;
    }
  }

  session.warn("Stop button did not appear after clicking any visible Run button.");
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
