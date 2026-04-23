#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const fs = require("fs");
const path = require("path");
const {
  parseArgs,
  screenshotLocator,
  withHoveredLocator,
  waitForBackend,
  waitForApp,
  waitForProjectRows,
  waitForStepStatusEvent,
  createContextWithStepTracking,
  findVisibleClonePair,
} = require("./lib/helpers");

async function main() {
  const { output = path.join(__dirname, "../docs/pages/screenshots"), url: baseUrl = "http://localhost" } =
    parseArgs(process.argv.slice(2));

  fs.mkdirSync(output, { recursive: true });

  const browser = await chromium.launch({
    headless: true,
    args: [
      "--no-sandbox",
      "--disable-setuid-sandbox",
      "--disable-dev-shm-usage",
    ],
  });

  const context = await createContextWithStepTracking(browser);
  const page = await context.newPage();

  await waitForBackend(page, baseUrl);
  await page.goto(`${baseUrl}/`, { waitUntil: "load" });
  await waitForApp(page);

  const firstProjectRow = await waitForProjectRows(page);
  await page.evaluate(() => {
    window.__pointyStepStatusEventCount = 0;
    window.__pointyLastStepStatusEventType = null;
  });
  await firstProjectRow.click();
  await waitForApp(page);
  await waitForStepStatusEvent(page);

  const { sourceStepRow, cloneStepRow, cloneButton } = await findVisibleClonePair(
    page,
  );

  if (sourceStepRow && cloneStepRow && cloneButton) {
    await withHoveredLocator(
      page,
      cloneButton,
      async () => {
        const sourceBox = await sourceStepRow.boundingBox();
        const cloneBox = await cloneStepRow.boundingBox();
        if (sourceBox && cloneBox) {
          const x = Math.min(sourceBox.x, cloneBox.x);
          const y = sourceBox.y;
          const width =
            Math.max(
              sourceBox.x + sourceBox.width,
              cloneBox.x + cloneBox.width,
            ) - x;
          const height = cloneBox.y + cloneBox.height - y;
          const filePath = path.join(output, "step-clone-button.png");
          await page.screenshot({
            path: filePath,
            clip: { x, y, width, height },
          });
          console.log(`Saved ${filePath}`);
        } else {
          await screenshotLocator(
            output,
            "step-clone-button.png",
            sourceStepRow,
          );
        }
      },
      "clone button for a visible step row",
    );
  } else {
    console.warn(
      "No visible step row with Clone and visible cloned counterpart was found.",
    );
  }

  await browser.close();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
