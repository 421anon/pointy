#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const path = require("path");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  withHoveredLocator,
  findVisibleClonePair,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;

  await prepareProjectPage(session);

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
        } else {
          await screenshotLocator(
            output,
            "step-clone-button.png",
            sourceStepRow,
          );
        }
      },
      "clone button for a visible step row",
      session.warn,
    );
  } else {
    session.warn("No visible step row with Clone and visible cloned counterpart was found.");
  }

}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
