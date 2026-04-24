#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const path = require("path");
const {
  findVisibleStepRowWithButton,
  prepareProjectPage,
  runStandalone,
  screenshot,
  withHoveredLocator,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const { stepRow, button: inspectBtn } = await findVisibleStepRowWithButton(
    page,
    "Inspect Parameters",
  );

  if (stepRow && inspectBtn) {
    await stepRow.scrollIntoViewIfNeeded();
    await page.waitForTimeout(300);

    await withHoveredLocator(
      page,
      inspectBtn,
      async (hoveredInspectBtn) => {
        await hoveredInspectBtn.click();
        await page.waitForTimeout(500);

        const inspectForm = page.locator(".table-form-wrapper").first();
        await inspectForm
          .waitFor({ state: "visible", timeout: 10000 })
          .catch(() => {});

        await hoveredInspectBtn.hover();
        await page.waitForTimeout(100);

        const headerBox = await stepRow
          .locator(".table-record-header")
          .first()
          .boundingBox();
        const formBox = await inspectForm.boundingBox();

        if (headerBox && formBox) {
          const x = Math.min(headerBox.x, formBox.x);
          const y = headerBox.y;
          const width =
            Math.max(
              headerBox.x + headerBox.width,
              formBox.x + formBox.width,
            ) - x;
          const height = formBox.y + formBox.height - y;
          const filePath = path.join(output, "step-inspect-form.png");
          await page.screenshot({
            path: filePath,
            clip: { x, y, width, height },
          });
        } else {
          await screenshot(page, output, "step-inspect-form.png");
        }
      },
      "inspect button for a visible shareable step",
      session.warn,
    );
  } else {
    session.warn("No visible step row exposed an Inspect Parameters button.");
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
