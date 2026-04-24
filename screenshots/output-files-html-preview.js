#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  clickFirstVisible,
  withHoveredLocator,
  waitForNoLoading,
  waitForDirectoryContents,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output } = session;
  await prepareProjectPage(session);

  const outputStepRow = page
    .locator('.table-record')
    .filter({
      has: page.locator(".table-record-id", { hasText: /^105$/ }),
    })
    .filter({
      has: page.locator('button.icon-btn[title="Browse output files"]'),
    })
    .first();
  const browseBtn = outputStepRow
    .locator('button.icon-btn[title="Browse output files"]')
    .first();

  if (await outputStepRow.isVisible().catch(() => false)) {
    const outputSection = outputStepRow.locator(".output-files-section").first();

    if (!(await outputSection.isVisible().catch(() => false))) {
      await clickFirstVisible(browseBtn);
    }

    await waitForNoLoading(outputStepRow);
    await outputSection.waitFor({ state: "visible", timeout: 30000 });
    await waitForDirectoryContents(outputSection);

    const chimR1Row = outputSection
      .locator(".directory-file-container")
      .filter({
        has: page.locator(".file-name", { hasText: /^chim_R1_fastqc\.html$/ }),
      })
      .first();

    await chimR1Row.waitFor({ state: "visible", timeout: 10000 });

    const previewBtn = chimR1Row
      .locator("button.dir-item-icon-btn")
      .filter({
        has: page.locator(".material-symbols-outlined", {
          hasText: /^visibility(_off)?$/ ,
        }),
      })
      .first();

    await previewBtn.waitFor({ state: "visible", timeout: 10000 });

    const fileViewer = chimR1Row.locator(".file-content-viewer").first();
    if (!(await fileViewer.isVisible().catch(() => false))) {
      await previewBtn.click();
      await waitForNoLoading(chimR1Row);
      await fileViewer.waitFor({ state: "visible", timeout: 10000 });
      await waitForNoLoading(fileViewer);

      const zoomOutBtn = chimR1Row.locator("button.iframe-zoom-btn.zoom-out").first();
      const zoomOutVisible = await zoomOutBtn
        .waitFor({ state: "visible", timeout: 5000 })
        .then(() => true)
        .catch(() => false);

      if (zoomOutVisible) {
        await zoomOutBtn.click();
        await page.waitForTimeout(200);
        await zoomOutBtn.click();
        await page.waitForTimeout(200);
        await zoomOutBtn.click();
        await page.waitForTimeout(300);
      }
    }

    await outputStepRow.scrollIntoViewIfNeeded();
    await page.waitForTimeout(300);

    await withHoveredLocator(
      page,
      previewBtn,
      async (hoveredBtn) => {
        await hoveredBtn.hover();
        await page.waitForTimeout(100);
        await screenshotLocator(output, "output-files-html-preview.png", outputStepRow);
      },
      "preview button",
      session.warn,
    );
  } else {
    session.warn("Step 105 did not expose a visible Browse output files button.");
  }

}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
