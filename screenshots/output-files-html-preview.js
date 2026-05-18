#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  screenshotLocator,
  waitForApp,
  waitForNoLoading,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output, baseUrl } = session;
  const projectId = 1;
  const stepId = 105;
  const htmlFileName = "chim_R1_fastqc.html";

  session.location = "project";
  await page.goto(
    `${baseUrl}/project/${projectId}?hi=${stepId}/${htmlFileName}`,
    { waitUntil: "load" },
  );
  await waitForApp(page);

  const outputStepRow = page.locator(`.table-record[id="${stepId}"]`).first();
  await outputStepRow.waitFor({ state: "visible", timeout: 30000 });
  await waitForNoLoading(outputStepRow);

  const htmlFileRow = outputStepRow
    .locator(".directory-file-container")
    .filter({
      has: page.locator(".file-name", { hasText: new RegExp(`^${htmlFileName}$`) }),
    })
    .first();
  await htmlFileRow.waitFor({ state: "visible", timeout: 30000 });

  const fileViewer = htmlFileRow.locator(".file-content-viewer").first();
  await fileViewer.waitFor({ state: "visible", timeout: 30000 });
  await waitForNoLoading(fileViewer);

  const previewButton = htmlFileRow
    .locator("button.dir-item-icon-btn")
    .filter({
      has: page.locator(".material-symbols-outlined", {
        hasText: /^visibility(_off)?$/,
      }),
    })
    .first();

  const zoomOutBtn = htmlFileRow.locator("button.iframe-zoom-btn.zoom-out").first();
  if (await zoomOutBtn.isVisible().catch(() => false)) {
    await zoomOutBtn.click();
    await page.waitForTimeout(200);
    await zoomOutBtn.click();
    await page.waitForTimeout(200);
    await zoomOutBtn.click();
  }

  await outputStepRow.scrollIntoViewIfNeeded();
  await page.waitForTimeout(300);

  await previewButton.hover();
  await page.waitForTimeout(100);
  await screenshotLocator(output, "output-files-html-preview.png", outputStepRow);
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
