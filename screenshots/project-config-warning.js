#!/usr/bin/env node
"use strict";

const { chromium } = require("playwright-core");
const {
  runStandalone,
  prepareProjectPage,
  screenshotLocator,
  waitForMaterialIcons,
} = require("./lib/helpers");

async function capture(session) {
  const { page, output, baseUrl } = session;
  const projectsRoute = "**/backend/projects**";
  const injectConfigWarning = async (route) => {
    const response = await route.fetch();
    const projects = await response.json();
    const firstProjectKey = Object.keys(projects)[0];

    if (firstProjectKey) {
      projects[firstProjectKey] = {
        ...projects[firstProjectKey],
        validationErrors: [
          "Unknown templates: archived-template. Remove it in the edit form.",
        ],
      };
    }

    await route.fulfill({ response, json: projects });
  };

  await page.route(projectsRoute, injectConfigWarning);

  try {
    session.location = "unknown";
    await prepareProjectPage(session);
    await waitForMaterialIcons(page);

    const warning = page.locator(".project-config-error, .project-config-warning").first();
    if (await warning.isVisible().catch(() => false)) {
      await screenshotLocator(output, "project-config-warning.png", warning);
    } else {
      session.warn("No project configuration warning or validation error is visible in the fixture project.");
    }
  } finally {
    await page.unroute(projectsRoute, injectConfigWarning);
    session.location = "unknown";
    await page.goto(`${baseUrl}/`, { waitUntil: "load" });
  }
}

module.exports = { capture };

if (require.main === module) {
  runStandalone(capture, chromium).catch((err) => {
    console.error(err);
    process.exit(1);
  });
}
