#!/usr/bin/env node
"use strict";

const fs = require("fs");
const { spawn } = require("child_process");
const path = require("path");

// List of all screenshot generator scripts
const screenshotScripts = [
  // Projects
  "projects-home.js",
  "projects-header-controls.js",
  "project-row-actions.js",
  "project-delete-button.js",
  "project-create-form.js",
  "project-view.js",

  // Steps overview
  "steps-search-box.js",
  "steps-visibility-controls.js",
  "steps-visibility-button.js",
  "steps-reorder-handle.js",
  "steps-add-existing-form.js",
  "steps-assign-unassign.js",

  // Step types
  "step-file-upload-header.js",
  "step-derivation-header.js",
  "step-derivation-stop-header.js",

  // Step actions
  "step-inspect-form.js",
  "step-clone-button.js",
  "step-edit-form.js",

  // Files
  "step-source-files-section.js",
  "output-file-share-button.js",
  "output-files-browser.js",
  "output-files-html-preview.js",
];

function resolveScriptsDir() {
  const candidates = [
    process.env.SCREENSHOTS_SCRIPTS_DIR,
    path.join(process.cwd(), "screenshots"),
    __dirname,
  ].filter(Boolean);

  for (const candidate of candidates) {
    if (fs.existsSync(path.join(candidate, screenshotScripts[0]))) {
      return candidate;
    }
  }

  throw new Error(
    `Could not locate screenshot scripts directory. Tried: ${candidates.join(", ")}`,
  );
}

function parseRunnerArgs(args) {
  let output = path.join(__dirname, "../docs/pages/screenshots");
  let themes = [];

  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--output" && args[i + 1]) {
      output = args[++i];
    } else if (args[i] === "--theme" && args[i + 1]) {
      themes = args[++i]
        .split(",")
        .map((theme) => theme.trim().toLowerCase())
        .filter(Boolean);
    }
  }

  if (themes.length === 0 && process.env.SCREENSHOT_THEMES) {
    themes = process.env.SCREENSHOT_THEMES
      .split(/[\s,]+/)
      .map((theme) => theme.trim().toLowerCase())
      .filter(Boolean);
  }

  themes = [...new Set(themes)].filter(
    (theme) => theme === "light" || theme === "dark",
  );

  if (themes.length === 0) {
    themes = ["light", "dark"];
  }

  return { output, themes };
}

function runScript(scriptPath, args, extraEnv = {}) {
  return new Promise((resolve, reject) => {
    console.log(`\n${"=".repeat(80)}`);
    console.log(`Running: ${path.basename(scriptPath)}`);
    console.log("=".repeat(80));

    const child = spawn("node", [scriptPath, ...args], {
      stdio: ["ignore", "pipe", "pipe"],
      env: { ...process.env, ...extraEnv },
    });

    let stderrOutput = "";

    child.stdout.on("data", (chunk) => {
      process.stdout.write(chunk);
    });

    child.stderr.on("data", (chunk) => {
      stderrOutput += chunk.toString();
      process.stderr.write(chunk);
    });

    child.on("error", (err) => {
      console.error(`Failed to start ${scriptPath}:`, err);
      reject(err);
    });

    child.on("exit", (code) => {
      if (code === 0) {
        if (stderrOutput.trim().length > 0) {
          console.error(`✗ Failed: ${path.basename(scriptPath)} emitted warnings on stderr`);
          reject(new Error("Script emitted warnings"));
        } else {
          console.log(`✓ Completed: ${path.basename(scriptPath)}`);
          resolve();
        }
      } else {
        console.error(`✗ Failed: ${path.basename(scriptPath)} (exit code ${code})`);
        reject(new Error(`Script exited with code ${code}`));
      }
    });
  });
}

async function main() {
  const args = process.argv.slice(2);
  const { output, themes } = parseRunnerArgs(args);
  const backendReadyMarker = path.join(output, ".backend-ready");

  const scriptsDir = resolveScriptsDir();
  console.log(`Using scripts directory: ${scriptsDir}`);

  console.log("Running all screenshot generation scripts...");
  console.log(`Total scripts: ${screenshotScripts.length}`);
  console.log(`Themes: ${themes.join(", ")}`);
  console.log(`Output root: ${output}`);
  console.log(`Args: ${args.join(" ")}`);

  fs.rmSync(backendReadyMarker, { force: true });

  let successCount = 0;
  let failureCount = 0;
  const totalRuns = screenshotScripts.length * themes.length;

  for (const theme of themes) {
    const themeOutput = path.join(output, theme);
    fs.rmSync(themeOutput, { recursive: true, force: true });
    fs.mkdirSync(themeOutput, { recursive: true });

    console.log(`\n${"#".repeat(80)}`);
    console.log(`Generating ${theme} screenshots in ${themeOutput}`);
    console.log("#".repeat(80));

    for (const scriptName of screenshotScripts) {
      const scriptPath = path.join(scriptsDir, scriptName);
      try {
        await runScript(
          scriptPath,
          [...args, "--output", themeOutput, "--theme", theme],
          {
            SCREENSHOT_THEME: theme,
            SCREENSHOT_BACKEND_READY_FILE: backendReadyMarker,
          },
        );
        successCount++;
      } catch (err) {
        console.error(`Error running ${scriptName} for ${theme}:`, err.message);
        failureCount++;
        // Continue with other screenshots even if one fails
      }
    }
  }

  console.log(`\n${"=".repeat(80)}`);
  console.log("Screenshot generation complete!");
  console.log(`Success: ${successCount}/${totalRuns}`);
  console.log(`Failures: ${failureCount}/${totalRuns}`);
  console.log("=".repeat(80));

  if (failureCount > 0) {
    process.exit(1);
  }
}

main().catch((err) => {
  console.error("Fatal error:", err);
  process.exit(1);
});
