import { existsSync, readdirSync, readFileSync } from "node:fs";
import { join, resolve } from "node:path";

const args = process.argv.slice(2);
const pagesDir = resolve(args[0] ?? "pages");
const screenshotsFlagIndex = args.indexOf("--screenshots-dir");
const screenshotsDir =
  screenshotsFlagIndex >= 0 ? resolve(args[screenshotsFlagIndex + 1]) : null;

function slugifyHeading(rawHeading) {
  return rawHeading
    .trim()
    .toLowerCase()
    .replace(/`([^`]*)`/g, "$1")
    .replace(/\[([^\]]+)\]\([^)]+\)/g, "$1")
    .replace(/[^a-z0-9\s-]/g, "")
    .replace(/\s+/g, "-")
    .replace(/-+/g, "-")
    .replace(/^-|-$/g, "");
}

function parsePageGroupsSlugs(configText) {
  const match = configText.match(/const PAGE_GROUPS = \[([\s\S]*?)\n\];/);
  if (!match) {
    throw new Error("Could not find PAGE_GROUPS in sourcey.config.ts");
  }
  const slugs = [];
  for (const pagesMatch of match[1].matchAll(/pages:\s*\[([\s\S]*?)\]/g)) {
    for (const slugMatch of pagesMatch[1].matchAll(/"([a-z0-9-]+)"/g)) {
      slugs.push(slugMatch[1]);
    }
  }
  return slugs;
}

const errors = [];
const markdownFiles = readdirSync(pagesDir)
  .filter((name) => name.endsWith(".md"))
  .sort();
const existingFiles = new Set(markdownFiles);

const configText = readFileSync(join(pagesDir, "sourcey.config.ts"), "utf8");
const configuredSlugs = parsePageGroupsSlugs(configText);
const configuredFiles = new Set(configuredSlugs.map((slug) => `${slug}.md`));

for (const file of markdownFiles) {
  if (!configuredFiles.has(file)) {
    errors.push(`Unlisted markdown page not in sourcey.config.ts: ${file}`);
  }
}
for (const file of configuredFiles) {
  if (!existingFiles.has(file)) {
    errors.push(`Page listed in sourcey.config.ts does not exist: ${file}`);
  }
}

const headingsByFile = new Map();
const internalLinks = [];
const images = [];

for (const file of markdownFiles) {
  const text = readFileSync(join(pagesDir, file), "utf8");
  const lines = text.split(/\r?\n/);

  let hasDescription = false;
  if (lines[0]?.trim() === "---") {
    for (let i = 1; i < lines.length; i++) {
      if (lines[i].trim() === "---") break;
      if (lines[i].startsWith("description:")) hasDescription = true;
    }
  }
  if (!hasDescription) {
    errors.push(`${file}: missing frontmatter description`);
  }

  let inCodeFence = false;
  const slugs = new Set();
  const slugCounts = new Map();

  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    const lineNumber = i + 1;
    if (/^\s*```/.test(line)) {
      inCodeFence = !inCodeFence;
      continue;
    }
    if (inCodeFence) continue;

    const headingMatch = line.match(/^#{1,6}\s+(.*)$/);
    if (headingMatch) {
      const baseSlug = slugifyHeading(headingMatch[1]);
      const count = slugCounts.get(baseSlug) ?? 0;
      slugCounts.set(baseSlug, count + 1);
      slugs.add(count === 0 ? baseSlug : `${baseSlug}-${count}`);
    }

    for (const imageMatch of line.matchAll(/!\[([^\]]*)\]\(([^)]+)\)/g)) {
      images.push({
        file,
        lineNumber,
        alt: imageMatch[1],
        src: imageMatch[2],
      });
    }

    for (const linkMatch of line.matchAll(/(?<!!)\[([^\]]+)\]\(([^)]+)\)/g)) {
      internalLinks.push({
        file,
        lineNumber,
        href: linkMatch[2],
      });
    }
  }

  headingsByFile.set(file, slugs);
}

const allowedExternalTargets = new Set([
  "api/",
  "api",
  "/api/",
  "openapi.json",
  "../openapi.json",
]);

for (const { file, lineNumber, href } of internalLinks) {
  if (/^(https?:|mailto:)/.test(href)) continue;

  if (href.startsWith("#")) {
    const anchor = href.slice(1);
    if (!headingsByFile.get(file)?.has(anchor)) {
      errors.push(`${file}:${lineNumber}: broken self anchor ${href}`);
    }
    continue;
  }

  const hashIndex = href.indexOf("#");
  const target = hashIndex >= 0 ? href.slice(0, hashIndex) : href;
  const anchor = hashIndex >= 0 ? href.slice(hashIndex + 1) : null;

  if (allowedExternalTargets.has(target)) continue;

  if (!existingFiles.has(target)) {
    errors.push(`${file}:${lineNumber}: broken link target ${href}`);
  } else if (anchor && !headingsByFile.get(target)?.has(anchor)) {
    errors.push(`${file}:${lineNumber}: broken anchor ${href}`);
  }
}

for (const { file, lineNumber, alt, src } of images) {
  if (!alt.trim()) {
    errors.push(`${file}:${lineNumber}: empty alt text for ${src}`);
  }
  if (screenshotsDir && src.startsWith("screenshots/")) {
    const relScreenshot = src.slice("screenshots/".length);
    const lightPath = join(screenshotsDir, relScreenshot);
    if (!existsSync(lightPath)) {
      errors.push(`${file}:${lineNumber}: missing screenshot ${src}`);
    }
    if (relScreenshot.startsWith("light/")) {
      const darkPath = join(
        screenshotsDir,
        "dark",
        relScreenshot.slice("light/".length),
      );
      if (!existsSync(darkPath)) {
        errors.push(`${file}:${lineNumber}: missing dark screenshot for ${src}`);
      }
    }
  }
}

if (errors.length > 0) {
  for (const error of errors) {
    console.error(error);
  }
  process.exit(1);
}
