import { readdir } from "node:fs/promises";
import { resolve, relative, extname } from "node:path";

const ASSET_EXTS: Record<string, true> = {
  ".apng": true,
  ".avif": true,
  ".bmp": true,
  ".css": true,
  ".gif": true,
  ".ico": true,
  ".jpeg": true,
  ".jpg": true,
  ".js": true,
  ".json": true,
  ".mjs": true,
  ".mp4": true,
  ".pdf": true,
  ".png": true,
  ".svg": true,
  ".txt": true,
  ".webm": true,
  ".webp": true,
  ".woff": true,
  ".woff2": true,
  ".zip": true,
};

const PAGE_GROUPS = [
  { label: "Overview", pages: ["index"] },
  {
    label: "Usage",
    pages: [
      "projects",
      "organize",
      "search",
      "steps",
      "execution",
      "outputs",
      "reviews",
      "sharing",
      "agent",
      "changesets",
      "chats",
    ],
  },
  {
    label: "Hosting",
    pages: [
      "admin",
      "login",
      "clusters",
      "config",
      "providers",
      "nixos",
      "storage",
      "sync",
      "sandbox",
      "builds",
      "troubleshooting",
      "backups",
      "upgrading",
    ],
  },
  {
    label: "Configuration",
    pages: [
      "user-repo-setup",
      "templates",
      "step-files",
      "presets",
      "resources",
      "extras",
      "linked-data",
      "type-reference",
      "forms",
    ],
  },
  {
    label: "Development",
    pages: [
      "cli-reference",
      "api-guide",
      "streams",
      "files",
      "agent-api",
      "contributing",
    ],
  },
];

interface AdapterPage {
  slug: string;
  file: string;
  sourceRoot: string;
}

interface AdapterGroup {
  label: string;
  pages: AdapterPage[];
}

interface AdapterAsset {
  file: string;
  outputPath: string;
}

interface AdapterResolved {
  kind: "markdown";
  groups: AdapterGroup[];
  assets: AdapterAsset[];
  watchPaths: string[];
}

interface AdapterContext {
  configDir: string;
  resolvePath(path: string): string;
}

async function collectAssets(configDir: string, absDir: string): Promise<AdapterAsset[]> {
  const results: AdapterAsset[] = [];
  try {
    for (const entry of await readdir(absDir, { withFileTypes: true })) {
      if (entry.name.startsWith(".")) continue;
      const fullPath = resolve(absDir, entry.name);
      if (entry.isDirectory()) {
        const nested = await collectAssets(configDir, fullPath);
        results.push(...nested);
      } else if (entry.isFile() && ASSET_EXTS[extname(entry.name)]) {
        results.push({
          file: fullPath,
          outputPath: relative(configDir, fullPath).replace(/\\/g, "/"),
        });
      }
    }
  } catch {
    // Directory does not exist or is unreadable; skip.
  }
  return results;
}

export default {
  name: "Pointy Notebook",
  baseUrl: "/",
  siteUrl: "https://pointy.cloud",
  repo: "https://github.com/421anon/pointy",
  ogImage: "screenshots/light/output-file-line-range-share.png",
  prettyUrls: "slash",
  theme: {
    colors: {
      primary: "#0969da",
      light: "#0969da",
      dark: "#0550ae",
    },
    fonts: {
      sans: "Fira Sans",
      mono: "Fira Mono",
    },
    layout: {
      content: "68rem",
    },
    css: ["stylesheets/extra.css"],
  },

  navbar: {
    links: [
      { type: "github", href: "https://github.com/421anon/pointy", label: "Star Pointy on GitHub" },
    ],
  },
  footer: {
    links: [
      { type: "github", href: "https://github.com/421anon/pointy", label: "GitHub" },
    ],
  },
  navigation: {
    tabs: [
      {
        tab: "Overview",
        slug: "",
        source: {
          name: "markdown",
          async resolve(ctx: AdapterContext): Promise<AdapterResolved> {
            const allPages: AdapterPage[] = [];
            for (const g of PAGE_GROUPS) {
              for (const slug of g.pages) {
                allPages.push({
                  slug,
                  file: ctx.resolvePath(`${slug}.md`),
                  sourceRoot: ctx.configDir,
                });
              }
            }
            const groups: AdapterGroup[] = PAGE_GROUPS.map((g) => ({
              label: g.label,
              pages: allPages.filter((p) => g.pages.includes(p.slug)),
            }));
            const assets: AdapterAsset[] = [
              ...(await collectAssets(ctx.configDir, ctx.resolvePath("screenshots"))),
              ...(await collectAssets(ctx.configDir, ctx.resolvePath("javascripts"))),
            ];
            return {
              kind: "markdown",
              groups,
              assets,
              watchPaths: [
                ctx.resolvePath("screenshots"),
                ctx.resolvePath("javascripts"),
              ],
            };
          },
        },
      },
      {
        tab: "API Reference",
        slug: "api",
        openapi: "./openapi.json",
      },
    ],
  },
};
