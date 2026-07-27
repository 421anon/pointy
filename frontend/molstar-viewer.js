let molstarPromise = null;

function loadMolstar() {
  if (molstarPromise === null) {
    molstarPromise = Promise.all([
      import(/* webpackChunkName: "molstar" */ "molstar/lib/apps/viewer/app"),
      import(/* webpackChunkName: "molstar" */ "molstar/lib/mol-util/color/utils"),
      import(
        /* webpackChunkName: "molstar-light" */ "molstar/lib/mol-plugin-ui/skin/light.scss?lazy"
      ),
      import(
        /* webpackChunkName: "molstar-dark" */ "molstar/lib/mol-plugin-ui/skin/dark.scss?lazy"
      ),
    ]).then(([{ Viewer }, { decodeColor }, lightSkin, darkSkin]) => ({
      Viewer,
      decodeColor,
      skins: { light: lightSkin.default, dark: darkSkin.default },
    }));
  }

  return molstarPromise;
}

class MolstarViewer extends HTMLElement {
  constructor() {
    super();
    this._viewer = null;
    this._initialized = false;
    this._generation = 0;
    this._loadingLabel = null;
    this._decodeColor = null;
    this._themeObserver = null;
    this._skins = null;
    this._activeSkin = null;
  }

  connectedCallback() {
    if (this._initialized) return;
    this._initialized = true;
    const generation = ++this._generation;

    const container = document.createElement("div");
    container.className = "molstar-viewer__canvas";

    const loadingIndicator = document.createElement("div");
    loadingIndicator.className = "molstar-viewer__loading";
    loadingIndicator.setAttribute("role", "status");

    const loadingIcon = document.createElement("span");
    loadingIcon.className =
      "molstar-viewer__loading-icon material-symbols-outlined";
    loadingIcon.setAttribute("aria-hidden", "true");
    loadingIcon.textContent = "progress_activity";

    const loadingLabel = document.createElement("span");
    loadingLabel.textContent = "Loading viewer…";
    this._loadingLabel = loadingLabel;

    loadingIndicator.append(loadingIcon, loadingLabel);
    this.append(container, loadingIndicator);

    this._initViewer(container, generation);
  }

  _setLoadingLabel(label) {
    if (this._loadingLabel) this._loadingLabel.textContent = label;
  }

  _hideLoadingIndicator() {
    if (!this._loadingLabel) return;
    this._loadingLabel.parentElement?.remove();
    this._loadingLabel = null;
  }

  _themeColor(name) {
    return getComputedStyle(document.documentElement)
      .getPropertyValue(name)
      .trim();
  }

  _applyTheme() {
    const theme =
      document.documentElement.getAttribute("data-theme") === "light"
        ? "light"
        : "dark";
    const skin = this._skins?.[theme];

    if (skin && skin !== this._activeSkin) {
      this._activeSkin?.unuse();
      skin.use();
      this._activeSkin = skin;
    }

    const backgroundColor = this._decodeColor?.(
      this._themeColor("--bg-primary"),
    );
    if (backgroundColor === undefined) return;

    this._viewer?.plugin.canvas3d?.setProps({
      renderer: { backgroundColor },
    });
  }

  _observeTheme() {
    this._applyTheme();
    this._themeObserver = new MutationObserver(() => this._applyTheme());
    this._themeObserver.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["data-theme"],
    });
  }

  _releaseSkin() {
    this._activeSkin?.unuse();
    this._activeSkin = null;
    this._skins = null;
  }


  async _initViewer(container, generation) {
    try {
      const { Viewer, decodeColor, skins } = await loadMolstar();
      if (generation !== this._generation || !this.isConnected) return;

      this._decodeColor = decodeColor;
      this._skins = skins;
      this._applyTheme();

      const viewer = await Viewer.create(container, {
        layoutIsExpanded: false,
        layoutShowLog: false,
        layoutShowLeftPanel: false,
        viewportShowExpand: false,
        viewportShowToggleFullscreen: false,
        viewportShowAnimation: false,
        viewportBackgroundColor: this._themeColor("--bg-primary"),
      });

      if (generation !== this._generation || !this.isConnected) {
        viewer.dispose();
        return;
      }

      this._viewer = viewer;
      this._observeTheme();

      const src = this.getAttribute("src");
      if (src) {
        this._setLoadingLabel("Loading structure…");
        await viewer.loadStructureFromUrl(src, "pdb", false);
      }

      if (generation === this._generation && this.isConnected) {
        this._hideLoadingIndicator();
      }
    } catch (_err) {
      if (generation !== this._generation) return;

      this._themeObserver?.disconnect();
      this._themeObserver = null;

      if (this._viewer) {
        this._viewer.dispose();
        this._viewer = null;
      }
      this._releaseSkin();

      if (this.isConnected) {
        this.innerHTML = "";
        this._loadingLabel = null;
        const message = document.createElement("div");
        message.textContent = "Failed to load structure";
        message.style.padding = "1rem";
        this.appendChild(message);
      }
    }
  }

  disconnectedCallback() {
    this._generation += 1;
    this._initialized = false;
    this._themeObserver?.disconnect();
    this._themeObserver = null;
    this._decodeColor = null;

    if (this._viewer) {
      this._viewer.dispose();
      this._viewer = null;
    }

    this.innerHTML = "";
    this._loadingLabel = null;
    this._releaseSkin();
  }
}

if (!customElements.get("molstar-viewer")) {
  customElements.define("molstar-viewer", MolstarViewer);
}
