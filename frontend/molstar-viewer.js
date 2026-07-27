let molstarPromise = null;

function loadMolstar() {
  if (molstarPromise === null) {
    molstarPromise = Promise.all([
      import(/* webpackChunkName: "molstar" */ "molstar/lib/apps/viewer/app"),
      import(/* webpackChunkName: "molstar" */ "molstar/lib/mol-util/color/utils"),
      import(/* webpackChunkName: "molstar" */ "./styles/molstar-dark.scss"),
      import(/* webpackChunkName: "molstar" */ "./styles/molstar-light.scss"),
    ]).then(([{ Viewer }, { decodeColor }]) => ({ Viewer, decodeColor }));
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

  async _initViewer(container, generation) {
    try {
      const { Viewer, decodeColor } = await loadMolstar();
      if (generation !== this._generation || !this.isConnected) return;
      this._decodeColor = decodeColor;

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
      if (generation === this._generation && this._viewer) {
        this._themeObserver?.disconnect();
        this._themeObserver = null;
        this._viewer.dispose();
        this._viewer = null;
      }

      if (generation === this._generation && this.isConnected) {
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
  }
}

if (!customElements.get("molstar-viewer")) {
  customElements.define("molstar-viewer", MolstarViewer);
}
