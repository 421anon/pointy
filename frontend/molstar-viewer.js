let molstarPromise = null;

function loadMolstar() {
  if (molstarPromise === null) {
    molstarPromise = Promise.all([
      import(/* webpackChunkName: "molstar" */ "molstar/lib/apps/viewer/app"),
      import(/* webpackChunkName: "molstar" */ "molstar/lib/mol-plugin-ui/skin/light.scss"),
    ]).then(([{ Viewer }]) => Viewer);
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

  async _initViewer(container, generation) {
    try {
      const Viewer = await loadMolstar();
      if (generation !== this._generation || !this.isConnected) return;

      const viewer = await Viewer.create(container, {
        layoutIsExpanded: false,
        layoutShowLog: false,
        layoutShowLeftPanel: false,
        viewportShowExpand: false,
        viewportShowToggleFullscreen: false,
        viewportShowAnimation: false,
      });

      if (generation !== this._generation || !this.isConnected) {
        viewer.dispose();
        return;
      }

      this._viewer = viewer;

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
