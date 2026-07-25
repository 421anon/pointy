import { Viewer } from "molstar/lib/apps/viewer/app";
import "molstar/lib/mol-plugin-ui/skin/light.scss";

class MolstarViewer extends HTMLElement {
  constructor() {
    super();
    this._viewer = null;
    this._initialized = false;
    this._generation = 0;
  }

  connectedCallback() {
    if (this._initialized) return;
    this._initialized = true;
    const generation = ++this._generation;

    const container = document.createElement("div");
    container.style.width = "100%";
    container.style.height = "100%";
    this.appendChild(container);

    this._initViewer(container, generation);
  }

  async _initViewer(container, generation) {
    try {
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
        await viewer.loadStructureFromUrl(src, "pdb", false);
      }
    } catch (_err) {
      if (generation === this._generation && this._viewer) {
        this._viewer.dispose();
        this._viewer = null;
      }

      if (generation === this._generation && this.isConnected) {
        this.innerHTML = "";
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
  }
}

if (!customElements.get("molstar-viewer")) {
  customElements.define("molstar-viewer", MolstarViewer);
}
