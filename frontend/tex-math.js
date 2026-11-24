import temml from "temml";
import "temml/dist/Temml-Local.css";
// Fontsource labels this file "latin", but it holds the whole STIX Two Math font, MATH table included.
import stixTwoMathUrl from "@fontsource/stix-two-math/files/stix-two-math-latin-400-normal.woff2";

document.fonts.add(new FontFace("STIX Two Math", `url(${stixTwoMathUrl}) format("woff2")`));

class TexMath extends HTMLElement {
  static observedAttributes = ["tex", "display"];

  connectedCallback() {
    this.typeset();
  }

  attributeChangedCallback() {
    if (this.isConnected) this.typeset();
  }

  typeset() {
    temml.render(this.getAttribute("tex") ?? "", this, {
      displayMode: this.getAttribute("display") === "block",
      annotate: true,
      throwOnError: false,
    });
  }
}

if (!customElements.get("tex-math")) {
  customElements.define("tex-math", TexMath);
}
