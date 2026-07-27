import { basicSetup } from "codemirror";
import { EditorState, Compartment } from "@codemirror/state";
import { EditorView } from "@codemirror/view";
import { LanguageDescription, LanguageSupport, StreamLanguage, defaultHighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { languages } from "@codemirror/language-data";
import { shell } from "@codemirror/legacy-modes/mode/shell";
import { oneDarkHighlightStyle } from "@codemirror/theme-one-dark";

function readOnlyExtensions(readOnly) {
  return [EditorState.readOnly.of(readOnly), EditorView.editable.of(!readOnly)];
}

const SHELL_LANGUAGE_NAMES = new Set(["bash", "sh", "shell", "zsh"]);
const SHELL_TOKEN_STYLES = {
  attribute: "atom",
  builtin: "keyword",
  operator: "keyword",
  quote: "string",
};

function shellLanguageSupport() {
  return new LanguageSupport(
    StreamLanguage.define({
      ...shell,
      token(stream, state) {
        const style = shell.token(stream, state);
        return SHELL_TOKEN_STYLES[style] || style;
      },
    })
  );
}

async function loadLanguageSupport(language, filename) {
  if (SHELL_LANGUAGE_NAMES.has(language.toLowerCase())) {
    return shellLanguageSupport();
  }

  const description = language
    ? LanguageDescription.matchLanguageName(languages, language, true)
    : LanguageDescription.matchFilename(languages, filename);
  return description ? await description.load() : null;
}

const LIGHT_HIGHLIGHTING = syntaxHighlighting(defaultHighlightStyle);
const DARK_HIGHLIGHTING = syntaxHighlighting(oneDarkHighlightStyle);

function highlightingExtension() {
  return document.documentElement.getAttribute("data-theme") === "light"
    ? LIGHT_HIGHLIGHTING
    : DARK_HIGHLIGHTING;
}

class CodeEditorElement extends HTMLElement {
  static observedAttributes = ["filename", "language", "readonly"];

  constructor() {
    super();
    this._value = "";
    this._readOnly = false;
    this.languageCompartment = new Compartment();
    this.readOnlyCompartment = new Compartment();
    this.highlightingCompartment = new Compartment();
    this.languageLoadRequest = 0;
    this.suppressInput = false;
    this.themeObserver = new MutationObserver(() => this.configureHighlighting());
  }

  connectedCallback() {
    if (this.view) return;

    if (!this.hasAttribute("role")) {
      this.setAttribute("role", "textbox");
    }
    this.setAttribute("aria-multiline", "true");

    this.view = new EditorView({
      parent: this,
      state: EditorState.create({
        doc: this.value,
        extensions: [
          basicSetup,
          this.highlightingCompartment.of(highlightingExtension()),
          EditorView.lineWrapping,
          this.languageCompartment.of([]),
          this.readOnlyCompartment.of(readOnlyExtensions(this.readOnly)),
          EditorView.updateListener.of((update) => {
            if (!update.docChanged || this.suppressInput) return;

            this._value = update.state.doc.toString();
            this.dispatchEvent(new Event("input", { bubbles: true }));
          }),
        ],
      }),
    });

    this.view.dom.addEventListener("input", (e) => {
      if (e.target.closest(".cm-panel")) e.stopPropagation();
    });

    this.configureLanguage();
    this.themeObserver.observe(document.documentElement, {
      attributes: true,
      attributeFilter: ["data-theme"],
    });
  }

  disconnectedCallback() {
    this.themeObserver.disconnect();
    this.view?.destroy();
    this.view = null;
  }

  get value() {
    return this._value;
  }

  set value(value) {
    const nextValue = value == null ? "" : String(value);
    this._value = nextValue;

    if (!this.view) return;

    const currentValue = this.view.state.doc.toString();
    if (currentValue === nextValue) return;

    this.suppressInput = true;
    try {
      this.view.dispatch({
        changes: { from: 0, to: currentValue.length, insert: nextValue },
      });
    } finally {
      this.suppressInput = false;
    }
  }

  get filename() {
    return this.getAttribute("filename") || "";
  }

  get language() {
    return (this.getAttribute("language") || "").trim();
  }

  set language(language) {
    if (language == null) {
      this.removeAttribute("language");
    } else {
      this.setAttribute("language", String(language));
    }
  }

  get readOnly() {
    return this._readOnly || this.hasAttribute("readonly");
  }

  set readOnly(readOnly) {
    const nextReadOnly =
      readOnly === true || readOnly === "" || readOnly === "true" || readOnly === "readonly";

    if (nextReadOnly) {
      this.setAttribute("readonly", "");
    } else {
      this.removeAttribute("readonly");
    }
  }

  focus() {
    this.view?.focus();
  }

  attributeChangedCallback(name, oldValue, newValue) {
    if (oldValue === newValue) return;

    if (name === "language" || name === "filename") {
      this.configureLanguage();
      return;
    }

    if (name === "readonly") {
      this._readOnly = this.hasAttribute("readonly");
      this.configureReadOnly();
    }
  }

  configureReadOnly() {
    if (!this.view) return;

    this.view.dispatch({
      effects: this.readOnlyCompartment.reconfigure(readOnlyExtensions(this.readOnly)),
    });
  }

  configureHighlighting() {
    if (!this.view) return;

    this.view.dispatch({
      effects: this.highlightingCompartment.reconfigure(highlightingExtension()),
    });
  }

  async configureLanguage() {
    const language = this.language;
    const request = ++this.languageLoadRequest;

    try {
      const languageSupport = await loadLanguageSupport(language, this.filename);
      if (request !== this.languageLoadRequest || !this.view) return;

      this.view.dispatch({
        effects: this.languageCompartment.reconfigure(
          languageSupport ? [languageSupport] : []
        ),
      });
    } catch (err) {
      console.warn(`Could not load CodeMirror language '${language}'`, err);
    }
  }
}

if (!customElements.get("code-editor")) {
  customElements.define("code-editor", CodeEditorElement);
}
