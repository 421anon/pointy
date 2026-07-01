{ stdenv, mkdocs, python313Packages, sourcey, openapiJson }:

stdenv.mkDerivation {
  pname = "docs";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ mkdocs python313Packages.mkdocs-material sourcey ];

  installPhase = ''
    # Render embeddable API reference assets from the spec, then wrap the
    # generated fragment in a MkDocs page so it keeps the documentation chrome.
    cp ${openapiJson} pages/openapi.json
    sourcey build pages/openapi.json --output pages/sourcey --embed --quiet

    substituteInPlace pages/sourcey/search-index.json \
      --replace-fail '"/api.html#' '"#'

    {
      printf '%s\n' '<div hidden>'
      printf '%s\n' '<meta name="sourcey-search" content="../sourcey/search-index.json">'
      printf '%s\n' '</div>'
      printf '%s\n' '<style>'
      printf '%s\n' '@import url("../sourcey/sourcey.css");'
      printf '%s\n' '</style>'
      printf '%s\n' '<div id="sourcey" class="pointy-api-reference">'
      sed \
        -e 's/href="api\.html#/href="#/g' \
        -e 's/href="api\.html"/href="."/g' \
        pages/sourcey/index.html
      printf '%s\n' '</div>'
      printf '%s\n' '<script src="../sourcey/sourcey.js" defer></script>'
    } > pages/api.md

    mkdocs build
    mv site $out
  '';
}
