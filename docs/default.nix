{ stdenv, sourcey, openapi-json, screenshots, nodejs, material-symbols }:

let
  materialSymbolsFont = "${material-symbols.webfont}/share/fonts/woff2/MaterialSymbolsOutlined[FILL,GRAD,opsz,wght].woff2";
in
stdenv.mkDerivation {
  pname = "docs";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ sourcey nodejs ];

  installPhase = ''
    rm -rf pages/screenshots
    cp -r ${screenshots} pages/screenshots
    node check-docs.mjs pages --screenshots-dir pages/screenshots
    cp ${openapi-json} pages/openapi.json
    sourcey build --config pages --output site --quiet
    cp ${openapi-json} site/openapi.json

    mkdir -p site/fonts site/stylesheets
    cp "${materialSymbolsFont}" site/fonts/material-symbols-outlined.woff2
    cat > site/stylesheets/material-symbols.css <<'EOF'
    @font-face {
      font-family: "Material Symbols Outlined";
      font-style: normal;
      font-weight: 100 700;
      font-display: block;
      src: url("../fonts/material-symbols-outlined.woff2") format("woff2");
    }
    EOF

    substituteInPlace site/index.html \
      --replace-fail '</head>' '<link rel="stylesheet" href="stylesheets/material-symbols.css"></head>' \
      --replace-fail '</body>' '<script src="javascripts/theme-screenshots.js" defer></script></body>'

    for html in site/*/index.html; do
      [ -f "$html" ] || continue
      substituteInPlace "$html" \
        --replace-fail '</head>' '<link rel="stylesheet" href="../stylesheets/material-symbols.css"></head>' \
        --replace-fail '</body>' '<script src="../javascripts/theme-screenshots.js" defer></script></body>'
    done

    mv site $out
  '';
}
