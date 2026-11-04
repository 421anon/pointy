{ stdenv, sourcey, openapi-json, screenshots }:

stdenv.mkDerivation {
  pname = "docs";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ sourcey ];

  installPhase = ''
    rm -rf pages/screenshots
    cp -r ${screenshots} pages/screenshots
    cp ${openapi-json} pages/openapi.json
    sourcey build --config pages --output site --quiet
    cp ${openapi-json} site/openapi.json

    substituteInPlace site/index.html \
      --replace-fail '</body>' '<script src="javascripts/posthog.js"></script><script src="javascripts/theme-screenshots.js" defer></script></body>'

    for html in site/*/index.html; do
      [ -f "$html" ] || continue
      substituteInPlace "$html" \
        --replace-fail '</body>' '<script src="../javascripts/posthog.js"></script><script src="../javascripts/theme-screenshots.js" defer></script></body>'
    done

    mv site $out
  '';
}
