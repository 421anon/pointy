{ stdenv, sourcey, openapiJson }:

stdenv.mkDerivation {
  pname = "docs";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ sourcey ];

  installPhase = ''
    cp ${openapiJson} pages/openapi.json
    sourcey build --config pages --output site --quiet
    cp ${openapiJson} site/openapi.json

    substituteInPlace site/index.html \
      --replace-fail '</body>' '<script src="javascripts/theme-screenshots.js" defer></script></body>'

    for html in site/*/index.html; do
      [ -f "$html" ] || continue
      substituteInPlace "$html" \
        --replace-fail '</body>' '<script src="../javascripts/theme-screenshots.js" defer></script></body>'
    done

    mv site $out
  '';
}
