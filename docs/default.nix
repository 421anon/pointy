{ stdenv, mkdocs, python313Packages, redocly, openapiJson }:

stdenv.mkDerivation {
  pname = "docs";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ mkdocs python313Packages.mkdocs-material redocly ];

  installPhase = ''
    # Render the API reference as a self-contained Redoc page from the spec.
    export HOME=$(mktemp -d)
    export REDOCLY_TELEMETRY=off
    cp ${openapiJson} pages/openapi.json
    redocly build-docs pages/openapi.json --output pages/api.html

    mkdocs build
    mv site $out
  '';
}
