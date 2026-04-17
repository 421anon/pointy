{ stdenv, mkdocs, python313Packages }:

stdenv.mkDerivation {
  pname = "docs";
  version = "1.0.0";

  src = ./.;

  buildInputs = [ mkdocs python313Packages.mkdocs-material ];

  installPhase = ''
    ls -l
    pwd
    mkdocs build
    mv site $out
  '';
}
