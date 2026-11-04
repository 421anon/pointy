{ buildNpmPackage, makeWrapper, nodejs }:

buildNpmPackage {
  pname = "sourcey";
  version = "3.6.4";
  src = ./.;
  npmDepsHash = "sha256-JvBfTaBKBCBo4MSfgnmK1qdb5Rt0H+abP8sY1+pxPEY=";
  dontNpmBuild = true;
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/sourcey $out/bin
    cp -r node_modules package.json package-lock.json $out/lib/sourcey/
    makeWrapper ${nodejs}/bin/node $out/bin/sourcey \
      --add-flags "$out/lib/sourcey/node_modules/sourcey/dist/cli.js"
    runHook postInstall
  '';
}
