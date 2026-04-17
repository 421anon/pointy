{ self, dream2nix, config, lib, ... }: {

  imports = [
    dream2nix.modules.dream2nix.nodejs-package-lock-v3
    dream2nix.modules.dream2nix.nodejs-devshell-v3
    dream2nix.modules.dream2nix.nodejs-granular-v3
  ];

  deps = { nixpkgs, ... }: {
    inherit (nixpkgs) nodejs rsync;
    elm = nixpkgs.elmPackages.elm;
    fetchElmDeps = nixpkgs.elmPackages.fetchElmDeps;
  };

  name = "pointy-frontend";
  version = "1.0.0";

  nodejs-package-lock-v3.packageLockFile = "${config.mkDerivation.src}/package-lock.json";

  mkDerivation = {
    src = lib.fileset.toSource {
      root = ./.;
      fileset = self.inputs.globset.lib.globs ./. [ "**/*" "!module.nix" ];
    };
    nativeBuildInputs = [ config.deps.elm ];
    postConfigure = ''
      ${config.deps.fetchElmDeps {
        elmPackages = import ./elm-srcs.nix;
        registryDat = ./registry.dat;
        elmVersion = "0.19.1";
      }}
      export NODE_ENV=production
    '';
    installPhase = "cp -r dist/* $out";
    shellHook =
      let
        nodeModules =
          "${config.nodejs-devshell-v3.nodeModules.public}/lib/node_modules/${config.name}/node_modules";
      in
      lib.mkForce ''
        export PATH=$PATH:${nodeModules}/.bin
        export NODE_PATH=${nodeModules}
      '';
  };
}
