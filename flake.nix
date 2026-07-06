{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    globset = {
      url = "github:pdtpartners/globset";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    nixos-shell = {
      url = "github:Mic92/nixos-shell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sbox.url = "github:DavHau/sbox";
    llm-agents.url = "github:numtide/llm-agents.nix";
  };

  outputs =
    { self, nixpkgs, flake-utils, dream2nix, nixos-shell, sbox, llm-agents, ... }:
    let
      pointyBootstrap =
        { lib, pkgs, ... }:
        let
          targetSystem = pkgs.stdenv.hostPlatform.system;
          internalOption =
            type:
            lib.mkOption {
              inherit type;
              internal = true;
              readOnly = true;
            };
        in
        {
          imports = [ sbox.nixosModules.sbox ];

          options.services.pointy.internal = {
            source = internalOption lib.types.str;
            nixpkgsFlake = internalOption lib.types.raw;
            piConfigDir = internalOption lib.types.path;
            packages = {
              backend = internalOption lib.types.package;
              docs = internalOption lib.types.package;
              frontend = internalOption lib.types.package;
              pi = internalOption lib.types.package;
              sbox = internalOption lib.types.package;
            };
          };

          config.services.pointy.internal = {
            source = toString self;
            nixpkgsFlake = nixpkgs;
            piConfigDir = "${self}/backend/pi";
            packages = {
              backend = self.packages.${targetSystem}.backend;
              docs = self.packages.${targetSystem}.docs;
              frontend = self.packages.${targetSystem}.frontend;
              pi = llm-agents.packages.${targetSystem}.pi;
              sbox = sbox.packages.${targetSystem}.sbox;
            };
          };
        };
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          mkApp = name: script: {
            type = "app";
            program = toString (pkgs.writeShellScript name script);
          };
        in
        {
          apps = {
            update-elm = mkApp "update-elm" ''
              cd frontend
              ${pkgs.elm2nix}/bin/elm2nix convert > elm-srcs.nix
              ${pkgs.elm2nix}/bin/elm2nix snapshot
            '';
            generate-openapi = mkApp "generate-openapi" ''
              exec ${self.packages.${system}.backend}/bin/generate-openapi "''${1:-docs/pages/openapi.json}"
            '';
            install-elm-pkg = mkApp "install-elm-pkg" ''
              ${pkgs.elmPackages.elm}/bin/elm install "$@"
            '';
            uninstall-elm-pkg = mkApp "uninstall-elm-pkg" ''
              ${pkgs.elmPackages.elm-json}/bin/elm-json uninstall "$@"
            '';
            dev-vm = {
              type = "app";
              program = toString (pkgs.writeScript "dev-vm" ''
                #!${pkgs.bash}/bin/bash
                if [ ! -f backend/dev-config.toml ]; then
                  echo "error: backend/dev-config.toml is missing. Copy backend/example-config.toml to backend/dev-config.toml and fill in your settings." >&2
                  exit 1
                fi
                export POINTY_DEV_CONFIG_DIR="$(realpath backend)"
                exec ${nixos-shell.packages.${system}.nixos-shell}/bin/nixos-shell --flake .#dev-vm
              '');
            };
            take-screenshots = mkApp "take-screenshots" ''
              set -euo pipefail
              export SCREENSHOTS_OUT="''${SCREENSHOTS_OUT:-$(pwd)/docs/pages/screenshots}"
              export POINTY_USER_REPO="''${POINTY_USER_REPO:-$(dirname "$(pwd)")/pointy-welker}"
              mkdir -p "$SCREENSHOTS_OUT" "$SCREENSHOTS_OUT/light" "$SCREENSHOTS_OUT/dark"
              echo "Screenshots → $SCREENSHOTS_OUT"
              echo "Modes       → light, dark"
              echo "User repo   → $POINTY_USER_REPO"
              exec ${self.nixosConfigurations.screenshots-vm.config.system.build.vm}/bin/run-nixos-vm
            '';
          };
          packages =
            let
              sourcey = pkgs.buildNpmPackage {
                pname = "sourcey";
                version = "3.6.4";
                src = ./docs/sourcey;
                npmDepsHash = "sha256-JvBfTaBKBCBo4MSfgnmK1qdb5Rt0H+abP8sY1+pxPEY=";
                dontNpmBuild = true;
                nativeBuildInputs = [ pkgs.makeWrapper ];
                installPhase = ''
                  runHook preInstall
                  mkdir -p $out/lib/sourcey $out/bin
                  cp -r node_modules package.json package-lock.json $out/lib/sourcey/
                  makeWrapper ${pkgs.nodejs}/bin/node $out/bin/sourcey \
                    --add-flags "$out/lib/sourcey/node_modules/sourcey/dist/cli.js"
                  runHook postInstall
                '';
              };
            in
            {
              inherit sourcey;
              frontend = dream2nix.lib.evalModules {
                packageSets.nixpkgs = nixpkgs.legacyPackages.${system};
                modules = [ ./frontend/module.nix ];
                specialArgs = { inherit self; };
              };
              backend = pkgs.haskellPackages.callCabal2nix "backend" ./backend { };
              docs = pkgs.callPackage ./docs {
                inherit sourcey;
                openapiJson = pkgs.runCommand "openapi.json" { } ''
                  ${self.packages.${system}.backend}/bin/generate-openapi $out
                '';
              };
              screenshots = pkgs.runCommand "pointy-screenshots" { } ''
                mkdir -p $out/light $out/dark
                cp ${./docs/pages/screenshots}/light/*.png $out/light/
                cp ${./docs/pages/screenshots}/dark/*.png $out/dark/
              '';
            };
          devShells = {
            backend = self.packages.${system}.backend.env.overrideAttrs (oldAttrs: {
              buildInputs = oldAttrs.buildInputs ++ (with pkgs; [ haskell-language-server cabal-install fourmolu ]);
            });
          };
        })
    // {
      nixosModules = rec {
        bootstrap = pointyBootstrap;
        shared = {
          imports = [
            bootstrap
            ./modules/shared.nix
          ];
        };
        pointy-host = {
          imports = [
            bootstrap
            ./modules/pointy-host.nix
          ];
        };
        dev-vm = {
          imports = [
            bootstrap
            ./modules/dev-vm.nix
          ];
        };
        screenshots-vm = {
          imports = [
            bootstrap
            ./modules/screenshots-vm.nix
          ];
        };
      };

      nixosConfigurations = {
        dev-vm = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ self.nixosModules.dev-vm ];
        };
        screenshots-vm = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [ self.nixosModules.screenshots-vm ];
        };
      };
    };
}
