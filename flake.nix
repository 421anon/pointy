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
  };

  outputs = { self, nixpkgs, flake-utils, dream2nix, nixos-shell, ... }:
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
                exec ${nixos-shell.packages.${system}.nixos-shell}/bin/nixos-shell --flake .#dev-vm
              '');
            };
            take-screenshots = mkApp "take-screenshots" ''
              export SCREENSHOTS_OUT="''${SCREENSHOTS_OUT:-$(pwd)/docs/pages/screenshots}"
              export POINTY_USER_REPO="''${POINTY_USER_REPO:-$(dirname "$(pwd)")/trotter-user}"
              mkdir -p "$SCREENSHOTS_OUT" "$SCREENSHOTS_OUT/light" "$SCREENSHOTS_OUT/dark"
              echo "Screenshots → $SCREENSHOTS_OUT"
              echo "Modes       → light, dark"
              echo "User repo   → $POINTY_USER_REPO"
              exec ${self.nixosConfigurations.screenshots-vm.config.system.build.vm}/bin/run-nixos-vm
            '';
          };
          packages = {
            frontend = dream2nix.lib.evalModules {
              packageSets.nixpkgs = nixpkgs.legacyPackages.${system};
              modules = [ ./frontend/module.nix ];
              specialArgs = { inherit self; };
            };
            backend = pkgs.haskellPackages.callCabal2nix "backend" ./backend { };
            docs = pkgs.callPackage ./docs { };
          };
          devShells = {
            backend = self.packages.${system}.backend.env.overrideAttrs (oldAttrs: {
              buildInputs = oldAttrs.buildInputs ++ (with pkgs; [ haskell-language-server cabal-install fourmolu ]);
            });
          };
        }) // {
      nixosModules = {
        shared = ./modules/shared.nix;
        pointy-host = ./modules/pointy-host.nix;
        dev-vm = ./modules/dev-vm.nix;
        screenshots-vm = ./modules/screenshots-vm.nix;
      };

      nixosConfigurations = {
        dev-vm = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [ ./modules/dev-vm.nix ];
        };
        screenshots-vm = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = { inherit self; };
          modules = [ ./modules/screenshots-vm.nix ];
        };
      };
    };
}
