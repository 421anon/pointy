{
  self,
  pkgs,
  lib,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;

  # Fake node_modules so require('playwright-core') resolves to the nixpkgs package.
  playwrightNodeModules = pkgs.runCommand "playwright-node-modules" { } ''
    mkdir -p $out/node_modules
    ln -s ${pkgs.playwright-driver} $out/node_modules/playwright-core
  '';

  takeScreenshots = pkgs.writeShellApplication {
    name = "take-screenshots";
    runtimeInputs = [ pkgs.nodejs ];
    text = ''
      export PLAYWRIGHT_BROWSERS_PATH=${pkgs.playwright-driver.browsers}
      export PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
      export NODE_PATH=${playwrightNodeModules}/node_modules
      export SCREENSHOTS_SCRIPTS_DIR=${../screenshots}
      exec node ${../screenshots/run-all-screenshots.js} "$@"
    '';
  };
in
{
  imports = [ ./dev-vm.nix ];

  # Serve the built frontend package statically (dev-vm only proxies /api and /backend/).
  services.nginx.virtualHosts."localhost".locations."/" = {
    root = "${self.packages.${system}.frontend}";
    tryFiles = "$uri $uri/ /index.html";
  };


  # Stream backend stdout/stderr to the shared dir for host-side inspection.
  systemd.services.backend.serviceConfig.StandardOutput = "append:/screenshots/backend.log";
  systemd.services.backend.serviceConfig.StandardError = "append:/screenshots/backend.log";

  # Service that takes screenshots then powers the VM off.
  systemd.services.take-screenshots = {
    description = "Take documentation screenshots then shut down";
    wantedBy = [ "multi-user.target" ];
    after = [
      "backend.service"
      "nginx.service"
      "network-online.target"
    ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Type = "oneshot";
      TimeoutStartSec = "600";
      WorkingDirectory = "${self}";
      # Pipe stdout+stderr into the shared screenshots dir so the host can read it.
      StandardOutput = "append:/screenshots/service.log";
      StandardError = "append:/screenshots/service.log";
      ExecStart = "${takeScreenshots}/bin/take-screenshots --url http://localhost --output /screenshots";
    };
    postStop = "${pkgs.systemd}/bin/systemctl poweroff";
  };

  environment.systemPackages = [ takeScreenshots ];

  fonts = {
    fontconfig.enable = true;
    packages = with pkgs; [
      dejavu_fonts
      liberation_ttf
    ];
  };

  virtualisation = {
    # Smaller footprint than the interactive dev-vm.
    memorySize = lib.mkForce 4096;
    diskSize = lib.mkForce 10240;
    # No host port forwarding needed; we access the app from inside the VM.
    forwardPorts = lib.mkForce [ ];
    # No display window — everything runs headlessly inside the VM.
    graphics = false;
    sharedDirectories = {
      # Input: checked-in backend config for screenshot generation.
      dev-config = lib.mkForce {
        source = "${../screenshots}";
        target = "/shared/dev-config";
      };
      # Output: written by playwright, read by the host after the VM exits.
      screenshots = {
        source = "$SCREENSHOTS_OUT";
        target = "/screenshots";
      };
      # Input: the user-repo git clone the backend will pull from.
      user-repo = {
        source = "$POINTY_USER_REPO";
        target = "/shared/user-repo";
      };
    };
  };
}
