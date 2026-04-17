{
  pkgs,
  self,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;
in
{
  # Nix configuration
  nix = {
    settings.auto-optimise-store = true;
    settings.experimental-features = "nix-command flakes pipe-operators";
    registry.nixpkgs.flake = self.inputs.nixpkgs;
  };

  services.openssh.enable = true;

  security.polkit = {
    enable = true;
    extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (subject.user === "backend" &&
            action.id === "org.freedesktop.systemd1.manage-units") {
          var unit = action.lookup("unit");
          if (unit && unit.indexOf("nix-build-") === 0) {
            return polkit.Result.YES;
          }
        }
      });
    '';
  };

  # User accounts for services
  users.users.backend = {
    isNormalUser = true;
    group = "backend";
    linger = true;
  };
  users.groups.backend = { };

  systemd.slices."pointy-builds" = {
    description = "Slice for background nix builds";
  };

  # Backend service
  systemd.services.backend = {
    description = "Pointy Notebook Backend Service";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network.target"
    ];
    path = with pkgs; [
      file
      nix
      gitMinimal
      openssh
      systemd
    ];
    serviceConfig = {
      Type = "simple";
      ExecStart = "${self.packages.${system}.backend}/bin/backend";
      Restart = "always";
      RestartSec = 5;
      User = "backend";
      Group = "backend";
    };
  };

  services.nginx = {
    enable = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    recommendedGzipSettings = true;
    recommendedProxySettings = true;
    proxyTimeout = "300s";
    clientMaxBodySize = "10G";
  };

  # System packages
  environment.systemPackages = with pkgs; [
    git
    kitty.terminfo
    pv
    tree
    jq
    btop
    screen
    socat
    vim
    xpra
  ];

  environment.variables = {
    HISTSIZE = "100000";
    HISTFILESIZE = "100000";
  };

  system.stateVersion = "23.05";
}
