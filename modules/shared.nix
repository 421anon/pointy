{
  config,
  pkgs,
  self,
  sbox,
  llm-agents,
  lib,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  slurmCpus = toString (config.virtualisation.cores or 1);
  # Slurm invalidates a node when slurmd reports less memory than configured.
  # QEMU guests report slightly less usable RAM than virtualisation.memorySize,
  # so reserve 512 MiB for the OS/hypervisor rather than advertising all VM RAM.
  slurmRealMemory = toString ((config.virtualisation.memorySize or 1536) - 512);
  cfg = config.services.pointy-backend;

  agentEnvLink = lib.optionalString (cfg.agentEnvFile != null) ''
    ln -sfn ${lib.escapeShellArg (toString cfg.agentEnvFile)} /home/backend/agent-env
  '';

  piConfigLink = lib.optionalString (cfg.piConfigDir != null) ''
    rm -rf /home/backend/.pi
    cp -r ${lib.escapeShellArg (toString cfg.piConfigDir)} /home/backend/.pi
    chmod -R u=rwX,go= /home/backend/.pi
  '';
in
{
  imports = [
    sbox.nixosModules.sbox
  ];

  options.services.pointy-backend = {
    agentEnvFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path on the host to a `KEY=VALUE` env file containing runtime secrets for the
        agent runner (e.g. DEEPSEEK_API_KEY). Sourced by systemd via EnvironmentFile.
        When set, /home/backend/agent-env is symlinked to this path before the backend
        service starts. Use this with sops-nix / agenix by pointing at the decrypted
        runtime path (typically /run/secrets/...).
      '';
      example = "/run/secrets/pointy-agent-env";
    };

    piConfigDir = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path on the host to a directory whose contents are copied to /home/backend/.pi
        before the backend service starts (e.g. agent/models.json for pi-mono provider
        definitions). The directory itself should not contain secrets; reference
        $DEEPSEEK_API_KEY (etc.) inside models.json and put the actual key in
        `agentEnvFile`.
      '';
      example = lib.literalExpression "./pi-config";
    };
  };

  config = {
    programs.sbox = {
      enable = true;
      network = "isolated";
      shareHistory = "off";
      shareKnownHosts = true;
    };

    nix = {
      settings.experimental-features = "nix-command flakes pipe-operators";
      registry.nixpkgs.flake = self.inputs.nixpkgs;
    };

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

    services.munge = {
      enable = true;
      password = "/var/lib/munge/munge.key";
    };

    system.activationScripts.pointyMungeKey.text = ''
      ${pkgs.coreutils}/bin/install -d -m 0711 -o munge -g munge /var/lib/munge
      if [ ! -e /var/lib/munge/munge.key ]; then
        ${pkgs.coreutils}/bin/head -c 1024 /dev/urandom > /var/lib/munge/munge.key
      fi
      ${pkgs.coreutils}/bin/chown munge:munge /var/lib/munge/munge.key
      ${pkgs.coreutils}/bin/chmod 0400 /var/lib/munge/munge.key
    '';

    services.slurm = {
      server.enable = true;
      client.enable = true;
      controlMachine = config.networking.hostName;
      nodeName = [ "${config.networking.hostName} CPUs=${slurmCpus} RealMemory=${slurmRealMemory} State=UNKNOWN" ];
      partitionName = [ "pointy Nodes=${config.networking.hostName} Default=YES MaxTime=INFINITE State=UP" ];
    };

    users.users.backend = {
      isNormalUser = true;
      group = "backend";
      linger = true;
    };
    users.groups.backend = { };

    systemd.slices."pointy-builds" = {
      description = "Slice for background nix builds";
    };

    systemd.services.backend = {
      description = "Pointy Notebook Backend Service";
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
        "munged.service"
        "slurmctld.service"
        "slurmd.service"
      ];
      requires = [
        "munged.service"
        "slurmctld.service"
        "slurmd.service"
      ];
      path =
        (with pkgs; [
          bashInteractive
          file
          nix
          gitMinimal
          openssh
          systemd
          config.services.slurm.package
        ])
        ++ [
          sbox.packages.${system}.sbox
          llm-agents.packages.${system}.pi
        ];
      environment.SLURM_CONF = "${config.services.slurm.etcSlurm}/slurm.conf";
      preStart = lib.mkBefore ''
        ${agentEnvLink}
        ${piConfigLink}
      '';
      serviceConfig = {
        Type = "simple";
        ExecStart = "${self.packages.${system}.backend}/bin/backend";
        Restart = "always";
        RestartSec = 5;
        User = "backend";
        Group = "backend";
        # Optional file for runtime secrets (e.g. DEEPSEEK_API_KEY=...).
        # The leading dash makes it tolerant of the file being absent.
        EnvironmentFile = "-/home/backend/agent-env";
      };
    };

    services.nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
    };
  };
}
