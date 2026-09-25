{
  config,
  pkgs,
  lib,
  ...
}:
let
  pointy = config.services.pointy.internal;
  slurmCpus = toString (config.virtualisation.cores or 1);
  slurmRealMemory = toString ((config.virtualisation.memorySize or 1536) - 512);
  cfg = config.services.pointy-backend;

  agentEnvLink = lib.optionalString (cfg.agentEnvFile != null) ''
    ln -sfn ${lib.escapeShellArg (toString cfg.agentEnvFile)} /home/backend/agent-env
  '';

  rpivExtension = pkgs.callPackage ./rpiv-ask-user-question.nix { };

  piConfigLink = ''
    rm -rf /home/backend/.pi
    cp -r ${lib.escapeShellArg (toString cfg.piConfigDir)} /home/backend/.pi
    chown -R backend:backend /home/backend/.pi
    chmod -R u=rwX,go= /home/backend/.pi
    mkdir -p -m u=rwx,go= /home/backend/.pi/agent/extensions
    ln -sfn ${rpivExtension} /home/backend/.pi/agent/extensions/rpiv-ask-user-question
  '';

  jobEndedHook = pkgs.writeShellScript "pointy-job-ended" ''
    case "$JOBNAME" in
      pointy-nix-build-*)
        exec ${pkgs.curl}/bin/curl -fsS --max-time 2 -o /dev/null -X POST -G \
          --data-urlencode "name=$JOBNAME" http://127.0.0.1:8081/job-ended
        ;;
    esac
  '';

  nixCopyIngest = pkgs.writeShellApplication {
    name = "pointy-ingest";
    runtimeInputs = [
      pkgs.nix
      pkgs.jq
    ];
    text = ''
      store_path=$(nix --extra-experimental-features nix-command store add --mode nar --hash-algo sha256 --name "$2" "$1")
      nix --extra-experimental-features nix-command path-info --json "$store_path" \
        | jq -c --arg path "$store_path" \
          '[.[]][0] | {ok: true, store_path: $path, nar_hash: .narHash, nar_size: .narSize, references_source: false}'
    '';
  };

  backendEnvironment =
    lib.optionalAttrs (cfg.storeUrl != null) { NIX_REMOTE = cfg.storeUrl; }
    // lib.optionalAttrs (cfg.scratchDirectory != null) { POINTY_SCRATCH_DIR = cfg.scratchDirectory; }
    // lib.optionalAttrs (cfg.uploadDirectory != null) { POINTY_UPLOAD_DIR = cfg.uploadDirectory; };
in
{

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
      type = lib.types.path;
      default = pointy.piConfigDir;
      defaultText = lib.literalExpression "config.services.pointy.internal.piConfigDir";
      description = ''
        Directory copied to /home/backend/.pi before the backend starts.
        Defaults to backend/pi from this flake.
      '';
      example = lib.literalExpression "./pi-config";
    };

    ingestPackage = lib.mkOption {
      type = lib.types.package;
      default = nixCopyIngest;
      defaultText = lib.literalMD "a `pointy-ingest` that copies the directory into the store with `nix store add`";
      description = ''
        Package providing `bin/pointy-ingest DIR NAME`, which turns a directory into a
        content-addressed store path in the store the backend builds with. It runs as the
        backend user and prints JSON lines: optional `{"progress":{"done":N,"total":N}}`,
        then `{"ok":true,"store_path":…,"nar_hash":…,"nar_size":N,"references_source":B}`
        or `{"ok":false,"error":…}`. With `references_source = true` the store path keeps
        reading the source bytes, so the backend never deletes the source.
      '';
    };

    storeUrl = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Nix store the backend, its evaluators, its agents and its Slurm build jobs use,
        exported as NIX_REMOTE. For `unix://SOCKET?root=ROOT` the backend reads store
        contents from ROOT/nix/store. Null uses the host store.
      '';
      example = "unix:///run/pointy-store/daemon.sock?root=/var/lib/pointy-store/root";
    };

    scratchDirectory = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Directory users browse to wrap a whole subdirectory into a file-upload step.
        Anything the backend user can read inside it can be wrapped. Null disables wrapping.
      '';
      example = "/data";
    };

    uploadDirectory = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = ''
        Directory that receives uploaded files before ingest. Uploads stay here when the
        ingest program references its source, so it must be persistent in that case.
        Null stages uploads in the system temporary directory.
      '';
      example = "/data/pointy-uploads";
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
      registry.nixpkgs.flake = pointy.nixpkgsFlake;
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
      extraConfig = ''
        JobCompType=jobcomp/script
        JobCompLoc=${jobEndedHook}
      '';
    };

    users.users.backend = {
      isNormalUser = true;
      group = "backend";
      linger = true;
    };
    users.groups.backend = { };

    systemd.tmpfiles.rules = lib.optional (
      cfg.uploadDirectory != null
    ) "d ${cfg.uploadDirectory} 0750 backend backend -";

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
          (diffoscope.override { enableBloat = false; })
          nix
          gitMinimal
          openssh
          systemd
          config.services.slurm.package
        ])
        ++ [
          pointy.packages.sbox
          pointy.packages.pi
          cfg.ingestPackage
        ];
      environment = {
        SLURM_CONF = "${config.services.slurm.etcSlurm}/slurm.conf";
      }
      // backendEnvironment;
      preStart = lib.mkBefore ''
        ${agentEnvLink}
        ${piConfigLink}
      '';
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pointy.packages.backend}/bin/backend";
        Restart = "always";
        RestartSec = 5;
        User = "backend";
        Group = "backend";
        EnvironmentFile = "-/home/backend/agent-env";
        LimitNOFILE = 65536;
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
