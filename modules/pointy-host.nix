{
  config,
  pkgs,
  lib,
  ...
}:
let
  pointy = config.services.pointy.internal;
  cfg = config.services.pointy-backend;
  hostCfg = config.services.pointy-host;
  backendUrl = "http://127.0.0.1:${toString cfg.listenPort}";

  agentSandbox = pkgs.symlinkJoin {
    name = "sbox";
    paths = [ pointy.packages.sbox ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/sbox --add-flags "--network isolated --history off"
    '';
  };

  agentEnvLink = lib.optionalString (cfg.agentEnvFile != null) ''
    ln -sfn ${lib.escapeShellArg (toString cfg.agentEnvFile)} ${cfg.home}/agent-env
  '';

  rpivExtension = pkgs.callPackage ./rpiv-ask-user-question.nix { };

  piConfigLink = ''
    rm -rf ${cfg.home}/.pi
    cp -r ${lib.escapeShellArg (toString cfg.piConfigDir)} ${cfg.home}/.pi
    chown -R ${cfg.user}:${cfg.group} ${cfg.home}/.pi
    chmod -R u=rwX,go= ${cfg.home}/.pi
    mkdir -p -m u=rwx,go= ${cfg.home}/.pi/agent/extensions
    ln -sfn ${rpivExtension} ${cfg.home}/.pi/agent/extensions/rpiv-ask-user-question
    ln -sfn ${pointy.packages.pi-glovebox}/share/pi-glovebox ${cfg.home}/.pi/agent/extensions/pi-glovebox
  '';

  jobEndedHook = pkgs.writeShellScript "pointy-job-ended" ''
    case "$JOBNAME" in
      pointy-nix-build-*)
        exec ${pkgs.curl}/bin/curl -fsS --max-time 2 -o /dev/null -X POST -G \
          --data-urlencode "name=$JOBNAME" http://127.0.0.1:${toString cfg.listenPort}/job-ended
        ;;
    esac
  '';

  nixCopyIngest = pkgs.writeShellApplication {
    name = "pointy-ingest";
    runtimeInputs = [
      config.nix.package
      pkgs.jq
    ];
    text = ''
      store_path=$(nix --extra-experimental-features nix-command store add --mode nar --hash-algo sha256 --name "$2" "$1")
      nix --extra-experimental-features nix-command path-info --json "$store_path" \
        | jq -c --arg path "$store_path" \
          '[.[]][0] | {ok: true, store_path: $path, nar_hash: .narHash, nar_size: .narSize, references_source: false}'
    '';
  };

  backendEnvironment = {
    POINTY_LISTEN_HOST = cfg.listenHost;
    POINTY_LISTEN_PORT = toString cfg.listenPort;
  }
  // lib.optionalAttrs (cfg.storeUrl != null) { NIX_REMOTE = cfg.storeUrl; }
  // lib.optionalAttrs (cfg.scratchDirectory != null) { POINTY_SCRATCH_DIR = cfg.scratchDirectory; }
  // lib.optionalAttrs (cfg.uploadDirectory != null) { POINTY_UPLOAD_DIR = cfg.uploadDirectory; }
  // lib.optionalAttrs (cfg.agentMemoryMax != null) { POINTY_AGENT_MEMORY_MAX = cfg.agentMemoryMax; }
  // lib.optionalAttrs (hostCfg.tokenAuth != null) {
    POINTY_TOKEN_AUDIENCE = hostCfg.tokenAuth.audience;
    POINTY_TOKEN_PUBLIC_KEY = "${hostCfg.tokenAuth.publicKeyFile}";
  }
  // lib.optionalAttrs (hostCfg.linkedDataBase != null) { POINTY_LINKED_DATA_BASE = hostCfg.linkedDataBase; };

  agentMemoryServiceConfig = lib.optionalAttrs (cfg.agentMemoryMax != null) {
    Delegate = "memory";
    DelegateSubgroup = "main";
    OOMPolicy = "continue";
  };
in
{
  options.services.pointy-backend = {
    user = lib.mkOption {
      type = lib.types.str;
      default = "backend";
      readOnly = true;
      description = "Account the backend, its agents and its Slurm build jobs run as. Grant access to host resources through this name.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "backend";
      readOnly = true;
      description = "Primary group of the backend account.";
    };

    home = lib.mkOption {
      type = lib.types.str;
      default = "/home/backend";
      readOnly = true;
      description = ''
        Home directory of the backend account. It holds the backend's state, `config.toml`
        (unless `POINTY_CONFIG_PATH` points elsewhere), the SSH keys `config.toml` names,
        and the `agent-env` link.
      '';
    };

    listenHost = lib.mkOption {
      type = lib.types.str;
      default = "127.0.0.1";
      description = ''
        Address Warp binds the backend HTTP listener to. The default keeps the
        backend on loopback behind nginx. Set to `0.0.0.0` only when the
        backend must be reachable directly.
      '';
      example = "0.0.0.0";
    };

    listenPort = lib.mkOption {
      type = lib.types.port;
      default = 8081;
      description = ''
        TCP port Warp binds the backend HTTP listener to. The bundled nginx
        vhosts and the Slurm job-ended hook use the same value.
      '';
      example = 8081;
    };

    agentEnvFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Path on the host to a `KEY=VALUE` env file containing runtime secrets for the
        backend and agent runner (e.g. NEURALWATT_API_KEY). Sourced by systemd via EnvironmentFile.
        When set, `agent-env` in the backend's home is symlinked to this path before the backend
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
      example = "unix:///nix/var/nix/daemon-socket/socket?root=/var/lib/pointy-scratch-store/root&real=/var/lib/pointy-scratch-store/root/nix/store";
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

    agentMemoryMax = lib.mkOption {
      type = lib.types.nullOr (lib.types.strMatching "[0-9]+[KMGT]?");
      default = "24G";
      description = ''
        Memory limit shared by every agent sandbox and the processes it starts, including
        files in the sandbox /tmp. When it is exceeded the kernel kills the largest process
        in the sandboxes and the backend keeps running. The agents are told the limit and
        pointed at step runs for full-size work. Null leaves the sandboxes unlimited.
      '';
      example = "16G";
    };

    slurm = {
      package = lib.mkOption {
        type = lib.types.package;
        default = config.services.slurm.package;
        defaultText = lib.literalExpression "config.services.slurm.package";
        description = "Slurm package whose client commands the backend runs to submit and track build jobs.";
      };

      confFile = lib.mkOption {
        type = lib.types.str;
        default = "${config.services.slurm.etcSlurm}/slurm.conf";
        defaultText = lib.literalExpression ''"''${config.services.slurm.etcSlurm}/slurm.conf"'';
        description = "slurm.conf of the cluster the backend submits build jobs to, exported as SLURM_CONF.";
      };

      jobCompletionHook = lib.mkOption {
        type = lib.types.path;
        default = jobEndedHook;
        defaultText = lib.literalMD "a script that notifies the backend's `/job-ended` route";
        readOnly = true;
        description = ''
          Script the cluster must run when a job completes, so the backend wakes the waiters
          of finished builds. Configure it in the cluster's slurm.conf as
          `JobCompType=jobcomp/script` and `JobCompLoc=<this path>`.
        '';
      };
    };
  };

  options.services.pointy-host = {
    hostname = lib.mkOption {
      type = lib.types.str;
      description = "Hostname of the Pointy Notebook nginx vhost, used as the virtualHost name and, with `tls`, for ACME.";
      example = "pointy.example.com";
    };

    tls = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Terminate TLS in the vhost with an ACME certificate and redirect HTTP to HTTPS.
        Disable when a reverse proxy in front of this host terminates TLS, or for
        plain-HTTP local use.
      '';
    };

    basicAuthFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to an htpasswd file gating the nginx vhost. When null, no basic auth is applied.";
    };

    tokenAuth = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          loginUrl = lib.mkOption {
            type = lib.types.str;
            description = "Login page unauthenticated visitors are redirected to. After login it must POST a form with a `token` field to /token-login on this host.";
            example = "https://login.example.com/";
          };
          audience = lib.mkOption {
            type = lib.types.str;
            description = "Required `aud` claim of the session token, naming this instance. The UI shows the token's `name` claim, or `sub` without one, as the signed-in user.";
            example = "lab";
          };
          publicKeyFile = lib.mkOption {
            type = lib.types.path;
            description = "Ed25519 public key in PEM format that verifies the session token signatures.";
            example = lib.literalExpression "./session.pub";
          };
        };
      });
      default = null;
      description = "Admit only requests carrying an unexpired EdDSA-signed JWT session token for the audience. The backend verifies the token on every request; nginx redirects unauthenticated navigations to the login page and answers unauthenticated background requests with 401. The status bar shows the signed-in user with a log out action, which clears the session cookie and returns to the login page. When null, the instance is open.";
    };

    linkedDataBase = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Origin that names this instance's linked data. Artifact Turtle starts with `@base <origin/>`, so its permalink IRIs resolve to this origin whichever host served them. When null, IRIs stay relative to the requesting URL.";
      example = "https://pointy.example.com";
    };

    frontendPackage = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "Built frontend package served as the site root. When null, the vhost serves no frontend, as in the development VM where it runs separately.";
    };

    docsPackage = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "Built docs package served under /docs/. If null, /docs/ is not served.";
    };
  };

  config = {
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
      "pipe-operators"
    ];

    security.polkit = {
      enable = true;
      extraConfig = ''
        polkit.addRule(function(action, subject) {
          if (subject.user === "${cfg.user}" &&
              action.id === "org.freedesktop.systemd1.manage-units") {
            var unit = action.lookup("unit");
            if (unit && unit.indexOf("nix-build-") === 0) {
              return polkit.Result.YES;
            }
          }
        });
      '';
    };

    users.users.${cfg.user} = {
      isNormalUser = true;
      group = cfg.group;
      home = cfg.home;
      linger = true;
    };
    users.groups.${cfg.group} = { };

    systemd.tmpfiles.rules = lib.optional (
      cfg.uploadDirectory != null
    ) "d ${cfg.uploadDirectory} 0750 ${cfg.user} ${cfg.group} -";

    systemd.slices."pointy-builds" = {
      description = "Slice for background nix builds";
    };

    systemd.services.backend = {
      description = "Pointy Notebook Backend Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];
      path =
        (with pkgs; [
          bashInteractive
          file
          (diffoscope.override { enableBloat = false; })
          gitMinimal
          gnutar
          openssh
          systemd
        ])
        ++ [
          cfg.slurm.package
          config.nix.package
          agentSandbox
          pointy.packages.pi
          pointy.packages.pi-glovebox
          cfg.ingestPackage
        ];
      environment = {
        SLURM_CONF = cfg.slurm.confFile;
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
        User = cfg.user;
        Group = cfg.group;
        EnvironmentFile = "-${cfg.home}/agent-env";
        LimitNOFILE = 65536;
      }
      // agentMemoryServiceConfig;
    };

    services.nginx = {
      enable = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      recommendedGzipSettings = true;
      recommendedProxySettings = true;
      virtualHosts.${hostCfg.hostname} = lib.mkMerge [
        (lib.mkIf (hostCfg.tokenAuth != null) (import ./token-auth-vhost.nix {
          inherit backendUrl;
          inherit (hostCfg.tokenAuth) loginUrl;
        }))
        {
          forceSSL = hostCfg.tls;
          enableACME = hostCfg.tls;
          basicAuthFile = lib.mkIf (hostCfg.basicAuthFile != null) hostCfg.basicAuthFile;
          locations."/backend/" = {
            proxyPass = "${backendUrl}/";
            extraConfig = ''
              # SSE safety: prevent proxy buffering so events are flushed immediately.
              proxy_buffering off;
              proxy_cache off;
              proxy_http_version 1.1;
              proxy_set_header Connection "";
              proxy_read_timeout 1h;
              add_header X-Accel-Buffering "no" always;
            '';
          };
          locations."/" = lib.mkIf (hostCfg.frontendPackage != null) {
            root = "${hostCfg.frontendPackage}";
            tryFiles = "$uri $uri/ /index.html";
          };
          locations."/artifact/" = {
            extraConfig = ''
              add_header Vary Accept;
              if ($http_accept ~* "text/turtle") {
                rewrite ^/artifact/(.*)$ /backend/rdf/artifact/$1 last;
              }
            '';
          }
          // lib.optionalAttrs (hostCfg.frontendPackage != null) {
            root = "${hostCfg.frontendPackage}";
            tryFiles = "/index.html =404";
          };
          locations."/docs/" = lib.mkIf (hostCfg.docsPackage != null) {
            alias = "${hostCfg.docsPackage}/";
            extraConfig = ''
              auth_basic off;
              auth_request off;
            '';
          };
        }
      ];
    };
  };
}
