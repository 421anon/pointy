{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.pointy-node;

  authority =
    "${cfg.hub.user}@${cfg.hub.host}"
    + lib.optionalString (cfg.hub.port != null) ":${toString cfg.hub.port}";

  hubStoreUri =
    "ssh-ng://${authority}?ssh-key=${lib.escapeURL (toString cfg.hub.sshKeyFile)}"
    + lib.optionalString (
      cfg.hub.hostPublicKey != null
    ) "&base64-ssh-public-host-key=${lib.escapeURL cfg.hub.hostPublicKey}";

  logDestination = "${cfg.hub.logUser}@${cfg.hub.host}";

  hostField = if cfg.hub.port == null then cfg.hub.host else "[${cfg.hub.host}]:${toString cfg.hub.port}";

  portOption = lib.optionalString (cfg.hub.port != null) "ssh_options+=(-p ${toString cfg.hub.port})";

  knownHostsOption = lib.optionalString (cfg.hub.hostPublicKey != null) ''
    known_hosts="$(mktemp)"
    {
      printf '%s ' ${lib.escapeShellArg hostField}
      printf '%s' ${lib.escapeShellArg cfg.hub.hostPublicKey} | base64 -d
      printf '\n'
    } > "$known_hosts"
    ssh_host_key_options=(-o "UserKnownHostsFile=$known_hosts" -o "GlobalKnownHostsFile=/dev/null")
  '';

  realiseHub = pkgs.writeShellApplication {
    name = "pointy-realise-hub";
    runtimeInputs = [
      config.nix.package
      pkgs.openssh
      pkgs.coreutils
    ];
    excludeShellChecks = [ "SC2029" ];
    text = ''
      if [ "$#" -ne 1 ]; then
        echo "usage: pointy-realise-hub DRV" >&2
        exit 2
      fi

      drv="$1"
      case "$drv" in
        /nix/store/????????????????????????????????-*.drv) ;;
        *)
          echo "pointy-realise-hub: expected a store derivation path, got '$drv'" >&2
          exit 2
          ;;
      esac
      case "''${drv#/nix/store/}" in
        .* | */*)
          echo "pointy-realise-hub: expected a store derivation path, got '$drv'" >&2
          exit 2
          ;;
      esac

      hub=${lib.escapeShellArg hubStoreUri}
      signing_key=${lib.escapeShellArg (toString cfg.signingKeyFile)}
      hub_key=${lib.escapeShellArg (toString cfg.hub.sshKeyFile)}
      log_destination=${lib.escapeShellArg logDestination}

      known_hosts=""
      log_file=""
      closure_file=""
      trap 'rm -f -- "$known_hosts" "$log_file" "$closure_file"' EXIT

      ssh_host_key_options=()
      ${knownHostsOption}
      ssh_options=(
        -o BatchMode=yes
        -o StrictHostKeyChecking=yes
        -i "$hub_key"
        "''${ssh_host_key_options[@]}"
      )
      ${portOption}

      upload_log() {
        local dependency="$1"
        log_file="$(mktemp)"
        if nix log -- "$dependency" > "$log_file" 2>/dev/null; then
          ssh "''${ssh_options[@]}" "$log_destination" "$dependency" < "$log_file" >/dev/null 2>&1 || \
            echo "pointy-realise-hub: could not upload the build log of $dependency" >&2
        fi
        rm -f -- "$log_file"
        log_file=""
      }

      build_status=0
      nix copy --derivation --from "$hub" -- "$drv" || build_status=$?

      if [ "$build_status" -eq 0 ]; then
        nix build --no-link -- "$drv^*" || build_status=$?
      fi

      if [ "$build_status" -eq 0 ]; then
        nix store sign --key-file "$signing_key" -- "$drv^*" || \
          echo "pointy-realise-hub: could not sign the outputs of $drv" >&2
      fi

      copy_status=0
      if [ "$build_status" -eq 0 ]; then
        nix copy --to "$hub" -- "$drv^*" || copy_status=$?
      fi

      closure_file="$(mktemp)"
      if nix-store -qR "$drv" > "$closure_file"; then
        while IFS= read -r dependency; do
          case "$dependency" in
            *.drv) upload_log "$dependency" ;;
          esac
        done < "$closure_file"
      fi
      rm -f -- "$closure_file"
      closure_file=""

      if [ "$build_status" -ne 0 ]; then
        exit "$build_status"
      fi
      exit "$copy_status"
    '';
  };
in
{
  options.services.pointy-node = {
    enable = lib.mkEnableOption "the Pointy compute-node integration for plain multi-node Slurm clusters";

    signingKeyFile = lib.mkOption {
      type = lib.types.path;
      description = ''
        Secret Nix signing key this node uses to sign the outputs it builds. The
        hub lists the corresponding public key in
        `services.pointy-hub.nodePublicKeys`. The Slurm build user must be able to
        read it.
      '';
      example = "/etc/pointy/node-signing-key";
    };

    nodePublicKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Nix public signing keys of every compute node, including this one. Paths
        substituted from the hub must carry a signature from one of these keys.
      '';
      example = [ "compute-1:AAAA..." ];
    };

    hub = {
      host = lib.mkOption {
        type = lib.types.str;
        description = "Hostname or address of the Pointy hub, the head node whose Nix store serves derivations, inputs and build logs.";
        example = "pointy.example.com";
      };

      user = lib.mkOption {
        type = lib.types.str;
        default = "nix-ssh";
        description = "SSH user of the hub's Nix store, matching `nix.sshServe` on the hub.";
      };

      logUser = lib.mkOption {
        type = lib.types.str;
        default = "pointy-log";
        description = "SSH user of the hub's build-log receiver, matching `services.pointy-hub.logUser`.";
      };

      port = lib.mkOption {
        type = lib.types.nullOr lib.types.port;
        default = null;
        description = "SSH port of the hub. Null uses the OpenSSH default.";
      };

      sshKeyFile = lib.mkOption {
        type = lib.types.path;
        description = ''
          Private SSH key this node presents to the hub's Nix store and build-log
          receiver. The Slurm build user must be able to read it, both for the
          copies the realiser runs and for Nix substitution.
        '';
        example = "/etc/pointy/hub-key";
      };

      hostPublicKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Base64-encoded SSH host public key of the hub, passed to the hub store URI
          as `base64-ssh-public-host-key` and used to pin the hub for build-log
          uploads. Obtain it with `base64 -w0 < /etc/ssh/ssh_host_ed25519_key.pub`.
          Null uses the node's OpenSSH known hosts, normally via
          `programs.ssh.knownHosts`.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ realiseHub ];

    nix.settings = {
      experimental-features = [ "nix-command" ];
      extra-substituters = [ hubStoreUri ];
      trusted-public-keys = cfg.nodePublicKeys;
      fallback = true;
    };
  };
}
