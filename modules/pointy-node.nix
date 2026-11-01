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
    "ssh-ng://${authority}?trusted=true&ssh-key=${lib.escapeURL (toString cfg.hub.sshKeyFile)}"
    + lib.optionalString (
      cfg.hub.hostPublicKey != null
    ) "&base64-ssh-public-host-key=${lib.escapeURL cfg.hub.hostPublicKey}";

  realiseHub = pkgs.writeShellApplication {
    name = "pointy-realise-hub";
    runtimeInputs = [
      config.nix.package
      pkgs.openssh
      pkgs.coreutils
    ];
    text = ''
      if [ "$#" -ne 1 ]; then
        echo "usage: pointy-realise-hub DRV" >&2
        exit 2
      fi

      drv="$1"
      hub=${lib.escapeShellArg hubStoreUri}

      nix copy --derivation --no-check-sigs --from "$hub" "$drv"

      build_status=0
      nix build --no-link "$drv^*" || build_status=$?

      log_status=0
      nix store copy-log --to "$hub" "$drv" || log_status=$?

      copy_status=0
      if [ "$build_status" -eq 0 ]; then
        nix copy --no-check-sigs --to "$hub" "$drv^*" || copy_status=$?
      fi

      if [ "$build_status" -ne 0 ]; then
        exit "$build_status"
      fi
      if [ "$log_status" -ne 0 ]; then
        exit "$log_status"
      fi
      exit "$copy_status"
    '';
  };
in
{
  options.services.pointy-node = {
    enable = lib.mkEnableOption "the Pointy compute-node integration for plain multi-node Slurm clusters";

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

      port = lib.mkOption {
        type = lib.types.nullOr lib.types.port;
        default = null;
        description = "SSH port of the hub. Null uses the OpenSSH default.";
      };

      sshKeyFile = lib.mkOption {
        type = lib.types.path;
        description = ''
          Private SSH key this node presents to the hub's Nix store. The Slurm build
          user must be able to read it, both for the copies the realiser runs and for
          Nix substitution.
        '';
        example = "/etc/pointy/hub-key";
      };

      hostPublicKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Base64-encoded SSH host public key of the hub, passed to the hub store URI
          as `base64-ssh-public-host-key`. This pins the hub and overrides OpenSSH
          `known_hosts` handling. Obtain it with `base64 -w0 < /etc/ssh/ssh_host_ed25519_key.pub`.
          Null uses the node's OpenSSH known hosts, normally via `programs.ssh.knownHosts`.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ realiseHub ];

    nix.settings = {
      experimental-features = [ "nix-command" ];
      extra-substituters = [ hubStoreUri ];
      fallback = true;
    };
  };
}
