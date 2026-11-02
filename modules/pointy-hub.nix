{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.pointy-hub;

  logUpload = pkgs.writeShellApplication {
    name = "pointy-log-upload";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      drv="''${SSH_ORIGINAL_COMMAND:-}"
      case "$drv" in
        /nix/store/????????????????????????????????-*.drv) ;;
        *)
          echo "pointy-log-upload: expected a store derivation path" >&2
          exit 2
          ;;
      esac
      base="''${drv#/nix/store/}"
      case "$base" in
        .* | */*)
          echo "pointy-log-upload: expected a store derivation path" >&2
          exit 2
          ;;
      esac

      prefix="''${base:0:2}"
      rest="''${base:2}"
      case "$rest" in
        . | ..)
          echo "pointy-log-upload: expected a store derivation path" >&2
          exit 2
          ;;
      esac
      directory="/nix/var/log/nix/drvs/$prefix"
      mkdir -p -- "$directory"
      temporary="$(mktemp -- "$directory/.pointy-log.XXXXXX")"
      trap 'rm -f -- "$temporary"' EXIT
      cat > "$temporary"
      chmod 0644 -- "$temporary"
      mv -f -- "$temporary" "$directory/$rest"
      trap - EXIT
    '';
  };
in
{
  options.services.pointy-hub = {
    enable = lib.mkEnableOption "the Pointy hub Nix store for plain multi-node Slurm clusters";

    nodeKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        SSH public keys of the compute nodes, in `authorized_keys` format. They
        authenticate to the hub's Nix store as the `nix-ssh` user, which may read
        and, with signatures, write store paths but is not trusted, and to the
        build-log receiver as the `logUser` user.
      '';
      example = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAA... node@compute-1" ];
    };

    nodePublicKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        Nix public signing keys of the compute nodes. The untrusted `nix-ssh`
        user may only add paths signed by one of these keys, and the hub serves
        those paths to the nodes as a substituter.
      '';
      example = [ "compute-1:AAAA..." ];
    };

    logUser = lib.mkOption {
      type = lib.types.str;
      default = "pointy-log";
      description = ''
        System user that receives build logs from the compute nodes. Its SSH key is
        the same node key, and a forced command only lets it write a log for the
        derivation path in its original command.
      '';
    };

    realiseCommand = lib.mkOption {
      type = lib.types.str;
      default = "/run/current-system/sw/bin/pointy-realise-hub";
      readOnly = true;
      description = ''
        Command the backend submits to Slurm to realise a store derivation on a
        compute node. It names the program installed by
        `services.pointy-node`, so it resolves on every node of the cluster.
        Point the backend at it with
        `services.pointy-backend.slurm.realiseCommand = config.services.pointy-hub.realiseCommand;`
        when the backend runs on the hub.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    nix.settings = {
      experimental-features = [ "nix-command" ];
      trusted-public-keys = cfg.nodePublicKeys;
    };

    nix.sshServe = {
      enable = true;
      write = true;
      trusted = false;
      protocol = "ssh-ng";
      keys = cfg.nodeKeys;
    };

    users.groups.${cfg.logUser} = { };

    users.users.${cfg.logUser} = {
      isSystemUser = true;
      group = cfg.logUser;
      shell = pkgs.bashInteractive;
      openssh.authorizedKeys.keys = cfg.nodeKeys;
    };

    services.openssh.extraConfig = lib.mkAfter ''
      Match User ${cfg.logUser}
        AllowAgentForwarding no
        AllowTcpForwarding no
        PermitTTY no
        PermitTunnel no
        X11Forwarding no
        ForceCommand ${logUpload}/bin/pointy-log-upload
      Match All
    '';

    system.activationScripts.pointyLogDirectory = {
      deps = [ "users" "groups" ];
      text = ''
        install -d -m 0755 /nix/var/log/nix
        install -d -m 2775 -o root -g ${cfg.logUser} /nix/var/log/nix/drvs
        ${pkgs.acl}/bin/setfacl -m d:g:${cfg.logUser}:rwx /nix/var/log/nix/drvs
        ${pkgs.findutils}/bin/find /nix/var/log/nix/drvs -mindepth 1 -maxdepth 1 -type d -exec ${pkgs.acl}/bin/setfacl -m g:${cfg.logUser}:rwx '{}' +
      '';
    };

    environment.systemPackages = [ logUpload ];
  };
}
