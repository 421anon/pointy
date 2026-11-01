{
  config,
  lib,
  ...
}:
let
  cfg = config.services.pointy-hub;
in
{
  options.services.pointy-hub = {
    enable = lib.mkEnableOption "the Pointy hub Nix store for plain multi-node Slurm clusters";

    nodeKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = ''
        SSH public keys of the compute nodes, in `authorized_keys` format. They
        authenticate to the hub's Nix store as the `nix-ssh` user and may copy
        derivations and outputs in both directions and upload build logs.
      '';
      example = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAA... node@compute-1" ];
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
    nix.settings.experimental-features = [ "nix-command" ];

    nix.sshServe = {
      enable = true;
      write = true;
      trusted = true;
      protocol = "ssh-ng";
      keys = cfg.nodeKeys;
    };
  };
}
