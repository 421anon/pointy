{
  outputs = _: {
    nixosModules.default = ./nixos-module.nix;
    nixosModules.slurm-events = ./nixos-module.nix;
  };
}
