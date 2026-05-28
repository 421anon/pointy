{
  config,
  pkgs,
  self,
  ...
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;
  slurmCpus = toString (config.virtualisation.cores or 1);
  # Slurm invalidates a node when slurmd reports less memory than configured.
  # QEMU guests report slightly less usable RAM than virtualisation.memorySize,
  # so reserve 512 MiB for the OS/hypervisor rather than advertising all VM RAM.
  slurmRealMemory = toString ((config.virtualisation.memorySize or 1536) - 512);
in
{
  # Nix configuration
  nix = {
    settings.experimental-features = "nix-command flakes pipe-operators";
    registry.nixpkgs.flake = self.inputs.nixpkgs;
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

  # User accounts for services
  users.users.backend = {
    isNormalUser = true;
    group = "backend";
    linger = true;
  };
  users.groups.backend = { };


  # Backend service
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
    path = with pkgs; [
      file
      nix
      gitMinimal
      openssh
      config.services.slurm.package
    ];
    environment.SLURM_CONF = "${config.services.slurm.etcSlurm}/slurm.conf";
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
  };

}
