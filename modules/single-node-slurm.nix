{ config, pkgs, ... }:
let
  slurmCpus = toString (config.virtualisation.cores or 1);
  slurmRealMemory = toString ((config.virtualisation.memorySize or 1536) - 512);
  slurmDaemons = [
    "munged.service"
    "slurmctld.service"
    "slurmd.service"
  ];
in
{
  imports = [ ./pointy-host.nix ];

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
      JobCompLoc=${config.services.pointy-backend.slurm.jobCompletionHook}
    '';
  };

  systemd.services.backend = {
    after = slurmDaemons;
    requires = slurmDaemons;
  };
}
