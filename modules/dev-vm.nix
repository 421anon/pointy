{
  pkgs,
  modulesPath,
  config,
  lib,
  ...
}:
let
  pointy = config.services.pointy.internal;
  slurmRealMemory = toString ((config.virtualisation.memorySize or 1536) - 512);
in
{
  imports = [
    "${modulesPath}/virtualisation/qemu-vm.nix"
    ./shared.nix
  ];

  services.openssh.enable = true;

  virtualisation = {
    memorySize = 8192;
    diskSize = 20480;
    cores = 4;
    forwardPorts = [
      {
        from = "host";
        host.port = 8080;
        guest.port = 80;
      } # nginx
      {
        from = "host";
        host.port = 2222;
        guest.port = 22;
      } # SSH
    ];
    writableStoreUseTmpfs = false;
    sharedDirectories = {
      # fetch and fingerprint caches
      cache = {
        source = "$HOME/.cache/nix";
        target = "/var/cache/nix";
      };
      nix-var = {
        source = "/nix/var/log";
        target = "/nix/var/log";
      };
      dev-config = {
        source = "$POINTY_DEV_CONFIG_DIR";
        target = "/shared/dev-config";
      };
    };
  };

  # Nix's libgit2-based fetcher writes packfile indexes via writable MAP_SHARED
  # mmap. The default 9p mount (cache=none) rejects those with EINVAL
  # ("appending to git packfile index: failed to mmap"), so opt this share into
  # the v9fs mmap cache mode.
  virtualisation.fileSystems."/var/cache/nix".options = [ "cache=mmap" ];

  systemd.services.backend.environment.NIX_CACHE_HOME = "/var/cache/nix";
  # Keep the dev scheduler permissive but low-concurrency: the backend's dev
  # config records requirements as metadata-only, so one advertised CPU is enough
  # to serialize jobs without rejecting large template CPU requirements.
  services.slurm.nodeName = lib.mkForce [
    "${config.networking.hostName} CPUs=4 RealMemory=${slurmRealMemory} State=UNKNOWN"
  ];

  # Automatically return DOWN nodes to service after unexpected reboots.
  # Value 2: resume as soon as slurmd re-registers, regardless of reason.
  services.slurm.extraConfig = "ReturnToService=2";


  nix.settings.store = "unix:///var/run/nix-daemon-socket";

  # use the host store if exposed at port 5000
  systemd.services.host-nix-daemon-proxy = {
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];

    script = ''
      orig=/nix/var/nix/daemon-socket/socket
      sock=/var/run/nix-daemon-socket

      rm -f "$sock"

      if ${pkgs.netcat}/bin/nc -z -w1 10.0.2.2 5000; then
        exec ${pkgs.socat}/bin/socat \
          UNIX-LISTEN:"$sock",fork,mode=0666 \
          TCP:10.0.2.2:5000
      else
        ln -s $orig $sock
      fi
    '';
  };

  systemd.services.backend.preStart = ''
    if [ ! -f /shared/dev-config/dev-config.toml ]; then
      echo "error: /shared/dev-config/dev-config.toml is missing — launch the VM via `nix run .#dev-vm` so backend/dev-config.toml is shared into the guest" >&2
      exit 1
    fi
    install -m 0600 -o backend -g backend /shared/dev-config/dev-config.toml /home/backend/config.toml

    # Optional agent secrets (e.g. DEEPSEEK_API_KEY=...) sourced as systemd EnvironmentFile.
    if [ -f /shared/dev-config/agent-env ]; then
      install -m 0600 -o backend -g backend /shared/dev-config/agent-env /home/backend/agent-env
    fi

  '';

  # Simple nginx configuration for dev
  services.nginx = {
    virtualHosts."localhost" = {
      locations."/api/".proxyPass = "http://127.0.0.1:3000/";
      locations."/backend/" = {
        proxyPass = "http://127.0.0.1:8081/";
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
    };
  };

  system.stateVersion = "25.11";
}
