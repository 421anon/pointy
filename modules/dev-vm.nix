{
  self,
  pkgs,
  modulesPath,
  ...
}:
{
  imports = [
    "${modulesPath}/virtualisation/qemu-vm.nix"
    ./shared.nix
  ];

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
    };
  };

  systemd.services.backend.environment.NIX_CACHE_HOME = "/var/cache/nix";

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

  systemd.services.backend.preStart =
    let
      devConfigPath = builtins.getEnv "POINTY_DEV_CONFIG";
      devConfig = builtins.path {
        path = devConfigPath;
        name = "dev-config.toml";
      };
    in
    if devConfigPath == "" then
      throw "POINTY_DEV_CONFIG is not set — launch the dev VM via `nix run .#dev-vm` so the gitignored backend/dev-config.toml is passed in at invocation"
    else
      ''
        cp ${devConfig} /home/backend/config.toml && chmod u+w /home/backend/config.toml
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
      locations."/docs/" = {
        alias = "${self.packages.${pkgs.stdenv.hostPlatform.system}.docs}/";
        extraConfig = ''
          auth_basic off;
        '';
      };
    };
  };
}
