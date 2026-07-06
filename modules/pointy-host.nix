{ config, lib, sharedModule ? ./shared.nix, ... }:
let
  cfg = config.services.pointy-host;
in
{
  imports = [ sharedModule ];
  
  options.services.pointy-host = {
    hostname = lib.mkOption {
      type = lib.types.str;
      description = "Public hostname for the Pointy Notebook nginx vhost (used for ACME and the virtualHost name).";
      example = "pointy.example.com";
    };

    basicAuthFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to an htpasswd file gating the nginx vhost. When null, no basic auth is applied.";
    };

    frontendPackage = lib.mkOption {
      type = lib.types.package;
      description = "Built frontend package served as the site root.";
    };

    docsPackage = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "Built docs package served under /docs/. If null, /docs/ is not served.";
    };
  };

  config = {
    services.nginx.virtualHosts.${cfg.hostname} = {
      forceSSL = true;
      enableACME = true;
      basicAuthFile = lib.mkIf (cfg.basicAuthFile != null) cfg.basicAuthFile;
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
      locations."/" = {
        root = "${cfg.frontendPackage}";
        tryFiles = "$uri $uri/ /index.html";
      };
      locations."/docs/" = lib.mkIf (cfg.docsPackage != null) {
        alias = "${cfg.docsPackage}/";
        extraConfig = ''
          auth_basic off;
        '';
      };
    };
  };
}
