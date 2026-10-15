{ config, lib, ... }:
let
  cfg = config.services.pointy-host;
in
{
  imports = [ ./shared.nix ];
  
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

    cloudAuth = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          url = lib.mkOption {
            type = lib.types.str;
            description = "Base URL of the Pointy Cloud frontend users log in at.";
            example = "https://pointy.cloud";
          };
          account = lib.mkOption {
            type = lib.types.str;
            description = "Pointy Cloud account whose users may use this instance.";
            example = "welker";
          };
          publicKeyFile = lib.mkOption {
            type = lib.types.path;
            description = "Ed25519 public key in PEM format that verifies the session tokens Pointy Cloud signs.";
            example = lib.literalExpression "./pointy-cloud-jwt.pub";
          };
        };
      });
      default = null;
      description = "Admit only users holding a Pointy Cloud session token for the account. The backend verifies the token on every request and nginx redirects everyone else to the cloud login page. When null, no cloud login is required.";
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
    systemd.services.backend.environment = lib.mkIf (cfg.cloudAuth != null) {
      POINTY_CLOUD_ACCOUNT = cfg.cloudAuth.account;
      POINTY_CLOUD_PUBLIC_KEY = "${cfg.cloudAuth.publicKeyFile}";
    };

    services.nginx.virtualHosts.${cfg.hostname} = lib.mkMerge [
      (lib.mkIf (cfg.cloudAuth != null) (import ./cloud-auth-vhost.nix {
        backendUrl = "http://127.0.0.1:8081";
        loginUrl = "${cfg.cloudAuth.url}/";
      }))
      {
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
            auth_request off;
          '';
        };
      }
    ];
  };
}
