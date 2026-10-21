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

    tokenAuth = lib.mkOption {
      type = lib.types.nullOr (lib.types.submodule {
        options = {
          loginUrl = lib.mkOption {
            type = lib.types.str;
            description = "Login page unauthenticated visitors are redirected to. After login it must POST a form with a `token` field to /token-login on this host.";
            example = "https://login.example.com/";
          };
          audience = lib.mkOption {
            type = lib.types.str;
            description = "Required `aud` claim of the session token, naming this instance. The UI shows the token's `name` claim, or `sub` without one, as the signed-in user.";
            example = "lab";
          };
          publicKeyFile = lib.mkOption {
            type = lib.types.path;
            description = "Ed25519 public key in PEM format that verifies the session token signatures.";
            example = lib.literalExpression "./session.pub";
          };
        };
      });
      default = null;
      description = "Admit only requests carrying an unexpired EdDSA-signed JWT session token for the audience. The backend verifies the token on every request; nginx redirects unauthenticated navigations to the login page and answers unauthenticated background requests with 401. The status bar shows the signed-in user with a log out action, which clears the session cookie and returns to the login page. When null, the instance is open.";
    };

    linkedDataBase = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Origin that names this instance's linked data. Artifact Turtle starts with `@base <origin/>`, so its permalink IRIs resolve to this origin whichever host served them. When null, IRIs stay relative to the requesting URL.";
      example = "https://pointy.example.com";
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
    systemd.services.backend.environment = lib.mkMerge [
      (lib.mkIf (cfg.tokenAuth != null) {
        POINTY_TOKEN_AUDIENCE = cfg.tokenAuth.audience;
        POINTY_TOKEN_PUBLIC_KEY = "${cfg.tokenAuth.publicKeyFile}";
      })
      (lib.mkIf (cfg.linkedDataBase != null) {
        POINTY_LINKED_DATA_BASE = cfg.linkedDataBase;
      })
    ];

    services.nginx.virtualHosts.${cfg.hostname} = lib.mkMerge [
      (lib.mkIf (cfg.tokenAuth != null) (import ./token-auth-vhost.nix {
        backendUrl = "http://127.0.0.1:8081";
        inherit (cfg.tokenAuth) loginUrl;
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
        locations."/artifact/" = {
          root = "${cfg.frontendPackage}";
          tryFiles = "/index.html =404";
          extraConfig = ''
            add_header Vary Accept;
            if ($http_accept ~* "text/turtle") {
              rewrite ^/artifact/(.*)$ /backend/rdf/artifact/$1 last;
            }
          '';
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
