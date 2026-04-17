{ config, lib, pkgs, ... }:
let
  cfg = config.services.pointy-host;
in
{
  options.services.pointy-host = {
    hostname = lib.mkOption {
      type = lib.types.str;
      description = "Public hostname for the Pointy Notebook nginx vhost (used for ACME and the virtualHost name).";
      example = "pointy.example.com";
    };

    acmeEmail = lib.mkOption {
      type = lib.types.str;
      description = "Contact email for Let's Encrypt / ACME.";
      example = "admin@example.com";
    };

    authorizedKeys = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "SSH public keys authorized to log in as root.";
    };

    basicAuthFile = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to an htpasswd file gating the nginx vhost. When null, no basic auth is applied.";
    };

    enableSslh = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether the host runs sslh (SSH/TLS multiplexer on :443). Configuration of sslh itself is the deployer's responsibility; this option only influences the default TLS listen port.";
    };

    enableEndlessh = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable the endlessh SSH tarpit on port 22.";
    };

    enableFail2ban = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable fail2ban.";
    };

    openSshPort = lib.mkOption {
      type = lib.types.port;
      default = 22;
      description = "Port openssh listens on.";
    };

    extraFirewallTCPPorts = lib.mkOption {
      type = lib.types.listOf lib.types.port;
      default = [ ];
      description = "Additional TCP ports to open in the firewall (80, 443 and openSshPort are always opened).";
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
    users.users.root.openssh.authorizedKeys.keys = cfg.authorizedKeys;

    networking.firewall.allowedTCPPorts =
      [ 80 443 cfg.openSshPort ] ++ cfg.extraFirewallTCPPorts;
    networking.firewall.allowedUDPPorts = [ ];

    services.openssh.ports = [ cfg.openSshPort ];
    services.fail2ban.enable = cfg.enableFail2ban;
    services.endlessh = lib.mkIf cfg.enableEndlessh {
      enable = true;
      port = 22;
    };

    security.acme.defaults.email = cfg.acmeEmail;
    security.acme.acceptTerms = true;

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
