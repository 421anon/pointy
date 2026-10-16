{ backendUrl, loginUrl }:
{
  extraConfig = ''
    auth_request /token-auth;
    error_page 401 = @login;
  '';
  locations."= /token-auth" = {
    proxyPass = "${backendUrl}/token-auth";
    extraConfig = ''
      internal;
      auth_request off;
      proxy_pass_request_body off;
      proxy_set_header Content-Length "";
    '';
  };
  locations."= /token-login" = {
    proxyPass = "${backendUrl}/token-login";
    extraConfig = ''
      auth_request off;
    '';
  };
  locations."= /token-logout".proxyPass = "${backendUrl}/token-logout";
  locations."@login".extraConfig = ''
    if ($http_sec_fetch_mode ~ "^(cors|same-origin|no-cors)$") {
      return 401;
    }
    return 302 ${loginUrl};
  '';
}
