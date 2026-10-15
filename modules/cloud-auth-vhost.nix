{ backendUrl, loginUrl }:
{
  extraConfig = ''
    auth_request /cloud-auth;
    error_page 401 = @cloud-login;
  '';
  locations."= /cloud-auth" = {
    proxyPass = "${backendUrl}/cloud-auth";
    extraConfig = ''
      internal;
      auth_request off;
      proxy_pass_request_body off;
      proxy_set_header Content-Length "";
    '';
  };
  locations."= /cloud-login" = {
    proxyPass = "${backendUrl}/cloud-login";
    extraConfig = ''
      auth_request off;
    '';
  };
  locations."@cloud-login".return = "302 ${loginUrl}";
}
