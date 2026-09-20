{
  fetchurl,
  runCommand,
}:
let
  extensionTarball = fetchurl {
    url = "https://registry.npmjs.org/@juicesharp/rpiv-ask-user-question/-/rpiv-ask-user-question-2.10.1.tgz";
    hash = "sha256-mBG/H3kVJnPw/9rLDKuKTxQDsM9agMTMbijc3Hppyc0=";
  };
  configTarball = fetchurl {
    url = "https://registry.npmjs.org/@juicesharp/rpiv-config/-/rpiv-config-2.10.1.tgz";
    hash = "sha256-cwwCjRifw/ejK/xZQ+ny67dcWck+2qBVELyKBUuzHrM=";
  };
in
runCommand "rpiv-ask-user-question-2.10.1" { } ''
  mkdir -p $out/node_modules/@juicesharp/rpiv-config
  tar -xzf ${extensionTarball} -C $out --strip-components=1
  tar -xzf ${configTarball} -C $out/node_modules/@juicesharp/rpiv-config --strip-components=1
''
