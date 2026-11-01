{ pkgs }:
let
  mungeKey = "pointy-hub-realise-shared-munge-key-0123456789abcdef";

  commonModule =
    { lib, ... }:
    {
      services.munge = {
        enable = true;
        password = "/var/lib/munge/munge.key";
      };

      system.activationScripts.pointyHubMungeKey = {
        deps = [ "users" "groups" ];
        text = ''
          install -d -m 0711 -o munge -g munge /var/lib/munge
          printf '%s\n' ${lib.escapeShellArg mungeKey} > /var/lib/munge/munge.key
          chown munge:munge /var/lib/munge/munge.key
          chmod 0400 /var/lib/munge/munge.key
        '';
      };

      services.slurm = {
        controlMachine = "hub";
        nodeName = [ "node CPUs=2 RealMemory=1024 State=UNKNOWN" ];
        partitionName = [ "pointy Nodes=node Default=YES MaxTime=INFINITE State=UP" ];
        extraConfig = "ReturnToService=2";
      };

      networking.firewall.enable = false;

      nix.settings.experimental-features = [
        "nix-command"
        "flakes"
      ];

      environment.systemPackages = [
        pkgs.nix
        pkgs.bash
        pkgs.coreutils
        pkgs.openssh
      ];

      virtualisation = {
        memorySize = 2048;
        cores = 2;
      };
    };
in
pkgs.testers.nixosTest {
  name = "pointy-hub-realise";

  nodes.hub =
    { lib, ... }:
    {
      imports = [
        commonModule
        ../modules/pointy-hub.nix
      ];

      networking.hostName = "hub";

      services.pointy-hub = {
        enable = true;
        nodeKeys = [ ];
      };

      services.slurm.server.enable = true;
      virtualisation.diskSize = 4096;
    };

  nodes.node =
    { lib, ... }:
    {
      imports = [
        commonModule
        ../modules/pointy-node.nix
      ];

      networking.hostName = "node";

      services.pointy-node = {
        enable = true;
        hub.host = "hub";
        hub.user = "nix-ssh";
        hub.sshKeyFile = "/root/hub-key";
      };

      services.slurm.client.enable = true;

      virtualisation.diskSize = 4096;
    };

  testScript =
    { nodes, ... }:
    ''
      import shlex

      start_all()

      hub.wait_for_unit("sshd.service")
      hub.wait_for_unit("slurmctld.service")
      node.wait_for_unit("slurmd.service")
      hub.wait_until_succeeds("sinfo -h -o '%T' | grep -q idle", timeout=120)

      node.succeed("install -d -m 0700 /root/.ssh")
      node.succeed("ssh-keygen -q -t ed25519 -N " + shlex.quote("") + " -f /root/hub-key")
      node_pubkey = node.succeed("cat /root/hub-key.pub").strip()
      hub.succeed("install -d -m 0755 /etc/ssh/authorized_keys.d")
      hub.succeed(
          "printf '%s\\n' " + shlex.quote(node_pubkey)
          + " > /etc/ssh/authorized_keys.d/nix-ssh"
      )
      hub.succeed("chmod 0444 /etc/ssh/authorized_keys.d/nix-ssh")

      host_key = node.wait_until_succeeds(
          "ssh-keyscan -t ed25519 hub 2>/dev/null | grep -v '^#'", timeout=60
      ).strip()
      node.succeed("install -d -m 0700 /root/.ssh")
      node.succeed(
          "printf '%s\\n' " + shlex.quote(host_key) + " > /root/.ssh/known_hosts"
      )

      assert "ssh-ng://nix-ssh@hub" in node.succeed("nix config show")

      drv_ok = hub.succeed(
          "nix-instantiate --expr "
          + shlex.quote(
              """
              let
                bash = builtins.storePath "${pkgs.bash}";
              in
              derivation {
                name = "pointy-hub-realise-ok";
                system = builtins.currentSystem;
                builder = "''${bash}/bin/bash";
                args = [ "-c" "echo pointy-hub-log-ok >&2; echo ok > $out" ];
              }
              """
          )
      ).strip()

      _, ok_out = hub.execute(
          "sbatch --wait --parsable --output=/root/pointy-ok.out --wrap "
          + shlex.quote("/run/current-system/sw/bin/pointy-realise-hub " + drv_ok),
          check_return=False,
      )
      ok_job_log = node.execute("cat /root/pointy-ok.out 2>&1", check_return=False)[1]
      ok_job_id = ok_out.strip().splitlines()[0]
      ok_job_state = hub.succeed("scontrol show job " + ok_job_id + " -o")
      assert "JobState=COMPLETED" in ok_job_state, ok_job_state + ok_job_log
      assert "ExitCode=0:0" in ok_job_state, ok_job_state + ok_job_log

      hub.succeed("nix path-info " + drv_ok + "^*")
      output = hub.succeed("nix-store -q --outputs " + drv_ok).strip()
      assert hub.succeed("cat " + shlex.quote(output)).strip() == "ok"

      log_ok = hub.succeed("nix log --substituters " + shlex.quote("") + " " + drv_ok)
      assert "pointy-hub-log-ok" in log_ok, log_ok

      drv_fail = hub.succeed(
          "nix-instantiate --expr "
          + shlex.quote(
              """
              let
                bash = builtins.storePath "${pkgs.bash}";
              in
              derivation {
                name = "pointy-hub-realise-fail";
                system = builtins.currentSystem;
                builder = "''${bash}/bin/bash";
                args = [ "-c" "echo pointy-hub-log-fail >&2; exit 7" ];
              }
              """
          )
      ).strip()

      _, fail_out = hub.execute(
          "sbatch --wait --parsable --wrap "
          + shlex.quote("/run/current-system/sw/bin/pointy-realise-hub " + drv_fail),
          check_return=False,
      )
      fail_job_id = fail_out.strip().splitlines()[0]
      fail_job_state = hub.succeed("scontrol show job " + fail_job_id + " -o")
      assert "JobState=FAILED" in fail_job_state, fail_job_state

      log_fail = hub.succeed("nix log --substituters " + shlex.quote("") + " " + drv_fail)
      assert "pointy-hub-log-fail" in log_fail, log_fail

      node.succeed("test -e /run/current-system/sw/bin/pointy-realise-hub")
    '';
}
