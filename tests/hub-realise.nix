{ pkgs }:
let
  mungeKey = "pointy-hub-realise-shared-munge-key-0123456789abcdef";

  nodeSigningKey = "pointy-test-node-1:LattTlnkKJwZ3/3D6c5u2K8gCduGONvKv4rZuv91YkMsQw1DxWzNCVKXbpjbXsap6sXqTPF7WDSWMw73LLt07A==";
  nodePublicKey = "pointy-test-node-1:LEMNQ8VszQlSl26Y217GqerF6kzxe1g0ljMO9yy7dOw=";

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

      users.groups.builder = { };

      users.users.builder = {
        isNormalUser = true;
        uid = 1001;
        group = "builder";
        home = "/home/builder";
        createHome = true;
        shell = pkgs.bashInteractive;
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
        pkgs.jq
        pkgs.openssh
        pkgs.util-linux
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
        nodePublicKeys = [ nodePublicKey ];
        logUser = "pointy-log";
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
        signingKeyFile = "/etc/pointy/node-signing-key";
        nodePublicKeys = [ nodePublicKey ];
        hub = {
          host = "hub";
          user = "nix-ssh";
          logUser = "pointy-log";
          sshKeyFile = "/etc/pointy/hub-key";
        };
      };

      services.slurm.client.enable = true;

      virtualisation.diskSize = 4096;
    };

  testScript =
    { nodes, ... }:
    ''
      import shlex

      node_signing_key = ${builtins.toJSON nodeSigningKey}
      node_public_key = ${builtins.toJSON nodePublicKey}

      start_all()

      hub.wait_for_unit("sshd.service")
      hub.wait_for_unit("nix-daemon.socket")
      hub.wait_for_unit("slurmctld.service")
      node.wait_for_unit("nix-daemon.socket")
      node.wait_for_unit("slurmd.service")
      hub.wait_until_succeeds("sinfo -h -o '%T' | grep -q idle", timeout=120)

      node.succeed("install -d -m 0755 /etc/pointy")
      host_key = node.succeed(
          "ssh-keyscan -t ed25519 hub 2>/dev/null | grep -v '^#'"
      ).strip()
      node.succeed("install -d -m 0700 /root/.ssh")
      node.succeed(
          "printf '%s\\n' " + shlex.quote(host_key) + " > /root/.ssh/known_hosts"
      )
      node.succeed("install -d -m 0700 -o builder -g builder /home/builder/.ssh")
      node.succeed(
          "printf '%s\\n' " + shlex.quote(host_key) + " > /home/builder/.ssh/known_hosts"
      )
      node.succeed("chown builder:builder /home/builder/.ssh/known_hosts")
      node.succeed("chmod 0600 /home/builder/.ssh/known_hosts")

      node.succeed(
          "ssh-keygen -q -t ed25519 -N " + shlex.quote("") + " -f /etc/pointy/hub-key"
      )
      node.succeed("chgrp builder /etc/pointy/hub-key")
      node.succeed("chmod 0440 /etc/pointy/hub-key")
      node_pubkey = node.succeed("cat /etc/pointy/hub-key.pub").strip()

      node.succeed(
          "printf '%s\\n' " + shlex.quote(node_signing_key) + " > /etc/pointy/node-signing-key"
      )
      node.succeed("chgrp builder /etc/pointy/node-signing-key")
      node.succeed("chmod 0440 /etc/pointy/node-signing-key")

      hub.succeed("install -d -m 0755 /etc/ssh/authorized_keys.d")
      for user in ["nix-ssh", "pointy-log"]:
          hub.succeed(
              "printf '%s\\n' " + shlex.quote(node_pubkey)
              + " > /etc/ssh/authorized_keys.d/" + user
          )
          hub.succeed("chmod 0444 /etc/ssh/authorized_keys.d/" + user)

      hub_uri = "ssh-ng://nix-ssh@hub?ssh-key=/etc/pointy/hub-key"

      def submit(drv, output):
          return hub.execute(
              "runuser -u builder -- env HOME=/home/builder USER=builder PATH=/run/current-system/sw/bin "
              "sbatch --parsable --wait --chdir=/tmp "
              "--export=POINTY_REQ_CPU=1,POINTY_REQ_RAM=1G,POINTY_REQ_IOR=0,POINTY_REQ_IOW=0 "
              "--output=/tmp/" + output + " --wrap "
              + shlex.quote("/run/current-system/sw/bin/pointy-realise-hub " + drv),
              check_return=False,
          )

      def job_state(submission):
          job_id = submission[1].strip().splitlines()[0]
          return hub.succeed("scontrol show job " + job_id + " -o")

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

      _, ok_out = submit(drv_ok, "pointy-ok.out")
      ok_job_log = node.execute("cat /tmp/pointy-ok.out 2>&1", check_return=False)[1]
      ok_state = job_state((0, ok_out))
      assert "JobState=COMPLETED" in ok_state, ok_state + ok_job_log
      assert "ExitCode=0:0" in ok_state, ok_state + ok_job_log

      ok_output = hub.succeed("nix-store -q --outputs " + drv_ok).strip()
      assert hub.succeed("cat " + shlex.quote(ok_output)).strip() == "ok"
      assert (
          node_public_key.split(":")[0]
          in hub.succeed("nix path-info --json " + shlex.quote(ok_output))
      )

      ok_log = hub.succeed("nix log --substituters " + shlex.quote("") + " " + drv_ok)
      assert "pointy-hub-log-ok" in ok_log, ok_log

      _, rerun_out = submit(drv_ok, "pointy-ok-rerun.out")
      rerun_job_log = node.execute("cat /tmp/pointy-ok-rerun.out 2>&1", check_return=False)[1]
      rerun_state = job_state((0, rerun_out))
      assert "JobState=COMPLETED" in rerun_state, rerun_state + rerun_job_log
      assert "ExitCode=0:0" in rerun_state, rerun_state + rerun_job_log

      drv_dep = hub.succeed(
          "nix-instantiate --expr "
          + shlex.quote(
              """
              let
                bash = builtins.storePath "${pkgs.bash}";
                failure = derivation {
                  name = "pointy-hub-realise-dep-fail";
                  system = builtins.currentSystem;
                  builder = "''${bash}/bin/bash";
                  args = [ "-c" "echo pointy-hub-log-dep-fail >&2; exit 7" ];
                };
              in
              derivation {
                name = "pointy-hub-realise-dependent";
                system = builtins.currentSystem;
                builder = "''${bash}/bin/bash";
                args = [ "-c" "cat ''${failure} >/dev/null || exit 9; echo ok > $out" ];
              }
              """
          )
      ).strip()
      fail_drv = hub.succeed(
          "nix-store -qR " + shlex.quote(drv_dep)
          + " | grep -- '-pointy-hub-realise-dep-fail.drv$'"
      ).strip()

      _, dep_out = submit(drv_dep, "pointy-dep.out")
      dep_state = job_state((0, dep_out))
      assert "JobState=FAILED" in dep_state, dep_state

      dep_log = hub.succeed("nix log --substituters " + shlex.quote("") + " " + fail_drv)
      assert "pointy-hub-log-dep-fail" in dep_log, dep_log

      drv_unsigned = hub.succeed(
          "nix-instantiate --expr "
          + shlex.quote(
              """
              let
                bash = builtins.storePath "${pkgs.bash}";
              in
              derivation {
                name = "pointy-hub-realise-unsigned";
                system = builtins.currentSystem;
                builder = "''${bash}/bin/bash";
                args = [ "-c" "echo unsigned > $out" ];
              }
              """
          )
      ).strip()
      node.succeed(
          "runuser -u builder -- env HOME=/home/builder USER=builder PATH=/run/current-system/sw/bin nix copy --derivation --from "
          + shlex.quote(hub_uri) + " -- " + drv_unsigned
      )
      node.succeed(
          "runuser -u builder -- env HOME=/home/builder USER=builder PATH=/run/current-system/sw/bin nix build --no-link -- "
          + shlex.quote(drv_unsigned + "^*")
      )
      unsigned_output = node.succeed("nix-store -q --outputs " + drv_unsigned).strip()
      unsigned_status, unsigned_error = node.execute(
          "runuser -u builder -- env HOME=/home/builder USER=builder PATH=/run/current-system/sw/bin nix copy --to "
          + shlex.quote(hub_uri) + " -- " + shlex.quote(drv_unsigned + "^*"),
          check_return=False,
      )
      assert unsigned_status != 0, unsigned_error
      hub_status, _ = hub.execute("nix path-info " + shlex.quote(unsigned_output), check_return=False)
      assert hub_status != 0

      node.succeed("test -e /run/current-system/sw/bin/pointy-realise-hub")
    '';
}
