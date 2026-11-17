{ config, lib, pkgs, ... }:
let
  cfg = config.services.pointy-slurm-events;
  slurmConfigured = config.services ? slurm;
  slurmEnabled = slurmConfigured && (config.services.slurm.server.enable or false);
  slurmClientEnabled = slurmConfigured && (config.services.slurm.client.enable or false);
  slurmPackage = if slurmConfigured then config.services.slurm.package else pkgs.slurm;
  slurmConf = if slurmConfigured then "${config.services.slurm.etcSlurm}/slurm.conf" else "/etc/slurm/slurm.conf";
  slurmUser = if slurmConfigured then config.services.slurm.user else "slurm";
  brokerBinary = "${cfg.package}/bin/pointy-slurm-broker";

  watcher = pkgs.writeShellScriptBin "pointy-slurm-watch" ''
    exec ${brokerBinary} watch --socket ${lib.escapeShellArg cfg.socketPath} native
  '';

  notifier = pkgs.writeShellScriptBin "pointy-slurm-notify" ''
    exec ${brokerBinary} notify --socket ${lib.escapeShellArg cfg.socketPath} "$@"
  '';

  nodeTriggerScript = pkgs.writeShellScript "pointy-slurm-events-node" ''
    exec ${cfg.notifyCommand} --event node
  '';

  canAppendCompletion = cfg.jobCompletionFile != null;
  jobCompletionScript = pkgs.writeShellScript "pointy-slurm-events-jobcomp" ''
    if [ -n "''${JOBID:-}" ] && [ -n "''${JOBNAME:-}" ]; then
      ${lib.optionalString canAppendCompletion ''
        start=Unknown
        end=Unknown
        submit=Unknown
        eligible=Unknown
        if [ "''${START:-0}" -gt 0 ] 2>/dev/null; then
          start=$(${pkgs.coreutils}/bin/date -d "@$START" +%FT%T)
        fi
        if [ "''${END:-0}" -gt 0 ] 2>/dev/null; then
          end=$(${pkgs.coreutils}/bin/date -d "@$END" +%FT%T)
        fi
        if [ "''${SUBMIT:-0}" -gt 0 ] 2>/dev/null; then
          submit=$(${pkgs.coreutils}/bin/date -d "@$SUBMIT" +%FT%T)
        fi
        if [ "''${ELIGIBLE:-0}" -gt 0 ] 2>/dev/null; then
          eligible=$(${pkgs.coreutils}/bin/date -d "@$ELIGIBLE" +%FT%T)
        fi
        line="JobId=$JOBID UserId=''${USERNAME:-unknown}(''${UID:-0}) GroupId=''${GROUPNAME:-unknown}(''${GID:-0}) Name=$JOBNAME JobState=''${JOBSTATE:-UNKNOWN} Partition=''${PARTITION:-} TimeLimit=UNLIMITED StartTime=$start EndTime=$end NodeList=''${NODES:-} NodeCnt=''${NODECNT:-0} ProcCnt=''${PROCS:-0} WorkDir=''${WORK_DIR:-unknown} ReservationName=''${RESERVATION:-} Tres= Account=''${ACCOUNT:-} QOS=''${QOS:-} WcKey=''${WCKEY:-} Cluster=''${CLUSTER:-unknown} SubmitTime=$submit EligibleTime=$eligible DerivedExitCode=''${DERIVED_EC:-0:0} ExitCode=''${EXITCODE:-0:0} SlurmRC=0 "
        exec 9>>${lib.escapeShellArg cfg.jobCompletionFile}
        ${pkgs.coreutils}/bin/chmod 0644 ${lib.escapeShellArg cfg.jobCompletionFile}
        ${pkgs.util-linux}/bin/flock -x 9
        printf '%s\n' "$line" >&9
      ''}
      ${cfg.notifyCommand} --event terminal \
        --job "$JOBID" \
        --name "$JOBNAME" \
        --account "''${ACCOUNT:-}" \
        --state "''${JOBSTATE:-}" || true
    fi
    exit 0
  '';

  prologScript = pkgs.writeShellScript "pointy-slurm-events-prolog" ''
    ${cfg.notifyCommand} --event start \
      --job "''${SLURM_JOB_ID:-}" \
      --name "''${SLURM_JOB_NAME:-}" \
      --account "''${SLURM_JOB_ACCOUNT:-''${SLURM_ACCOUNT:-}}" || true
    exit 0
  '';

  triggerSetup = pkgs.writeShellApplication {
    name = "pointy-slurm-events-triggers";
    runtimeInputs = [
      cfg.slurmPackage
      pkgs.coreutils
      pkgs.gawk
      pkgs.util-linux
    ];
    text = ''
      export SLURM_CONF=${lib.escapeShellArg slurmConf}
      program=${lib.escapeShellArg "${nodeTriggerScript}"}
      slurm_user=${lib.escapeShellArg slurmUser}

      as_slurm() {
        SLURM_CONF="$SLURM_CONF" ${pkgs.util-linux}/bin/runuser -u "$slurm_user" -- "$@"
      }

      ready=0
      attempts=0
      while [ "$attempts" -lt 100 ]; do
        attempts=$((attempts + 1))
        if as_slurm scontrol ping >/dev/null 2>&1; then
          ready=1
          break
        fi
        sleep 0.1
      done
      if [ "$ready" -ne 1 ]; then
        echo "pointy-slurm-events: slurmctld did not become ready" >&2
        exit 1
      fi

      strigger --get --noheader |
        awk 'index($0, "pointy-slurm-events-node") { print $1 }' |
        while read -r stale; do
          strigger --clear --id="$stale" >/dev/null || true
        done

      has_trigger() {
        as_slurm strigger --get --noheader |
          awk -v type="$1" -v program="$program" '$4 == type && index($0, program) { found = 1 } END { exit !found }'
      }

      set_trigger() {
        if ! has_trigger "$1"; then
          as_slurm strigger --set --flags=perm --node "--$1" --program="$program" >/dev/null
        fi
      }

      for event in down drained draining up resume fail reconfig; do
        set_trigger "$event"
      done
    '';
  };
in
{
  options.services.pointy-slurm-events = {
    enable = lib.mkEnableOption "the Pointy Slurm event broker and lifecycle hooks";

    package = lib.mkOption {
      type = lib.types.package;
      readOnly = true;
      default = pkgs.callPackage ./package.nix { };
      defaultText = lib.literalExpression "pkgs.callPackage ./package.nix { }";
      description = "Package providing bin/pointy-slurm-broker, the local Slurm event broker and notifier.";
    };

    brokerCommand = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = brokerBinary;
      description = "Path of the broker executable for gateways that stream account-filtered snapshots and submit notifications.";
    };

    watchCommand = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${watcher}/bin/pointy-slurm-watch";
      description = "Command the notebook executes, with no arguments, to receive native job snapshots and heartbeats as NDJSON.";
    };

    notifyCommand = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${notifier}/bin/pointy-slurm-notify";
      description = "Command lifecycle hooks run to send one event to the broker. It never fails a Slurm job.";
    };

    jobCompletionHook = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${jobCompletionScript}";
      description = "Script for JobCompLoc when JobCompType=jobcomp/script.";
    };

    prologHook = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${prologScript}";
      description = "Script for the Slurm Prolog setting; it notifies a job start after allocation.";
    };

    nodeTriggerHook = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "${nodeTriggerScript}";
      description = "Program Slurm strigger runs for node health and configuration changes.";
    };

    socketPath = lib.mkOption {
      type = lib.types.str;
      default = "/run/pointy-slurm-events/broker.sock";
      description = "Local UNIX socket the broker listens on and the watcher and notifier connect to.";
    };

    group = lib.mkOption {
      type = lib.types.str;
      readOnly = true;
      default = "pointy-slurm-events";
      description = "Group allowed to connect to the broker socket. Add the backend user through extraUsers.";
    };

    extraUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Users added to group so their notebook backend or job runner can watch and notify.";
    };

    heartbeatIntervalSeconds = lib.mkOption {
      type = lib.types.ints.positive;
      default = 10;
      description = "Interval between broker heartbeat frames. Heartbeats never run Slurm commands.";
    };

    accountWatchUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "Users allowed to request account-filtered watches. Native watches are allowed for the socket group; the shared cluster gateway user belongs here.";
    };

    scopePrefix = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Prefix placed before a Slurm account name in job names for account-filtered watches. When null, account watches are refused.";
      example = "site-";
    };

    stateFile = lib.mkOption {
      type = lib.types.str;
      default = "/var/lib/pointy-slurm-events/terminal-state.json";
      description = "Bounded terminal-job hints persisted across broker and controller restarts.";
    };

    trustedUsers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "root" ] ++ lib.optional slurmConfigured slurmUser;
      defaultText = lib.literalExpression ''[ "root" "slurm" ]'';
      description = "Producer users allowed to submit terminal, start, node and reset events. Saving events from unprivileged runners are restricted to the runner's own Slurm job cgroup.";
    };

    configureSlurm = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Configure JobComp, Prolog, Slurm trigger hooks and slurmd lifecycle notifications when the Slurm server module is present.";
    };

    jobCompletionFile = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      description = "Optional jobcomp/filetxt log to append when JobCompType becomes jobcomp/script, preserving sacct -f accounting.";
    };

    slurmPackage = lib.mkOption {
      type = lib.types.package;
      default = slurmPackage;
      defaultText = lib.literalExpression "config.services.slurm.package";
      description = "Slurm package providing squeue, sinfo, scontrol and strigger.";
    };
  };

  config = lib.mkIf cfg.enable (lib.mkMerge [
    {
      users.groups.${cfg.group} = { };
      users.users = lib.genAttrs (lib.unique (cfg.extraUsers ++ lib.optional slurmConfigured slurmUser)) (name: {
        extraGroups = [ cfg.group ];
      });

      environment.systemPackages = [ watcher ];

      systemd.tmpfiles.rules = [
        "d ${dirOf cfg.socketPath} 0750 root ${cfg.group} -"
        "d ${dirOf cfg.stateFile} 0750 root ${cfg.group} -"
      ];

      systemd.services.pointy-slurm-events = {
        description = "Pointy Slurm event broker";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "simple";
          ExecStart = lib.escapeShellArgs (
            [
              brokerBinary
              "serve"
              "--socket"
              cfg.socketPath
              "--heartbeat"
              (toString cfg.heartbeatIntervalSeconds)
              "--trusted-users"
              (lib.concatStringsSep "," (lib.unique cfg.trustedUsers))
            ]
              ++ lib.optionals (cfg.accountWatchUsers != [ ]) [
                "--account-watch-users"
                (lib.concatStringsSep "," (lib.unique cfg.accountWatchUsers))
              ]
              ++ lib.optionals (cfg.scopePrefix != null) [
                "--scope-prefix"
                (toString cfg.scopePrefix)
              ]
              ++ [
                "--state-file"
                cfg.stateFile
              ]
          );
          Restart = "always";
          RestartSec = 1;
          UMask = "0077";
          Group = cfg.group;
        };
        environment = {
          SLURM_CONF = slurmConf;
        };
        path = [
          cfg.slurmPackage
          pkgs.coreutils
        ];
      };
    }

    (lib.mkIf slurmEnabled {
      systemd.services.pointy-slurm-events = {
        upheldBy = [ "slurmctld.service" ];
        bindsTo = [ "slurmctld.service" ];
        partOf = [ "slurmctld.service" ];
        after = [ "slurmctld.service" ];
      };
    })

    (lib.mkIf (cfg.configureSlurm && slurmEnabled) {
      services.slurm.extraConfig = lib.mkAfter ''
        JobCompType=jobcomp/script
        JobCompLoc=${cfg.jobCompletionHook}
        Prolog=${cfg.prologHook}
      '';

      systemd.services.pointy-slurm-events-triggers = {
        description = "Pointy Slurm node health and configuration triggers";
        wantedBy = [ "multi-user.target" ];
        after = [
          "pointy-slurm-events.service"
          "slurmctld.service"
        ];
        wants = [
          "pointy-slurm-events.service"
          "slurmctld.service"
        ];
        partOf = [ "slurmctld.service" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = "${triggerSetup}/bin/pointy-slurm-events-triggers";
        };
      };
    })

    (lib.mkIf (cfg.configureSlurm && slurmClientEnabled) {
      systemd.services.slurmd.postStart = lib.mkAfter "${nodeTriggerScript} || true";
      systemd.services.slurmd.preStop = lib.mkAfter "${nodeTriggerScript} || true";
    })
  ]);
}
