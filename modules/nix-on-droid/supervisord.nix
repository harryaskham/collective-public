{ config, lib, pkgs, typed, untyped, ... }:

# Supervisord process manager for nix-on-droid.
# Provides a lightweight systemd-like service manager that persists
# daemons through app restarts. Other modules declare programs via
# supervisord.programs.NAME = { command = "..."; ... };

with typed;
with lib;

let
  cfg = config.supervisord;

  boolToConf = b: if b then "true" else "false";

  autorestartToConf = v:
    if v == true then "true"
    else if v == false then "false"
    else v; # "unexpected"

  programOpts = { name, ... }: {
    options = {
      command = mkOption {
        type = types.str;
        description = "The command to execute.";
      };
      directory = mkOption {
        type = types.str;
        default = "/";
        description = "Working directory for the program.";
      };
      environment = mkOption {
        type = types.attrsOf types.str;
        default = {};
        description = "Environment variables for the program.";
      };
      path = mkOption {
        type = types.listOf types.package;
        default = [];
        description = "Packages to add to PATH for the program.";
      };
      autostart = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to start this program when supervisord starts.";
      };
      autorestart = mkOption {
        type = types.either types.bool (types.enum [ "unexpected" ]);
        default = true;
        description = "Whether to restart the program when it exits. Can be true, false, or \"unexpected\".";
      };
      startsecs = mkOption {
        type = types.int;
        default = 1;
        description = "Seconds the program must stay running to consider the start successful. Use 0 for oneshot.";
      };
      stopsignal = mkOption {
        type = types.str;
        default = "TERM";
        description = "Signal to send when stopping the program.";
      };
      stopasgroup = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to send the stop signal to the whole process group.";
      };
      redirect_stderr = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to redirect stderr to stdout.";
      };
    };
  };

  pidFile = "${cfg.stateDir}/run/supervisord.pid";

  configFile = pkgs.writeText "supervisord.conf" ''
    [supervisord]
    pidfile=${pidFile}
    childlogdir=${cfg.stateDir}/log/
    logfile=${cfg.stateDir}/log/supervisord.log
    nodaemon=false

    [supervisorctl]
    serverurl = http://localhost:${toString cfg.port}

    [inet_http_server]
    port = 127.0.0.1:${toString cfg.port}

    [rpcinterface:supervisor]
    supervisor.rpcinterface_factory = supervisor.rpcinterface:make_main_rpcinterface

    ${concatStringsSep "\n" (mapAttrsToList (name: prog:
      let
        pathStr = concatStringsSep ":" (map (p: "${p}/bin") prog.path);
        envPairs = mapAttrsToList (k: v: "${k}=\"${v}\"") (
          prog.environment // optionalAttrs (prog.path != []) {
            PATH = concatStringsSep ":" (
              [ "%(ENV_PATH)s" pathStr ]
              ++ optional (prog.environment ? PATH) prog.environment.PATH
            );
          }
        );
      in ''
        [program:${name}]
        command=${prog.command}
        directory=${prog.directory}
        autostart=${boolToConf prog.autostart}
        autorestart=${autorestartToConf prog.autorestart}
        startsecs=${toString prog.startsecs}
        stopsignal=${prog.stopsignal}
        stopasgroup=${boolToConf prog.stopasgroup}
        redirect_stderr=${boolToConf prog.redirect_stderr}
        ${optionalString (envPairs != []) "environment=${concatStringsSep "," envPairs}"}
      ''
    ) cfg.programs)}
  '';

  supervisor = pkgs.python3Packages.supervisor;

  supervisord-start = pkgs.writeScriptBin "supervisord-start" ''
    #!${pkgs.runtimeShell}

    # Idempotent supervisord launcher: only starts if not already running.
    PIDFILE="${pidFile}"
    STATEDIR="${cfg.stateDir}"

    mkdir -p "$STATEDIR"/{run,log}

    # Check if already running
    if [ -f "$PIDFILE" ]; then
      PID=$(cat "$PIDFILE" 2>/dev/null)
      if [ -n "$PID" ] && kill -0 "$PID" 2>/dev/null; then
        exit 0
      fi
      # Stale pidfile
      rm -f "$PIDFILE"
    fi

    exec ${supervisor}/bin/supervisord -c ${configFile}
  '';

  supervisorctl-wrapped = pkgs.writeScriptBin "supervisorctl" ''
    #!${pkgs.runtimeShell}
    exec ${supervisor}/bin/supervisorctl -c ${configFile} "$@"
  '';

in {
  options.supervisord = {
    enable = mkEnable "Whether to enable the supervisord process manager.";

    stateDir = mkOption {
      type = types.str;
      default = "/tmp/run/supervisord";
      description = "Directory for supervisord state (pid, logs).";
    };

    port = mkOption {
      type = types.port;
      default = 65123;
      description = "Port for the supervisord HTTP control interface (localhost only).";
    };

    programs = mkOption {
      type = types.attrsOf (types.submodule programOpts);
      default = {};
      description = "Programs to manage with supervisord.";
    };
  };

  config = mkIf cfg.enable {
    environment.packages = [
      supervisord-start
      supervisorctl-wrapped
    ];

    # Start/restart supervisord during nix-on-droid activation.
    # Always restart to pick up config changes (new programs, etc).
    build.activationAfter.supervisord = ''
      PIDFILE="${pidFile}"
      if [ -f "$PIDFILE" ]; then
        OLD_PID=$(cat "$PIDFILE" 2>/dev/null)
        if [ -n "$OLD_PID" ] && kill -0 "$OLD_PID" 2>/dev/null; then
          echo "[supervisord] Stopping old instance (pid $OLD_PID) to pick up config changes..."
          kill "$OLD_PID" 2>/dev/null || true
          # Wait for it to stop
          for i in $(seq 1 10); do
            kill -0 "$OLD_PID" 2>/dev/null || break
            sleep 0.5
          done
          kill -9 "$OLD_PID" 2>/dev/null || true
        fi
        rm -f "$PIDFILE"
      fi
      $DRY_RUN_CMD ${supervisord-start}/bin/supervisord-start
    '';

    # Belt-and-suspenders: also check on every shell open
    shell.init = let pf = pidFile; in ''
      # Ensure supervisord is running (survives app restarts)
      _sd_pid=""
      if [ -f "${pf}" ]; then
        _sd_pid=$(cat "${pf}" 2>/dev/null)
      fi
      if [ -z "$_sd_pid" ] || ! kill -0 "$_sd_pid" 2>/dev/null; then
        ${supervisord-start}/bin/supervisord-start &
      fi
      unset _sd_pid
    '';
  };
}
