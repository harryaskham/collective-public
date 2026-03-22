{
  config,
  lib,
  pkgs,
  typed,
  untyped,
  ...
}:
# Supervisord process manager for nix-on-droid.
# Provides a lightweight systemd-like service manager that persists
# daemons through app restarts. Other modules declare programs via
# supervisord.programs.NAME = { command = "..."; ... };
with typed;
with lib; let
  cfg = config.supervisord;
  homePrograms = lib.attrByPath ["home-manager" "config" "supervisord" "programs"] {} config;
  homeProgramNames = builtins.attrNames homePrograms;
  duplicateProgramNames = lib.intersectLists (builtins.attrNames cfg.programs) homeProgramNames;
  allPrograms = cfg.programs // homePrograms;

  boolToConf = b:
    if b
    then "true"
    else "false";

  autorestartToConf = v:
    if v == true
    then "true"
    else if v == false
    then "false"
    else v; # "unexpected"
  programOpts = import ../supervisord-program-options.nix { inherit lib typed; };

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

    ${concatStringsSep "\n" (mapAttrsToList (
        name: prog: let
          pathStr = concatStringsSep ":" (map (p: "${p}/bin") prog.path);
          envLine =
            if isString prog.environment
            then prog.environment
            else let
              envPairs = mapAttrsToList (k: v: "${k}=\"${v}\"") (
                prog.environment
                // optionalAttrs (prog.path != []) {
                  PATH = concatStringsSep ":" (
                    ["%(ENV_PATH)s" pathStr]
                    ++ optional (prog.environment ? PATH) prog.environment.PATH
                  );
                }
              );
            in optionalString (envPairs != []) (concatStringsSep "," envPairs);
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
          ${optionalString (envLine != "") "environment=${envLine}"}
          ${optionalString (prog.stdout_logfile != null) "stdout_logfile=${prog.stdout_logfile}"}
          ${optionalString (prog.stderr_logfile != null) "stderr_logfile=${prog.stderr_logfile}"}
        ''
      )
      allPrograms)}
  '';

  supervisor = pkgs.python3Packages.supervisor;

  flock = "${pkgs.util-linux}/bin/flock";
  lockFile = "${cfg.stateDir}/run/supervisord.lock";

  supervisord-start = pkgs.writeScriptBin "supervisord-start" ''
    #!${pkgs.runtimeShell}

    # Idempotent supervisord launcher: only starts if not already running.
    # Uses flock to serialize concurrent startup attempts (race-safe).
    PIDFILE="${pidFile}"
    STATEDIR="${cfg.stateDir}"
    LOCKFILE="${lockFile}"

    mkdir -p "$STATEDIR"/{run,log}

    exec ${flock} "$LOCKFILE" ${supervisord-start-inner}/bin/supervisord-start-inner
  '';

  supervisord-start-inner = pkgs.writeScriptBin "supervisord-start-inner" ''
    #!${pkgs.runtimeShell}

    # Inner launcher, called under flock. Checks pidfile and port before starting.
    PIDFILE="${pidFile}"

    # Check if already running via pidfile
    if [ -f "$PIDFILE" ]; then
      PID=$(cat "$PIDFILE" 2>/dev/null)
      if [ -n "$PID" ] && kill -0 "$PID" 2>/dev/null; then
        exit 0
      fi
      # Stale pidfile
      rm -f "$PIDFILE"
    fi

    # Defence-in-depth: check if port is already bound (another instance starting)
    if ${pkgs.curl}/bin/curl -sf --max-time 1 http://127.0.0.1:${toString cfg.port}/ >/dev/null 2>&1; then
      exit 0
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

  config = mkMerge [
    (mkIf (homeProgramNames != []) {
      supervisord.enable = mkDefault true;
    })

    (mkIf cfg.enable {
      assertions = [
        {
          assertion = duplicateProgramNames == [];
          message = ''
            Duplicate supervisord program names were declared in both the
            nix-on-droid system config and home-manager config:
            ${concatStringsSep ", " duplicateProgramNames}
          '';
        }
      ];

      environment.packages = [
        supervisord-start
        supervisorctl-wrapped
      ];

      # Start/restart supervisord during nix-on-droid activation.
      # Always restart to pick up config changes (new programs, etc).
      build.activationAfter.supervisord = ''
        PIDFILE="${pidFile}"

        # Kill any existing supervisord and ALL its children
        if [ -f "$PIDFILE" ]; then
          OLD_PID=$(cat "$PIDFILE" 2>/dev/null)
          if [ -n "$OLD_PID" ] && kill -0 "$OLD_PID" 2>/dev/null; then
            echo "[supervisord] Stopping old instance (pid $OLD_PID) and children..."
            # Kill the whole process group
            kill -- -"$OLD_PID" 2>/dev/null || true
            kill "$OLD_PID" 2>/dev/null || true
            for i in $(seq 1 10); do
              kill -0 "$OLD_PID" 2>/dev/null || break
              sleep 0.5
            done
            kill -9 "$OLD_PID" 2>/dev/null || true
          fi
          rm -f "$PIDFILE"
        fi

        # Also kill any orphaned supervisord/managed processes from previous generations
        for pid in $(${pkgs.procps}/bin/pgrep -f 'supervisord.*supervisord.conf' 2>/dev/null); do
          kill -9 "$pid" 2>/dev/null || true
        done
        for pid in $(${pkgs.procps}/bin/pgrep -f 'sops-watcher-supervisord' 2>/dev/null); do
          kill -9 "$pid" 2>/dev/null || true
        done

        $DRY_RUN_CMD ${supervisord-start}/bin/supervisord-start
      '';

      # Belt-and-suspenders: also check on every shell open.
      # Uses flock to prevent races when multiple shells open simultaneously.
      # The entire check+start runs in a background subshell so shell startup
      # isn't blocked waiting for the lock or for supervisord to bind its port.
      shell.init = let
        pf = pidFile;
      in ''
        # Ensure supervisord is running (survives app restarts)
        {
          _sd_pid=""
          if [ -f "${pf}" ]; then
            _sd_pid=$(cat "${pf}" 2>/dev/null)
          fi
          if [ -z "$_sd_pid" ] || ! kill -0 "$_sd_pid" 2>/dev/null; then
            ${supervisord-start}/bin/supervisord-start
          fi
          unset _sd_pid
        } >/dev/null 2>&1 &
        disown 2>/dev/null
      '';
    })
  ];
}
