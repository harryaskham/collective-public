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
          killasgroup=${boolToConf prog.killasgroup}
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

  # Shared shell helpers (bd-63202d). A bare pidfile-alive or port-open probe
  # cannot tell a CURRENT-generation supervisord from a wedged or
  # old-generation one. These let the start path and the activation
  # post-condition agree on (a) which supervisord pid is "ours" and (b) which
  # -c config store path it was launched with, so a single source of truth
  # decides whether to keep or replace the running instance.
  sdRunningPidFn = ''
    __sd_running_pid() {
      # echo the pid of a managed supervisord: pidfile first, then pgrep so the
      # /tmp-wiped (pidfile-gone) case is still covered. empty if none.
      __p=""
      if [ -f "${pidFile}" ]; then
        __p=$(${pkgs.coreutils}/bin/cat "${pidFile}" 2>/dev/null)
        if [ -n "$__p" ] && kill -0 "$__p" 2>/dev/null; then
          echo "$__p"; return 0
        fi
      fi
      for __p in $(${pkgs.procps}/bin/pgrep -f 'supervisord.*supervisord\.conf' 2>/dev/null); do
        echo "$__p"; return 0
      done
      return 0
    }
  '';

  sdRunningConfFn = ''
    __sd_running_conf() {
      # echo the -c config store path the supervisord pid in $1 was launched
      # with, parsed from its NUL-separated /proc/PID/cmdline. empty if it
      # cannot be determined (no pid, unreadable cmdline, or no -c token).
      [ -n "$1" ] || return 0
      ${pkgs.coreutils}/bin/tr '\0' '\n' < /proc/"$1"/cmdline 2>/dev/null \
        | ${pkgs.gnused}/bin/sed -n '/^-c$/{n;p;q;}'
    }
  '';

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

    # Inner launcher, called under flock. Only declines to start when a
    # CURRENT-generation supervisord is positively already running. A bare
    # pidfile-alive or port-open short-circuit used to let a wedged or
    # OLD-generation instance persist; an old-generation instance pinned to a
    # superseded config becomes a GC footgun once nix-collect-garbage -d deletes
    # that generation out from under the still-running process (bd-63202d).
    CONFIGFILE="${configFile}"

    ${sdRunningPidFn}
    ${sdRunningConfFn}

    _pid=$(__sd_running_pid)
    if [ -n "$_pid" ]; then
      _conf=$(__sd_running_conf "$_pid")
      if [ "$_conf" = "$CONFIGFILE" ]; then
        # Current-generation instance already running: nothing to do.
        exit 0
      fi
      if [ -n "$_conf" ]; then
        # Confirmed mismatch: an old-generation / wrong-config supervisord is
        # alive. Replace it so we never leave a process pinned to a GC-able gen.
        echo "[supervisord-start-inner] running supervisord (pid $_pid) serves '$_conf' != current '$CONFIGFILE'; replacing" >&2
        ${supervisorctl-wrapped}/bin/supervisorctl shutdown 2>/dev/null || true
        ${pkgs.coreutils}/bin/sleep 1
        kill -9 "$_pid" 2>/dev/null || true
        rm -f "${pidFile}"
      else
        # Could not read the running instance's config (e.g. unreadable cmdline
        # under proot). Stay conservative and do not churn a possibly-healthy
        # instance; the activation post-condition is the stronger backstop.
        exit 0
      fi
    else
      # No managed supervisord found. Last-resort concurrent-start guard only:
      # if the port is already bound, another instance is coming up right now --
      # do not double-start and race on the bind.
      if ${pkgs.curl}/bin/curl -sf --max-time 1 http://127.0.0.1:${toString cfg.port}/ >/dev/null 2>&1; then
        exit 0
      fi
    fi

    exec ${supervisor}/bin/supervisord -c "$CONFIGFILE"
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
            # Ask supervisord to stop all programs first (cleanest path)
            ${supervisorctl-wrapped}/bin/supervisorctl shutdown 2>/dev/null || true
            for i in $(seq 1 15); do
              kill -0 "$OLD_PID" 2>/dev/null || break
              sleep 0.5
            done
            # Force kill if still around
            kill -9 "$OLD_PID" 2>/dev/null || true
          fi
          rm -f "$PIDFILE"
        fi

        # Kill any orphaned managed processes from previous generations.
        # Under proot, process groups don't work reliably, so we kill by
        # matching the nix store paths of known managed commands.
        #
        # Two-phase TERM-then-KILL: send SIGTERM first so processes with a
        # cleanup trap get to run it -- nod-exec-server in particular reaps its
        # two socat listeners on TERM, freeing ports 18357/18358. A bare kill -9
        # bypasses that trap and orphans the socat listeners, which then block
        # the next nod-exec bind with EADDRINUSE (the recurring "unstick
        # nod-exec" footgun). We also match the socat EXEC handler path directly
        # ('socat.*nod-exec-handler', port-agnostic) so a survivor is reaped even
        # if its parent server already died. Patterns stay single-quoted (they
        # contain '*') to avoid shell glob/word-split surprises.
        __sd_sweep_orphans() {
          for pattern in \
            'supervisord.*supervisord.conf' \
            'sops-watcher-supervisord' \
            'am-supervisor' \
            'caco-supervise' \
            'litellm-proxy-start' \
            'nod-exec-server' \
            'socat.*nod-exec-handler' \
            'dbus-start'; do
            for pid in $(${pkgs.procps}/bin/pgrep -f "$pattern" 2>/dev/null || true); do
              kill "-$1" "$pid" 2>/dev/null || true
            done
          done
        }
        __sd_sweep_orphans TERM
        # Give trap-based cleanups a moment, then SIGKILL any survivors.
        sleep 1
        __sd_sweep_orphans KILL

        $DRY_RUN_CMD ${supervisord-start}/bin/supervisord-start

        # Post-condition (bd-63202d): assert the now-running supervisord serves
        # THIS generation's config. If a stale/old-generation instance survived
        # the kills above (e.g. a proot session where the kill did not take),
        # force a replacement so a switch can never silently leave the old
        # generation pinned (the GC footgun this bead is about). Skipped on dry
        # runs since nothing was actually (re)started.
        if [ -z "$DRY_RUN_CMD" ]; then
          ${sdRunningPidFn}
          ${sdRunningConfFn}
          _sd_pid=$(__sd_running_pid)
          _sd_conf=$(__sd_running_conf "$_sd_pid")
          if [ -n "$_sd_conf" ] && [ "$_sd_conf" != "${configFile}" ]; then
            echo "[supervisord] post-activation: running config '$_sd_conf' != current '${configFile}'; forcing replacement" >&2
            ${supervisorctl-wrapped}/bin/supervisorctl shutdown 2>/dev/null || true
            ${pkgs.coreutils}/bin/sleep 1
            [ -n "$_sd_pid" ] && kill -9 "$_sd_pid" 2>/dev/null || true
            rm -f "${pidFile}"
            ${supervisord-start}/bin/supervisord-start
          fi
          unset _sd_pid _sd_conf
        fi
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
