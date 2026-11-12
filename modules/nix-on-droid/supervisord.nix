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
  execCanaryName = "collective-exec-canary";
  execCanaryLib = builtins.readFile ./supervisord-exec-canary.sh;

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

  # supervisord refuses to start at all when any program's log file has no
  # parent directory, and fails the program when its working directory is
  # missing. The start path creates those first. Only literal absolute paths:
  # not /dev/*, NONE/AUTO or %(...)s expansions.
  isLiteralPath = p: p != null && hasPrefix "/" p && !(hasPrefix "/dev/" p) && !(hasInfix "%(" p);
  programLogDirs = prog: unique (
    map dirOf (filter isLiteralPath [prog.stdout_logfile prog.stderr_logfile])
    ++ optional (prog.directory != "/" && isLiteralPath prog.directory) prog.directory
  );

  # Programs with requiresPath (e.g. caco, whose state a bootstrap creates) get
  # their own config file, included from confDir only while that path exists.
  # Their directories are left to the bootstrap: creating them here would hide
  # that it has not run.
  conditionalPrograms = filterAttrs (_: prog: prog.requiresPath != null) allPrograms;
  unconditionalPrograms = filterAttrs (_: prog: prog.requiresPath == null) allPrograms;
  programDirs = unique (concatLists (mapAttrsToList (_: programLogDirs) unconditionalPrograms));
  confDir = "${cfg.stateDir}/conf.d";

  renderProgram = name: prog: let
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
    ${optionalString (prog.stopwaitsecs != null) "stopwaitsecs=${toString prog.stopwaitsecs}"}
    stopasgroup=${boolToConf prog.stopasgroup}
    killasgroup=${boolToConf prog.killasgroup}
    redirect_stderr=${boolToConf prog.redirect_stderr}
    ${optionalString (envLine != "") "environment=${envLine}"}
    ${optionalString (prog.stdout_logfile != null) "stdout_logfile=${prog.stdout_logfile}"}
    ${optionalString (prog.stderr_logfile != null) "stderr_logfile=${prog.stderr_logfile}"}
  '';

  conditionalConfFiles = mapAttrs (name: prog:
    pkgs.writeText "supervisord-program-${name}.conf" (renderProgram name prog))
  conditionalPrograms;

  # Under the start lock: link each conditional program whose path exists into
  # confDir, warn about the rest, and note whether the set changed so a running
  # supervisord can reread it.
  linkConditionalPrograms = optionalString (conditionalPrograms != {}) ''
    CONFD=${escapeShellArg confDir}
    mkdir -p "$CONFD"
    __sd_confd() {
      for __f in "$CONFD"/*.conf; do
        [ -L "$__f" ] && echo "$__f -> $(${pkgs.coreutils}/bin/readlink "$__f")"
      done
      return 0
    }
    _confd_before=$(__sd_confd)
    rm -f "$CONFD"/*.conf
    ${concatStringsSep "\n" (mapAttrsToList (name: prog: ''
        if [ -e ${escapeShellArg prog.requiresPath} ]; then
          ${optionalString (programLogDirs prog != []) "mkdir -p ${escapeShellArgs (programLogDirs prog)}"}
          ${pkgs.coreutils}/bin/ln -sfn ${conditionalConfFiles.${name}} "$CONFD"/${escapeShellArg "${name}.conf"}
        else
          echo ${escapeShellArg ("[supervisord] warning: not starting ${name}: ${prog.requiresPath} does not exist."
            + optionalString (prog.requiresHint != "") " ${prog.requiresHint}")} >&2
        fi
      '')
      conditionalPrograms)}
    _confd_changed=0
    [ "$(__sd_confd)" = "$_confd_before" ] || _confd_changed=1
  '';

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

    # On-demand proof that the live proot session can still exec a new child.
    # Never autostarts; supervisord-start-inner invokes it only when an existing
    # current-generation supervisor would otherwise be accepted as healthy.
    [program:${execCanaryName}]
    command=${pkgs.coreutils}/bin/true
    directory=/
    autostart=false
    autorestart=false
    startsecs=0
    startretries=1
    exitcodes=0
    redirect_stderr=true
    stdout_logfile=/dev/null

    ${concatStringsSep "\n" (mapAttrsToList renderProgram unconditionalPrograms)}
    ${optionalString (conditionalPrograms != {}) ''
      [include]
      files = ${confDir}/*.conf
    ''}
  '';

  supervisor = pkgs.python3Packages.supervisor;

  flock = "${pkgs.util-linux}/bin/flock";
  lockFile = "${cfg.stateDir}/run/supervisord.lock";
  # Longest a starter waits for the lock. A holder is another starter: at most
  # an exec canary probe, a shutdown of the old instance and supervisord's own
  # start, a few seconds under proot.
  startLockTimeout = 30;
  # A timed-out wait, told apart from the inner script's own failures.
  startLockBusy = 75;

  # Ends every starter: those waiting for the lock and one inside the inner
  # script. A starter left waiting takes the lock the moment the running
  # supervisord stops and starts one from its own, possibly older, generation.
  sdKillStartersFn = ''
    __sd_kill_starters() {
      for __p in $(${pkgs.procps}/bin/pgrep -f 'supervisord-start-inner' 2>/dev/null || true); do
        kill "-$1" "$__p" 2>/dev/null || true
      done
    }
  '';

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
    ${optionalString (programDirs != []) "mkdir -p ${escapeShellArgs programDirs}"}

    # An activation holds the lock itself across its stop and start.
    if [ "''${1:-}" = --locked ]; then
      exec ${supervisord-start-inner}/bin/supervisord-start-inner
    fi

    # The lock covers the start only. --close keeps the lock's descriptor out
    # of the inner script, and so out of the supervisord it becomes. Without it
    # the daemon held the lock for its whole life: every later starter queued
    # behind it for good, and when an activation stopped supervisord the oldest
    # queued starter took the lock and started its own generation's supervisord,
    # which left the activation's start waiting forever (astra and sgu24,
    # 2026-11-12: switches hung at "Activating supervisord" for hours).
    # --timeout bounds the wait whatever holds the lock.
    exec ${flock} --close --timeout ${toString startLockTimeout} \
      --conflict-exit-code ${toString startLockBusy} \
      "$LOCKFILE" ${supervisord-start-inner}/bin/supervisord-start-inner
  '' // {
    # For tests: the locked inner script links conditional programs.
    inherit supervisord-start-inner;
  };

  supervisord-start-inner = pkgs.writeScriptBin "supervisord-start-inner" ''
    #!${pkgs.runtimeShell}

    # Inner launcher, called under flock. Only declines to start when a
    # CURRENT-generation supervisord is positively already running. A bare
    # pidfile-alive or port-open short-circuit used to let a wedged or
    # OLD-generation instance persist; an old-generation instance pinned to a
    # superseded config becomes a GC footgun once nix-collect-garbage -d deletes
    # that generation out from under the still-running process (bd-63202d).
    CONFIGFILE="${configFile}"

    ${linkConditionalPrograms}

    ${sdRunningPidFn}
    ${sdRunningConfFn}
    ${execCanaryLib}

    __sd_apply_confd() {
      # A kept instance must reread conf.d to add or drop conditional programs.
      [ "''${_confd_changed:-0}" = 1 ] || return 0
      ${supervisorctl-wrapped}/bin/supervisorctl reread >/dev/null 2>&1 || true
      ${supervisorctl-wrapped}/bin/supervisorctl update >/dev/null 2>&1 || true
    }

    __sd_replace_running() {
      _replace_pid="$1"
      _replace_reason="$2"
      echo "[supervisord-start-inner] $_replace_reason; replacing pid $_replace_pid" >&2
      ${supervisorctl-wrapped}/bin/supervisorctl shutdown 2>/dev/null || true
      ${pkgs.coreutils}/bin/sleep 1
      kill -9 "$_replace_pid" 2>/dev/null || true
      rm -f "${pidFile}"
    }

    _pid=$(__sd_running_pid)
    if [ -n "$_pid" ]; then
      _conf=$(__sd_running_conf "$_pid")
      if [ "$_conf" = "$CONFIGFILE" ]; then
        # The config matches, but a stale boot-proot can keep supervisord/RPC
        # alive while every new execve fails ENOENT. Probe a known-good store
        # executable. Only the exact on-device ENOENT signal permits restart;
        # timeout or any other supervisor response is conservative/no-churn.
        _canary_rc=0
        SUPERVISORD_CANARY_TIMEOUT_SECONDS=3 \
          supervisord_exec_canary_probe \
            ${supervisorctl-wrapped}/bin/supervisorctl \
            ${execCanaryName} \
            ${pkgs.coreutils}/bin/timeout \
          || _canary_rc=$?
        case "$_canary_rc" in
          0)
            __sd_apply_confd
            exit 0
            ;;
          10)
            __sd_replace_running "$_pid" "current config '$CONFIGFILE' failed exec canary with ENOENT"
            ;;
          *)
            echo "[supervisord-start-inner] exec canary inconclusive (rc=$_canary_rc); preserving current instance" >&2
            __sd_apply_confd
            exit 0
            ;;
        esac
      elif [ -n "$_conf" ]; then
        # Confirmed mismatch: an old-generation / wrong-config supervisord is
        # alive. Replace it so we never leave a process pinned to a GC-able gen.
        __sd_replace_running "$_pid" "running config '$_conf' != current '$CONFIGFILE'"
      else
        # Could not read the running instance's config (e.g. unreadable cmdline
        # under proot). Stay conservative and do not churn a possibly-healthy
        # instance; the activation post-condition is the stronger backstop.
        __sd_apply_confd
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

    # Services must not inherit the identity of whatever interactive session started supervisord
    # (activation run from a herdr/tmux/zellij/screen pane): every program, and every shell nod-exec
    # or sshd later spawns, would otherwise look nested inside that pane, and herdr refuses to start.
    for _v in $(${pkgs.coreutils}/bin/env | ${pkgs.gnused}/bin/sed -n 's/^\(HERDR_[A-Za-z0-9_]*\|ZELLIJ[A-Za-z0-9_]*\)=.*/\1/p'); do
      unset "$_v"
    done
    unset TMUX TMUX_PANE STY WINDOW
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
        {
          assertion = all (prog: hasPrefix "/" prog.requiresPath) (attrValues conditionalPrograms);
          message = "supervisord programs' requiresPath must be absolute: ${concatStringsSep ", " (attrNames (filterAttrs (_: prog: !(hasPrefix "/" prog.requiresPath)) conditionalPrograms))}";
        }
        {
          assertion = !(allPrograms ? ${execCanaryName});
          message = "supervisord program name '${execCanaryName}' is reserved for the proot exec canary";
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

        # End starters queued on the lock before the old instance stops and
        # releases it, or the oldest of them starts a supervisord first.
        ${sdKillStartersFn}
        __sd_kill_starters KILL

        # Hold the start lock from here until this generation's supervisord is
        # up. A shell opened meanwhile sees no supervisord and tries to start
        # one (the shell.init check below); with the lock held it waits
        # instead of starting a second instance in the middle of the restart.
        # Descriptor 9 is closed for the start itself, or the daemon would
        # inherit the lock and keep it.
        mkdir -p "${cfg.stateDir}/run"
        exec 9>>"${lockFile}"
        _sd_locked=0
        if ${flock} --nonblock 9; then _sd_locked=1; fi

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
        if [ "$_sd_locked" = 0 ]; then
          # The old instance held the lock itself: it was started before the
          # lock stopped being inherited. It has let go now, or soon will.
          if ${flock} --timeout ${toString startLockTimeout} 9; then _sd_locked=1; fi
        fi
        # Give trap-based cleanups a moment, then SIGKILL any survivors.
        sleep 1
        # That includes a supervisord some starter brought up before the lock
        # was ours.
        __sd_kill_starters KILL
        __sd_sweep_orphans KILL

        # With the lock held, start under it. Without it (something else still
        # holds it), start the ordinary way, which waits for the lock a bounded
        # time. A failed start must not end the activation here: the
        # post-condition below decides, and replaces whatever is in the way.
        __sd_start() {
          if [ "$_sd_locked" = 1 ]; then
            ${supervisord-start}/bin/supervisord-start --locked 9>&-
          else
            ${supervisord-start}/bin/supervisord-start 9>&-
          fi
        }
        _sd_rc=0
        $DRY_RUN_CMD __sd_start || _sd_rc=$?
        if [ "$_sd_rc" = ${toString startLockBusy} ]; then
          echo "[supervisord] start lock still held after ${toString startLockTimeout}s" >&2
        elif [ "$_sd_rc" != 0 ]; then
          echo "[supervisord] start exited $_sd_rc" >&2
        fi

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
          _sd_replace=""
          if [ -n "$_sd_conf" ] && [ "$_sd_conf" != "${configFile}" ]; then
            _sd_replace="running config '$_sd_conf' != current '${configFile}'"
          elif [ "$_sd_rc" != 0 ] && [ -z "$_sd_pid" ]; then
            _sd_replace="nothing is running after a failed start"
          fi
          if [ -n "$_sd_replace" ]; then
            echo "[supervisord] post-activation: $_sd_replace; forcing replacement" >&2
            __sd_kill_starters KILL
            ${supervisorctl-wrapped}/bin/supervisorctl shutdown 2>/dev/null || true
            ${pkgs.coreutils}/bin/sleep 1
            [ -n "$_sd_pid" ] && kill -9 "$_sd_pid" 2>/dev/null || true
            rm -f "${pidFile}"
            __sd_start
          fi
          unset _sd_pid _sd_conf _sd_replace
        fi
        # Starters that queued meanwhile have nothing left to do.
        __sd_kill_starters KILL
        exec 9>&-
        unset _sd_rc _sd_locked
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
