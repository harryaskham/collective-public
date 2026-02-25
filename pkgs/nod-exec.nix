# nod-exec-server: TCP command server for Nix-on-Droid.
#
# Runs inside NOD (under proot) and accepts TCP connections from
# external apps (Tasker, KWGT, custom APKs) on localhost.
# Executes commands in the full NOD/Nix environment and returns output.
#
# Protocol (mirrors termux-exec/termux-command-daemon):
#   1. Client connects via TCP, sends one line: the shell command
#   2. Server prints sentinel: __NOD_EXEC_READY__
#   3. Server execs the command; all subsequent output is from the command
#   4. Connection closes when command exits
#
# Modes:
#   - Interactive (PTY): for terminal emulators / floating shells
#   - Pipe (non-PTY): for Tasker/KWGT one-shot commands
#
# The server runs under supervisord and auto-starts on boot.
{
  pkgs,
  port ? "18357",
  host ? "127.0.0.1",
  ...
}:

let
  defaultPort = port;
  defaultHost = host;

  # Per-connection helper script
  # socat forks this for each incoming connection
  nod-exec-handler = pkgs.writeScript "nod-exec-handler" ''
    #!${pkgs.bash}/bin/bash

    # Read one line: the command to execute
    IFS= read -r CMD_LINE 2>/dev/null || exit 1
    [ -z "$CMD_LINE" ] && exit 1

    # Signal readiness
    printf '%s\n' "__NOD_EXEC_READY__"

    # Execute in a login-like environment
    exec ${pkgs.bash}/bin/bash -l -c "$CMD_LINE"
  '';

  nod-exec-server = pkgs.writeScriptBin "nod-exec-server" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    PORT="''${NOD_EXEC_PORT:-${defaultPort}}"
    HOST="''${NOD_EXEC_HOST:-${defaultHost}}"
    PIDFILE="''${NOD_EXEC_PIDFILE:-/tmp/run/nod-exec-server.pid}"

    cleanup() { rm -f "$PIDFILE"; }
    trap cleanup EXIT

    echo "nod-exec-server: listening on $HOST:$PORT (pipe mode)" >&2
    echo "nod-exec-server: protocol - connect, send one line (command), then I/O" >&2
    echo "nod-exec-server: pidfile $PIDFILE" >&2

    echo $$ > "$PIDFILE"

    # Pipe mode (no PTY) — clean output for one-shot commands.
    # Interactive terminal use (nod-exec bash) gets PTY from the client side.
    exec ${pkgs.socat}/bin/socat \
      TCP-LISTEN:"$PORT",bind="$HOST",reuseaddr,fork \
      EXEC:"${nod-exec-handler}",stderr
  '';

  # Client: lightweight script for sending commands to the server
  # Can be used from Tasker (via Run Shell), KWGT ($sh()), or other apps
  nod-exec-client = pkgs.writeScriptBin "nod-exec" ''
    #!${pkgs.bash}/bin/bash
    set -euo pipefail

    PORT="''${NOD_EXEC_PORT:-${defaultPort}}"
    HOST="''${NOD_EXEC_HOST:-${defaultHost}}"
    SENTINEL="__NOD_EXEC_READY__"

    if [ $# -eq 0 ]; then
      echo "Usage: nod-exec <command> [args...]" >&2
      echo "" >&2
      echo "Run a command in Nix-on-Droid via the nod-exec-server." >&2
      echo "Server must be running (managed by supervisord)." >&2
      exit 1
    fi

    # Check connectivity
    if ! (echo >/dev/tcp/"$HOST"/"$PORT") 2>/dev/null; then
      echo "nod-exec: cannot connect to server at $HOST:$PORT" >&2
      exit 1
    fi

    CMD="$(printf '%q ' "$@")"

    if [ -t 0 ] && [ -t 1 ]; then
      # Interactive mode
      ORIG_STTY="$(stty -g)"
      stty raw -echo icrnl
      cleanup() { stty "$ORIG_STTY" 2>/dev/null || true; }
      trap cleanup EXIT

      exec 4<>/dev/tcp/"$HOST"/"$PORT"
      printf '%s\n' "$CMD" >&4

      BUF=""
      NL="$(printf '\n')" CR="$(printf '\r')"
      while IFS= read -r -n1 -d "" CH <&4 || [ -n "$CH" ]; do
        if [ "$CH" = "$NL" ] || [ "$CH" = "$CR" ]; then
          BUF="''${BUF%"$CR"}"
          if [ "$BUF" = "$SENTINEL" ]; then break; fi
          BUF=""
        else
          BUF="$BUF$CH"
        fi
      done

      cat <&0 >&4 &
      BG=$!
      trap "kill $BG 2>/dev/null || true; exec 4>&-; stty '$ORIG_STTY' 2>/dev/null || true" EXIT
      cat <&4
    else
      # Piped mode
      exec 4<>/dev/tcp/"$HOST"/"$PORT"
      printf '%s\n' "$CMD" >&4

      CR="$(printf '\r')"
      while IFS= read -r LINE <&4; do
        LINE="''${LINE%"$CR"}"
        if [ "$LINE" = "$SENTINEL" ]; then break; fi
      done

      cat <&0 >&4 &
      BG=$!
      trap "kill $BG 2>/dev/null || true; exec 4>&-" EXIT
      cat <&4
    fi
  '';

  # Tiny netcat-based client for environments without bash /dev/tcp
  # (e.g. KWGT's $sh()$, busybox shells, Tasker Run Shell)
  # Requires: nc (netcat) — available on most Android systems
  nod-exec-nc = pkgs.writeScriptBin "nod-exec-nc" ''
    #!${pkgs.bash}/bin/bash
    # Minimal client using nc for maximum compatibility.
    # Output only (no interactive PTY). Perfect for Tasker/KWGT.
    PORT="''${NOD_EXEC_PORT:-${defaultPort}}"
    HOST="''${NOD_EXEC_HOST:-${defaultHost}}"

    if [ $# -eq 0 ]; then
      echo "Usage: nod-exec-nc <command> [args...]" >&2
      exit 1
    fi

    CMD="$(printf '%q ' "$@")"
    # Send command, wait for sentinel, print only command output
    printf '%s\n' "$CMD" | ${pkgs.netcat-gnu}/bin/nc -q5 "$HOST" "$PORT" 2>/dev/null \
      | sed -n '/__NOD_EXEC_READY__/,$p' | tail -n +2
  '';

  # Lightweight Android-side client script for calling nod-exec-server
  # from outside proot (Tasker, KWGT, busybox shells).
  # Written to shared storage by the nod-exec module.
  nod-exec-android = pkgs.writeText "nod-exec-android.sh" ''
    #!/system/bin/sh
    # nod-exec-android: lightweight client for calling nod-exec-server
    # from outside proot (e.g. Tasker Run Shell, KWGT $sh()$).
    #
    # Requires: nc (netcat) — available via busybox on most Android systems
    # or via Termux's netcat.
    #
    # Usage:
    #   sh /storage/emulated/0/shared/nod-exec-android.sh "echo hello"
    #   sh /storage/emulated/0/shared/nod-exec-android.sh "supervisorctl status"
    #
    # For KWGT:
    #   $sh("sh /storage/emulated/0/shared/nod-exec-android.sh 'uptime'")$
    #
    # For Tasker Run Shell:
    #   Command: sh /storage/emulated/0/shared/nod-exec-android.sh '%par1'

    PORT="''${NOD_EXEC_PORT:-${defaultPort}}"
    HOST="''${NOD_EXEC_HOST:-${defaultHost}}"

    CMD="$*"
    if [ -z "$CMD" ]; then
      echo "Usage: nod-exec-android <command>" >&2
      exit 1
    fi

    # Try nc variants available on Android
    if command -v nc >/dev/null 2>&1; then
      NC="nc"
    elif command -v netcat >/dev/null 2>&1; then
      NC="netcat"
    elif [ -x /data/data/com.termux/files/usr/bin/nc ]; then
      NC="/data/data/com.termux/files/usr/bin/nc"
    else
      echo "nod-exec-android: nc/netcat not found" >&2
      exit 1
    fi

    # Send command, strip sentinel, output result
    printf '%s\n' "$CMD" | $NC -w5 "$HOST" "$PORT" 2>/dev/null | {
      READY=0
      while IFS= read -r LINE; do
        case "$LINE" in
          *__NOD_EXEC_READY__*) READY=1; continue ;;
        esac
        if [ "$READY" -eq 1 ]; then
          printf '%s\n' "$LINE"
        fi
      done
    }
  '';

in {
  inherit nod-exec-server nod-exec-client nod-exec-nc nod-exec-handler nod-exec-android;
  server = nod-exec-server;
  client = nod-exec-client;
  nc = nod-exec-nc;
  android = nod-exec-android;
}
