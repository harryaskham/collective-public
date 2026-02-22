# termux-exec: run native Termux commands from inside Nix-on-Droid's proot.
#
# Bionic/app_process cannot run under proot (ptrace breaks ART).
# This client connects to a termux-command-daemon running in regular
# Termux via TCP localhost, sends a command, and relays I/O.
#
# Protocol:
#   1. Client connects via TCP, sends one line: the shell command
#   2. Daemon reads the command, prints a sentinel line: __TERMUX_EXEC_READY__
#   3. Client discards all output up to and including the sentinel
#   4. Remaining I/O is pure passthrough (command stdout/stderr)
{
  pkgs,
  port ? "18356",
  host ? "127.0.0.1",
  ...
}:
pkgs.stdenvNoCC.mkDerivation {
  pname = "termux-exec";
  version = "0.6.0";
  meta.mainProgram = "termux-exec";
  dontUnpack = true;

  installPhase = let
    defaultPort = port;
    defaultHost = host;
  in ''
    mkdir -p $out/bin

    cat > $out/bin/termux-exec <<'SCRIPT'
    #!/usr/bin/env bash
    set -euo pipefail

    TERMUX_CMD_PORT="''${TERMUX_CMD_PORT:-@defaultPort@}"
    TERMUX_CMD_HOST="''${TERMUX_CMD_HOST:-@defaultHost@}"
    SENTINEL="__TERMUX_EXEC_READY__"

    if [ $# -eq 0 ]; then
      echo "Usage: termux-exec <command> [args...]" >&2
      echo "" >&2
      echo "Run a command in native Termux (outside proot)." >&2
      echo "Requires termux-command-daemon running in Termux." >&2
      exit 1
    fi

    # Check if the daemon is reachable
    if ! (echo >/dev/tcp/"$TERMUX_CMD_HOST"/"$TERMUX_CMD_PORT") 2>/dev/null; then
      echo "termux-exec: cannot connect to daemon at $TERMUX_CMD_HOST:$TERMUX_CMD_PORT" >&2
      echo "" >&2
      echo "Start the daemon in Termux first:" >&2
      echo "  bash /storage/emulated/0/shared/termux-exec/termux-command-daemon.sh" >&2
      exit 1
    fi

    # Build the command string - quote args for safe transport
    CMD="$(printf '%q ' "$@")"

    # Detect if stdin is a terminal (interactive use)
    if [ -t 0 ] && [ -t 1 ]; then
      # Interactive mode: put local terminal into raw mode so that
      # arrow keys, Ctrl-C, etc. are sent byte-for-byte to the
      # remote PTY (which handles line discipline).
      ORIG_STTY="$(stty -g)"
      stty raw -echo icrnl
      cleanup() {
        stty "$ORIG_STTY" 2>/dev/null || true
      }
      trap cleanup EXIT

      exec 4<>/dev/tcp/"$TERMUX_CMD_HOST"/"$TERMUX_CMD_PORT"
      printf '%s\n' "$CMD" >&4

      # Wait for sentinel - discard the echoed command and any PTY preamble.
      # Read in raw mode char-by-char to handle \r\n line endings from PTY.
      BUF=""
      NL="$(printf '\n')" CR="$(printf '\r')"
      while IFS= read -r -n1 -d "" CH <&4 || [ -n "$CH" ]; do
        if [ "$CH" = "$NL" ] || [ "$CH" = "$CR" ]; then
          # Strip trailing \r if present
          BUF="''${BUF%"$CR"}"
          if [ "$BUF" = "$SENTINEL" ]; then
            break
          fi
          BUF=""
        else
          BUF="$BUF$CH"
        fi
      done

      # Relay stdin -> daemon in background
      cat <&0 >&4 &
      BG=$!
      trap "kill $BG 2>/dev/null || true; exec 4>&-; stty '$ORIG_STTY' 2>/dev/null || true" EXIT
      cat <&4
    else
      # Piped / non-interactive mode: pure passthrough, no PTY tricks.
      exec 4<>/dev/tcp/"$TERMUX_CMD_HOST"/"$TERMUX_CMD_PORT"
      printf '%s\n' "$CMD" >&4

      # Wait for sentinel - discard echoed command and PTY preamble
      CR="$(printf '\r')"
      while IFS= read -r LINE <&4; do
        # Strip trailing \r from PTY line endings
        LINE="''${LINE%"$CR"}"
        if [ "$LINE" = "$SENTINEL" ]; then
          break
        fi
      done

      # Relay stdin -> daemon in background (in case of piped input)
      cat <&0 >&4 &
      BG=$!
      trap "kill $BG 2>/dev/null || true; exec 4>&-" EXIT
      cat <&4
    fi
    SCRIPT

    substituteInPlace $out/bin/termux-exec \
      --replace-warn '@defaultPort@' '${defaultPort}' \
      --replace-warn '@defaultHost@' '${defaultHost}'
    chmod +x $out/bin/termux-exec
  '';
}
