# termux-exec: run native Termux commands from inside Nix-on-Droid's proot.
#
# Bionic/app_process cannot run under proot (ptrace breaks ART).
# This client connects to a termux-command-daemon running in regular
# Termux via TCP localhost, sends a command, and relays I/O.

{ pkgs
, port ? "18356"
, host ? "127.0.0.1"
, ...
}:

pkgs.stdenvNoCC.mkDerivation {
  pname = "termux-exec";
  version = "0.4.0";
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

    # Open TCP connection, send command, relay I/O
    exec 4<>/dev/tcp/"$TERMUX_CMD_HOST"/"$TERMUX_CMD_PORT"
    printf '%s\n' "$CMD" >&4

    # Bidirectional relay
    cat <&0 >&4 &
    BG=$!
    trap "kill $BG 2>/dev/null || true; exec 4>&-" EXIT
    cat <&4
    SCRIPT

    substituteInPlace $out/bin/termux-exec \
      --replace-warn '@defaultPort@' '${defaultPort}' \
      --replace-warn '@defaultHost@' '${defaultHost}'
    chmod +x $out/bin/termux-exec
  '';
}
