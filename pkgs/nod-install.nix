# nod-install: install APKs from nix-on-droid via Shizuku/rish.
#
# Copies APK to /data/local/tmp (required by SELinux) then runs pm install
# via rish-nix. On failure, starts termux-command-daemon via am, retries,
# and if still failing prompts user to enable wireless debugging + Shizuku.
#
# Supports --host/--port for remote installation via SSH delegation.
# If local and APK doesn't exist yet, waits up to 60s for it to appear.
{
  pkgs,
  ...
}:

pkgs.writeScriptBin "nod-install" ''
  #!${pkgs.bash}/bin/bash
  set -euo pipefail

  HOST=""
  PORT=""
  APK=""

  while [ $# -gt 0 ]; do
    case "$1" in
      --host)  HOST="$2"; shift 2 ;;
      --port)  PORT="$2"; shift 2 ;;
      -*)      echo "nod-install: unknown flag: $1" >&2; exit 1 ;;
      *)       APK="$1"; shift ;;
    esac
  done

  if [ -z "$APK" ]; then
    echo "Usage: nod-install [--host <host>] [--port <port>] <path/to.apk>" >&2
    exit 1
  fi

  # Remote mode: delegate via SSH
  if [ -n "$HOST" ] && [ "$HOST" != "localhost" ] && [ "$HOST" != "127.0.0.1" ]; then
    SSH_ARGS=(-o StrictHostKeyChecking=accept-new)
    [ -n "$PORT" ] && SSH_ARGS+=(-p "$PORT")
    SSH_ARGS+=("$HOST")
    echo "nod-install: delegating to $HOST..." >&2
    exec ${pkgs.openssh}/bin/ssh "''${SSH_ARGS[@]}" "nod-install \"$APK\" || ~/.nix-profile/bin/nod-install \"$APK\""
  fi

  # Local mode: wait up to 60s for APK to appear
  if [ ! -f "$APK" ]; then
    echo "nod-install: waiting for $APK to appear..." >&2
    WAITED=0
    while [ ! -f "$APK" ] && [ "$WAITED" -lt 60 ]; do
      sleep 1
      WAITED=$((WAITED + 1))
    done
    if [ ! -f "$APK" ]; then
      echo "nod-install: file not found after 60s: $APK" >&2
      exit 1
    fi
    echo "nod-install: file appeared after ''${WAITED}s" >&2
  fi

  BASENAME="$(basename "$APK")"
  TMP="/data/local/tmp/$BASENAME"

  try_install() {
    rish-nix -c "cp \"$APK\" \"$TMP\" && pm install \"$TMP\" && rm -f \"$TMP\""
  }

  echo "Installing $BASENAME..." >&2

  # First attempt
  if try_install; then
    exit 0
  fi

  echo "nod-install: first attempt failed, starting termux-command-daemon..." >&2

  # Launch termux-command-daemon in com.termux via am
  am start -n com.termux/com.termux.app.RunCommandService \
    --es com.termux.RUN_COMMAND_PATH "/data/data/com.termux/files/usr/bin/bash" \
    --esa com.termux.RUN_COMMAND_ARGUMENTS "/storage/emulated/0/shared/termux-exec/termux-command-daemon.sh" \
    --ez com.termux.RUN_COMMAND_BACKGROUND true \
    2>/dev/null || true

  echo "nod-install: waiting for daemon..." >&2
  sleep 3

  # Retry
  if try_install; then
    exit 0
  fi

  echo "" >&2
  echo "nod-install: install failed." >&2
  echo "" >&2
  echo "Please ensure:" >&2
  echo "  1. Enable Wireless Debugging in Developer Options" >&2
  echo "  2. Start Shizuku (grant wireless debugging permission)" >&2
  echo "  3. Open Termux and run:" >&2
  echo "       bash /storage/emulated/0/shared/termux-exec/termux-command-daemon.sh" >&2
  echo "  4. Retry: nod-install \"$APK\"" >&2
  exit 1
''
