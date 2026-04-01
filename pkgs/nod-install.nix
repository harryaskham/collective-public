# nod-install: install APKs from nix-on-droid via Shizuku/rish.
#
# Copies APK to /data/local/tmp (required by SELinux) then runs pm install
# via rish-nix. On failure, starts termux-command-daemon via am, retries,
# and if still failing prompts user to enable wireless debugging + Shizuku.
#
# Supports --host/--port for remote installation via SSH delegation.
# Use --remote-path for an APK already on the remote device.
# Use a bare positional path for a local file — if --host is set, it gets
# scp'd to --apk-dir on the remote first, then installed.
# If local and APK doesn't exist yet, waits up to 60s for it to appear.
{
  pkgs,
  ...
}:

let
  defaultApkDir = "/storage/emulated/0/Default Folder/apks";
in

pkgs.writeScriptBin "nod-install" ''
  #!${pkgs.bash}/bin/bash
  set -euo pipefail

  HOST=""
  PORT=""
  REMOTE_PATH=""
  LOCAL_PATH=""
  APK_DIR="${defaultApkDir}"

  while [ $# -gt 0 ]; do
    case "$1" in
      --host)        HOST="$2"; shift 2 ;;
      --port)        PORT="$2"; shift 2 ;;
      --remote-path) REMOTE_PATH="$2"; shift 2 ;;
      --apk-dir)     APK_DIR="$2"; shift 2 ;;
      -*)            echo "nod-install: unknown flag: $1" >&2; exit 1 ;;
      *)             LOCAL_PATH="$1"; shift ;;
    esac
  done

  if [ -z "$REMOTE_PATH" ] && [ -z "$LOCAL_PATH" ]; then
    cat >&2 <<USAGE
  Usage: nod-install [options] [<local-path>]

  Options:
    --host <host>          Remote host (user@host supported)
    --port <port>          SSH port (default: 22)
    --remote-path <path>   APK path on remote device (skip scp)
    --apk-dir <dir>        Remote dir for scp uploads (default: ${defaultApkDir})

  If <local-path> given with --host, scp's to remote --apk-dir then installs.
  If --remote-path given, installs directly on the target device.
  USAGE
    exit 1
  fi

  # Build SSH args shared by ssh and scp
  SSH_OPTS=(-o StrictHostKeyChecking=accept-new)
  [ -n "$PORT" ] && SSH_OPTS+=(-p "$PORT")

  SCP_OPTS=(-o StrictHostKeyChecking=accept-new)
  [ -n "$PORT" ] && SCP_OPTS+=(-P "$PORT")

  IS_REMOTE=false
  if [ -n "$HOST" ] && [ "$HOST" != "localhost" ] && [ "$HOST" != "127.0.0.1" ]; then
    IS_REMOTE=true
  fi

  # Remote + local file: scp then install
  if $IS_REMOTE && [ -n "$LOCAL_PATH" ]; then
    if [ ! -f "$LOCAL_PATH" ]; then
      echo "nod-install: local file not found: $LOCAL_PATH" >&2
      exit 1
    fi
    BASENAME="$(basename "$LOCAL_PATH")"
    REMOTE_DEST="$APK_DIR/$BASENAME"
    echo "nod-install: uploading $BASENAME to $HOST:$REMOTE_DEST..." >&2
    ${pkgs.openssh}/bin/scp "''${SCP_OPTS[@]}" "$LOCAL_PATH" "$HOST:\"$REMOTE_DEST\""
    REMOTE_PATH="$REMOTE_DEST"
  fi

  # Remote: delegate via SSH
  if $IS_REMOTE; then
    echo "nod-install: delegating to $HOST..." >&2
    exec ${pkgs.openssh}/bin/ssh "''${SSH_OPTS[@]}" "$HOST" \
      "nod-install --remote-path \"$REMOTE_PATH\" || ~/.nix-profile/bin/nod-install --remote-path \"$REMOTE_PATH\""
  fi

  # Local mode
  APK="''${REMOTE_PATH:-$LOCAL_PATH}"

  # Wait up to 60s for APK to appear
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
