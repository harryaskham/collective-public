# nod-install: install APKs from nix-on-droid via Shizuku/rish.
#
# Copies APK to /data/local/tmp (required by SELinux) then runs pm install
# via rish-nix. On failure, starts termux-command-daemon via am, retries,
# and if still failing prompts user to enable wireless debugging + Shizuku.
{
  pkgs,
  ...
}:

pkgs.writeScriptBin "nod-install" ''
  #!${pkgs.bash}/bin/bash
  set -euo pipefail

  if [ $# -eq 0 ]; then
    echo "Usage: nod-install <path/to.apk>" >&2
    exit 1
  fi

  APK="$1"

  if [ ! -f "$APK" ]; then
    echo "nod-install: file not found: $APK" >&2
    exit 1
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
