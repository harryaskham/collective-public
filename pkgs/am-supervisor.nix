# am-supervisor: generic health-check watchdog for Android apps.
#
# Runs a check command on an interval. On failure, executes a restart
# action (typically an `am start` intent) with exponential backoff.
# Designed to run under supervisord on nix-on-droid.
#
# Usage:
#   am-supervisor \
#     --name PulseServer \
#     --check "pactl info" \
#     --action "am start -n com.harryaskham.pulse/.MainActivity" \
#     --interval 10 \
#     --backoff-base 5 \
#     --backoff-max 300
{
  pkgs,
  ...
}:

pkgs.writeScriptBin "am-supervisor" ''
  #!${pkgs.bash}/bin/bash
  set -uo pipefail

  # --- Defaults ---
  NAME="am-supervisor"
  CHECK_CMD=""
  ACTION_CMD=""
  INTERVAL=10
  BACKOFF_BASE=5
  BACKOFF_MAX=300

  # --- Parse args ---
  while [[ $# -gt 0 ]]; do
    case "$1" in
      --name)          NAME="$2"; shift 2 ;;
      --check)         CHECK_CMD="$2"; shift 2 ;;
      --action)        ACTION_CMD="$2"; shift 2 ;;
      --interval)      INTERVAL="$2"; shift 2 ;;
      --backoff-base)  BACKOFF_BASE="$2"; shift 2 ;;
      --backoff-max)   BACKOFF_MAX="$2"; shift 2 ;;
      -h|--help)
        echo "Usage: am-supervisor --check CMD --action CMD [options]"
        echo ""
        echo "Options:"
        echo "  --name NAME          Label for log messages (default: am-supervisor)"
        echo "  --check CMD          Health check command (exit 0 = healthy)"
        echo "  --action CMD         Restart action on check failure"
        echo "  --interval SECS      Check interval when healthy (default: 10)"
        echo "  --backoff-base SECS  Initial backoff after failure (default: 5)"
        echo "  --backoff-max SECS   Maximum backoff ceiling (default: 300)"
        exit 0
        ;;
      *) echo "[$NAME] Unknown option: $1" >&2; exit 1 ;;
    esac
  done

  if [[ -z "$CHECK_CMD" ]] || [[ -z "$ACTION_CMD" ]]; then
    echo "[$NAME] --check and --action are required" >&2
    exit 1
  fi

  echo "[$NAME] Starting supervisor"
  echo "[$NAME]   check:   $CHECK_CMD"
  echo "[$NAME]   action:  $ACTION_CMD"
  echo "[$NAME]   interval=''${INTERVAL}s  backoff=''${BACKOFF_BASE}s..''${BACKOFF_MAX}s"

  BACKOFF=$BACKOFF_BASE
  CONSECUTIVE_FAILURES=0

  while true; do
    if eval "$CHECK_CMD" >/dev/null 2>&1; then
      # Healthy - reset backoff
      if [[ $CONSECUTIVE_FAILURES -gt 0 ]]; then
        echo "[$NAME] Recovered after $CONSECUTIVE_FAILURES failure(s)"
      fi
      BACKOFF=$BACKOFF_BASE
      CONSECUTIVE_FAILURES=0
      sleep "$INTERVAL"
    else
      CONSECUTIVE_FAILURES=$((CONSECUTIVE_FAILURES + 1))
      echo "[$NAME] Check failed (#$CONSECUTIVE_FAILURES), restarting (backoff=''${BACKOFF}s)"
      eval "$ACTION_CMD" 2>&1 | sed "s/^/[$NAME] /"
      sleep "$BACKOFF"
      # Exponential backoff, capped
      BACKOFF=$((BACKOFF * 2))
      if [[ $BACKOFF -gt $BACKOFF_MAX ]]; then
        BACKOFF=$BACKOFF_MAX
      fi
    fi
  done
''
