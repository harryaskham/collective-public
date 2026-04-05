# tasker-run: trigger a named Tasker task from nix-on-droid via am broadcast.
#
# Usage:
#   tasker-run "Termux Boot Scripts"
#   tasker-run --project Nix "Termux Boot Scripts"
{
  pkgs,
  ...
}:

pkgs.writeScriptBin "tasker-run" ''
  #!${pkgs.bash}/bin/bash
  set -euo pipefail

  PROJECT=""

  while [ $# -gt 0 ]; do
    case "$1" in
      --project|-p) PROJECT="$2"; shift 2 ;;
      -*)           echo "tasker-run: unknown flag: $1" >&2; exit 1 ;;
      *)            break ;;
    esac
  done

  if [ $# -eq 0 ]; then
    cat >&2 <<USAGE
Usage: tasker-run [--project <name>] <task-name>

Trigger a named Tasker task via am broadcast.

Options:
  --project, -p <name>   Tasker project (optional, for disambiguation)

Examples:
  tasker-run "Termux Boot Scripts"
  tasker-run -p Nix "Termux Boot Scripts"
USAGE
    exit 1
  fi

  TASK_NAME="$1"

  ARGS=(
    am broadcast
    -a net.dinglisch.android.taskerm.ACTION_TASK
    -e task_name "$TASK_NAME"
  )

  [ -n "$PROJECT" ] && ARGS+=(-e task_project "$PROJECT")

  echo "tasker-run: triggering '$TASK_NAME'..." >&2
  "''${ARGS[@]}"
''
