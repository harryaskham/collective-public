#!/usr/bin/env bash
# Classify whether a live supervisord can exec a known-good canary child.
# Return codes: 0 healthy, 10 positively wedged (ENOENT), 20 inconclusive.

supervisord_exec_canary_probe() {
  local supervisorctl=${1:?supervisorctl path required}
  local canary=${2:-exec-canary}
  local timeout_bin=${3:-timeout}
  local timeout_seconds=${SUPERVISORD_CANARY_TIMEOUT_SECONDS:-3}
  local output rc

  output=$("$timeout_bin" "$timeout_seconds" "$supervisorctl" start "$canary" 2>&1)
  rc=$?

  if [[ "$rc" -eq 0 && "$output" == *"$canary: started"* ]]; then
    echo "[supervisord-exec-canary] healthy: $output" >&2
    return 0
  fi

  # Supervisor 4.3 on real nix-on-droid/proot reports an execve ENOENT directly
  # from `start` and leaves the process STOPPED; it does not transition FATAL.
  if [[ "$rc" -ne 0 && "$output" == *"ERROR (no such file)"* ]]; then
    echo "[supervisord-exec-canary] confirmed exec wedge: $output" >&2
    return 10
  fi

  echo "[supervisord-exec-canary] inconclusive rc=$rc output=$output" >&2
  return 20
}
