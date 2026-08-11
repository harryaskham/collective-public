#!/usr/bin/env bash
set -euo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=../nix-on-droid/supervisord-exec-canary.sh
source "$HERE/../nix-on-droid/supervisord-exec-canary.sh"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT
pass=0
fail=0
check() {
  local description=$1 actual=$2 expected=$3
  if [[ "$actual" == "$expected" ]]; then
    printf 'ok   - %s\n' "$description"
    pass=$((pass + 1))
  else
    printf "FAIL - %s (got '%s', want '%s')\n" "$description" "$actual" "$expected" >&2
    fail=$((fail + 1))
  fi
}

cat > "$TMP/supervisorctl" <<'EOF'
#!/usr/bin/env bash
case "${CANARY_SCENARIO:?}" in
  healthy) printf '%s: started\n' "$2"; exit 0 ;;
  missing) printf '%s: ERROR (no such file)\n' "$2"; exit 1 ;;
  generic) printf '%s: ERROR (already started)\n' "$2"; exit 1 ;;
  hang) sleep 5; exit 0 ;;
esac
EOF
chmod +x "$TMP/supervisorctl"

probe_rc() {
  local scenario=$1
  set +e
  CANARY_SCENARIO=$scenario SUPERVISORD_CANARY_TIMEOUT_SECONDS=1 \
    supervisord_exec_canary_probe "$TMP/supervisorctl" exec-canary "$(command -v timeout)" \
    >/dev/null 2>"$TMP/$scenario.err"
  local rc=$?
  set -e
  printf '%s\n' "$rc"
}

check "healthy start is accepted" "$(probe_rc healthy)" 0
check "explicit ENOENT is positive wedge proof" "$(probe_rc missing)" 10
check "generic supervisor error is inconclusive" "$(probe_rc generic)" 20
check "bounded timeout is inconclusive" "$(probe_rc hang)" 20
check "healthy diagnostic" "$(grep -c 'healthy: exec-canary: started' "$TMP/healthy.err")" 1
check "wedge diagnostic" "$(grep -c 'confirmed exec wedge.*ERROR (no such file)' "$TMP/missing.err")" 1
check "timeout diagnostic contains rc=124" "$(grep -c 'inconclusive rc=124' "$TMP/hang.err")" 1

printf '\npassed: %d, failed: %d\n' "$pass" "$fail"
[[ "$fail" -eq 0 ]]
echo 'PASS: supervisord exec canary classification'
