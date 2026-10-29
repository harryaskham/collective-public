#!/usr/bin/env bash
# Registration preflight of devbox-switch.sh against fixture checkouts.
# No WSL, network or real secrets: ssh-to-age and sops are stubbed.
#   bash collective-public/windows/tests/devbox-switch-preflight-test.sh
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/../devbox-switch.sh"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# Load only the preflight function, not the bootstrap program around it.
eval "$(sed -n '/^preflight_devbox_registration() {$/,/^}$/p' "$script")"
declare -F preflight_devbox_registration >/dev/null

ssh-to-age() { echo AGE-SECRET-KEY-TEST; }
sops() { return 0; }

registry_checkout() {
  local root="$tmp/$1"
  mkdir -p "$root/machines" "$root/pkgs/collective-lib" "$root/standalone/secrets"
  : > "$root/standalone/secrets/secrets.yaml"
  : > "$root/flake.nix"
  cat > "$root/machines/registry.nix" <<'NIX'
{
  machines = {
    devbox-a = {
      system = Tagged [VM WSL] System.NixOS.X86;
      groups = [Group.Work];
      pending = true;
    };
    devbox-a2 = {
      system = Tagged [VM] System.NixOS.X86;
      vm = {
        host = "mac";
      };
    };
    laptop = {
      system = System.NixOS.X86;
    };
  };
}
NIX
  cat > "$root/pkgs/collective-lib/ssh.nix" <<'NIX'
{
  authorizedKeys = {
    devbox-a = "ssh-ed25519 KEY";
    devbox-a2 = "ssh-ed25519 KEY";
    laptop = "ssh-ed25519 KEY";
    devbox-nokeys = "ssh-ed25519 KEY";
  };
  precomputedAgeKeys = {
    devbox-a = "age1";
    devbox-a2 = "age1";
    laptop = "age1";
  };
}
NIX
  for host in devbox-a devbox-a2 laptop devbox-nokeys devbox-missing-config; do
    [ "$host" = devbox-missing-config ] && continue
    mkdir -p "$root/machines/$host/nixos"
    : > "$root/machines/$host/nixos/configuration.nix"
  done
  echo "$root"
}

failures=0
expect() {
  local want="$1" checkout="$2" host="$3" pattern="${4:-}" out status
  set +e
  out="$(COLLECTIVE="$checkout" DEVBOX_HOST="$host" KEY=/dev/null SECRETS_FILE="$checkout/standalone/secrets/secrets.yaml" preflight_devbox_registration 2>&1)"
  status=$?
  set -e
  if { [ "$want" = pass ] && [ "$status" -eq 0 ]; } || { [ "$want" = fail ] && [ "$status" -ne 0 ]; }; then
    if [ -n "$pattern" ] && ! grep -q -- "$pattern" <<<"$out"; then
      echo "FAIL $host: output lacks '$pattern': $out"; failures=$((failures + 1)); return
    fi
    echo "ok   $want $host"
  else
    echo "FAIL $host: wanted $want, status $status: $out"; failures=$((failures + 1))
  fi
}

registry="$(registry_checkout registry)"
expect pass "$registry" devbox-a "Registration preflight passed"
expect fail "$registry" devbox-a2 "not registered as Tagged \[VM WSL\]"   # VM but not WSL
expect fail "$registry" laptop "not registered as Tagged \[VM WSL\]"
expect fail "$registry" devbox-nokeys "not registered"                   # no registry entry
expect fail "$registry" devbox-missing-config "missing machines/devbox-missing-config"

# A registry entry whose name only prefixes another must not satisfy it.
expect fail "$registry" devbox "missing machines/devbox"
mkdir -p "$registry/machines/devbox/nixos" && : > "$registry/machines/devbox/nixos/configuration.nix"
expect fail "$registry" devbox "not registered as Tagged \[VM WSL\]"

# SSH and age mappings are still required.
legacy="$(registry_checkout legacy)"
rm "$legacy/machines/registry.nix"
printf '  devbox-b = Tagged [VM WSL] System.NixOS.X86;\n' > "$legacy/flake.nix"
mkdir -p "$legacy/machines/devbox-b/nixos" && : > "$legacy/machines/devbox-b/nixos/configuration.nix"
expect fail "$legacy" devbox-b "needs authorizedKeys and precomputedAgeKeys"
printf '  devbox-b = "ssh-ed25519 KEY";\n  devbox-b = "age1";\n' >> "$legacy/pkgs/collective-lib/ssh.nix"
expect pass "$legacy" devbox-b "Registration preflight passed"   # older flake.nix layout

# Undecryptable secrets fail.
sops() { return 1; }
expect fail "$registry" devbox-a "missing or not decryptable"

# The GitHub token reaches Nix through NIX_CONFIG, appended to any existing
# configuration, and an undecryptable or empty token is reported as a failure.
eval "$(sed -n '/^configure_github_access_token() {$/,/^}$/p' "$script")"
declare -F configure_github_access_token >/dev/null
token_case() {
  local name="$1" want="$2" before="$3" expected="$4" status
  set +e
  NIX_CONFIG="$before"
  KEY=/dev/null SECRETS_FILE=/dev/null configure_github_access_token
  status=$?
  set -e
  if { [ "$want" = pass ] && [ "$status" -eq 0 ] && [ "$NIX_CONFIG" = "$expected" ]; } || \
     { [ "$want" = fail ] && [ "$status" -ne 0 ]; }; then
    echo "ok   token $name"
  else
    echo "FAIL token $name: status $status, NIX_CONFIG='$NIX_CONFIG'"; failures=$((failures + 1))
  fi
}
sops() { [ "${*: -2:1}" = '["keys"]["github"]["personal"]' ] && echo ghp_TEST; }
token_case fresh pass "" "access-tokens = github.com=ghp_TEST"
token_case appended pass "tarball-ttl = 0" $'tarball-ttl = 0\naccess-tokens = github.com=ghp_TEST'
sops() { return 1; }
token_case undecryptable fail "" ""
sops() { return 0; }
token_case empty fail "" ""
unset NIX_CONFIG

# cacophony-config registration is only a warning; a stubbed git supplies it.
eval "$(sed -n '/^preflight_cacophony_registration() {$/,/^}$/p' "$script")"
declare -F preflight_cacophony_registration >/dev/null
git() {
  [ "$1" = clone ] || return 1
  [ -n "${GIT_FAIL:-}" ] && return 128
  local dest="${*: -1}"
  mkdir -p "$dest"
  printf '  - imports:\n      - devbox_node.yaml\n    name: ms-dev-0\n    short_name: md0\n    host: ms-dev-0.miku-owl.ts.net\n' > "$dest/config.yaml"
}
caco_case() {
  local name="$1" host="$2" pattern="$3" out status
  set +e
  out="$(DEVBOX_HOST="$host" preflight_cacophony_registration 2>&1)"
  status=$?
  set -e
  if [ "$status" -eq 0 ] && grep -q -- "$pattern" <<<"$out"; then
    echo "ok   caco $name"
  else
    echo "FAIL caco $name: status $status: $out"; failures=$((failures + 1))
  fi
}
caco_case registered ms-dev-0 "cacophony-config lists ms-dev-0"
caco_case missing ms-dev-b "not in cacophony-config"
caco_case prefix ms-dev "not in cacophony-config"
GIT_FAIL=1 caco_case unreadable ms-dev-0 "could not read cacophony-config"
unset -f git

if [ "$failures" -ne 0 ]; then
  echo "$failures failure(s)"
  exit 1
fi
echo "all preflight cases passed"
