#!/usr/bin/env bash
# Bootstrap a fresh Nix-on-Droid install into Collective: install the machine's
# SSH private key, clone (or update) the Collective repository and run the
# regular switch. Safe to re-run: an installed key is kept and the checkout is
# fast-forwarded.
#
# On the fresh install (the Nix-on-Droid flake bootstrap enables flakes):
#
#   nix shell github:NixOS/nixpkgs/nixos-24.05#curl -c curl -fsSLO \
#     https://raw.githubusercontent.com/harryaskham/collective-public/main/scripts/bootstrap-nod.sh
#   bash bootstrap-nod.sh <machine>
#
# Paste the machine's private key when asked (Ctrl-D to finish). For a new
# key, add the printed public key to GitHub before the clone, and the public
# and age keys to pkgs/collective-lib/ssh.nix and the secrets recipients so
# SOPS secrets decrypt; re-run after pushing to pull and switch again.
set -euo pipefail

machine=${1:-}
if [[ -z $machine || $machine == -h || $machine == --help ]]; then
  sed -n '2,16p' "$0" | sed 's/^# \{0,1\}//'
  exit 2
fi

# The bare install lacks sed and friends (busybox), and its Nix cannot
# evaluate Collective's relative path inputs, so the switch runs under a
# current Nix. git, ssh and ssh-to-age come from the bootstrap's release line.
if [[ -z ${BOOTSTRAP_NOD_SHELL:-} ]]; then
  nixpkgs=${BOOTSTRAP_NIXPKGS:-github:NixOS/nixpkgs/nixos-24.05}
  nix=${BOOTSTRAP_NIX:-github:NixOS/nixpkgs/nixos-unstable#nix}
  export BOOTSTRAP_NOD_SHELL=1
  exec nix shell "$nix" "$nixpkgs#git" "$nixpkgs#openssh" "$nixpkgs#ssh-to-age" \
    "$nixpkgs#busybox" --command bash "$0" "$@"
fi

repo=${COLLECTIVE_REPO:-git@github.com:harryaskham/collective.git}
dir=${COLLECTIVE_DIR:-$HOME/collective}
key=$HOME/.ssh/id_ed25519 # ssh.perMachineKeys.paths.sshPrivateKey

mkdir -p "$HOME/.ssh"
chmod 700 "$HOME/.ssh"
if [[ ! -s $key ]]; then
  echo "Paste the SSH private key for $machine, then press Ctrl-D:"
  (umask 077 && tr -d '\r' >"$key.new")
  [[ -z $(tail -c1 "$key.new") ]] || echo >>"$key.new"
  if ! ssh-keygen -y -P '' -f "$key.new" >/dev/null 2>&1; then
    rm -f "$key.new"
    echo "bootstrap-nod: that is not an unencrypted OpenSSH private key" >&2
    exit 1
  fi
  mv "$key.new" "$key"
fi
chmod 600 "$key"
ssh-keygen -y -f "$key" >"$key.pub"
age=$(ssh-to-age <"$key.pub") || age="(ssh-to-age failed)"

cat <<EOF

$machine
  SSH public key: $(cat "$key.pub")
  Age recipient:  $age
  App uid/gid:    $(id -u)/$(id -g) (machines/$machine/nix-on-droid pins user.uid/gid)

EOF

# Also used by the switch's fetches of private git+ssh flake inputs.
export GIT_SSH_COMMAND="ssh -i $key -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new"
if [[ -d $dir/.git ]]; then
  git -C "$dir" pull --ff-only
else
  git clone "$repo" "$dir"
fi || {
  echo "bootstrap-nod: git could not use $repo; is $key.pub on GitHub (https://github.com/settings/ssh/new)?" >&2
  exit 1
}

exec nix-on-droid switch --flake "$dir#$machine"
