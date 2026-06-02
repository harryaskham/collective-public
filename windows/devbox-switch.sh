#!/usr/bin/env bash
# devbox-switch.sh — WSL-side half of devbox bootstrap.
#
# Run this from INSIDE a NixOS-WSL distro that already has the shared devbox
# SSH key placed at ~/.ssh/id_ed25519 (the "wsl-with-keys" checkpoint). It
# clones the private collective flake over SSH and runs the first
# nixos-rebuild switch for the given host.
#
# Usage (from inside NixOS-WSL):
#   curl -fsSL https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/devbox-switch.sh | bash -s -- ms-dev-2
# or:
#   DEVBOX_HOST=ms-dev-2 bash devbox-switch.sh
#
# It is idempotent: re-running skips the clone if ~/collective exists and just
# re-switches.

set -euo pipefail

DEVBOX_HOST="${1:-${DEVBOX_HOST:-}}"
if [ -z "$DEVBOX_HOST" ]; then
  read -r -p "Collective hostname for this devbox (e.g. ms-dev-2): " DEVBOX_HOST
fi
if [ -z "$DEVBOX_HOST" ]; then
  echo "ERROR: no devbox hostname given." >&2
  exit 1
fi

export PATH="/run/current-system/sw/bin:$PATH"

# Resolve a real home directory even in a bare login shell on a fresh distro.
HOME_DIR="$(eval echo ~"$(id -un)" 2>/dev/null || true)"
if [ -z "$HOME_DIR" ] || [ "$HOME_DIR" = "~$(id -un)" ]; then
  HOME_DIR="$(getent passwd "$(id -un)" | cut -d: -f6)"
fi
if [ -z "$HOME_DIR" ]; then HOME_DIR="/root"; fi
export HOME="$HOME_DIR"

KEY="$HOME_DIR/.ssh/id_ed25519"
if [ ! -f "$KEY" ]; then
  echo "ERROR: expected the shared devbox SSH key at $KEY but it is missing." >&2
  echo "Place the shared id_ed25519 there (chmod 600), then re-run." >&2
  exit 1
fi
chmod 600 "$KEY" 2>/dev/null || true

# Fresh NixOS-WSL may not have git on PATH until the first switch. Provide it
# via nix-shell if missing so the clone works on a brand-new distro.
GIT_BIN="$(command -v git || true)"
if [ -z "$GIT_BIN" ]; then
  echo "[devbox] git not found; providing it via nix-shell for the clone..."
  if command -v nix-shell >/dev/null 2>&1; then
    GIT="nix-shell -p git --run"
  else
    echo "ERROR: neither git nor nix-shell is available." >&2
    exit 1
  fi
else
  GIT=""
fi

# A fresh NixOS-WSL runs as root and its sudo is not yet setuid-wrapped
# ("sudo must be owned by uid 0 and have the setuid bit set"). Only use sudo
# when we are not already root.
if [ "$(id -u)" -eq 0 ]; then SUDO=""; else SUDO="sudo"; fi

cd "$HOME_DIR"
if [ ! -d "$HOME_DIR/collective/.git" ]; then
  echo "[devbox] Cloning collective over SSH..."
  export GIT_SSH_COMMAND="ssh -i $KEY -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new"
  if [ -n "$GIT" ]; then
    $GIT "git clone git@github.com:harryaskham/collective.git '$HOME_DIR/collective'"
  else
    git clone git@github.com:harryaskham/collective.git "$HOME_DIR/collective"
  fi
else
  echo "[devbox] collective already cloned; using existing checkout."
fi

cd "$HOME_DIR/collective"
echo "[devbox] Running first switch for '$DEVBOX_HOST' (this builds the system; may take a while)..."

# A fresh NixOS-WSL nix (bootstrap profile) does not yet have flakes enabled in
# nix.conf, and flake git operations (archive/lock) must use the repo's SSH
# multiplex wrapper rather than the clone-time single-key GIT_SSH_COMMAND.
# Mirror what `cltv switch` relies on so the first switch evaluates cleanly.
export GIT_SSH_COMMAND="$HOME_DIR/collective/scripts/git-ssh-multiplex"
NIX_FEATURE_ARGS=(--extra-experimental-features "nix-command flakes")

# Pre-fetch flake inputs over SSH so nixos-rebuild's eval does not block on a
# git remote it cannot reach with the wrong identity.
if command -v nix >/dev/null 2>&1; then
  echo "[devbox] Archiving flake inputs (nix flake archive)..."
  nix "${NIX_FEATURE_ARGS[@]}" flake archive . || true
fi

# Use nixos-rebuild directly since cltv may not be on PATH yet. Pass the
# experimental-features flag through so flake eval works pre-first-switch.
$SUDO nixos-rebuild switch --flake ".#$DEVBOX_HOST" \
  --option extra-experimental-features "nix-command flakes" \
  --show-trace --print-build-logs --impure || {
  echo "[devbox] First nixos-rebuild failed; you can re-run with:"
  echo "  cd ~/collective && cltv switch"
  exit 1
}
echo "[devbox] First switch complete for $DEVBOX_HOST."
echo "[devbox] Tailnet join + Windows convergence run automatically on switch."
