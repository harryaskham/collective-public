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

cd "$HOME_DIR"
if [ ! -d "$HOME_DIR/collective/.git" ]; then
  echo "[devbox] Cloning collective over SSH..."
  GIT_SSH_COMMAND="ssh -i $KEY -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new" \
    git clone git@github.com:harryaskham/collective.git "$HOME_DIR/collective"
else
  echo "[devbox] collective already cloned; using existing checkout."
fi

cd "$HOME_DIR/collective"
echo "[devbox] Running first switch for '$DEVBOX_HOST' (this builds the system; may take a while)..."
# Use nixos-rebuild directly since cltv may not be on PATH yet.
sudo nixos-rebuild switch --flake ".#$DEVBOX_HOST" --show-trace --print-build-logs --impure || {
  echo "[devbox] First nixos-rebuild failed; you can re-run with:"
  echo "  cd ~/collective && cltv switch"
  exit 1
}
echo "[devbox] First switch complete for $DEVBOX_HOST."
echo "[devbox] Tailnet join + Windows convergence run automatically on switch."
