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

# Binary caches the first switch needs. A fresh/untrusted bootstrap nix ignores
# the flake's nixConfig, so without these on the CLI it cannot fetch CUDA blobs
# (e.g. nvidia-cublas-cu12 pulled in by pi's tts dependency) and the build
# fails with "no substituter that can build it". These mirror flake.nix and the
# system nix.settings; once the first switch lands they become authoritative.
SUBS="https://harryaskham-cache.redhill-3c400511.eastus.azurecontainerapps.io/collective https://cuda-maintainers.cachix.org https://ghc.cachix.org https://nix-community.cachix.org https://nixpkgs.cachix.org https://cache.nixos.org"
KEYS="collective:r0dctotsGy3NnTdwb03tFA1ZENTvWGukej3jwZq5vQw= cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E= ghc.cachix.org-1:a751hwq9ydeP3Nr6h84iA9zSjxg9Z3uznqi4YBGjsiw= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

# Pre-fetch flake inputs over SSH so nixos-rebuild's eval does not block on a
# git remote it cannot reach with the wrong identity.
if command -v nix >/dev/null 2>&1; then
  echo "[devbox] Archiving flake inputs (nix flake archive)..."
  nix "${NIX_FEATURE_ARGS[@]}" flake archive . || true
fi

# home-manager sops derives its per-user age key from the configured user's
# ~/.ssh/id_ed25519 (the collective username, e.g. harryaskham), NOT the
# default WSL user (often root/nixos). On a brand-new distro that user does not
# exist until the first switch *builds and creates* it, so activation fails on
# the FIRST switch with:
#   "SSH key file /home/<user>/.ssh/id_ed25519 not found for deriving private
#    age key"
# and, because sops-install-secrets can then block multi-user.target, the
# distro appears to "hang" on every subsequent boot.
#
# Proven recovery (verified live on ms-dev-2):
#   1. Run the switch once. It BUILDS the system generation and creates the
#      configured user, but its activation fails on the missing key.
#   2. With the user now present, place the shared key into that user's home
#      (owned by the user, mode 600).
#   3. Re-run activation directly via /nix/var/nix/profiles/system/activate
#      (fast — no rebuild). sops derives the age key, imports it, and finishes.
#   4. wsl --shutdown + reopen for a clean systemd boot (services start, the
#      box joins the tailnet).
DEVBOX_USER="${DEVBOX_USER:-harryaskham}"

# Place the shared key into a specific user's home. Falls back to root ownership
# when the user is not yet resolvable (early boot), since `activate` reads the
# key as root and only needs it readable to derive the age key.
place_key_for_user() {
  user="$1"; home="/home/$user"
  [ "$user" = root ] && home=/root
  mkdir -p "$home/.ssh"
  if [ ! -f "$home/.ssh/id_ed25519" ]; then
    cp "$KEY" "$home/.ssh/id_ed25519" || return 1
  fi
  # chown to the user if it exists; otherwise leave root-owned (activate still
  # derives the age key fine). `id` failing means the user is not created yet.
  # Use the user's *primary group* (often `users` on NixOS, not a per-user
  # group), or fall back to chowning by user only.
  if id "$user" >/dev/null 2>&1; then
    grp="$(id -gn "$user" 2>/dev/null || echo "")"
    if [ -n "$grp" ]; then ownspec="$user:$grp"; else ownspec="$user"; fi
    chown "$ownspec" "$home/.ssh" "$home/.ssh/id_ed25519" 2>/dev/null || chown "$user" "$home/.ssh" "$home/.ssh/id_ed25519" 2>/dev/null || true
    [ -f "$home/.ssh/id_ed25519.age" ] && { chown "$ownspec" "$home/.ssh/id_ed25519.age" 2>/dev/null || chown "$user" "$home/.ssh/id_ed25519.age" 2>/dev/null || true; }
  fi
  chmod 700 "$home/.ssh" 2>/dev/null || true
  chmod 600 "$home/.ssh/id_ed25519" 2>/dev/null || true
  echo "[devbox] Placed shared key in $home/.ssh/id_ed25519 (user $user)."
}

# Place the key into every existing human home up front (best effort).
place_key_for_user root || true
for d in /home/*; do
  [ -d "$d" ] || continue
  place_key_for_user "$(basename "$d")" || true
done

do_switch() {
  # Use nixos-rebuild directly since cltv may not be on PATH yet. Pass the
  # experimental-features flag and binary caches through so flake eval + CUDA
  # fetches work pre-first-switch.
  $SUDO nixos-rebuild switch --flake ".#$DEVBOX_HOST" \
    --option extra-experimental-features "nix-command flakes" \
    --option extra-substituters "$SUBS" \
    --option extra-trusted-public-keys "$KEYS" \
    --show-trace --print-build-logs --impure
}

# Re-run activation directly from the built system generation. Much faster than
# a full re-switch and exactly what unblocks the box once the key is in place.
reactivate() {
  if [ -x /nix/var/nix/profiles/system/activate ]; then
    $SUDO /nix/var/nix/profiles/system/activate
  else
    $SUDO /nix/var/nix/profiles/system/bin/switch-to-configuration switch
  fi
}

if do_switch; then
  echo "[devbox] First switch complete for $DEVBOX_HOST."
else
  echo "[devbox] Switch did not fully activate (expected on first run: the"
  echo "[devbox] configured user '$DEVBOX_USER' was just created and the home"
  echo "[devbox] sops age key could not be derived). Placing the key now that"
  echo "[devbox] the user exists, then re-activating directly..."
  place_key_for_user "$DEVBOX_USER" || true
  if reactivate; then
    echo "[devbox] Re-activation succeeded after placing the key for $DEVBOX_USER."
  else
    echo "[devbox] Re-activation still failed. Inspect manually, then re-run:"
    echo "  cd ~/collective && cltv switch"
    exit 1
  fi
fi

# Ensure the configured user owns its key + derived age key for the home-manager
# sops generation that runs on subsequent (user) logins. Use the user's primary
# group (e.g. `users`), not a same-named group which may not exist.
if id "$DEVBOX_USER" >/dev/null 2>&1; then
  grp="$(id -gn "$DEVBOX_USER" 2>/dev/null || echo "")"
  if [ -n "$grp" ]; then ownspec="$DEVBOX_USER:$grp"; else ownspec="$DEVBOX_USER"; fi
  for f in /home/$DEVBOX_USER/.ssh/id_ed25519 /home/$DEVBOX_USER/.ssh/id_ed25519.age; do
    [ -e "$f" ] && { chown "$ownspec" "$f" 2>/dev/null || chown "$DEVBOX_USER" "$f" 2>/dev/null || true; }
  done
fi

echo "[devbox] Done. Run 'wsl --shutdown' from Windows for a clean systemd boot"
echo "[devbox] (wsl.defaultUser=$DEVBOX_USER takes effect; tailscale joins),"
echo "[devbox] then reopen the distro. Windows convergence runs on switch."
