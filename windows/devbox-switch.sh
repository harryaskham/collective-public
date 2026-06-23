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
# It is idempotent AND resumable: safe to re-run at any stage to CONTINUE a
# partial bringup to completion. It self-bootstraps PATH (so it works even before
# the system is activated), auto-locates the shared key and the collective
# checkout wherever a prior/partial run left them, places the key for root + the
# configured user (deriving id_ed25519.pub), runs/continues the switch, and
# completes activation + home-manager linking. Just re-run it.

set -euo pipefail

DEVBOX_HOST="${1:-${DEVBOX_HOST:-}}"
if [ -z "$DEVBOX_HOST" ]; then
  read -r -p "Collective hostname for this devbox (e.g. ms-dev-2): " DEVBOX_HOST
fi
if [ -z "$DEVBOX_HOST" ]; then
  echo "ERROR: no devbox hostname given." >&2
  exit 1
fi

# Tools must be available whether or not the system is fully activated yet.
# /run/current-system only exists AFTER activation; the BUILT generation's tools
# live at /nix/var/nix/profiles/system/sw/bin, and the bootstrap nix is in the
# default profile. Without all of these a partial/resumed run hits
# "id: command not found" / "ls: command not found" on a pre-activation distro.
export PATH="/run/current-system/sw/bin:/nix/var/nix/profiles/system/sw/bin:/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/bin:/usr/bin:$PATH"

# Resolve a real home directory even in a bare login shell on a fresh distro.
HOME_DIR="$(eval echo ~"$(id -un)" 2>/dev/null || true)"
if [ -z "$HOME_DIR" ] || [ "$HOME_DIR" = "~$(id -un)" ]; then
  HOME_DIR="$(getent passwd "$(id -un)" | cut -d: -f6)"
fi
if [ -z "$HOME_DIR" ]; then HOME_DIR="/root"; fi
export HOME="$HOME_DIR"

# Locate the shared devbox SSH key. On a resumed/partial bringup the key may live
# in a different home than whoever runs this (bootstrap places it for the default
# WSL user; a root-driven resume looks in /root). Adopt whichever copy exists and
# ensure it is at $HOME_DIR/.ssh so the clone + age-key derivation can read it.
KEY="$HOME_DIR/.ssh/id_ed25519"
if [ ! -f "$KEY" ]; then
  FOUND=""
  for c in /root/.ssh/id_ed25519 /home/*/.ssh/id_ed25519; do
    [ -s "$c" ] && FOUND="$c" && break
  done
  if [ -n "$FOUND" ]; then
    echo "[devbox] Adopting shared key found at $FOUND -> $KEY"
    mkdir -p "$HOME_DIR/.ssh" && chmod 700 "$HOME_DIR/.ssh"
    cp "$FOUND" "$KEY"
  else
    echo "ERROR: shared devbox SSH key (id_ed25519) not found in $HOME_DIR/.ssh, /root/.ssh, or any /home/*/.ssh." >&2
    echo "Place the shared id_ed25519 in one of those (chmod 600), then re-run." >&2
    exit 1
  fi
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
# Reuse an existing collective checkout wherever a prior run left it (this home,
# /root, or the default WSL user's home) so a resumed bringup does not re-clone.
COLLECTIVE=""
for c in "$HOME_DIR/collective" /root/collective /home/*/collective; do
  [ -d "$c/.git" ] && COLLECTIVE="$c" && break
done
if [ -z "$COLLECTIVE" ]; then
  COLLECTIVE="$HOME_DIR/collective"
  echo "[devbox] Cloning collective over SSH to $COLLECTIVE..."
  export GIT_SSH_COMMAND="ssh -i $KEY -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new"
  if [ -n "$GIT" ]; then
    $GIT "git clone git@github.com:harryaskham/collective.git '$COLLECTIVE'"
  else
    git clone git@github.com:harryaskham/collective.git "$COLLECTIVE"
  fi
else
  echo "[devbox] Using existing collective checkout at $COLLECTIVE."
fi

cd "$COLLECTIVE"
echo "[devbox] Running first switch for '$DEVBOX_HOST' (this builds the system; may take a while)..."

# A fresh NixOS-WSL nix (bootstrap profile) does not yet have flakes enabled in
# nix.conf, and flake git operations (archive/lock) must use the repo's SSH
# multiplex wrapper rather than the clone-time single-key GIT_SSH_COMMAND.
# Mirror what `cltv switch` relies on so the first switch evaluates cleanly.
export GIT_SSH_COMMAND="$COLLECTIVE/scripts/git-ssh-multiplex"
NIX_FEATURE_ARGS=(--extra-experimental-features "nix-command flakes")

# Binary caches the first switch needs. A fresh/untrusted bootstrap nix ignores
# the flake's nixConfig, so without these on the CLI it cannot fetch CUDA blobs
# (e.g. nvidia-cublas-cu12 pulled in by pi's tts dependency) and the build
# fails with "no substituter that can build it". These mirror flake.nix and the
# system nix.settings; once the first switch lands they become authoritative.
SUBS="https://cuda-maintainers.cachix.org https://ghc.cachix.org https://nix-community.cachix.org https://nixpkgs.cachix.org https://cache.nixos.org"
KEYS="cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E= ghc.cachix.org-1:a751hwq9ydeP3Nr6h84iA9zSjxg9Z3uznqi4YBGjsiw= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

# Materialize the corp GitHub key from sops BEFORE fetching flake inputs. Some
# flake inputs live on corp GitHub (infinity-microsoft / gim-home / msft.ghe.com)
# and git-ssh-multiplex routes those to ~/.ssh/corp-github-key, which home-manager
# sops only places AFTER the first switch. Decrypt it now using the SSH key as the
# age identity (ssh-to-age) so the very first archive / eval can reach corp
# remotes. Verified on ms-dev-2.
SECRETS_FILE="$COLLECTIVE/standalone/secrets/secrets.yaml"
if [ ! -s "$HOME_DIR/.ssh/corp-github-key" ] && [ -f "$SECRETS_FILE" ]; then
  echo "[devbox] Materializing corp-github-key from sops..."
  export SECRETS_FILE
  nix-shell -p sops ssh-to-age openssh --run '
    set -e
    install -d -m700 "$HOME/.ssh"
    AGE=$(ssh-to-age -private-key -i "$HOME/.ssh/id_ed25519")
    SOPS_AGE_KEY="$AGE" sops decrypt --extract "[\"keys\"][\"ms\"][\"ssh\"][\"private\"]" "$SECRETS_FILE" > "$HOME/.ssh/corp-github-key"
    chmod 600 "$HOME/.ssh/corp-github-key"
    SOPS_AGE_KEY="$AGE" sops decrypt --extract "[\"keys\"][\"ms\"][\"ssh\"][\"public\"]" "$SECRETS_FILE" > "$HOME/.ssh/corp-github-key.pub" 2>/dev/null || true
  ' || echo "[devbox] WARN: corp-github-key materialization failed; corp flake inputs may not fetch."
fi

# Pre-fetch flake inputs over SSH so nixos-rebuild's eval does not block on a
# git remote it cannot reach with the wrong identity. Run via nix-shell so a real
# git+openssh is on PATH: a fresh bootstrap nix has no nix `git`, so nix would
# otherwise fall through to the Windows-interop git.exe and fail with
# `error: executing 'git': Permission denied`. GIT_SSH_COMMAND is inherited.
if command -v nix >/dev/null 2>&1; then
  echo "[devbox] Archiving flake inputs (nix flake archive)..."
  nix-shell -p git openssh --run 'nix --extra-experimental-features "nix-command flakes" flake archive .' || true
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
  # home-manager's sops activation (deriveSshPublicKey) runs `ssh-to-age` on the
  # PUBLIC key and fails without id_ed25519.pub. Derive it from the private key.
  if [ ! -s "$home/.ssh/id_ed25519.pub" ] && command -v ssh-keygen >/dev/null 2>&1; then
    ssh-keygen -y -f "$home/.ssh/id_ed25519" > "$home/.ssh/id_ed25519.pub" 2>/dev/null || true
  fi
  # chown the WHOLE .ssh tree to the user if it exists; otherwise leave
  # root-owned (system `activate` still derives the age key fine). The home-
  # manager activation runs AS the user and must own .ssh and every file in it
  # (incl. the derived id_ed25519.age) or deriveAgePrivateKey fails with
  # "chmod: ... Operation not permitted". Use the user's *primary group*
  # (often `users` on NixOS, not a per-user group), or fall back to user only.
  if id "$user" >/dev/null 2>&1; then
    grp="$(id -gn "$user" 2>/dev/null || echo "")"
    if [ -n "$grp" ]; then ownspec="$user:$grp"; else ownspec="$user"; fi
    chown -R "$ownspec" "$home/.ssh" 2>/dev/null || chown -R "$user" "$home/.ssh" 2>/dev/null || true
  fi
  chmod 700 "$home/.ssh" 2>/dev/null || true
  chmod 600 "$home/.ssh/id_ed25519" 2>/dev/null || true
  [ -f "$home/.ssh/id_ed25519.pub" ] && chmod 644 "$home/.ssh/id_ed25519.pub" 2>/dev/null || true
  echo "[devbox] Placed shared key + derived .pub in $home/.ssh (user $user)."
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

# Ensure the configured user fully owns its .ssh tree (key, derived .age, and
# .pub) so the home-manager activation (which runs AS the user) can derive its
# age key and ssh public key without permission errors. place_key_for_user
# already does the recursive chown + .pub derivation; call it once more in case
# `activate` created root-owned files (e.g. id_ed25519.age) after the build.
if id "$DEVBOX_USER" >/dev/null 2>&1; then
  place_key_for_user "$DEVBOX_USER" || true

  # Kick the home-manager activation explicitly. On the first boot it usually
  # fails before the .ssh ownership/.pub fixes above, so a restart applies the
  # home-file links (.zshrc etc.) and the user's sops generation now.
  if command -v systemctl >/dev/null 2>&1; then
    echo "[devbox] (Re)starting home-manager-$DEVBOX_USER.service to link home files..."
    $SUDO systemctl restart "home-manager-$DEVBOX_USER.service" 2>/dev/null || true
    if systemctl is-active --quiet "home-manager-$DEVBOX_USER.service" 2>/dev/null; then
      echo "[devbox] home-manager-$DEVBOX_USER active; home files linked (.zshrc, etc.)."
    else
      echo "[devbox] WARNING: home-manager-$DEVBOX_USER not active yet. Check:"
      echo "  journalctl -u home-manager-$DEVBOX_USER.service -n 40"
    fi
  fi
fi

echo "[devbox] Done. Run 'wsl --shutdown' from Windows for a clean systemd boot"
echo "[devbox] (wsl.defaultUser=$DEVBOX_USER takes effect; tailscale joins),"
echo "[devbox] then reopen the distro. Windows convergence runs on switch."
