#!/usr/bin/env sh
# devbox-fix-key.sh — recover a devbox stuck on the home-sops age-key step.
#
# Symptom this fixes:
#   nixos-rebuild activation fails with
#     "SSH key file /home/<user>/.ssh/id_ed25519 not found for deriving private
#      age key"
#   because the shared key was placed under the default WSL user's home
#   (/home/nixos/.ssh) instead of the configured collective user's home
#   (/home/harryaskham/.ssh). With the key missing where the system/home sops
#   units expect it, sops-install-secrets can also block multi-user.target so
#   the distro appears to "hang" on every boot.
#
# This script is POSIX sh, sets an explicit PATH (works even with no profile /
# empty PATH under `wsl --exec`), copies the key into the target user's home
# with correct owner+mode, then optionally pulls + runs the switch.
#
# Run from a Windows PowerShell against the WSL distro (no interactive shell
# needed, bypasses the wedged systemd boot via --exec):
#
#   wsl -d NixOS -u root --exec /run/current-system/sw/bin/sh -c \
#     "curl -fsSL https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/devbox-fix-key.sh | sh -s -- ms-dev-2"
#
# If curl is not on PATH yet, fetch with nix-provided curl first, or run the
# embedded commands directly. The script also accepts env overrides:
#   DEVBOX_HOST     target collective hostname (default: arg 1, else ms-dev-2)
#   TARGET_USER     configured collective user (default: harryaskham)
#   SOURCE_USER     user whose ~/.ssh currently holds the key (default: nixos)
#   DO_SWITCH       1 to also git pull + nixos-rebuild switch (default: 1)
#
set -eu

# Explicit PATH so coreutils/git/nixos-rebuild resolve under a bare --exec shell
# with no profile sourced. Cover the common NixOS-WSL locations.
PATH="/run/current-system/sw/bin:/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/usr/bin:/bin:${PATH:-}"
export PATH

DEVBOX_HOST="${1:-${DEVBOX_HOST:-ms-dev-2}}"
TARGET_USER="${TARGET_USER:-harryaskham}"
SOURCE_USER="${SOURCE_USER:-nixos}"
DO_SWITCH="${DO_SWITCH:-1}"

SRC_KEY="/home/${SOURCE_USER}/.ssh/id_ed25519"
DST_DIR="/home/${TARGET_USER}/.ssh"
DST_KEY="${DST_DIR}/id_ed25519"

echo "[fix-key] host=${DEVBOX_HOST} target=${TARGET_USER} source=${SOURCE_USER}"
echo "[fix-key] PATH=${PATH}"

if [ ! -f "$SRC_KEY" ]; then
  # Fall back to root's key if the source user's is absent.
  if [ -f "/root/.ssh/id_ed25519" ]; then
    SRC_KEY="/root/.ssh/id_ed25519"
    echo "[fix-key] source key missing; falling back to /root/.ssh/id_ed25519"
  else
    echo "[fix-key] ERROR: no source key at /home/${SOURCE_USER}/.ssh/id_ed25519 or /root/.ssh/id_ed25519" >&2
    echo "[fix-key] Place the shared id_ed25519 somewhere reachable and re-run." >&2
    exit 1
  fi
fi

echo "[fix-key] placing key from ${SRC_KEY} -> ${DST_KEY}"
mkdir -p "$DST_DIR"
cp "$SRC_KEY" "$DST_KEY"
# Copy the matching pubkey if present (harmless if missing).
if [ -f "${SRC_KEY}.pub" ]; then cp "${SRC_KEY}.pub" "${DST_KEY}.pub" || true; fi
# home-manager's sops activation (deriveSshPublicKey) runs ssh-to-age on the
# PUBLIC key and fails without id_ed25519.pub. Derive it if still missing.
if [ ! -s "${DST_KEY}.pub" ] && command -v ssh-keygen >/dev/null 2>&1; then
  ssh-keygen -y -f "$DST_KEY" > "${DST_KEY}.pub" 2>/dev/null || true
fi

# Ownership: chown the WHOLE .ssh tree to the target user using its PRIMARY
# GROUP (e.g. `users` on NixOS, not a same-named group which usually does not
# exist). The home-manager activation runs AS the user and must own .ssh and
# every file in it (incl. the derived id_ed25519.age) or deriveAgePrivateKey
# fails with "chmod: ... Operation not permitted". Fall back to user-only chown,
# then leave root-owned if the user is not resolvable yet.
if id "$TARGET_USER" >/dev/null 2>&1; then
  TGRP="$(id -gn "$TARGET_USER" 2>/dev/null || echo '')"
  if [ -n "$TGRP" ]; then OWNSPEC="${TARGET_USER}:${TGRP}"; else OWNSPEC="${TARGET_USER}"; fi
else
  OWNSPEC="${TARGET_USER}"
fi
chown -R "$OWNSPEC" "$DST_DIR" 2>/dev/null || \
  chown -R "${TARGET_USER}" "$DST_DIR" 2>/dev/null || true
chmod 700 "$DST_DIR"
chmod 600 "$DST_KEY"
[ -f "${DST_KEY}.pub" ] && chmod 644 "${DST_KEY}.pub" || true

echo "[fix-key] KEY_PLACED"
ls -l "$DST_DIR"

if [ "$DO_SWITCH" != "1" ]; then
  echo "[fix-key] DO_SWITCH!=1; skipping pull + switch. Done."
  exit 0
fi

# Locate the checkout (default WSL user clones into its own home).
REPO=""
for c in "/home/${SOURCE_USER}/collective" "/home/${TARGET_USER}/collective" "/root/collective"; do
  if [ -d "$c/.git" ]; then REPO="$c"; break; fi
done
if [ -z "$REPO" ]; then
  echo "[fix-key] WARNING: no collective checkout found; key is placed but no switch run." >&2
  echo "[fix-key] Re-run with the repo present, or run the switch manually." >&2
  exit 0
fi
echo "[fix-key] using checkout: ${REPO}"
cd "$REPO"

# Use the repo's SSH multiplexer so git uses the right key, matching cltv switch.
if [ -x "./scripts/git-ssh-multiplex" ]; then
  GIT_SSH_COMMAND="$REPO/scripts/git-ssh-multiplex"
  export GIT_SSH_COMMAND
fi

echo "[fix-key] git pull --ff-only (to pick up wsl.defaultUser + cache fixes)"
git pull --ff-only || echo "[fix-key] git pull failed (continuing with current checkout)"
git log --oneline -1 || true

# Binary caches baked on the CLI in case a fresh/untrusted nix ignores flake nixConfig.
SUBS="https://cuda-maintainers.cachix.org https://ghc.cachix.org https://nix-community.cachix.org https://nixpkgs.cachix.org https://cache.nixos.org"
KEYS="cuda-maintainers.cachix.org-1:0dq3bujKpuEPMCX6U4WylrUDZ9JyUG0VpVZa7CNfq5E= ghc.cachix.org-1:a751hwq9ydeP3Nr6h84iA9zSjxg9Z3uznqi4YBGjsiw= nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs= nixpkgs.cachix.org-1:q91R6hxbwFvDqTSDKwDAV4T5PxqXGxswD8vhONFMeOE= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="

echo "[fix-key] running nixos-rebuild switch for ${DEVBOX_HOST}..."
nixos-rebuild switch --flake ".#${DEVBOX_HOST}" \
  --option extra-experimental-features "nix-command flakes" \
  --option extra-substituters "$SUBS" \
  --option extra-trusted-public-keys "$KEYS" \
  --show-trace --print-build-logs --impure || \
  echo "[fix-key] nixos-rebuild failed; trying direct re-activation instead..."

# Whether the switch built or just re-activation is needed, ensure the system is
# activated and the user's home-manager generation is applied. Re-activate from
# the built generation (fast) and restart the user's HM service so home files
# (.zshrc etc.) get linked now rather than on the next boot.
if [ -x /nix/var/nix/profiles/system/activate ]; then
  /nix/var/nix/profiles/system/activate || true
fi
if command -v systemctl >/dev/null 2>&1; then
  echo "[fix-key] (re)starting home-manager-${TARGET_USER}.service to link home files..."
  systemctl restart "home-manager-${TARGET_USER}.service" 2>/dev/null || true
  if systemctl is-active --quiet "home-manager-${TARGET_USER}.service" 2>/dev/null; then
    echo "[fix-key] home-manager-${TARGET_USER} active; .zshrc + home files linked."
  else
    echo "[fix-key] WARNING: home-manager-${TARGET_USER} not active. Check:"
    echo "  journalctl -u home-manager-${TARGET_USER}.service -n 40"
  fi
fi

echo "[fix-key] switch complete for ${DEVBOX_HOST}."
echo "[fix-key] After this, run 'wsl --shutdown' from Windows so wsl.defaultUser"
echo "[fix-key] (${TARGET_USER}) takes effect, then reopen the distro."
