#!/usr/bin/env bash
# devbox-switch.sh — WSL-side half of devbox bootstrap.
#
# Run this from INSIDE a NixOS-WSL distro that already has the shared devbox
# SSH key placed at ~/.ssh/id_ed25519 (the "wsl-with-keys" checkpoint). It
# clones the private collective flake over SSH and runs the first
# nixos-rebuild switch for the given host.
#
# Usage (from inside NixOS-WSL):
#   curl -fsSLo /tmp/devbox-switch.sh https://raw.githubusercontent.com/harryaskham/collective-public/main/windows/devbox-switch.sh
#   bash /tmp/devbox-switch.sh ms-dev-2
# or from an existing checkout:
#   bash collective-public/windows/devbox-switch.sh ms-dev-2
#
# Do not use curl|bash here: Nix/systemctl may read stdin. The PowerShell
# bootstrap likewise stages this complete file before executing it.
#
# It is idempotent AND resumable: safe to re-run at any stage to CONTINUE a
# partial bringup to completion. It self-bootstraps PATH (so it works even before
# the system is activated), auto-locates the shared key and the collective
# checkout wherever a prior/partial run left them, places the key for root + the
# configured user (deriving id_ed25519.pub), runs/continues the switch, and
# completes activation + home-manager linking. Just re-run it.

set -euo pipefail
# Keep diagnostics ASCII so Windows PowerShell 5.1 / legacy console code pages
# cannot turn GNU's Unicode quotes into mojibake in bootstrap output.
export LC_ALL=C

DEVBOX_HOST="${DEVBOX_HOST:-}"
USE_DEFAULT_SUBS="${USE_DEFAULT_SUBS:-0}"
while [ "$#" -gt 0 ]; do
  case "$1" in
    --use-default-subs)
      USE_DEFAULT_SUBS=1
      shift
      ;;
    --help|-h)
      echo "Usage: devbox-switch.sh [ms-dev-N] [--use-default-subs]"
      echo "By default the accepted flake nixConfig is authoritative for caches."
      exit 0
      ;;
    -*)
      echo "ERROR: unknown option: $1" >&2
      exit 2
      ;;
    *)
      if [ -n "$DEVBOX_HOST" ]; then
        echo "ERROR: multiple devbox hostnames supplied." >&2
        exit 2
      fi
      DEVBOX_HOST="$1"
      shift
      ;;
  esac
done
if [ -z "$DEVBOX_HOST" ]; then
  read -r -p "Collective hostname for this devbox (e.g. ms-dev-2): " DEVBOX_HOST
fi
if [ -z "$DEVBOX_HOST" ]; then
  echo "ERROR: no devbox hostname given." >&2
  exit 1
fi
DEVBOX_USER="${DEVBOX_USER:-harryaskham}"

# Tools must be available whether or not the system is fully activated yet.
# /run/current-system only exists AFTER activation; the BUILT generation's tools
# live at /nix/var/nix/profiles/system/sw/bin, and the bootstrap nix is in the
# default profile. Without all of these a partial/resumed run hits
# "id: command not found" / "ls: command not found" on a pre-activation distro.
export PATH="/run/current-system/sw/bin:/nix/var/nix/profiles/system/sw/bin:/run/wrappers/bin:/nix/var/nix/profiles/default/bin:/bin:/usr/bin:$PATH"
mkdir -p /var/lib/collective-bootstrap
printf '%s\n' "$DEVBOX_HOST" > /var/lib/collective-bootstrap/devbox-name.txt

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

# Fresh NixOS-WSL may not have git on PATH until the first switch. Do not use a
# one-command `nix-shell --run` only for the clone: Nix evaluation later invokes
# git itself for builtins.fetchGit/Cargo git dependencies. Persist the
# Nix-provided git+openssh PATH for this entire bootstrap process.
if ! command -v git >/dev/null 2>&1 || \
   ! command -v ssh-to-age >/dev/null 2>&1 || \
   ! command -v sops >/dev/null 2>&1; then
  echo "[devbox] Adding nix-shell git+openssh+sops+ssh-to-age to bootstrap PATH..."
  if command -v nix-shell >/dev/null 2>&1; then
    # The nested nix-shell must expand its own PATH, not this shell's PATH.
    # shellcheck disable=SC2016
    BOOTSTRAP_PATH="$(nix-shell -p git openssh sops ssh-to-age --run 'printf "%s" "$PATH"')"
    if [ -z "$BOOTSTRAP_PATH" ]; then
      echo "ERROR: nix-shell returned an empty bootstrap PATH." >&2
      exit 1
    fi
    export PATH="$BOOTSTRAP_PATH"
  else
    echo "ERROR: neither git nor nix-shell is available." >&2
    exit 1
  fi
fi
if ! command -v git >/dev/null 2>&1; then
  echo "ERROR: git is still unavailable after nix-shell PATH setup." >&2
  exit 1
fi
echo "[devbox] Using bootstrap git: $(command -v git)"

# A fresh NixOS-WSL runs as root and its sudo is not yet setuid-wrapped
# ("sudo must be owned by uid 0 and have the setuid bit set"). Only use sudo
# when we are not already root.
if [ "$(id -u)" -eq 0 ]; then SUDO=""; else SUDO="sudo"; fi

# Once the configured user exists, make its home the canonical checkout
# location. A first run must clone as root before that user is created; a resume
# should adopt that fully fetched /root/collective instead of cloning again.
# Moving at the end is also safe when this script itself was launched from the
# root checkout: bash already has the script open.
migrate_collective_checkout() {
  id "$DEVBOX_USER" >/dev/null 2>&1 || return 0
  target_home="$(getent passwd "$DEVBOX_USER" | cut -d: -f6)"
  [ -n "$target_home" ] || target_home="/home/$DEVBOX_USER"
  target="$target_home/collective"
  if [ -d "$target/.git" ]; then
    COLLECTIVE="$target"
    return 0
  fi
  if [ -d /root/collective/.git ] && [ "$target" != /root/collective ]; then
    echo "[devbox] Moving bootstrap checkout /root/collective -> $target"
    mkdir -p "$target_home"
    mv /root/collective "$target"
    grp="$(id -gn "$DEVBOX_USER" 2>/dev/null || echo users)"
    chown -R "$DEVBOX_USER:$grp" "$target"
    # The remainder of this bootstrap still runs as root. Trust the now
    # user-owned checkout explicitly so Git's safe.directory guard does not
    # reject flake input operations before we hand execution to the user.
    git config --global --add safe.directory "$target" 2>/dev/null || true
    COLLECTIVE="$target"
  fi
}

cd "$HOME_DIR"
# Reuse an existing collective checkout wherever a prior run left it. Prefer
# the configured user's checkout; migrate the root bootstrap clone when able.
COLLECTIVE=""
migrate_collective_checkout
if [ -z "$COLLECTIVE" ]; then
  for c in "/home/$DEVBOX_USER/collective" "$HOME_DIR/collective" /root/collective /home/*/collective; do
    [ -d "$c/.git" ] && COLLECTIVE="$c" && break
  done
fi
if [ -z "$COLLECTIVE" ]; then
  COLLECTIVE="$HOME_DIR/collective"
  echo "[devbox] Cloning collective over SSH to $COLLECTIVE..."
  export GIT_SSH_COMMAND="ssh -i $KEY -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new"
  git clone git@github.com:harryaskham/collective.git "$COLLECTIVE"
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
echo "[devbox] Updating the existing checkout before switching..."
git pull --ff-only

# Emergency fallback cache set. Normally `--accept-flake-config` makes the
# flake's nixConfig authoritative (including a pre-tailnet Funnel/Attic URL), so
# we do NOT add CLI cache options. Set USE_DEFAULT_SUBS=1 or pass
# --use-default-subs only when the configured caches are unavailable.
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
  # The nested shell intentionally expands these variables, not this shell.
  # shellcheck disable=SC2016
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
  nix-shell -p git openssh --run 'nix --option accept-flake-config true --extra-experimental-features "nix-command flakes" flake archive .' || true
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
# Place the shared key into a specific user's home. Falls back to root ownership
# when the user is not yet resolvable (early boot), since `activate` reads the
# key as root and only needs it readable to derive the age key.
place_key_for_user() {
  user="$1"; home="/home/$user"
  [ "$user" = root ] && home=/root
  mkdir -p "$home/.ssh"
  # Root owns the durable bootstrap material. Mirror it into the configured
  # user's home on every resume so an explicitly replaced root key cannot leave
  # stale user credentials behind.
  if [ "$KEY" != "$home/.ssh/id_ed25519" ]; then
    cp "$KEY" "$home/.ssh/id_ed25519" || return 1
  fi
  # Derive/refresh both companion keys before activation. This is the same
  # private age identity that the NixOS/home-manager activation would derive,
  # but having it ready makes a partially built machine immediately resumable.
  if command -v ssh-keygen >/dev/null 2>&1; then
    ssh-keygen -y -f "$home/.ssh/id_ed25519" > "$home/.ssh/id_ed25519.pub" 2>/dev/null || true
  fi
  if command -v ssh-to-age >/dev/null 2>&1; then
    ssh-to-age -private-key < "$home/.ssh/id_ed25519" > "$home/.ssh/id_ed25519.age" 2>/dev/null || true
  fi
  # The corp key is materialized under root before the first evaluation. Seed
  # the user's expected paths from that already-decrypted root material; sops
  # activation will subsequently own/refresh the same files declaratively.
  if [ "$home" != /root ]; then
    for extra in corp-github-key corp-github-key.pub known_hosts; do
      [ -s "/root/.ssh/$extra" ] && cp "/root/.ssh/$extra" "$home/.ssh/$extra"
    done
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
  [ -f "$home/.ssh/id_ed25519.age" ] && chmod 664 "$home/.ssh/id_ed25519.age" 2>/dev/null || true
  [ -f "$home/.ssh/corp-github-key" ] && chmod 600 "$home/.ssh/corp-github-key" 2>/dev/null || true
  [ -f "$home/.ssh/corp-github-key.pub" ] && chmod 644 "$home/.ssh/corp-github-key.pub" 2>/dev/null || true
  [ -f "$home/.ssh/known_hosts" ] && chmod 644 "$home/.ssh/known_hosts" 2>/dev/null || true
  echo "[devbox] Seeded SSH, age, and available corp key material in $home/.ssh (user $user)."
}

# Place the key into every existing human home up front (best effort).
place_key_for_user root || true
for d in /home/*; do
  [ -d "$d" ] || continue
  place_key_for_user "$(basename "$d")" || true
done

do_switch() {
  # Use nixos-rebuild directly since cltv may not be on PATH yet. The accepted
  # flake nixConfig supplies caches by default; CLI fallback caches are opt-in.
  local cache_args=()
  if [ "$USE_DEFAULT_SUBS" = 1 ]; then
    echo "[devbox] USE_DEFAULT_SUBS=1: adding fallback public substituters."
    cache_args=(
      --option extra-substituters "$SUBS"
      --option extra-trusted-public-keys "$KEYS"
    )
  else
    echo "[devbox] Using substituters and trusted keys from accepted flake nixConfig."
  fi
  $SUDO nixos-rebuild switch --flake ".#$DEVBOX_HOST" \
    --option accept-flake-config true \
    --option extra-experimental-features "nix-command flakes" \
    "${cache_args[@]}" \
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

# Only use the key-placement/reactivation recovery when this switch actually
# produced a NEW system generation. A fetch/evaluation/build failure leaves the
# profile unchanged; reactivating that old profile would hide the real failure.
SYSTEM_BEFORE="$(readlink -f /nix/var/nix/profiles/system 2>/dev/null || true)"
if do_switch; then
  echo "[devbox] First switch complete for $DEVBOX_HOST."
else
  SYSTEM_AFTER="$(readlink -f /nix/var/nix/profiles/system 2>/dev/null || true)"
  if [ -z "$SYSTEM_AFTER" ] || [ "$SYSTEM_AFTER" = "$SYSTEM_BEFORE" ]; then
    echo "[devbox] ERROR: switch failed before producing a new system generation;" >&2
    echo "[devbox] not reactivating the previous generation. Fix the fetch/build" >&2
    echo "[devbox] error above, then re-run this resumable bootstrap." >&2
    exit 1
  fi
  echo "[devbox] Switch built a new generation but did not fully activate"
  echo "[devbox] (expected on first run when '$DEVBOX_USER' lacks its sops age"
  echo "[devbox] key). Placing the key, then re-activating directly..."
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

  # The configured user now exists; hand it the bootstrap checkout so the
  # documented `cd ~/collective && cltv switch` path works after WSL restarts.
  migrate_collective_checkout
  if [ -d "/home/$DEVBOX_USER/collective/.git" ]; then
    echo "[devbox] User checkout ready at /home/$DEVBOX_USER/collective."
  fi
fi

echo "[devbox] Done. Run 'wsl --shutdown' from Windows for a clean systemd boot"
echo "[devbox] (wsl.defaultUser=$DEVBOX_USER takes effect; tailscale joins),"
echo "[devbox] then reopen the distro. Windows convergence runs on switch."
