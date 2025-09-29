#!/bin/bash

cd /workspace/
set -eo pipefail

apt update -y
apt install curl -y
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm

touch $HOME/.no_auto_tmux
export USER=$(id -un)
export PATH="${PATH}:/nix/var/nix/profiles/default/bin"

mkdir -p $HOME/.config/home-manager
for filename in home.nix flake.nix flake.lock; do
  curl -o "$HOME/.config/home-manager/$filename" "https://raw.githubusercontent.com/harryaskham/collective-public/refs/heads/main/pkgs/vastai/provisioning_scripts/flake/$filename"
done
nix run home-manager/master -- switch

for RC_FILE in $HOME/.bashrc $HOME/.zshrc; do
  echo 'export PATH="${PATH}:/nix/var/nix/profiles/default/bin"' >> $RC_FILE
  echo 'source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"' >> $RC_FILE
done

echo "/nix/var/nix/profiles/default/bin/zsh" >> /etc/shells
chsh -s /nix/var/nix/profiles/default/bin/zsh