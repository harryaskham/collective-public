#!/bin/bash

cd /workspace/
set -eo pipefail

usermod -d /home/harry -m -g root -l harry user
passwd -d harry

apt update -y
apt install curl -y
curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm

touch $HOME/.no_auto_tmux
export USER=$(id -un)
export PATH="${PATH}:/nix/var/nix/profiles/default/bin"
nix run home-manager/master -- init --switch

for RC_FILE in $HOME/.bashrc $HOME/.zshrc; do
  echo 'export PATH="${PATH}:/nix/var/nix/profiles/default/bin"' >> $RC_FILE
  echo 'source "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"' >> $RC_FILE
done