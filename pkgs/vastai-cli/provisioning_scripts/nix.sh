#!/bin/bash

# 
cd /workspace/
# Cause the script to exit on failure.
set -eo pipefail

# Activate the main virtual environment
. /venv/main/bin/activate

sudo apt update -y
sudo apt install curl -y

# curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  # --extra-conf "sandbox = false" \
  # --init none \
  # --no-confirm

# export PATH="${PATH}:/nix/var/nix/profiles/default/bin"
# nix run home-manager/master -- init --switch

# EXTRA_SHELLRC=$(cat << EOF
# export PATH="\${PATH}:/nix/var/nix/profiles/default/bin"
# source "\$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
# EOF
# )

# for RC_FILE in $HOME/.bashrc $HOME/.zshrc; do
  # echo "$EXTRA_SHELLRC" >> $RC_FILE
# done