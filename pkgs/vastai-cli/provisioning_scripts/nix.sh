#!/bin/bash

cd /workspace/
set -eo pipefail
. /venv/main/bin/activate

touch $HOME/.no_auto_tmux

usermod -d /home/harry -m -g root -l harry user
su harry

touch $HOME/.no_auto_tmux

apt update -y
apt install curl -y

curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm

export PATH="${PATH}:/nix/var/nix/profiles/default/bin"
nix run home-manager/master -- init --switch

EXTRA_SHELLRC=$(cat << EOF
export PATH="\${PATH}:/nix/var/nix/profiles/default/bin"
source "\$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
EOF
)

for RC_FILE in $HOME/.bashrc $HOME/.zshrc; do
  echo "$EXTRA_SHELLRC" >> $RC_FILE
done