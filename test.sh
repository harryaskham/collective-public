#!/usr/bin/env bash

CONTAINER=nix-container

function install-docker() {
  if ! which docker; then
    curl -fsSL https://get.docker.com -o /tmp/get-docker.sh
    sudo sh /tmp/get-docker.sh
    sudo service docker start
  fi
}

function start-container() {
  sudo docker run -it -d \
    --name $CONTAINER \
    --mount type=bind,src=/workspace,dst=/workspace \
    --mount type=bind,src=/tmp,dst=/tmphost \
    nixos/nix 2>/dev/null

  sudo docker start $CONTAINER 2>/dev/null
}

function run-in-container() {
  sudo docker exec -it $CONTAINER \
    nix-shell -p expect tmux --command "$(cat << EOF
export IN_DOCKER=1 \
&& cd /workspace \
&& tmux new -A -s agent \; set-buffer "($@ | tee /tmphost/agentout.txt);tmux detach" \; paste-buffer
EOF
)"
  cat /tmp/agentout.txt
}

function get-raw-nix-expr() {
  if [[ -z "$@" ]]; then
    echo -n "collective-lib._tests.run {}"
  else
    echo -n "$@"
  fi
}

function wrap-nix-expr() {
  cat << EOF
let
  pkgs = import <nixpkgs> {};
  lib = pkgs.lib;
  collective-lib = import ./pkgs/collective-lib {
    inherit pkgs lib;
    traceOpts = {
      traceLevel = 0;
      enablePartialTrace = false;
      enableVerboseTrace = false;
      enableShortTrace = false;
    };
  };
in
  with collective-lib;
  lib.traceSeq ($@) {}
EOF
}

function run-in-nix-eval() {
  nix eval --show-trace --expr "$@"
}

function run-in-nix-repl() {
  EXPECT_SCRIPT=$(cat << EOF
spawn nix repl --show-trace
expect "nix-repl> "
send "$@\r" 
expect "nix-repl> "
send ""
interact
EOF
)
  expect -c "$EXPECT_SCRIPT"
}

if [[ "$IN_DOCKER" == 1 ]]; then
  RAW_EXPR=$(get-raw-nix-expr $@)
  EXPR=$(wrap-nix-expr "$RAW_EXPR")
  echo "Running in nix repl:" >&2
  echo "$EXPR" >&2
  run-in-nix-eval "$EXPR" 
else
  install-docker
  start-container
  run-in-container "$0 $@"
fi

